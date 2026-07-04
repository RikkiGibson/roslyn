#!/usr/bin/env python3
"""
Fast, reliable nullable (CS8xxx) diagnostics for a single file via the Roslyn
language server, without a full `dotnet build`.

Problem this solves
--------------------
`get_errors`/pull-diagnostics against the whole `Roslyn.slnx` is slow to load
and, per nullable-migration/LOOP.md, unreliable for multi-targeted compiler
projects: the "current" project context silently reverts to `netstandard2.0`
(where Nullable warnings are suppressed), giving false "clean" results.

Fix: explicitly pin the LSP `textDocument/diagnostic` pull request to the
`net10.0` project context using the VS LSP extension field
`_vs_projectContext` (see src/LanguageServer/Protocol/Handler/Diagnostics and
Protocol/Extensions/VSTextDocumentIdentifier.cs). This bypasses the "current
context" heuristic entirely, so results are deterministic regardless of what
the editor's active context happens to be.

Also loads only `CompilerConsumers.slnf` (not the full `Roslyn.slnx`), so
project load is much cheaper, and keeps the language server running as a
background daemon so repeated edits are cheap (only a didChange + a diagnostic
pull, no reload).

Usage
-----
    python3 nullable-migration/lsp/lsp_diagnostics.py <file.cs> [more.cs ...]
    python3 nullable-migration/lsp/lsp_diagnostics.py --stop     # stop the daemon
    python3 nullable-migration/lsp/lsp_diagnostics.py --restart <file.cs>

The first call starts a background daemon (loads CompilerConsumers.slnf once);
subsequent calls reuse it and are fast. The daemon auto-idles; use --stop when
you're done with a session, or --restart if source files were added/removed
in a way the daemon won't pick up automatically.
"""
from __future__ import annotations

import json
import os
import socket
import subprocess
import sys
import time
import urllib.parse

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SLNF_PATH = os.path.join(REPO_ROOT, "CompilerConsumers.slnf")
STATE_DIR = "/tmp/roslyn-nullable-lsp"
SOCK_PATH = os.path.join(STATE_DIR, "daemon.sock")
LOG_PATH = os.path.join(STATE_DIR, "daemon.log")
PID_PATH = os.path.join(STATE_DIR, "daemon.pid")
ROSLYN_LS_VERSION = "5.9.0-1.26303.1"


def _path_to_uri(path: str) -> str:
    return "file://" + urllib.parse.quote(os.path.abspath(path))


def _is_daemon_alive() -> bool:
    if not os.path.exists(SOCK_PATH):
        return False
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
            s.settimeout(2)
            s.connect(SOCK_PATH)
            s.sendall(b'{"cmd":"ping"}\n')
            resp = s.recv(4096)
            return b'"ok"' in resp
    except OSError:
        return False


def _stop_daemon() -> None:
    if os.path.exists(PID_PATH):
        try:
            pid = int(open(PID_PATH).read().strip())
            os.kill(pid, 15)
            print(f"Stopped daemon (pid {pid}).")
        except (ValueError, ProcessLookupError, OSError):
            pass
    for p in (SOCK_PATH, PID_PATH):
        if os.path.exists(p):
            os.remove(p)


def _ensure_daemon(verbose: bool = True) -> None:
    os.makedirs(STATE_DIR, exist_ok=True)
    if _is_daemon_alive():
        return
    # Clean up stale socket/pid from a crashed daemon.
    for p in (SOCK_PATH, PID_PATH):
        if os.path.exists(p):
            os.remove(p)
    if verbose:
        print(f"Starting LSP daemon (loading {os.path.basename(SLNF_PATH)})...", file=sys.stderr)
    log = open(LOG_PATH, "a")
    proc = subprocess.Popen(
        [sys.executable, os.path.abspath(__file__), "--daemon"],
        stdout=log,
        stderr=log,
        stdin=subprocess.DEVNULL,
        cwd=REPO_ROOT,
        start_new_session=True,
    )
    with open(PID_PATH, "w") as f:
        f.write(str(proc.pid))
    # Wait for the daemon to finish loading the solution (can take ~30-90s the
    # first time; much faster once dnx has the tool cached).
    deadline = time.time() + 240
    while time.time() < deadline:
        if _is_daemon_alive():
            if verbose:
                print("Daemon ready.", file=sys.stderr)
            return
        if proc.poll() is not None:
            raise RuntimeError(
                f"Daemon process exited early (code {proc.returncode}); see {LOG_PATH}"
            )
        time.sleep(1)
    raise RuntimeError(f"Timed out waiting for daemon to become ready; see {LOG_PATH}")


def _request_diagnostics(files: list[str]) -> dict:
    abs_files = [os.path.abspath(f) for f in files]
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.settimeout(120)
        s.connect(SOCK_PATH)
        s.sendall((json.dumps({"cmd": "diagnostics", "files": abs_files}) + "\n").encode())
        chunks = []
        while True:
            chunk = s.recv(65536)
            if not chunk:
                break
            chunks.append(chunk)
        return json.loads(b"".join(chunks).decode())


_SEVERITY_NAMES = {1: "error", 2: "warning", 3: "info", 4: "hint"}


def _print_diagnostics(result: dict) -> int:
    total = 0
    for file_result in result.get("results", []):
        path = file_result["file"]
        if file_result.get("error"):
            print(f"{path}: ERROR: {file_result['error']}")
            continue
        diags = file_result.get("diagnostics", [])
        for d in diags:
            sev = _SEVERITY_NAMES.get(d.get("severity"), "?")
            line = d["range"]["start"]["line"] + 1
            col = d["range"]["start"]["character"] + 1
            code = d.get("code", "")
            msg = d.get("message", "")
            print(f"{path}({line},{col}): {sev} {code}: {msg}")
        total += len(diags)
    print(f"\n{total} diagnostic(s) total.")
    return total


def _client_main(argv: list[str]) -> int:
    if "--stop" in argv:
        _stop_daemon()
        return 0
    restart = "--restart" in argv
    if restart:
        argv = [a for a in argv if a != "--restart"]
        _stop_daemon()
    files = argv
    if not files:
        print(__doc__)
        return 1
    _ensure_daemon()
    result = _request_diagnostics(files)
    if result.get("error"):
        print(f"Daemon error: {result['error']}", file=sys.stderr)
        return 1
    total = _print_diagnostics(result)
    return 1 if total > 0 else 0


# ----------------------------------------------------------------------------
# Daemon implementation
# ----------------------------------------------------------------------------

class LspClient:
    """Minimal JSON-RPC (LSP framing) client speaking to roslyn-language-server over stdio."""

    def __init__(self, proc: subprocess.Popen):
        self.proc = proc
        self._next_id = 1
        self._notification_handlers = {}

    def _write(self, obj: dict) -> None:
        data = json.dumps(obj).encode("utf-8")
        header = f"Content-Length: {len(data)}\r\n\r\n".encode("ascii")
        self.proc.stdin.write(header + data)
        self.proc.stdin.flush()

    def _read_message(self) -> dict:
        headers = {}
        while True:
            line = self.proc.stdout.readline()
            if not line:
                raise EOFError("Language server stdout closed")
            line = line.decode("ascii").strip()
            if line == "":
                break
            key, _, value = line.partition(":")
            headers[key.strip().lower()] = value.strip()
        length = int(headers["content-length"])
        body = self.proc.stdout.read(length)
        return json.loads(body.decode("utf-8"))

    def notify(self, method: str, params: dict | None = None) -> None:
        msg = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            msg["params"] = params
        self._write(msg)

    def request(self, method: str, params: dict | None, timeout: float = 60.0) -> dict:
        req_id = self._next_id
        self._next_id += 1
        msg = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params is not None:
            msg["params"] = params
        self._write(msg)
        deadline = time.time() + timeout
        while time.time() < deadline:
            msg = self._read_message()
            if msg.get("id") == req_id:
                if "error" in msg:
                    raise RuntimeError(f"{method} failed: {msg['error']}")
                return msg.get("result")
            else:
                self._dispatch_unsolicited(msg)
        raise TimeoutError(f"Timed out waiting for response to {method}")

    def wait_for_notification(self, method: str, timeout: float) -> dict | None:
        deadline = time.time() + timeout
        while time.time() < deadline:
            msg = self._read_message()
            if msg.get("method") == method:
                return msg.get("params")
            self._dispatch_unsolicited(msg)
        return None

    def _dispatch_unsolicited(self, msg: dict) -> None:
        method = msg.get("method")
        if method in self._notification_handlers:
            self._notification_handlers[method](msg.get("params"))
        # else: ignore server->client requests/notifications we don't care about
        # (window/logMessage, window/workDoneProgress/create, etc.)
        if "id" in msg and method:
            # Server sent a request expecting a response (e.g. workspace/configuration).
            # Respond with a generic empty/null result so it doesn't hang.
            self._write({"jsonrpc": "2.0", "id": msg["id"], "result": None})


def _start_language_server() -> subprocess.Popen:
    return subprocess.Popen(
        [
            "dotnet", "dnx", "roslyn-language-server",
            "--version", ROSLYN_LS_VERSION,
            "--yes",
            "--",
            "--stdio",
            "--logLevel", "Warning",
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        cwd=REPO_ROOT,
    )


def _daemon_main() -> None:
    proc = _start_language_server()
    client = LspClient(proc)

    root_uri = _path_to_uri(REPO_ROOT)
    client.request(
        "initialize",
        {
            "processId": os.getpid(),
            "rootUri": root_uri,
            "workspaceFolders": [{"uri": root_uri, "name": os.path.basename(REPO_ROOT)}],
            "capabilities": {},
            "initializationOptions": {"disableAutoConfigureNuGetSourceOnClient": True},
        },
        timeout=120,
    )
    client.notify("initialized", {})

    # Load only CompilerConsumers.slnf (not the full Roslyn.slnx) to keep project
    # load fast, and disable auto-load-projects discovery (we open the slnf ourselves).
    client.notify("solution/open", {"solution": _path_to_uri(SLNF_PATH)})

    # Wait for the initial project load to finish before accepting requests.
    client.wait_for_notification("workspace/projectInitializationComplete", timeout=240)

    # uri -> {"version": int, "text": str}. The server only supports incremental
    # sync (see DefaultCapabilitiesProvider.cs: Change = TextDocumentSyncKind.Incremental),
    # so didChange must always carry an explicit range (a bare {"text": ...} with no
    # range crashes DidChangeHandler.GetUpdatedSourceText with a NullReferenceException).
    # We work around this by sending a range that spans the entire previously-known
    # document text, effectively doing a full-document replace over the incremental API.
    open_docs: dict[str, dict] = {}
    project_context_cache: dict[str, dict] = {}

    def _end_position(text: str) -> dict:
        lines = text.split("\n")
        last_line = lines[-1]
        return {"line": len(lines) - 1, "character": len(last_line)}

    def get_pinned_context(uri: str) -> dict | None:
        if uri in project_context_cache:
            return project_context_cache[uri]
        result = client.request(
            "textDocument/_vs_getProjectContexts",
            {"_vs_textDocument": {"uri": uri}},
        )
        if not result or not result.get("_vs_projectContexts"):
            return None
        contexts = result["_vs_projectContexts"]
        # Prefer a non-netstandard context (i.e. the net10.0/NetRoslynSourceBuild TFM),
        # where nullable warnings are actually enabled.
        chosen = next(
            (c for c in contexts if "netstandard" not in c.get("_vs_label", "").lower()),
            contexts[0],
        )
        project_context_cache[uri] = chosen
        return chosen

    def get_diagnostics_for_file(path: str) -> dict:
        uri = _path_to_uri(path)
        with open(path, "r", encoding="utf-8") as f:
            text = f.read()

        if uri not in open_docs:
            client.notify(
                "textDocument/didOpen",
                {
                    "textDocument": {
                        "uri": uri,
                        "languageId": "csharp",
                        "version": 1,
                        "text": text,
                    }
                },
            )
            open_docs[uri] = {"version": 1, "text": text}
        else:
            state = open_docs[uri]
            if text == state["text"]:
                # No change since last time; skip didChange (still fine to re-pull diagnostics).
                pass
            else:
                state["version"] += 1
                old_end = _end_position(state["text"])
                client.notify(
                    "textDocument/didChange",
                    {
                        "textDocument": {"uri": uri, "version": state["version"]},
                        "contentChanges": [
                            {
                                "range": {
                                    "start": {"line": 0, "character": 0},
                                    "end": old_end,
                                },
                                "text": text,
                            }
                        ],
                    },
                )
                state["text"] = text

        text_document = {"uri": uri}
        ctx = get_pinned_context(uri)
        if ctx is not None:
            text_document["_vs_projectContext"] = ctx

        result = client.request("textDocument/diagnostic", {"textDocument": text_document}, timeout=90)
        items = (result or {}).get("items", [])
        return {"file": path, "diagnostics": items, "context": ctx.get("_vs_label") if ctx else None}

    # ---- Unix socket server loop ----
    if os.path.exists(SOCK_PATH):
        os.remove(SOCK_PATH)
    server_sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server_sock.bind(SOCK_PATH)
    server_sock.listen(8)

    while True:
        conn, _ = server_sock.accept()
        try:
            data = b""
            conn.settimeout(30)
            while b"\n" not in data:
                chunk = conn.recv(65536)
                if not chunk:
                    break
                data += chunk
            request = json.loads(data.decode().strip())
            if request.get("cmd") == "ping":
                conn.sendall(b'{"status":"ok"}')
                continue
            if request.get("cmd") == "diagnostics":
                results = []
                for f in request["files"]:
                    try:
                        results.append(get_diagnostics_for_file(f))
                    except Exception as e:  # noqa: BLE001
                        results.append({"file": f, "error": str(e)})
                conn.sendall(json.dumps({"results": results}).encode())
        except Exception as e:  # noqa: BLE001
            try:
                conn.sendall(json.dumps({"error": str(e)}).encode())
            except OSError:
                pass
        finally:
            conn.close()


if __name__ == "__main__":
    if "--daemon" in sys.argv:
        _daemon_main()
    else:
        sys.exit(_client_main(sys.argv[1:]))
