// Ralph-wiggum loop driver for the nullable-migration worklist.
//
// Repeatedly invokes a non-interactive `copilot` sub-agent, one file per
// invocation, following nullable-migration/LOOP.md. Each invocation is told to
// use the LSP diagnostics daemon (nullable-migration/lsp/lsp_diagnostics.py)
// instead of `dotnet build` for its per-edit feedback loop:
//   - The daemon auto-starts on first use (loads CompilerConsumers.slnf once)
//     and stays resident across files/iterations, so only the very first
//     invocation of the whole loop pays the "cold start" cost.
//   - The agent is told NOT to stop the daemon between files, since later
//     iterations reuse it and are much faster while it's warm.
//
// Usage:
//   dotnet run nullable-migration/run_loop.cs [maxIterations]
//
// Stops when `loop.py next` reports no pending files, when a `copilot`
// invocation fails/errors, or after maxIterations (default: unlimited).
using System.Diagnostics;
using System.Text.Json;

var here = Path.GetDirectoryName(Path.GetFullPath(ThisFilePath()))!;
var repo = Path.GetFullPath(Path.Combine(here, ".."));
var logDir = Path.Combine(here, "logs");
Directory.CreateDirectory(logDir);

var maxIterations = args.Length > 0 && int.TryParse(args[0], out var m) ? m : 0; // 0 == unlimited

const string AgentPrompt = """
Follow nullable-migration/LOOP.md exactly, processing exactly ONE file (whatever
`python3 nullable-migration/loop.py next` gives you) end to end, then stop.

Feedback loop: use `python3 nullable-migration/lsp/lsp_diagnostics.py <file> [...]`
for reading CS8xxx diagnostics while iterating (per LOOP.md's "Feedback loop"
section) instead of running `dotnet build` after every edit. That script starts
its background daemon automatically the first time it's needed -- you do not
need to start it yourself, and you should NOT stop it when you're done (leave
it running so the next loop iteration can reuse it warm). Only fall back to a
real `dotnet build` for the baseline (step 3, optional) and the final
verification (step 8, required) -- and if you ever suspect the daemon's
diagnostics are stale or wrong, use `--restart` first before assuming that.

Do the full procedure: pick file, mark in-progress, remove directives, iterate
with the LSP daemon until clean, decide enable/island/defer, run final
verification build, record status, commit (or revert+record for a defer), then
stop. Do not process more than one file.
""";

Console.WriteLine($"Repo: {repo}");

var iteration = 0;
while (true)
{
    if (maxIterations > 0 && iteration >= maxIterations)
    {
        Console.WriteLine($"Reached max iterations ({maxIterations}); stopping.");
        break;
    }

    var path = RunCapture(repo, "python3", ["nullable-migration/loop.py", "next", "--path"]).Trim();
    if (string.IsNullOrEmpty(path))
    {
        Console.WriteLine("No pending files left; loop complete.");
        break;
    }

    iteration++;
    var logFile = Path.Combine(logDir, $"{DateTime.Now:yyyyMMdd-HHmmss}-{Path.GetFileName(path)}.log");
    Console.WriteLine($"=== Iteration {iteration}: {path} (log: {logFile}) ===");

    var exitCode = RunAgentWithTeeOutput(repo, logFile);
    if (exitCode != 0)
    {
        Console.WriteLine($"Agent invocation failed on {path} (exit {exitCode}); stopping loop. See {logFile}.");
        return 1;
    }

    var status = GetWorklistStatus(here, path);
    if (status == "pending")
    {
        Console.WriteLine($"Agent left {path} still pending; stopping loop to avoid spinning. See {logFile}.");
        return 1;
    }
    Console.WriteLine($"--- {path} -> {status} ---");
}

Console.WriteLine("Stopping the LSP daemon (loop finished).");
RunCapture(repo, "python3", ["nullable-migration/lsp/lsp_diagnostics.py", "--stop"]);
return 0;

static string ThisFilePath([System.Runtime.CompilerServices.CallerFilePath] string path = "") => path;

static string RunCapture(string workingDir, string fileName, string[] arguments)
{
    var psi = new ProcessStartInfo(fileName)
    {
        WorkingDirectory = workingDir,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false,
    };
    foreach (var a in arguments)
        psi.ArgumentList.Add(a);

    using var proc = Process.Start(psi)!;
    var stdout = proc.StandardOutput.ReadToEnd();
    proc.WaitForExit();
    return stdout;
}

int RunAgentWithTeeOutput(string workingDir, string logFile)
{
    var psi = new ProcessStartInfo("copilot")
    {
        WorkingDirectory = workingDir,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false,
    };
    psi.ArgumentList.Add("--allow-all-tools");
    psi.ArgumentList.Add("-p");
    psi.ArgumentList.Add(AgentPrompt);

    using var log = new StreamWriter(logFile, append: false) { AutoFlush = true };
    using var proc = new Process { StartInfo = psi };

    proc.OutputDataReceived += (_, e) =>
    {
        if (e.Data is null) return;
        Console.WriteLine(e.Data);
        log.WriteLine(e.Data);
    };
    proc.ErrorDataReceived += (_, e) =>
    {
        if (e.Data is null) return;
        Console.Error.WriteLine(e.Data);
        log.WriteLine(e.Data);
    };

    proc.Start();
    proc.BeginOutputReadLine();
    proc.BeginErrorReadLine();
    proc.WaitForExit();
    return proc.ExitCode;
}

static string GetWorklistStatus(string here, string path)
{
    var worklistPath = Path.Combine(here, "worklist.json");
    using var doc = JsonDocument.Parse(File.ReadAllText(worklistPath));
    foreach (var item in doc.RootElement.GetProperty("items").EnumerateArray())
    {
        if (item.GetProperty("path").GetString() == path)
            return item.GetProperty("status").GetString() ?? "unknown";
    }
    return "unknown";
}
