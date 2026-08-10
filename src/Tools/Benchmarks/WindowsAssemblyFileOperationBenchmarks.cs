// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Loader;
using System.Text;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Filters;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.Win32.SafeHandles;

namespace Benchmarks;

[Config(typeof(Config))]
public class WindowsAssemblyFileOperationBenchmarks
{
    private class Config : ManualConfig
    {
        public Config()
        {
            AddFilter(new SimpleFilter(_ => RuntimeInformation.IsOSPlatform(OSPlatform.Windows)));
        }
    }

    private const int FileOperationCount = 16;

    private string _sourcePath = null!;
    private string _destinationDirectory = null!;
    private string[] _destinationPaths = null!;
    private int _destinationIndex;
    private IAntimalware _antimalware = null!;

    [GlobalSetup]
    public void GlobalSetup()
    {
        if (!OperatingSystem.IsWindows())
        {
            throw new PlatformNotSupportedException("Hard-link creation is benchmarked through the Windows API.");
        }

        _sourcePath = typeof(CSharpCompilation).Assembly.Location;
        var temporaryPath = Path.GetTempPath();
        if (!string.Equals(Path.GetPathRoot(_sourcePath), Path.GetPathRoot(temporaryPath), StringComparison.OrdinalIgnoreCase))
        {
            throw new InvalidOperationException("The assembly and temporary directory must be on the same volume.");
        }

        _destinationDirectory = Path.Combine(
            temporaryPath,
            $"{nameof(WindowsAssemblyFileOperationBenchmarks)}-{Guid.NewGuid():N}");
        Directory.CreateDirectory(_destinationDirectory);

        var antimalwareType = Type.GetTypeFromCLSID(new Guid("fdb00e52-a214-4aa1-8fba-4357bb0072ec"), throwOnError: true)!;
        _antimalware = (IAntimalware)Activator.CreateInstance(antimalwareType)!;
    }

    [IterationSetup]
    public void IterationSetup()
    {
        _destinationPaths = new string[FileOperationCount];
        for (var i = 0; i < _destinationPaths.Length; i++)
        {
            _destinationPaths[i] = Path.Combine(_destinationDirectory, $"destination-{_destinationIndex++}.dll");
        }
    }

    [IterationCleanup]
    public void IterationCleanup()
    {
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        foreach (var path in _destinationPaths)
        {
            try
            {
                File.Delete(path);
            }
            catch (UnauthorizedAccessException)
            {
            }
        }
    }

    [GlobalCleanup]
    public void GlobalCleanup()
    {
        if (_antimalware is not null && OperatingSystem.IsWindows())
        {
            Marshal.FinalReleaseComObject(_antimalware);
        }

        try
        {
            Directory.Delete(_destinationDirectory, recursive: true);
        }
        catch (UnauthorizedAccessException)
        {
        }
    }

    [Benchmark(OperationsPerInvoke = FileOperationCount)]
    public void CopyAssembly()
    {
        foreach (var path in _destinationPaths)
        {
            File.Copy(_sourcePath, path);
        }
    }

    [Benchmark(OperationsPerInvoke = FileOperationCount)]
    public void HardLinkAssembly()
    {
        foreach (var path in _destinationPaths)
        {
            CreateHardLink(path);
        }
    }

    [Benchmark(Baseline = true, OperationsPerInvoke = FileOperationCount)]
    public int CopyAndLoadAssembly()
    {
        var result = 0;
        foreach (var path in _destinationPaths)
        {
            File.Copy(_sourcePath, path);
            result += LoadAssembly(path);
        }

        return result;
    }

    [Benchmark(OperationsPerInvoke = FileOperationCount)]
    public int ScanCopyAndLoadAssembly()
    {
        var result = 0;
        foreach (var path in _destinationPaths)
        {
            ScanFile(_sourcePath);
            File.Copy(_sourcePath, path);
            result += LoadAssembly(path);
        }

        return result;
    }

    [Benchmark(OperationsPerInvoke = FileOperationCount)]
    public int HardLinkAndLoadAssembly()
    {
        var result = 0;
        foreach (var path in _destinationPaths)
        {
            CreateHardLink(path);
            result += LoadAssembly(path);
        }

        return result;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static int LoadAssembly(string path)
    {
        var loadContext = new AssemblyLoadContext(path, isCollectible: true);
        try
        {
            Assembly assembly = loadContext.LoadFromAssemblyPath(path);
            return assembly.ManifestModule.ModuleVersionId.GetHashCode();
        }
        finally
        {
            loadContext.Unload();
        }
    }

    private void CreateHardLink(string path)
    {
        if (!CreateHardLink(path, _sourcePath, IntPtr.Zero))
        {
            throw new Win32Exception(Marshal.GetLastWin32Error());
        }
    }

    private void ScanFile(string path)
    {
        using var stream = new AmsiFileStream(path);
        var hr = _antimalware.Scan(stream, out var result, out var provider);
        if (provider != IntPtr.Zero)
        {
            Marshal.Release(provider);
        }

        Marshal.ThrowExceptionForHR(hr);
        if (result >= AmsiResult.BlockedByAdminStart)
        {
            throw new InvalidOperationException($"AMSI blocked or detected '{path}' with result {result}.");
        }
    }

    private enum AmsiAttribute
    {
        AppName,
        ContentName,
        ContentSize,
        ContentAddress,
        Session,
    }

    private enum AmsiResult
    {
        BlockedByAdminStart = 0x4000,
    }

    [ComImport]
    [Guid("82d29c2e-f062-44e6-b5c9-3d9a2f24a2df")]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    private interface IAntimalware
    {
        [PreserveSig]
        int Scan([MarshalAs(UnmanagedType.Interface)] IAmsiStream stream, out AmsiResult result, out IntPtr provider);

        void CloseSession(ulong session);
    }

    [ComVisible(true)]
    [Guid("3e47f2e5-81d4-4d3b-897f-545096770373")]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    private interface IAmsiStream
    {
        [PreserveSig]
        int GetAttribute(AmsiAttribute attribute, uint dataSize, IntPtr data, out uint retData);

        [PreserveSig]
        int Read(ulong position, uint size, IntPtr buffer, out uint readSize);
    }

    [ComVisible(true)]
    [ClassInterface(ClassInterfaceType.None)]
    private sealed class AmsiFileStream : IAmsiStream, IDisposable
    {
        private const int E_NOTIMPL = unchecked((int)0x80004001);
        private const int E_NOT_SUFFICIENT_BUFFER = unchecked((int)0x8007007A);

        private static readonly byte[] s_appName = Encoding.Unicode.GetBytes($"{nameof(WindowsAssemblyFileOperationBenchmarks)}\0");

        private readonly SafeFileHandle _fileHandle;
        private readonly byte[] _contentName;
        private readonly byte[] _contentSize;

        public AmsiFileStream(string path)
        {
            _fileHandle = File.OpenHandle(path, FileMode.Open, FileAccess.Read, FileShare.Read);
            _contentName = Encoding.Unicode.GetBytes(path + '\0');
            _contentSize = BitConverter.GetBytes((ulong)RandomAccess.GetLength(_fileHandle));
        }

        public int GetAttribute(AmsiAttribute attribute, uint dataSize, IntPtr data, out uint retData)
        {
            return attribute switch
            {
                AmsiAttribute.AppName => CopyAttribute(s_appName, dataSize, data, out retData),
                AmsiAttribute.ContentName => CopyAttribute(_contentName, dataSize, data, out retData),
                AmsiAttribute.ContentSize => CopyAttribute(_contentSize, dataSize, data, out retData),
                AmsiAttribute.Session => CopyAttribute(new byte[IntPtr.Size], dataSize, data, out retData),
                _ => NotImplemented(out retData),
            };
        }

        public int Read(ulong position, uint size, IntPtr buffer, out uint readSize)
        {
            var overlapped = new Overlapped
            {
                Offset = (uint)position,
                OffsetHigh = (uint)(position >> 32),
            };

            if (!ReadFile(_fileHandle, buffer, size, out readSize, ref overlapped))
            {
                return Marshal.GetHRForLastWin32Error();
            }

            return 0;
        }

        public void Dispose()
            => _fileHandle.Dispose();

        private static int CopyAttribute(byte[] value, uint dataSize, IntPtr data, out uint retData)
        {
            retData = (uint)value.Length;
            if (dataSize < value.Length)
            {
                return E_NOT_SUFFICIENT_BUFFER;
            }

            Marshal.Copy(value, 0, data, value.Length);
            return 0;
        }

        private static int NotImplemented(out uint retData)
        {
            retData = 0;
            return E_NOTIMPL;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct Overlapped
        {
            public IntPtr Internal;
            public IntPtr InternalHigh;
            public uint Offset;
            public uint OffsetHigh;
            public IntPtr EventHandle;
        }

        [DllImport("Kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool ReadFile(
            SafeFileHandle file,
            IntPtr buffer,
            uint numberOfBytesToRead,
            out uint numberOfBytesRead,
            ref Overlapped overlapped);
    }

    [DllImport("Kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern bool CreateHardLink(string lpFileName, string lpExistingFileName, IntPtr lpSecurityAttributes);
}
