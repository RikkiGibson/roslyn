﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Console;

namespace BuildValidator
{
    /// <summary>
    /// Build Validator enumerates the output of the Roslyn build, extracts the compilation options
    /// from the PE and attempts to rebuild the source using that information. It then checks
    /// that the new build output is the same as the original build
    /// </summary>
    class Program
    {
        const int ExitSuccess = 0;
        const int ExitFailure = 1;

        private static readonly Regex[] s_ignorePatterns = new Regex[]
        {
            new Regex(@"\\runtimes?\\"),
            new Regex(@"\\ref\\"),
            new Regex(@"\.resources?\.")
        };

        static Task<int> Main(string[] args)
        {
            var rootCommand = new RootCommand
            {
                new Option<string>(
                    "--assembliesPath", "Path to assemblies to rebuild"
                ) { IsRequired = true },
                new Option<string>(
                    "--sourcePath", "Path to sources to use in rebuild"
                ) { IsRequired = true },
                new Option<bool>(
                    "--verbose", "Output verbose log information"
                ),
                new Option<bool>(
                    "--quiet", "Do not output log information to console"
                ),
                new Option<bool>(
                    "--openDiff", "Open a diff tool when rebuild failures are found"
                ),
                new Option<string>(
                    "--debugPath", "(optional) Path to output debug visualization of the rebuild"
                )
            };
            rootCommand.Handler = CommandHandler.Create<string, string, bool, bool, bool, string>(HandleCommandAsync);
            return rootCommand.InvokeAsync(args);
        }

        static async Task<int> HandleCommandAsync(string assembliesPath, string sourcePath, bool verbose, bool quiet, bool openDiff, string? debugPath)
        {
            var options = new Options(assembliesPath, sourcePath, verbose, quiet, openDiff, debugPath);

            var loggerFactory = new LoggerFactory(
                new[] { new ConsoleLoggerProvider(new ConsoleLoggerSettings()) },
                new LoggerFilterOptions()
                {
                    MinLevel = options.Verbose ? LogLevel.Trace : LogLevel.Information
                });

            if (!options.Quiet)
            {
                loggerFactory.AddProvider(new DemoLoggerProvider());
            }

            try
            {
                var logger = loggerFactory.CreateLogger<Program>();
                var sourceResolver = new LocalSourceResolver(options, loggerFactory);
                var referenceResolver = new LocalReferenceResolver(options, loggerFactory);

                var buildConstructor = new BuildConstructor(referenceResolver, sourceResolver, logger);

                var artifactsDir = new DirectoryInfo(options.AssembliesPath);

                var filesToValidate = artifactsDir.EnumerateFiles("*.exe", SearchOption.AllDirectories)
                    .Concat(artifactsDir.EnumerateFiles("*.dll", SearchOption.AllDirectories))
                    .Distinct(FileNameEqualityComparer.Instance);

                var success = await ValidateFilesAsync(filesToValidate, buildConstructor, logger, options).ConfigureAwait(false);
                await Console.Out.FlushAsync().ConfigureAwait(false);
                return success ? ExitSuccess : ExitFailure;
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
                throw;
            }
        }

        // TODO: it feels like "logger" and "options" should be instance variables of something
        private static async Task<bool> ValidateFilesAsync(IEnumerable<FileInfo> originalBinaries, BuildConstructor buildConstructor, ILogger logger, Options options)
        {
            var assembliesCompiled = new List<CompilationDiff>();
            var sb = new StringBuilder();

            foreach (var file in originalBinaries)
            {
                var compilationDiff = await ValidateFileAsync(file, buildConstructor, logger, options).ConfigureAwait(false);

                if (compilationDiff is null)
                {
                    sb.AppendLine($"Ignoring {file.FullName}");
                    continue;
                }

                assembliesCompiled.Add(compilationDiff);
            }

            bool success = true;

            sb.AppendLine("====================");
            sb.AppendLine("Summary:");
            sb.AppendLine();
            sb.AppendLine("Successful Tests:");

            foreach (var diff in assembliesCompiled.Where(a => a.AreEqual == true))
            {
                sb.AppendLine($"\t{diff.OriginalPath}");
            }

            sb.AppendLine();

            sb.AppendLine("Failed Tests:");
            foreach (var diff in assembliesCompiled.Where(a => a.AreEqual == false))
            {
                sb.AppendLine($"\t{diff.OriginalPath}");
                success = false;
            }
            sb.AppendLine("====================");

            logger.LogInformation(sb.ToString());

            return success;
        }

        private static async Task<CompilationDiff?> ValidateFileAsync(FileInfo originalBinary, BuildConstructor buildConstructor, ILogger logger, Options options)
        {
            if (s_ignorePatterns.Any(r => r.IsMatch(originalBinary.FullName)))
            {
                logger.LogTrace($"Ignoring {originalBinary.FullName}");
                return null;
            }

            MetadataReaderProvider? pdbReaderProvider = null;

            try
            {
                // Find the embedded pdb
                using var originalBinaryStream = originalBinary.OpenRead();
                using var originalPeReader = new PEReader(originalBinaryStream);

                var pdbOpened = originalPeReader.TryOpenAssociatedPortablePdb(
                    peImagePath: originalBinary.FullName,
                    filePath => File.Exists(filePath) ? File.OpenRead(filePath) : null,
                    out pdbReaderProvider,
                    out var pdbPath);

                if (!pdbOpened || pdbReaderProvider is null)
                {
                    logger.LogError($"Could not find pdb for {originalBinary.FullName}");
                    return null;
                }

                logger.LogInformation($"Verifying {originalBinary.FullName} with pdb {pdbPath ?? "[embedded]"}");
                using var _ = logger.BeginScope("");

                var pdbReader = pdbReaderProvider.GetMetadataReader();
                var optionsReader = new CompilationOptionsReader(pdbReader, originalPeReader);

                var compilation = await buildConstructor.CreateCompilationAsync(
                    optionsReader,
                    Path.GetFileNameWithoutExtension(originalBinary.Name)).ConfigureAwait(false);

                var compilationDiff = CompilationDiff.Create(originalBinary, optionsReader, compilation, GetDebugEntryPoint(), options);
                logger.LogInformation(compilationDiff?.AreEqual == true ? "Verification succeeded" : "Verification failed");
                return compilationDiff;

                IMethodSymbol? GetDebugEntryPoint()
                {
                    var optionsReader = new CompilationOptionsReader(pdbReader, originalPeReader);
                    if (optionsReader.GetMainTypeName() is { } mainTypeName &&
                        optionsReader.GetMainMethodName() is { } mainMethodName)
                    {
                        var typeSymbol = compilation.GetTypeByMetadataName(mainTypeName);
                        if (typeSymbol is object)
                        {
                            var methodSymbols = typeSymbol
                                .GetMembers()
                                .OfType<IMethodSymbol>()
                                .Where(x => x.Name == mainMethodName);
                            return methodSymbols.FirstOrDefault();
                        }
                    }

                    return null;
                }
            }
            finally
            {
                pdbReaderProvider?.Dispose();
            }
        }
    }
}
