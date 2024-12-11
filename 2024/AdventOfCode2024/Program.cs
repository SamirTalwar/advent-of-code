var assembly = typeof(Program).Assembly;

if (args.Length == 0)
{
    throw new Exception($"Usage: {assembly.GetName().Name} DAY [PART]");
}
var day = int.Parse(args[0]);
var part = int.Parse(args[1]);
var className = $"AdventOfCode2024.Day{day:D2}Part{part}";

var entrypointType = assembly.GetType(className);
if (entrypointType is null)
{
    throw new NullReferenceException($"No such type: {className}");
}
var entrypointMethod = entrypointType.GetMethod("Run");
if (entrypointMethod is null)
{
    throw new NullReferenceException($"No Run method in: {entrypointType}");
}

var entrypointArgs = new string[args.Length - 2];
Array.Copy(args, 2, entrypointArgs, 0, entrypointArgs.Length);

var watch = System.Diagnostics.Stopwatch.StartNew();
entrypointMethod.Invoke(null, [entrypointArgs]);
watch.Stop();

Console.Error.WriteLine("Completed in {0}ms.", watch.ElapsedMilliseconds);
