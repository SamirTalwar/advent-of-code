var assembly = typeof(Program).Assembly;

if (args.Length == 0)
{
    throw new Exception(string.Format("Usage: {0} DAY [PART]", assembly.GetName().Name));
}
var day = int.Parse(args[0]);
var part = int.Parse(args[1]);
var className = string.Format("AdventOfCode2024.Day{0:D2}Part{1}", day, part);

var entrypointType = assembly.GetType(className);
if (entrypointType is null)
{
    throw new NullReferenceException(string.Format("No such type: {0}", className));
}
var entrypointMethod = entrypointType.GetMethod("Run");
if (entrypointMethod is null)
{
    throw new NullReferenceException(string.Format("No Run method in: {0}", entrypointType));
}

var entrypointArgs = new string[args.Length - 2];
Array.Copy(args, 2, entrypointArgs, 0, entrypointArgs.Length);

var watch = System.Diagnostics.Stopwatch.StartNew();
entrypointMethod.Invoke(null, new object[] { entrypointArgs });
watch.Stop();

Console.Error.WriteLine("Completed in {0}ms.", watch.ElapsedMilliseconds);
