namespace AdventOfCode2024;

class Day02Part1
{
    public static void Run(string[] args)
    {
        var inputs = Input.Lines().Select(line => line.Split().Select(int.Parse).ToList());

        var result = inputs.Count(Solve);

        Console.WriteLine("{0}", result);
    }

    static bool Solve(IList<int> input)
    {
        var differences = input.Zip(input.Skip(1), (a, b) => b - a);
        var safelyIncreasing = differences.All(d => d >= 1 && d <= 3);
        var safelyDecreasing = differences.All(d => d >= -3 && d <= -1);
        return safelyIncreasing || safelyDecreasing;
    }
}
