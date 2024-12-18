using System.Text.RegularExpressions;

namespace AdventOfCode2024;

class Day01Part1
{
    public static void Run(string[] args)
    {
        var parser = new Regex(@"(\d+) +(\d+)");
        var inputs =
            Input.Lines()
                .Select(line =>
                {
                    var parsed = parser.Match(line);
                    return (int.Parse(parsed.Groups[1].Value), int.Parse(parsed.Groups[2].Value));
                })
                .ToList();

        var left = inputs.Select(input => input.Item1).ToList();
        var right = inputs.Select(input => input.Item2).ToList();
        left.Sort();
        right.Sort();

        var result =
            left.Zip(right)
                .Select(pair => Math.Abs(pair.Item1 - pair.Item2))
                .Sum();

        Console.WriteLine("{0}", result);
    }
}