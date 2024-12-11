using System.Text.RegularExpressions;

namespace AdventOfCode2024;

class Day01Part2
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

        var left = inputs.Select(input => input.Item1);
        var right = inputs.CountBy(input => input.Item2).ToDictionary();

        var result = left.Select(n => n * right.GetValue(n)).Sum();

        Console.WriteLine("{0}", result);
    }
}
