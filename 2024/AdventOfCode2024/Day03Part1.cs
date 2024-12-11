using System.Text.RegularExpressions;

namespace AdventOfCode2024;

class Day03Part1
{
    public static void Run(string[] args)
    {
        var parser = new Regex(@"mul\((\d+),(\d+)\)");
        var input = Input.String();
        var instructions = parser.Matches(input).Select(match =>
            new Multiply(int.Parse(match.Groups[1].Value), int.Parse(match.Groups[2].Value))
        );

        var result = instructions.Select(i => i.Run()).Sum();

        Console.WriteLine("{0}", result);
    }

    // ReSharper disable once InconsistentNaming
    private interface Instruction
    {
        public int Run();
    }

    private readonly struct Multiply(int a, int b) : Instruction
    {
        public int Run() => a * b;
    }
}
