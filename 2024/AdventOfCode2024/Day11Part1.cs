namespace AdventOfCode2024;

public class Day11Part1
{
    public static void Run(string[] args)
    {
        var nth = args.Length > 0 ? int.Parse(args[0]) : 25;
        var input = Input.String().Split(" ").Select(long.Parse).ToList();

        var iterations = (input as IEnumerable<long>).Expand(Blink);
        var result = iterations.Skip(nth).First().LongCount();

        Console.WriteLine("{0}", result);
    }

    private static IEnumerable<long> Blink(IEnumerable<long> arrangement) =>
        arrangement.SelectMany(Blink);

    private static IEnumerable<long> Blink(long rock)
    {
        if (rock == 0)
        {
            return [1];
        }

        var digits = (int)Math.Log10(rock) + 1;
        if (digits % 2 == 1)
        {
            return [rock * 2024];
        }

        var divisor = 10L.Pow(digits / 2);
        var left = rock / divisor;
        var right = rock % divisor;
        return [left, right];
    }
}
