namespace AdventOfCode2024;

public class Day11Part2
{
    public static void Run(string[] args)
    {
        var nth = args.Length > 0 ? int.Parse(args[0]) : 75;
        var input = Input.String().Split(" ").Select(long.Parse).ToList();

        var solver = new Solver();
        var result = solver.BlinkAndCount(input, nth);

        Console.WriteLine("{0}", result);
    }

    private class Solver
    {
        private readonly IDictionary<(long, int), long> _memo = new Dictionary<(long, int), long>();

        public long BlinkAndCount(IEnumerable<long> arrangement, int iterations) =>
            arrangement.Select(r => BlinkAndCount(r, iterations)).Sum();

        long BlinkAndCount(long rock, int iterations)
        {
            if (iterations == 0)
            {
                return 1;
            }

            var memoizedResult = _memo.GetValue((rock, iterations));
            if (memoizedResult > 0)
            {
                return memoizedResult;
            }

            var result = BlinkAndCount(Blink(rock), iterations - 1);
            _memo[(rock, iterations)] = result;
            return result;
        }

        static IEnumerable<long> Blink(long rock)
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
}
