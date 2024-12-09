class AOC_02_2
{
    public static void Main(string[] args)
    {
        var inputs = Input.Lines().Select(line => line.Split().Select(int.Parse).ToList());

        var result = inputs.Count(Solve);

        Console.WriteLine("{0}", result);
    }

    static bool Solve(IList<int> input) =>
        RemoveOne(input).Any(numbers =>
        {
            var differences = numbers.Zip(numbers.Skip(1), (a, b) => b - a);
            var safelyIncreasing = differences.All(d => d >= 1 && d <= 3);
            var safelyDecreasing = differences.All(d => d >= -3 && d <= -1);
            return safelyIncreasing || safelyDecreasing;
        });

    static IEnumerable<IList<int>> RemoveOne(IList<int> input) =>
        Enumerable.Range(0, input.Count).Select(i =>
        {
            var list = new List<int>(input);
            list.RemoveRange(i, 1);
            return list;
        });
}
