using System.Text.RegularExpressions;

class AOC_01_2
{
    public static void Main(string[] args)
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

        var result = left.Select(n => n * right.GetValue(n, 0)).Sum();

        Console.WriteLine("{0}", result);
    }
}
