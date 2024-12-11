namespace AdventOfCode2024;

class Day08Part2
{
    public static void Run(string[] args)
    {
        var grid = Input.Grid();
        var antennas = new MultiDictionary<char, Point2D>();
        foreach (var (point, frequency) in grid)
        {
            if (frequency != '.')
            {
                antennas.Add(frequency, point);
            }
        }

        var antinodes = new SortedSet<Point2D>(
            antennas.Keys
                .Select(frequency => antennas.GetValueSet(frequency))
                .SelectMany(positions => positions.SelectMany(a => positions.Select(b => (a, b))))
                .Where(pair => pair.Item1 != pair.Item2)
                .SelectMany(pair =>
                {
                    var (a, b) = pair;
                    return a.Expand(x => x + (a - b)).TakeWhile(grid.Contains)
                        .Concat(b.Expand(x => x + (b - a)).TakeWhile(grid.Contains));
                })
        );

        var result = antinodes.Count;

        Console.WriteLine("{0}", result);
    }
}
