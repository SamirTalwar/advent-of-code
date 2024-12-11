namespace AdventOfCode2024;

class Day08Part1
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
                .Select(pair => pair.Item2 + (pair.Item2 - pair.Item1))
                .Where(grid.Contains)
        );

        var result = antinodes.Count;

        Console.WriteLine("{0}", result);
    }
}
