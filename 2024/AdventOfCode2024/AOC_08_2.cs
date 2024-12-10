class AOC_08_2
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

        var inBounds = (Point2D point) => point.Y >= 0 && point.Y < grid.Rows && point.X >= 0 && point.X < grid.Columns;

        var antinodes = new SortedSet<Point2D>(
            antennas.Keys
                .Select(frequency => antennas.GetValues(frequency))
                .SelectMany(positions => positions.SelectMany(a => positions.Select(b => (a, b))))
                .Where(pair => pair.Item1 != pair.Item2)
                .SelectMany(pair =>
                {
                    var (a, b) = pair;
                    return a.Expand(x => x + (a - b)).TakeWhile(inBounds)
                        .Concat(b.Expand(x => x + (b - a)).TakeWhile(inBounds));
                })
        );

        var result = antinodes.Count;

        Console.WriteLine("{0}", result);
    }
}
