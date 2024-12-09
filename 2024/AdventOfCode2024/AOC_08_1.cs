class AOC_08_1
{
    public static void Run(string[] args)
    {
        var grid = Input.Grid();
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        var antennas = new MultiDictionary<char, Point2D>();
        foreach (var y in Enumerable.Range(0, rows))
        {
            foreach (var x in Enumerable.Range(0, columns))
            {
                var frequency = grid[y, x];
                if (frequency != '.')
                {
                    antennas.Add(frequency, new Point2D { Y = y, X = x });
                }
            }
        }

        var antinodes = new SortedSet<Point2D>(
            antennas.Keys
                .Select(frequency => antennas.GetValues(frequency))
                .SelectMany(positions => positions.SelectMany(a => positions.Select(b => (a, b))))
                .Where(pair => pair.Item1 != pair.Item2)
                .Select(pair => pair.Item2 + (pair.Item2 - pair.Item1))
                .Where(antinode => antinode.Y >= 0 && antinode.Y < rows && antinode.X >= 0 && antinode.X < columns)
        );

        var result = antinodes.Count;

        Console.WriteLine("{0}", result);
    }
}
