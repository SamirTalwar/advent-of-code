namespace AdventOfCode2024;

class Day10Part2
{
    public static void Run(string[] args)
    {
        var grid = Input.Grid().Convert(c => c - '0');

        var trailheads = grid.Where(x => x.Value == 0).Select(x => x.Key);

        var result = trailheads
            .SelectMany(trailhead => HikingTrailsFrom(trailhead, 0, grid))
            .Count();

        Console.WriteLine("{0}", result);
    }

    private static IEnumerable<IEnumerable<Point2D>> HikingTrailsFrom(Point2D position, int value, Grid2D<int> grid) =>
        value == 9
            ? new List<List<Point2D>> { new List<Point2D> { position } }
            : grid.NeighborsOf(position)
                .Where(neighbor => grid[neighbor] == value + 1)
                .SelectMany(neighbor => HikingTrailsFrom(neighbor, value + 1, grid).Select(trail => trail.Prepend(position)));
}
