using System.Collections.Concurrent;

namespace AdventOfCode2024;

public class Day12Part1
{
    public static void Run(string[] args)
    {
        var grid = Input.Grid();

        var regions = FindRegions(grid);
        var fences = new ConcurrentDictionary<Point2D, (int, int)>();
        foreach (var (position, region) in grid)
        {
            var leader = regions[position];
            var fencesRequired =
                Point2D.Neighbors
                    .Select(direction => position + direction)
                    .Count(n => !grid.Contains(n) || grid[n] != region);
            fences.AddOrUpdate(leader, _ => (1, fencesRequired), (_, data) =>
            {
                var area = data.Item1 + 1;
                var perimeter = data.Item2 + fencesRequired;
                return (area, perimeter);
            });
        }

        var costs = fences.Select(pair => pair.Value.Item1 * pair.Value.Item2);
        var result = costs.Sum();

        Console.WriteLine(result);
    }

    private static IDictionary<Point2D, Point2D> FindRegions(Grid2D<char> grid)
    {
        var leaders = new Dictionary<Point2D, Point2D>();
        foreach (var (position, region) in grid)
        {
            if (leaders.ContainsKey(position))
            {
                continue;
            }

            var pointsInRegion = new SortedSet<Point2D>();
            CrawlRegion(grid, position, region, pointsInRegion);
            foreach (var point in pointsInRegion)
            {
                leaders[point] = position;
            }
        }

        return leaders;
    }

    private static void CrawlRegion(Grid2D<char> grid, Point2D position, char region, SortedSet<Point2D> inRegion)
    {
        if (!inRegion.Add(position))
        {
            return;
        }

        var neighbors = grid.NeighborsOf(position);
        foreach (var neighbor in neighbors.Where(n => grid[n] == region))
        {
            CrawlRegion(grid, neighbor, region, inRegion);
        }
    }
}
