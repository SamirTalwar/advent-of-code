using System.Collections.Concurrent;

namespace AdventOfCode2024;

public class Day12Part2
{
    private static readonly IEnumerable<Fence> NeighboringFences =
    [
        new(Point2D.Up, FenceDirection.Up),
        new(Point2D.Down, FenceDirection.Down),
        new(Point2D.Left, FenceDirection.Left),
        new(Point2D.Right, FenceDirection.Right),
    ];

    public static void Run(string[] args)
    {
        var grid = Input.Grid();

        var regions = FindRegions(grid);
        var areas = new ConcurrentDictionary<Point2D, int>();
        var fencesByRegion = new MultiDictionary<Point2D, Fence>();
        foreach (var (position, region) in grid)
        {
            var leader = regions[position];
            var newFences =
                NeighboringFences
                    .Where(fence =>
                        !grid.Contains(position + fence.Position) || grid[position + fence.Position] != region)
                    .Select(fence => fence with { Position = position });
            areas.AddOrUpdate(leader, _ => 1, (_, current) => current + 1);
            fencesByRegion.AddMultiple(leader, newFences);
        }

        var resultPerRegion = fencesByRegion.Keys.Select(leader =>
        {
            var area = areas[leader];
            var fences = fencesByRegion.GetValueSet(leader);
            var sides = new MultiDictionary<Fence, Fence>();
            foreach (var fence in fences)
            {
                var currentLeader = fence;
                var previousDirection = fence.Direction switch
                {
                    FenceDirection.Up => Point2D.Left,
                    FenceDirection.Down => Point2D.Left,
                    FenceDirection.Left => Point2D.Up,
                    FenceDirection.Right => Point2D.Up,
                    _ => throw new IndexOutOfRangeException()
                };
                while (true)
                {
                    var previous = currentLeader with { Position = currentLeader.Position + previousDirection };
                    if (fences.Contains(previous))
                    {
                        currentLeader = previous;
                    }
                    else
                    {
                        sides.Add(currentLeader, fence);
                        break;
                    }
                }
            }

            return area * sides.Keys.Count();
        });

        var result = resultPerRegion.Sum();

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

    private readonly record struct Fence(Point2D Position, FenceDirection Direction) : IComparable<Fence>
    {
        public int CompareTo(Fence other) => (Position, Direction).CompareTo((other.Position, other.Direction));
    }

    enum FenceDirection
    {
        Up,
        Down,
        Left,
        Right,
    }
}
