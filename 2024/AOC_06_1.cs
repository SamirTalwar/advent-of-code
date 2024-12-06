using System.Collections.Concurrent;

class AOC_06_1
{
    public static void Main(string[] args)
    {
        var grid = new Grid(Input.Grid());

        var result = grid.Run();

        Console.WriteLine("{0}", result);
    }

    struct Guard
    {
        public Point2D Position { get; set; }
        public Direction Direction { get; set; }
    }

    enum Direction
    {
        Up,
        Down,
        Left,
        Right,
    }

    class Grid
    {
        static readonly SortedSet<int> emptySet = new();

        Guard guard = new Guard { Direction = Direction.Up };
        int rows;
        int columns;
        ConcurrentDictionary<int, SortedSet<int>> obstaclesByY = new();
        ConcurrentDictionary<int, SortedSet<int>> obstaclesByX = new();

        public Grid(char[,] grid)
        {
            rows = grid.GetLength(0);
            columns = grid.GetLength(1);

            var obstacles = new HashSet<Point2D>();
            foreach (var row in Enumerable.Range(0, rows))
            {
                foreach (var column in Enumerable.Range(0, columns))
                {
                    switch (grid[row, column])
                    {
                        case '^':
                            guard.Position = new Point2D { Y = row, X = column };
                            break;
                        case '#':
                            obstacles.Add(new Point2D { Y = row, X = column });
                            break;
                    }
                }
            }

            foreach (var obstacle in obstacles)
            {
                obstaclesByY.AddOrUpdate(obstacle.Y, _ => new SortedSet<int> { obstacle.X }, (_, xs) => { xs.Add(obstacle.X); return xs; });
                obstaclesByX.AddOrUpdate(obstacle.X, _ => new SortedSet<int> { obstacle.Y }, (_, ys) => { ys.Add(obstacle.Y); return ys; });
            }
        }

        public int Run()
        {
            var allVisited = new HashSet<Point2D>();
            var running = true;
            while (running)
            {
                ISet<Point2D> visited;
                (running, visited) = Hop();
                allVisited.UnionWith(visited);
            }
            return allVisited.Count;
        }

        (bool, ISet<Point2D>) Hop()
        {
            switch (guard.Direction)
            {
                case Direction.Up:
                    {
                        var view = (obstaclesByX.GetValue(guard.Position.X) ?? emptySet).GetViewBetween(0, guard.Position.Y);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Last() + 1 : 0;
                        var visited = Enumerable.Range(newPositionY, guard.Position.Y - newPositionY + 1).Select(y => new Point2D { Y = y, X = guard.Position.X }).ToHashSet();
                        guard.Position = new Point2D { Y = newPositionY, X = guard.Position.X };
                        guard.Direction = Direction.Right;
                        return (running, visited);
                    }
                case Direction.Down:
                    {
                        var view = (obstaclesByX.GetValue(guard.Position.X) ?? emptySet).GetViewBetween(guard.Position.Y, rows);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.First() - 1 : rows - 1;
                        var visited = Enumerable.Range(guard.Position.Y, newPositionY - guard.Position.Y + 1).Select(y => new Point2D { Y = y, X = guard.Position.X }).ToHashSet();
                        guard.Position = new Point2D { Y = newPositionY, X = guard.Position.X };
                        guard.Direction = Direction.Left;
                        return (running, visited);
                    }
                case Direction.Left:
                    {
                        var view = (obstaclesByY.GetValue(guard.Position.Y) ?? emptySet).GetViewBetween(0, guard.Position.X);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Last() + 1 : 0;
                        var visited = Enumerable.Range(newPositionX, guard.Position.X - newPositionX + 1).Select(x => new Point2D { Y = guard.Position.Y, X = x }).ToHashSet();
                        guard.Position = new Point2D { Y = guard.Position.Y, X = newPositionX };
                        guard.Direction = Direction.Up;
                        return (running, visited);
                    }
                case Direction.Right:
                    {
                        var view = (obstaclesByY.GetValue(guard.Position.Y) ?? emptySet).GetViewBetween(guard.Position.X, columns);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.First() - 1 : columns - 1;
                        var visited = Enumerable.Range(guard.Position.X, newPositionX - guard.Position.X + 1).Select(x => new Point2D { Y = guard.Position.Y, X = x }).ToHashSet();
                        guard.Position = new Point2D { Y = guard.Position.Y, X = newPositionX };
                        guard.Direction = Direction.Down;
                        return (running, visited);
                    }
                // This should not be necessary.
                default:
                    throw new Exception("no.");
            }
        }
    }
}
