class AOC_06_2
{
    public static void Run(string[] args)
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

    class Obstacles
    {
        static readonly SortedSet<int> emptySet = new();

        HashSet<Point2D> positions;
        MultiDictionary<int, int> obstaclesByY = new();
        MultiDictionary<int, int> obstaclesByX = new();

        public Obstacles(ISet<Point2D> positions)
        {
            this.positions = new(positions);

            foreach (var position in positions)
            {
                obstaclesByY.Add(position.Y, position.X);
                obstaclesByX.Add(position.X, position.Y);
            }
        }

        public SortedSet<int> ByY(int y) => obstaclesByY.GetValueSet(y);
        public SortedSet<int> ByX(int x) => obstaclesByX.GetValueSet(x);

        public Obstacles WithExtra(Point2D extraPosition) =>
            new Obstacles(positions.Union(new HashSet<Point2D> { extraPosition }).ToHashSet());
    }

    class Grid
    {
        private readonly int rows;
        private readonly int columns;
        private readonly Guard guard = new Guard { Direction = Direction.Up };
        private readonly Obstacles obstacles;

        public Grid(Grid2D<char> grid)
        {
            this.rows = grid.Rows;
            this.columns = grid.Columns;

            var obstacles = new HashSet<Point2D>();
            foreach (var (point, value) in grid)
            {
                switch (value)
                {
                    case '^':
                        guard.Position = point;
                        break;
                    case '#':
                        obstacles.Add(point);
                        break;
                }
            }
            this.obstacles = new Obstacles(obstacles);
        }

        public int Run()
        {
            var potentialObstacles = new HashSet<Point2D>();
            {
                var running = true;
                var currentGuard = guard;
                while (running)
                {
                    IEnumerable<Point2D> visited;
                    (running, currentGuard, visited) = Hop(currentGuard, obstacles);
                    potentialObstacles.UnionWith(visited);
                }
            }
            potentialObstacles.Remove(guard.Position);

            return
                potentialObstacles
                    .Select(obstacle =>
                    {
                        var os = obstacles.WithExtra(obstacle);
                        var running = true;
                        var guards = new HashSet<Guard> { guard };
                        var currentGuard = guard;
                        while (running)
                        {
                            (running, currentGuard, _) = Hop(currentGuard, os);
                            if (guards.Contains(currentGuard))
                            {
                                return true;
                            }
                            guards.Add(currentGuard);
                        }
                        return false;
                    })
                    .Count(x => x);
        }

        (bool, Guard, IEnumerable<Point2D>) Hop(Guard guard, Obstacles obstacles)
        {
            switch (guard.Direction)
            {
                case Direction.Up:
                    {
                        var view = obstacles.ByX(guard.Position.X).GetViewBetween(0, guard.Position.Y);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Max + 1 : 0;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = newPositionY, X = guard.Position.X },
                            Direction = Direction.Right,
                        };
                        var visited = Enumerable.Range(newPositionY, guard.Position.Y - newPositionY + 1).Select(y => new Point2D { Y = y, X = guard.Position.X });
                        return (running, newGuard, visited);
                    }
                case Direction.Down:
                    {
                        var view = obstacles.ByX(guard.Position.X).GetViewBetween(guard.Position.Y, rows);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Min - 1 : rows - 1;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = newPositionY, X = guard.Position.X },
                            Direction = Direction.Left,
                        };
                        var visited = Enumerable.Range(guard.Position.Y, newPositionY - guard.Position.Y + 1).Select(y => new Point2D { Y = y, X = guard.Position.X });
                        return (running, newGuard, visited);
                    }
                case Direction.Left:
                    {
                        var view = obstacles.ByY(guard.Position.Y).GetViewBetween(0, guard.Position.X);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Max + 1 : 0;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = guard.Position.Y, X = newPositionX },
                            Direction = Direction.Up,
                        };
                        var visited = Enumerable.Range(newPositionX, guard.Position.X - newPositionX + 1).Select(x => new Point2D { Y = guard.Position.Y, X = x });
                        return (running, newGuard, visited);
                    }
                case Direction.Right:
                    {
                        var view = obstacles.ByY(guard.Position.Y).GetViewBetween(guard.Position.X, columns);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Min - 1 : columns - 1;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = guard.Position.Y, X = newPositionX },
                            Direction = Direction.Down,
                        };
                        var visited = Enumerable.Range(guard.Position.X, newPositionX - guard.Position.X + 1).Select(x => new Point2D { Y = guard.Position.Y, X = x });
                        return (running, newGuard, visited);
                    }
                // This should not be necessary.
                default:
                    throw new Exception("no.");
            }
        }
    }
}
