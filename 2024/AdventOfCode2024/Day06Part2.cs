namespace AdventOfCode2024;

class Day06Part2
{
    public static void Run(string[] args)
    {
        var grid = new Grid(Input.Grid());

        var result = grid.Run();

        Console.WriteLine("{0}", result);
    }

    private record struct Guard
    {
        public Point2D Position { get; set; }
        public Direction Direction { get; init; }
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
        private readonly HashSet<Point2D> _positions;
        private readonly MultiDictionary<int, int> _obstaclesByY = new();
        private readonly MultiDictionary<int, int> _obstaclesByX = new();

        public Obstacles(ISet<Point2D> positions)
        {
            _positions = [..positions];

            foreach (var position in positions)
            {
                _obstaclesByY.Add(position.Y, position.X);
                _obstaclesByX.Add(position.X, position.Y);
            }
        }

        public SortedSet<int> ByY(int y) => _obstaclesByY.GetValueSet(y);
        public SortedSet<int> ByX(int x) => _obstaclesByX.GetValueSet(x);

        public Obstacles WithExtra(Point2D extraPosition) =>
            new(_positions.Union(new HashSet<Point2D> { extraPosition }).ToHashSet());
    }

    class Grid
    {
        private readonly int _rows;
        private readonly int _columns;
        private readonly Guard _guard = new() { Direction = Direction.Up };
        private readonly Obstacles _obstacles;

        public Grid(Grid2D<char> grid)
        {
            _rows = grid.Rows;
            _columns = grid.Columns;

            var obstacles = new HashSet<Point2D>();
            foreach (var (point, value) in grid)
            {
                switch (value)
                {
                    case '^':
                        _guard.Position = point;
                        break;
                    case '#':
                        obstacles.Add(point);
                        break;
                }
            }
            _obstacles = new Obstacles(obstacles);
        }

        public int Run()
        {
            var potentialObstacles = new HashSet<Point2D>();
            {
                var running = true;
                var currentGuard = _guard;
                while (running)
                {
                    (running, currentGuard, var visited) = Hop(currentGuard, _obstacles);
                    potentialObstacles.UnionWith(visited);
                }
            }
            potentialObstacles.Remove(_guard.Position);

            return
                potentialObstacles
                    .Select(obstacle =>
                    {
                        var os = _obstacles.WithExtra(obstacle);
                        var running = true;
                        var guards = new HashSet<Guard> { _guard };
                        var currentGuard = _guard;
                        while (running)
                        {
                            (running, currentGuard, _) = Hop(currentGuard, os);
                            if (!guards.Add(currentGuard))
                            {
                                return true;
                            }
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
                        var visited = Enumerable.Range(newPositionY, guard.Position.Y - newPositionY + 1).Select(y => guard.Position with { Y = y });
                        return (running, newGuard, visited);
                    }
                case Direction.Down:
                    {
                        var view = obstacles.ByX(guard.Position.X).GetViewBetween(guard.Position.Y, _rows);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Min - 1 : _rows - 1;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = newPositionY, X = guard.Position.X },
                            Direction = Direction.Left,
                        };
                        var visited = Enumerable.Range(guard.Position.Y, newPositionY - guard.Position.Y + 1).Select(y => guard.Position with { Y = y });
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
                        var visited = Enumerable.Range(newPositionX, guard.Position.X - newPositionX + 1).Select(x => guard.Position with { X = x });
                        return (running, newGuard, visited);
                    }
                case Direction.Right:
                    {
                        var view = obstacles.ByY(guard.Position.Y).GetViewBetween(guard.Position.X, _columns);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Min - 1 : _columns - 1;
                        var newGuard = new Guard
                        {
                            Position = new Point2D { Y = guard.Position.Y, X = newPositionX },
                            Direction = Direction.Down,
                        };
                        var visited = Enumerable.Range(guard.Position.X, newPositionX - guard.Position.X + 1).Select(x => guard.Position with { X = x });
                        return (running, newGuard, visited);
                    }
                // This should not be necessary.
                default:
                    throw new Exception("no.");
            }
        }
    }
}
