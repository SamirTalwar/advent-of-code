namespace AdventOfCode2024;

class Day06Part1
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

    class Grid
    {
        private readonly int _rows;
        private readonly int _columns;
        private Guard _guard = new() { Direction = Direction.Up };
        private readonly MultiDictionary<int, int> _obstaclesByY = [];
        private readonly MultiDictionary<int, int> _obstaclesByX = [];

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

            foreach (var obstacle in obstacles)
            {
                _obstaclesByY.Add(obstacle.Y, obstacle.X);
                _obstaclesByX.Add(obstacle.X, obstacle.Y);
            }
        }

        public int Run()
        {
            var allVisited = new HashSet<Point2D>();
            var running = true;
            while (running)
            {
                (running, var visited) = Hop();
                allVisited.UnionWith(visited);
            }
            return allVisited.Count;
        }

        (bool, ISet<Point2D>) Hop()
        {
            switch (_guard.Direction)
            {
                case Direction.Up:
                    {
                        var view = _obstaclesByX.GetValueSet(_guard.Position.X).GetViewBetween(0, _guard.Position.Y);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Max + 1 : 0;
                        var visited = Enumerable.Range(newPositionY, _guard.Position.Y - newPositionY + 1).Select(y => _guard.Position with { Y = y }).ToHashSet();
                        _guard.Position = new Point2D { Y = newPositionY, X = _guard.Position.X };
                        _guard.Direction = Direction.Right;
                        return (running, visited);
                    }
                case Direction.Down:
                    {
                        var view = _obstaclesByX.GetValueSet(_guard.Position.X).GetViewBetween(_guard.Position.Y, _rows);
                        var running = view.Count > 0;
                        var newPositionY = running ? view.Min - 1 : _rows - 1;
                        var visited = Enumerable.Range(_guard.Position.Y, newPositionY - _guard.Position.Y + 1).Select(y => _guard.Position with { Y = y }).ToHashSet();
                        _guard.Position = new Point2D { Y = newPositionY, X = _guard.Position.X };
                        _guard.Direction = Direction.Left;
                        return (running, visited);
                    }
                case Direction.Left:
                    {
                        var view = _obstaclesByY.GetValueSet(_guard.Position.Y).GetViewBetween(0, _guard.Position.X);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Max + 1 : 0;
                        var visited = Enumerable.Range(newPositionX, _guard.Position.X - newPositionX + 1).Select(x => _guard.Position with { X = x }).ToHashSet();
                        _guard.Position = new Point2D { Y = _guard.Position.Y, X = newPositionX };
                        _guard.Direction = Direction.Up;
                        return (running, visited);
                    }
                case Direction.Right:
                    {
                        var view = _obstaclesByY.GetValueSet(_guard.Position.Y).GetViewBetween(_guard.Position.X, _columns);
                        var running = view.Count > 0;
                        var newPositionX = running ? view.Min - 1 : _columns - 1;
                        var visited = Enumerable.Range(_guard.Position.X, newPositionX - _guard.Position.X + 1).Select(x => _guard.Position with { X = x }).ToHashSet();
                        _guard.Position = new Point2D { Y = _guard.Position.Y, X = newPositionX };
                        _guard.Direction = Direction.Down;
                        return (running, visited);
                    }
                // This should not be necessary.
                default:
                    throw new Exception("no.");
            }
        }
    }
}
