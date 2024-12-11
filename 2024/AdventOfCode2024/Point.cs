namespace AdventOfCode2024;

public readonly record struct Point2D(int Y, int X) : IComparable
{
    public static Point2D Up => new Point2D { Y = -1, X = 0 };
    public static Point2D Down => new Point2D { Y = 1, X = 0 };
    public static Point2D Left => new Point2D { Y = 0, X = -1 };
    public static Point2D Right => new Point2D { Y = 0, X = 1 };
    public static IReadOnlyList<Point2D> Neighbors => new List<Point2D> { Up, Down, Left, Right };

    public static Point2D operator +(Point2D a, Point2D b) =>
        new() { Y = a.Y + b.Y, X = a.X + b.X };

    public static Point2D operator +(Point2D point, int value) =>
        new() { Y = point.Y + value, X = point.X + value };

    public static Point2D operator -(Point2D a, Point2D b) =>
        new() { Y = a.Y - b.Y, X = a.X - b.X };

    public int CompareTo(object? obj) => obj is Point2D other ? (Y, X).CompareTo((other.Y, other.X)) : 1;

    public override string ToString() => $"({X},{Y})";
}
