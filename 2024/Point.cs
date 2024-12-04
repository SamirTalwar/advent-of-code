struct Point2D
{
    public int Y { get; init; }
    public int X { get; init; }

    public static Point2D operator +(Point2D a, Point2D b) =>
        new Point2D { Y = a.Y + b.Y, X = a.X + b.X };

    public static Point2D operator +(Point2D point, int value) =>
        new Point2D { Y = point.Y + value, X = point.X + value };

    public T In<T>(T[,] grid) =>
        grid[Y, X];
}
