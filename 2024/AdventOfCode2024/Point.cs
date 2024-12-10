public struct Point2D : IComparable
{
    public int Y { get; init; }
    public int X { get; init; }

    public static Point2D operator +(Point2D a, Point2D b) =>
        new Point2D { Y = a.Y + b.Y, X = a.X + b.X };

    public static Point2D operator +(Point2D point, int value) =>
        new Point2D { Y = point.Y + value, X = point.X + value };

    public static Point2D operator -(Point2D a, Point2D b) =>
        new Point2D { Y = a.Y - b.Y, X = a.X - b.X };

    public override bool Equals(object? obj) => obj is Point2D other && this.Equals(other);

    public bool Equals(Point2D p) => Y == p.Y && X == p.X;

    public static bool operator ==(Point2D lhs, Point2D rhs) => lhs.Equals(rhs);

    public static bool operator !=(Point2D lhs, Point2D rhs) => !lhs.Equals(rhs);

    public override int GetHashCode() => (Y, X).GetHashCode();

    public int CompareTo(object? obj) => obj is Point2D other ? (this.Y, this.X).CompareTo((other.Y, other.X)) : 1;

    public override string ToString() => string.Format("({0},{1})", X, Y);
}
