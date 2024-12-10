using System.Collections;

namespace AdventOfCode2024;

public class Grid2D<T> : IEnumerable<KeyValuePair<Point2D, T>>
{
    private readonly T[,] items;

    public int Rows { get; private init; }
    public int Columns { get; private init; }

    public Grid2D(T[,] items)
    {
        this.items = items;
        this.Rows = items.GetLength(0);
        this.Columns = items.GetLength(1);
    }

    public T this[Point2D point] => items[point.Y, point.X];

    public bool Contains(Point2D point) => point.Y >= 0 && point.Y < Rows && point.X >= 0 && point.X < Columns;

    public SortedSet<Point2D> NeighborsOf(Point2D position) =>
        new SortedSet<Point2D>(
            Point2D.Neighbors
                .Select(direction => position + direction)
                .Where(Contains)
        );

    public IEnumerable<Point2D> Points
    {
        get
        {
            for (var y = 0; y < Rows; y++)
            {
                for (var x = 0; x < Columns; x++)
                {
                    yield return new Point2D { Y = y, X = x };
                }
            }
        }
    }

    IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable<KeyValuePair<Point2D, T>>)this).GetEnumerator();

    IEnumerator<KeyValuePair<Point2D, T>> IEnumerable<KeyValuePair<Point2D, T>>.GetEnumerator() =>
        Points.Select(point => KeyValuePair.Create(point, items[point.Y, point.X])).GetEnumerator();

    public Grid2D<U> Convert<U>(Converter<T, U> convert)
    {
        var convertedItems = new U[Rows, Columns];
        foreach (var (y, x) in Points)
        {
            convertedItems[y, x] = convert(items[y, x]);
        }
        return new Grid2D<U>(convertedItems);
    }
}
