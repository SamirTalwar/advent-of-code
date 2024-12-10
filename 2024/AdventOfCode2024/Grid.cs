using System.Collections;

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
}
