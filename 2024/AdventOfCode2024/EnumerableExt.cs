using System.Collections;

namespace AdventOfCode2024;

public static class EnumerableExt
{
    public static IEnumerable<T> Expand<T>(this T value, Func<T, T> next) =>
        new Expand<T>(value, next);
}

class Expand<T>(T value, Func<T, T> next) : IEnumerable<T>
{
    public IEnumerator<T> GetEnumerator()
    {
        var current = value;
        while (true)
        {
            yield return current;
            current = next(current);
        }
        // ReSharper disable once IteratorNeverReturns
    }

    IEnumerator IEnumerable.GetEnumerator() =>
        (this as IEnumerable<T>).GetEnumerator();
}