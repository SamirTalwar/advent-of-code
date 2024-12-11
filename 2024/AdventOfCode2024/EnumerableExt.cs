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
        return new Enumerator(value, next);
    }

    IEnumerator IEnumerable.GetEnumerator() =>
        (this as IEnumerable<T>).GetEnumerator();

    class Enumerator(T value, Func<T, T> next) : IEnumerator<T>
    {
        private readonly T _value = value;
        private bool _started;

        public T Current { get; private set; } = value;

        object? IEnumerator.Current => Current;

        public bool MoveNext()
        {
            if (_started)
            {
                Current = next(Current);
            }
            else
            {
                _started = true;
            }

            return true;
        }

        public void Reset()
        {
            Current = _value;
        }

        public void Dispose()
        {
        }
    }
}