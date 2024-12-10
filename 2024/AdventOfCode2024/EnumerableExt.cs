using System.Collections;

namespace AdventOfCode2024;

public static class EnumerableExt
{
    public static IEnumerable<T> Expand<T>(this T value, Func<T, T> next) =>
        new Expand<T>(value, next);
}

class Expand<T> : IEnumerable<T>
{
    private T value;
    private Func<T, T> next;

    public Expand(T value, Func<T, T> next)
    {
        this.value = value;
        this.next = next;
    }

    public IEnumerator<T> GetEnumerator()
    {
        return new Enumerator(value, next);
    }

    IEnumerator IEnumerable.GetEnumerator() =>
        (this as IEnumerable<T>).GetEnumerator();

    class Enumerator : IEnumerator<T>
    {
        private T value;
        private Func<T, T> next;
        private bool started;

        public T Current { get; private set; }

        public Enumerator(T value, Func<T, T> next)
        {
            this.value = value;
            this.next = next;
            this.started = false;
            Current = value;
        }

        object? IEnumerator.Current => Current;

        public bool MoveNext()
        {
            if (started)
            {
                Current = next(Current);
                return true;
            }
            else
            {
                started = true;
                return true;
            }
        }

        public void Reset()
        {
            Current = value;
        }

        public void Dispose() { }
    }
}

