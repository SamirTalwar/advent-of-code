namespace AdventOfCode2024;

class Debug
{
    public static void Value(Object? value)
    {
        Console.Error.WriteLine(value);
    }

    public static void Lines(IEnumerable<string> lines)
    {
        foreach (var line in lines)
        {
            Console.Error.WriteLine(line);
        }
    }

    public static void List<T>(ICollection<T>? list)
    {
        if (list == null)
        {
            Console.Error.WriteLine("null");
            return;
        }
        Console.Error.Write("[");
        using var enumerator = list.GetEnumerator();
        if (enumerator.MoveNext())
        {
            Console.Error.Write(enumerator.Current);
            while (enumerator.MoveNext())
            {
                Console.Error.Write(", ");
                Console.Error.Write(enumerator.Current);
            }
        }
        Console.Error.WriteLine("]");
    }

    public static void Dictionary<TKey, TValue>(IDictionary<TKey, TValue>? dictionary)
    {
        if (dictionary == null)
        {
            Console.Error.WriteLine("null");
            return;
        }
        Console.Error.Write("[");
        using var enumerator = dictionary.GetEnumerator();
        if (enumerator.MoveNext())
        {
            Console.Error.Write(enumerator.Current.Key);
            Console.Error.Write(" -> ");
            Console.Error.Write(enumerator.Current.Value);
            while (enumerator.MoveNext())
            {
                Console.Error.Write(", ");
                Console.Error.Write(enumerator.Current.Key);
                Console.Error.Write(" -> ");
                Console.Error.Write(enumerator.Current.Value);
            }
        }
        Console.Error.WriteLine("]");
    }

    public static void Grid(char[,] grid)
    {
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < columns; x++)
            {
                Console.Error.Write("{0}", grid[y, x]);
            }
            Console.Error.WriteLine();
        }
    }
}
