class Debug
{
    public static void Value(Object value)
    {
        Console.WriteLine(value);
    }

    public static void Lines(IEnumerable<string> lines)
    {
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
    }

    public static void Grid(char[,] grid)
    {
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < columns; x++)
            {
                Console.Write("{0}", grid[y, x]);
            }
            Console.WriteLine();
        }
    }
}
