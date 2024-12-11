namespace AdventOfCode2024;

static class Input
{
    public static string String()
    {
        using var reader = new StreamReader(Console.OpenStandardInput(), Console.InputEncoding);
        return reader.ReadToEnd();
    }

    public static IEnumerable<string> Lines()
    {
        while (Console.ReadLine() is { } line)
        {
            yield return line;
        }
    }

    public static Grid2D<char> Grid()
    {
        var rows = Lines().ToList();

        if (rows.Count == 0)
        {
            throw new Exception("No input.");
        }
        var columnCount = rows[0].Length;

        var grid = new char[rows.Count, columnCount];
        foreach (var (i, row) in rows.Index())
        {
            Buffer.BlockCopy(row.ToCharArray(), 0, grid, columnCount * i * sizeof(char), columnCount * sizeof(char));
        }
        return new Grid2D<char>(grid);
    }
}
