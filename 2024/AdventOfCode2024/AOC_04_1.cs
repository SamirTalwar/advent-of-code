class AOC_04_1
{
    static readonly char[] word = new char[] { 'X', 'M', 'A', 'S' };

    public static void Main(string[] args)
    {
        var grid = Input.Grid();

        var result = 0;
        result += Search(grid, new Point2D { Y = 0, X = 1 });
        result += Search(grid, new Point2D { Y = 0, X = -1 });
        result += Search(grid, new Point2D { Y = 1, X = 0 });
        result += Search(grid, new Point2D { Y = -1, X = 0 });
        result += Search(grid, new Point2D { Y = 1, X = 1 });
        result += Search(grid, new Point2D { Y = -1, X = -1 });
        result += Search(grid, new Point2D { Y = -1, X = 1 });
        result += Search(grid, new Point2D { Y = 1, X = -1 });

        Console.WriteLine("{0}", result);
    }

    static int Search(char[,] grid, Point2D move)
    {
        var count = 0;
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < columns; x++)
            {
                if (CheckWord(grid, new Point2D { Y = y, X = x }, move))
                {
                    count += 1;
                }
            }
        }
        return count;
    }

    static bool CheckWord(char[,] grid, Point2D position, Point2D move)
    {
        try
        {
            foreach (var c in word)
            {
                if (position.In(grid) == c)
                {
                    position += move;
                }
                else
                {
                    return false;
                }
            }
            return true;
        }
        catch (IndexOutOfRangeException)
        {
            return false;
        }
    }
}
