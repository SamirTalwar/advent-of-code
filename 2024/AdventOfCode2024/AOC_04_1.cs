class AOC_04_1
{
    static readonly char[] word = new char[] { 'X', 'M', 'A', 'S' };

    public static void Run(string[] args)
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

    static int Search(Grid2D<char> grid, Point2D move) =>
        grid.Points.Where(point => CheckWord(grid, point, move)).Count();

    static bool CheckWord(Grid2D<char> grid, Point2D position, Point2D move)
    {
        try
        {
            foreach (var c in word)
            {
                if (grid[position] == c)
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
