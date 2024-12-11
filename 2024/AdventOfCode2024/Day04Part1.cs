namespace AdventOfCode2024;

class Day04Part1
{
    static readonly char[] Word = ['X', 'M', 'A', 'S'];

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
        grid.Points.Count(point => CheckWord(grid, point, move));

    static bool CheckWord(Grid2D<char> grid, Point2D position, Point2D move)
    {
        try
        {
            foreach (var c in Word)
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
