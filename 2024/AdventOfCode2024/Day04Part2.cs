namespace AdventOfCode2024;

class Day04Part2
{
    static readonly char[] WordForwards = ['M', 'A', 'S'];
    static readonly char[] WordBackwards = ['S', 'A', 'M'];

    public static void Run(string[] args)
    {
        var grid = Input.Grid();

        var result = 0;
        result += Search(grid, WordForwards, WordForwards);
        result += Search(grid, WordBackwards, WordForwards);
        result += Search(grid, WordForwards, WordBackwards);
        result += Search(grid, WordBackwards, WordBackwards);

        Console.WriteLine("{0}", result);
    }

    static int Search(Grid2D<char> grid, char[] wordOne, char[] wordTwo) =>
        grid.Points.Count(point => CheckCross(grid, wordOne, wordTwo, point));

    static bool CheckCross(Grid2D<char> grid, char[] wordOne, char[] wordTwo, Point2D position)
    {
        try
        {
            foreach (var (i, c) in wordOne.Index())
            {
                if (grid[position + i] != c)
                {
                    return false;
                }
            }
            var yOffset = wordOne.Length - 1;
            foreach (var (i, c) in wordTwo.Index())
            {
                if (grid[position + new Point2D { Y = yOffset - i, X = i }] != c)
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
