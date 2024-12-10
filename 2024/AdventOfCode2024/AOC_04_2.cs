class AOC_04_2
{
    static readonly char[] wordForwards = new char[] { 'M', 'A', 'S' };
    static readonly char[] wordBackwards = new char[] { 'S', 'A', 'M' };

    public static void Run(string[] args)
    {
        var grid = Input.Grid();

        var result = 0;
        result += Search(grid, wordForwards, wordForwards);
        result += Search(grid, wordBackwards, wordForwards);
        result += Search(grid, wordForwards, wordBackwards);
        result += Search(grid, wordBackwards, wordBackwards);

        Console.WriteLine("{0}", result);
    }

    static int Search(Grid2D<char> grid, char[] wordOne, char[] wordTwo) =>
        grid.Points.Where(point => CheckCross(grid, wordOne, wordTwo, point)).Count();

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
