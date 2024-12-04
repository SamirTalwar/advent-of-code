class AOC_04_2
{
    static readonly char[] wordForwards = new char[] { 'M', 'A', 'S' };
    static readonly char[] wordBackwards = new char[] { 'S', 'A', 'M' };

    public static void Main(string[] args)
    {
        var grid = Input.Grid();

        var result = 0;
        result += Search(grid, wordForwards, wordForwards);
        result += Search(grid, wordBackwards, wordForwards);
        result += Search(grid, wordForwards, wordBackwards);
        result += Search(grid, wordBackwards, wordBackwards);

        Console.WriteLine("{0}", result);
    }

    static int Search(char[,] grid, char[] wordOne, char[] wordTwo)
    {
        var count = 0;
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < columns; x++)
            {
                if (CheckCross(grid, wordOne, wordTwo, y, x))
                {
                    count += 1;
                }
            }
        }
        return count;
    }

    static bool CheckCross(char[,] grid, char[] wordOne, char[] wordTwo, int y, int x)
    {
        try
        {
            foreach (var (i, c) in wordOne.Index())
            {
                if (!(grid[y + i, x + i] == c))
                {
                    return false;
                }
            }
            var yOffset = wordOne.Length - 1;
            foreach (var (i, c) in wordTwo.Index())
            {
                if (!(grid[y + yOffset - i, x + i] == c))
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
