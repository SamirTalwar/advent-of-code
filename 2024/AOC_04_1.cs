class AOC_04_1
{
    static readonly char[] word = new char[] { 'X', 'M', 'A', 'S' };

    public static void Main(string[] args)
    {
        var grid = Input.Grid();

        var result = 0;
        result += Search(grid, 0, 1);
        result += Search(grid, 0, -1);
        result += Search(grid, 1, 0);
        result += Search(grid, -1, 0);
        result += Search(grid, 1, 1);
        result += Search(grid, -1, -1);
        result += Search(grid, -1, 1);
        result += Search(grid, 1, -1);

        Console.WriteLine("{0}", result);
    }

    static int Search(char[,] grid, int moveY, int moveX)
    {
        var count = 0;
        var rows = grid.GetLength(0);
        var columns = grid.GetLength(1);
        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < columns; x++)
            {
                if (CheckWord(grid, y, x, moveY, moveX))
                {
                    count += 1;
                }
            }
        }
        return count;
    }

    static bool CheckWord(char[,] grid, int y, int x, int moveY, int moveX)
    {
        try
        {
            foreach (var c in word)
            {
                if (grid[y, x] == c)
                {
                    y += moveY;
                    x += moveX;
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
