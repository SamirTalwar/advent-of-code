using Pidgin;
using static Pidgin.Parser;

namespace AdventOfCode2024;

public class Day14Part2
{
    private const int Rewind = -2; // Magic number; found through trial and error on my input.

    public static void Run(string[] args)
    {
        var (columns, rows) = args.Length >= 2 ? (int.Parse(args[0]), int.Parse(args[1])) : (101, 103);
        var robots = Parse.Robots.ParseOrThrow(Input.String()).ToList();
        var robotCount = robots.Count();

        var start = robots.Select(r => r.Hop(rows, columns, steps: Rewind));

        var iterations =
            start
                .Expand(rs => rs.Select(r => r.Hop(rows, columns, steps: columns)))
                .Index()
                .Where(iteration =>
                {
                    // Check that there are no overlapping robots.
                    var positions = iteration.Item.Select(r => r.Position).ToHashSet();
                    return positions.Count == robotCount;
                });

        var (index, item) = iterations.First();
        var result = index * columns + Rewind;

        using (var stream = new StreamWriter(Console.OpenStandardError()))
        {
            RenderGrid(rows, columns, item, stream);
            stream.WriteLine();
        }

        Console.WriteLine("{0}", result);
    }

    private static void RenderGrid(int rows, int columns, IEnumerable<Robot> robots, TextWriter output)
    {
        var grid = new char[rows * columns];
        Array.Fill(grid, '.');
        foreach (var robot in robots)
        {
            grid[robot.Position.Y * columns + robot.Position.X] = 'x';
        }

        foreach (var row in Enumerable.Range(0, rows))
        {
            output.WriteLine("{0}", new string(grid, row * columns, columns));
        }
    }

    // `%` is the remainder, not the modulus, so a negative input will result in a negative output.
    private static int Mod(int a, int b)
    {
        while (a < 0)
        {
            a += b;
        }

        return a % b;
    }

    readonly record struct Robot(Point2D Position, Point2D Velocity)
    {
        public Robot Hop(int rows, int columns, int steps)
        {
            var newY = Mod(Position.Y + Velocity.Y * steps, rows);
            var newX = Mod(Position.X + Velocity.X * steps, columns);
            return this with { Position = new Point2D(Y: newY, X: newX) };
        }
    }

    private static class Parse
    {
        public static Parser<char, IEnumerable<Robot>> Robots =>
            Robot.SeparatedAndOptionallyTerminated(EndOfLine);

        // ReSharper disable once MemberHidesStaticFromOuterClass
        private static Parser<char, Robot> Robot =>
            from _1 in String("p=")
            from positionX in Num
            from _2 in String(",")
            from positionY in Num
            from _3 in String(" v=")
            from velocityX in Num
            from _4 in String(",")
            from velocityY in Num
            select new Robot(Position: new Point2D(Y: positionY, X: positionX),
                Velocity: new Point2D(Y: velocityY, X: velocityX));
    }
}
