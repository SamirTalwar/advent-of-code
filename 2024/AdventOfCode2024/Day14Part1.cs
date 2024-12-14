using Pidgin;
using static Pidgin.Parser;

namespace AdventOfCode2024;

public class Day14Part1
{
    public static void Run(string[] args)
    {
        var (columns, rows) = args.Length >= 2 ? (int.Parse(args[0]), int.Parse(args[1])) : (101, 103);
        var midX = columns / 2;
        var midY = rows / 2;
        var robots = Parse.Robots.ParseOrThrow(Input.String());

        var moved = robots.Select(robot => robot.Expand(r => r.Step(rows, columns)).Skip(100).First());
        var quadrants = new int[4];
        foreach (var robot in moved)
        {
            var quadrant = (robot.Position.Y.CompareTo(midY), robot.Position.X.CompareTo(midX)) switch
            {
                (-1, -1) => 0,
                (-1, 1) => 1,
                (1, -1) => 2,
                (1, 1) => 3,
                _ => -1,
            };
            if (quadrant >= 0)
            {
                quadrants[quadrant] += 1;
            }
        }

        var result = quadrants.Aggregate(1, (aggregate, quadrant) => aggregate * quadrant);

        Console.WriteLine("{0}", result);
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
        public Robot Step(int rows, int columns)
        {
            var newY = Mod(Position.Y + Velocity.Y, rows);
            var newX = Mod(Position.X + Velocity.X, columns);
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
