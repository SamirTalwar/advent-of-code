using Pidgin;
using static Pidgin.Parser;

namespace AdventOfCode2024;

public class Day13Part1
{
    private const int ButtonACost = 3;
    private const int ButtonBCost = 1;

    public static void Run(string[] args)
    {
        var problems = Parse.Problems.ParseOrThrow(Input.String());

        var result = 0;
        foreach (var problem in problems)
        {
            var presses =
                0.Expand(x => x + problem.ButtonB.X)
                    .Index()
                    .TakeWhile(x => x.Item < problem.Prize.X)
                    .Select(pair =>
                    {
                        var (pressB, targetX) = pair;
                        var (pressA, remainder) = Math.DivRem(problem.Prize.X - targetX, problem.ButtonA.X);
                        if (remainder > 0)
                        {
                            return null;
                        }

                        if (pressB * problem.ButtonB.Y + pressA * problem.ButtonA.Y != problem.Prize.Y)
                        {
                            return null;
                        }

                        return new
                        {
                            PressA = pressA, ACost = pressA * ButtonACost, PressB = pressB, BCost = pressB * ButtonBCost
                        };
                    })
                    .FirstOrDefault(presses => presses is not null);
            if (presses is not null)
            {
                result += presses.ACost + presses.BCost;
            }
        }

        Console.WriteLine("{0}", result);
    }

    private record struct Problem
    {
        public Point2D ButtonA { get; init; }
        public Point2D ButtonB { get; init; }
        public Point2D Prize { get; init; }
    }

    static class Parse
    {
        public static Parser<char, IEnumerable<Problem>> Problems =>
            Problem.SeparatedAndOptionallyTerminated(EndOfLine);

        // ReSharper disable once MemberHidesStaticFromOuterClass
        private static Parser<char, Problem> Problem =>
            from buttonA in Button("A")
            from buttonB in Button("B")
            from prize in Prize
            select new Problem { ButtonA = buttonA, ButtonB = buttonB, Prize = prize };

        private static Parser<char, Point2D> Button(string name) =>
            from _1 in String("Button ")
            from _2 in String(name)
            from _3 in String(": X+")
            from x in Num
            from _4 in String(", Y+")
            from y in Num
            from _5 in EndOfLine
            select new Point2D(Y: y, X: x);

        private static Parser<char, Point2D> Prize =>
            from _1 in String("Prize: X=")
            from x in Num
            from _2 in String(", Y=")
            from y in Num
            from _3 in EndOfLine
            select new Point2D(Y: y, X: x);
    }
}
