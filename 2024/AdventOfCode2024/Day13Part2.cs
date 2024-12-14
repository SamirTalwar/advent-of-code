using Pidgin;
using static Pidgin.Parser;

namespace AdventOfCode2024;

public class Day13Part2
{
    /* Solving for m and n:
     *
     *      m ax + n bx = px
     *      m ay + n by = py
     * ---- n = (py - m ay) / by
     *      m ax + ((py - m ay) / by) bx = px
     *      m ax + bx (py - m ay) / by = px
     *      m ax + bx py / by - m bx ay / by = px
     *      m (ax - bx ay / by) + bx py / by = px
     *      m (ax - bx ay / by) = px - bx py / by
     *      m = (px - bx py / by) / (ax - bx ay / by)
     *      m = (px by / by - bx py / by) / (ax by / by - bx ay / by)
     *      m = (px by - bx py) / (ax by - bx ay)
     * ---- m = (px by - py bx) / (ax by - ay bx)
     */

    private const long ButtonACost = 3;
    private const long ButtonBCost = 1;
    private const long PrizeOffset = 10000000000000;

    public static void Run(string[] args)
    {
        var problems = Parse.Problems.ParseOrThrow(Input.String());

        var result = 0L;
        foreach (var problem in problems)
        {
            var pressA = (problem.Prize.X * problem.ButtonB.Y - problem.Prize.Y * problem.ButtonB.X) /
                         (problem.ButtonA.X * problem.ButtonB.Y - problem.ButtonA.Y * problem.ButtonB.X);
            var pressB = (problem.Prize.Y - pressA * problem.ButtonA.Y) / problem.ButtonB.Y;
            var solutionX = pressA * problem.ButtonA.X + pressB * problem.ButtonB.X;
            var solutionY = pressA * problem.ButtonA.Y + pressB * problem.ButtonB.Y;
            if (solutionX == problem.Prize.X && solutionY == problem.Prize.Y)
            {
                result += pressA * problem.ButtonA.Cost + pressB * problem.ButtonB.Cost;
            }
        }

        Console.WriteLine("{0}", result);
    }

    private record struct Button(long Cost, long X, long Y);

    private record struct Prize(long X, long Y);

    // ReSharper disable once MemberHidesStaticFromOuterClass
    private record struct Problem
    {
        public Button ButtonA { get; init; }
        public Button ButtonB { get; init; }
        public Prize Prize { get; init; }
    }

    static class Parse
    {
        public static Parser<char, IEnumerable<Problem>> Problems =>
            Problem.SeparatedAndOptionallyTerminated(EndOfLine);

        // ReSharper disable once MemberHidesStaticFromOuterClass
        private static Parser<char, Problem> Problem =>
            from buttonA in Button("A", ButtonACost)
            from buttonB in Button("B", ButtonBCost)
            from prize in Prize
            select new Problem { ButtonA = buttonA, ButtonB = buttonB, Prize = prize };

        // ReSharper disable once MemberHidesStaticFromOuterClass
        private static Parser<char, Button> Button(string name, long cost) =>
            from _1 in String("Button ")
            from _2 in String(name)
            from _3 in String(": X+")
            from x in Num
            from _4 in String(", Y+")
            from y in Num
            from _5 in EndOfLine
            select new Button(Cost: cost, Y: y, X: x);

        // ReSharper disable once MemberHidesStaticFromOuterClass
        private static Parser<char, Prize> Prize =>
            from _1 in String("Prize: X=")
            from x in Num
            from _2 in String(", Y=")
            from y in Num
            from _3 in EndOfLine
            select new Prize(Y: y + PrizeOffset, X: x + PrizeOffset);
    }
}
