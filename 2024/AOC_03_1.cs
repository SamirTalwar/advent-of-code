using System.Text.RegularExpressions;

class AOC_03_1
{
    public static void Main(string[] args)
    {
        var parser = new Regex(@"mul\((\d+),(\d+)\)");
        var input = Input.String();
        var instructions = parser.Matches(input).Select(match =>
            new Multiply(int.Parse(match.Groups[1].Value), int.Parse(match.Groups[2].Value))
        );

        var result = instructions.Select(i => i.Run()).Sum();

        Console.WriteLine("{0}", result);
    }

    interface Instruction
    {
        public int Run();
    }

    readonly struct Multiply : Instruction
    {
        private int A { get; }
        private int B { get; }

        public Multiply(int a, int b)
        {
            A = a;
            B = b;
        }

        public int Run() => A * B;
    }
}
