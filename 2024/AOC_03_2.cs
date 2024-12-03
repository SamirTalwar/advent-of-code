using System.Text.RegularExpressions;

class AOC_03_2
{
    public static void Main(string[] args)
    {
        var parser = new Regex(@"do\(\)|don't\(\)|mul\((\d+),(\d+)\)");
        var nameParser = new Regex(@"^[\w']+");
        var input = Input.String();
        var instructions = parser.Matches(input).Select(Instruction (match) =>
        {
            var nameMatch = nameParser.Match(match.Value);
            return nameMatch.Value switch
            {
                "do" => new Enable(),
                "don't" => new Disable(),
                "mul" => new Multiply(int.Parse(match.Groups[1].Value), int.Parse(match.Groups[2].Value)),
                _ => throw new Exception("Invalid instruction: " + match.Groups[0].Value),
            };
        });

        var state = new State();
        var result = instructions.Aggregate(state, (s, i) => i.Run(s)).Value;

        Console.WriteLine("{0}", result);
    }

    struct State
    {
        public int Value { get; set; } = 0;
        public bool Enabled { get; set; } = true;

        public State() { }

        public override string ToString()
        {
            return string.Format("State {{ Value = {0}, Enabled = {1} }}", this.Value, this.Enabled);
        }
    }

    interface Instruction
    {
        public State Run(State state);
    }

    readonly struct Enable : Instruction
    {
        public State Run(State state)
        {
            state.Enabled = true;
            return state;
        }
    }

    readonly struct Disable : Instruction
    {
        public State Run(State state)
        {
            state.Enabled = false;
            return state;
        }
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

        public State Run(State state)
        {
            if (state.Enabled)
            {
                state.Value += A * B;
            }
            return state;
        }
    }
}
