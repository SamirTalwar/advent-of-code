class AOC_07_1
{
    public static void Main(string[] args)
    {
        var problems = Input.Lines().Select(line =>
        {
            var split = line.Split(": ", 2);
            var output = long.Parse(split[0]);
            var inputSplit = split[1].Split(" ");
            var inputs = inputSplit.Select(long.Parse).ToList();
            return new Problem { Inputs = inputs, Output = output };
        });

        var result =
            problems
                .Where(problem => problem.IsSolveable())
                .Select(problem => problem.Output)
                .Sum();

        Console.WriteLine("{0}", result);
    }

    struct Problem
    {
        public List<long> Inputs { get; init; }
        public long Output { get; init; }

        public bool IsSolveable()
        {
            return IsSolveable(Inputs[0], 1);
        }

        bool IsSolveable(long value, int nextIndex)
        {
            if (nextIndex == Inputs.Count)
            {
                return value == Output;
            }
            else
            {
                return IsSolveable(value + Inputs[nextIndex], nextIndex + 1)
                    || IsSolveable(value * Inputs[nextIndex], nextIndex + 1);
            }
        }

        public override string ToString() =>
            string.Format("{0}: {1}", Output, string.Join(' ', Inputs));
    }
}
