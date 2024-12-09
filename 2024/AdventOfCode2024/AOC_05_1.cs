class AOC_05_1
{
    public static void Run(string[] args)
    {
        var ordering = new MultiDictionary<int, int>();
        var updates = new List<List<int>>();

        var lines = Input.Lines();
        foreach (var line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                break;
            }
            var split = line.Split("|");
            var before = int.Parse(split[0]);
            var after = int.Parse(split[1]);
            ordering.Add(before, after);
        }
        foreach (var line in lines)
        {
            var update = line.Split(",").Select(int.Parse).ToList();
            updates.Add(update);
        }

        var validUpdateMiddleNumbers = new List<int>();
        foreach (var update in updates)
        {
            if (ValidUpdate(update, ordering))
            {
                validUpdateMiddleNumbers.Add(update[update.Count / 2]);
            }
        }

        var result = validUpdateMiddleNumbers.Sum();

        Console.WriteLine("{0}", result);
    }

    static bool ValidUpdate(IList<int> update, MultiDictionary<int, int> ordering)
    {
        var seen = new HashSet<int>();
        foreach (var value in update)
        {
            if (seen.Overlaps(ordering.GetValues(value)))
            {
                return false;
            }
            seen.Add(value);
        }
        return true;
    }
}
