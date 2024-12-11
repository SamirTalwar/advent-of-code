namespace AdventOfCode2024;

class Day05Part2
{
    public static void Run(string[] args)
    {
        var ordering = new MultiDictionary<int, int>();
        var updates = new List<List<int>>();

        var lines = Input.Lines();
        // ReSharper disable once PossibleMultipleEnumeration
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
        // ReSharper disable once PossibleMultipleEnumeration
        foreach (var line in lines)
        {
            var update = line.Split(",").Select(int.Parse).ToList();
            updates.Add(update);
        }

        var reorderedUpdateMiddleNumbers = new List<int>();
        foreach (var update in updates)
        {
            if (!ValidUpdate(update, ordering))
            {
                var reordered = ReorderUpdate(update, ordering);
                reorderedUpdateMiddleNumbers.Add(reordered[reordered.Count / 2]);
            }
        }

        var result = reorderedUpdateMiddleNumbers.Sum();

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

    static IList<int> ReorderUpdate(IList<int> update, MultiDictionary<int, int> ordering)
    {
        var values = new HashSet<int>(update);
        var subOrdering = ordering.Filtered(pair => values.Contains(pair.Key) || values.Contains(pair.Value));
        var reordered = new List<int>();
        foreach (var before in values)
        {
            var index =
                subOrdering.GetValues(before)
                    .Select(a => reordered.IndexOf(a))
                    .Select<int, int?>(i => i >= 0 ? i : null)
                    .Min();
            reordered.Insert(index ?? reordered.Count, before);
        }
        return reordered;
    }
}
