using System.Collections.Concurrent;

class AOC_05_2
{
    static readonly ISet<int> emptySet = new HashSet<int>();

    public static void Main(string[] args)
    {
        var ordering = new ConcurrentDictionary<int, ISet<int>>();
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
            ordering.AddOrUpdate(
                before,
                _ => new HashSet<int> { after },
                (_, existing) => { existing.Add(after); return existing; }
            );
        }
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

    static bool ValidUpdate(IList<int> update, IDictionary<int, ISet<int>> ordering)
    {
        var seen = new HashSet<int>();
        foreach (var value in update)
        {
            var intersected = (ordering.GetValue(value) ?? emptySet).Intersect(seen).Count();
            if (intersected > 0)
            {
                return false;
            }
            seen.Add(value);
        }
        return true;
    }

    static IList<int> ReorderUpdate(IList<int> update, IDictionary<int, ISet<int>> ordering)
    {
        var values = new HashSet<int>(update);
        var subOrdering =
            ordering
                .Where(pair => values.Contains(pair.Key))
                .Select(pair => (pair.Key, values.Intersect(pair.Value).ToHashSet()))
                .Where(pair => pair.Item2.Count > 0)
                .ToDictionary();
        var reordered = new List<int>();
        foreach (var before in values)
        {
            var after = subOrdering.GetValue(before) ?? emptySet;
            var index = after.Select(a => reordered.IndexOf(a)).Select<int, int?>(i => i >= 0 ? i : null).Min();
            reordered.Insert(index ?? reordered.Count, before);
        }
        return reordered;
    }
}
