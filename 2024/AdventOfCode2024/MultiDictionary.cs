using System.Collections;
using System.Collections.Concurrent;

namespace AdventOfCode2024;

public class MultiDictionary<TKey, TValue> : IEnumerable<KeyValuePair<TKey, TValue>> where TKey : notnull
{
    private static readonly SortedSet<TValue> EmptySet = new();

    private ConcurrentDictionary<TKey, SortedSet<TValue>> _inner = new();

    public MultiDictionary()
    {
    }

    public MultiDictionary(IEnumerable<KeyValuePair<TKey, TValue>> pairs)
    {
        foreach (var pair in pairs)
        {
            Add(pair);
        }
    }

    public TValue this[TKey key]
    {
        set => Add(key, value);
    }

    public IEnumerable<TKey> Keys =>
        _inner.Keys;

    public IEnumerable<TValue> GetValues(TKey key) =>
        GetValueSet(key);

    public SortedSet<TValue> GetValueSet(TKey key) =>
        _inner.GetValue(key) ?? EmptySet;

    IEnumerator IEnumerable.GetEnumerator() =>
        ((IEnumerable<KeyValuePair<TKey, TValue>>)this).GetEnumerator();

    IEnumerator<KeyValuePair<TKey, TValue>> IEnumerable<KeyValuePair<TKey, TValue>>.GetEnumerator() =>
        _inner
            .SelectMany(pair => pair.Value.Select(value => KeyValuePair.Create(pair.Key, value)))
            .GetEnumerator();

    public void Add(KeyValuePair<TKey, TValue> pair) =>
        Add(pair.Key, pair.Value);

    public void Add(TKey key, TValue value) =>
        AddMultiple(key, [value]);

    public void AddMultiple(TKey key, IEnumerable<TValue> values)
    {
        _inner.AddOrUpdate(
            key,
            _ => new SortedSet<TValue>(values),
            (_, existing) =>
            {
                existing.UnionWith(values);
                return existing;
            }
        );
    }

    public MultiDictionary<TKey, TValue> Filtered(Func<KeyValuePair<TKey, TValue>, bool> predicate) =>
        new(this.Where(predicate));

    public override string ToString() =>
        "[" +
        string.Join(", ",
            _inner.Select(pair =>
                pair.Key + " -> [" + string.Join(", ", pair.Value) + "]")) +
        "]";
}
