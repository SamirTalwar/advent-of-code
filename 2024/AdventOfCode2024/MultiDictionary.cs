using System.Collections;
using System.Collections.Concurrent;

namespace AdventOfCode2024;

public class MultiDictionary<K, V> : IEnumerable<KeyValuePair<K, V>> where K : notnull
{
    static readonly SortedSet<V> emptySet = new SortedSet<V>();

    ConcurrentDictionary<K, SortedSet<V>> inner = new();

    public MultiDictionary() { }

    public MultiDictionary(IEnumerable<KeyValuePair<K, V>> pairs)
    {
        foreach (var pair in pairs)
        {
            Add(pair);
        }
    }

    public V this[K key]
    {
        set => Add(key, value);
    }

    public IEnumerable<K> Keys =>
        inner.Keys;

    public IEnumerable<V> GetValues(K key) =>
        GetValueSet(key);

    public SortedSet<V> GetValueSet(K key) =>
        inner.GetValue(key) ?? emptySet;

    IEnumerator IEnumerable.GetEnumerator() =>
        ((IEnumerable<KeyValuePair<K, V>>)this).GetEnumerator();

    IEnumerator<KeyValuePair<K, V>> IEnumerable<KeyValuePair<K, V>>.GetEnumerator() =>
            inner
                .SelectMany(pair => pair.Value.Select(value => KeyValuePair.Create(pair.Key, value)))
                .GetEnumerator();

    public void Add(KeyValuePair<K, V> pair) =>
        Add(pair.Key, pair.Value);

    public void Add(K key, V value)
    {
        inner.AddOrUpdate(
            key,
            _ => new SortedSet<V> { value },
            (_, existing) => { existing.Add(value); return existing; }
        );
    }

    public MultiDictionary<K, V> Filtered(Func<KeyValuePair<K, V>, bool> predicate) =>
        new MultiDictionary<K, V>(this.Where(predicate));
}
