namespace AdventOfCode2024;

public static class DictionaryExt
{
    public static TValue? GetValue<TKey, TValue>(
        this IDictionary<TKey, TValue> dict,
        TKey key,
        TValue? defaultValue = default
    ) =>
        dict.TryGetValue(key, out var value) ? value : defaultValue;
}
