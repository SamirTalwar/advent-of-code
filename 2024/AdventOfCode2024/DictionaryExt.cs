public static class DictionaryExt
{
    public static V? GetValue<K, V>(
        this IDictionary<K, V> dict,
        K key,
        V? defaultValue = default(V)
    )
    {
        V? value;
        return dict.TryGetValue(key, out value) ? value : defaultValue;
    }
}
