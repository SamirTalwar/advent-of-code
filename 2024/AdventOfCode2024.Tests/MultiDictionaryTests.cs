using Xunit;

namespace AdventOfCode2024.Tests;

public class MultiDictionaryTests
{
    [Fact]
    public void IsEnumerable()
    {
        var dictionary = new MultiDictionary<int, char> { [1] = 'a', [1] = 'b', [2] = 'c', [3] = 'd', [2] = 'e' };

        var list = dictionary.Select(pair => (pair.Key, pair.Value)).ToList();
        list.Sort();

        Assert.Equal(list, [
            (1, 'a'),
            (1, 'b'),
            (2, 'c'),
            (2, 'e'),
            (3, 'd'),
        ]);
    }

    [Fact]
    public void ProducesASetForAGivenKey()
    {
        var dictionary = new MultiDictionary<int, char> { [1] = 'a', [1] = 'b', [2] = 'c', [3] = 'd', [2] = 'e' };

        var values = dictionary.GetValueSet(2);

        Assert.Equal(values, ['c', 'e']);
    }
}
