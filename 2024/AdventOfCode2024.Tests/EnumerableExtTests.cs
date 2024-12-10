using Xunit;

namespace AdventOfCode2024.Tests;

public class EnumerableExtTests
{
    [Fact]
    public void Expand()
    {
        var numbers = 1.Expand(x => x + 1);

        var output = numbers.Take(10).ToList();

        Assert.Equal(output, new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });
    }
}

