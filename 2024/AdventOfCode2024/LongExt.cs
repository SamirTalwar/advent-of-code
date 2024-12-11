namespace AdventOfCode2024;

public static class LongExt
{
    public static long Pow(this long value, long exponent)
    {
        if (exponent < 0)
        {
            throw new ArgumentException("Exponent cannot be negative.");
        }

        if (exponent == 0)
        {
            return 1;
        }

        var result = value;
        for (int i = 1; i < exponent; i += 1)
        {
            result *= value;
        }

        return result;
    }
}
