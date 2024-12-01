class Input
{
    public static IEnumerable<string> Lines()
    {
        string? line;
        while ((line = Console.ReadLine()) != null)
        {
            yield return line;
        }
    }
}
