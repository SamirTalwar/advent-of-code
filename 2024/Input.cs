class Input
{
    public static string String()
    {
        using (var reader = new StreamReader(Console.OpenStandardInput(), Console.InputEncoding))
        {
            return reader.ReadToEnd();
        }
    }

    public static IEnumerable<string> Lines()
    {
        string? line;
        while ((line = Console.ReadLine()) != null)
        {
            yield return line;
        }
    }
}
