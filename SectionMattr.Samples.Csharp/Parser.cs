using System.Text.Json;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace SectionMattr.Samples.Csharp.Parser;

public class ParsedJsonData
{
    public string title { get; set; }
}

class MyJsonParser : IMattrParser<ParsedJsonData>
{
    public MattrSection<ParsedJsonData> Parse(MattrSection<string> section, MattrSection<ParsedJsonData>[] sections)
    {
        ParsedJsonData data = JsonSerializer.Deserialize<ParsedJsonData>(section.Data);
        return NewMattr.AppendData<ParsedJsonData>(data, section);
    }
}

public class ParsedYamlData
{
    public string title { get; set; }
}

class MyYamlParser : IMattrParser<ParsedYamlData>
{
    public MattrSection<ParsedYamlData> Parse(MattrSection<string> section, MattrSection<ParsedYamlData>[] sections)
    {
        ParsedYamlData data = new DeserializerBuilder().WithNamingConvention(UnderscoredNamingConvention.Instance).Build().Deserialize<ParsedYamlData>(section.Data);
        return NewMattr.AppendData<ParsedYamlData>(data, section);
    }
}

public class Parser
{
    static void Main()
    {
        string stringContent =
    @"Content before the sections.

---

More content.

---one
title: One
---

This is section one.";

        // JSON Parse
        Mattr<ParsedJsonData> mattrJSON = NewMattr.Sections(stringContent, new MyJsonParser());

        // YAML Parse
        Mattr<ParsedYamlData> mattrYAML = NewMattr.Sections(stringContent, new MyYamlParser());
    }
}

