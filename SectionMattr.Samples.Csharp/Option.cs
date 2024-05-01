using System.Text.Json;

namespace SectionMattr.Samples.Csharp.Option;

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

public class Option
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

        // Set delimiter only
        Mattr<string> mattrDELIMIER = NewMattr.Sections(stringContent, NewMattr.DefaultOption().SetSectionDelimiter("---"));

        // Set parser only
        Mattr<ParsedJsonData> mattrPARSER = NewMattr.Sections(stringContent, NewMattr.DefaultOption().SetSectionParse(new MyJsonParser()));

        // Set delimiter or parser
        Mattr<ParsedJsonData> mattrOPTION1 = NewMattr.Sections(stringContent, NewMattr.DefaultOption().SetSectionDelimiter("---").SetSectionParse(new MyJsonParser()));
        // or
        Mattr<ParsedJsonData> mattrOPTION2 = NewMattr.Sections(stringContent, new MattrOption<ParsedJsonData>("---", new MyJsonParser()));
    }
}

