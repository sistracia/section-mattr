namespace SectionMattr.Samples.Csharp.Basic;

public class Basic
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

        // string -> Mattr<string>
        Mattr<string> mattr1 = NewMattr.Sections(stringContent);

        // byte[] -> Mattr<string>
        Mattr<string> mattr2 = NewMattr.Sections(System.Text.Encoding.UTF8.GetBytes(stringContent));

        // Mattr<string> -> Mattr<string>
        // `Mattr.Sections` will always be replaced by the section from `Mattr.Content`
        // no matter there is section or not in `Mattr.Content`
        Mattr<string> mattr3 = NewMattr.Sections(new Mattr<string>(content: stringContent, sections: []));
    }
}

