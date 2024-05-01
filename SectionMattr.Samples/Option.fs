module Option

open System.Text.Json
open SectionMattr

type ParsedJsonData = { title: string }

type MyJsonParser() =
    interface IMattrParser<ParsedJsonData> with
        member _.Parse
            (
                section: MattrSection<string>,
                sections: MattrSection<ParsedJsonData> array
            ) : MattrSection<ParsedJsonData> =
            let data: ParsedJsonData = JsonSerializer.Deserialize<ParsedJsonData> section.Data
            NewMattr.AppendData data section

let stringContent =
    @"Content before the sections.

---

More content.

---one
title: One
---

This is section one."

// Set delimiter only

let mattrDELIMIER: Mattr<string> =
    NewMattr.Sections(stringContent, NewMattr.DefaultOption().SetSectionDelimiter("---"))

mattrDELIMIER |> ignore

// Set parser only

let mattrPARSER: Mattr<ParsedJsonData> =
    NewMattr.Sections(stringContent, NewMattr.DefaultOption().SetSectionParse(MyJsonParser()))

mattrPARSER |> ignore

// Set delimiter or parser

let mattrOPTION1: Mattr<ParsedJsonData> =
    NewMattr.Sections(
        stringContent,
        NewMattr
            .DefaultOption()
            .SetSectionDelimiter("---")
            .SetSectionParse(MyJsonParser())
    )

mattrOPTION1 |> ignore

// or

let mattrOPTION2: Mattr<ParsedJsonData> =
    NewMattr.Sections(
        stringContent,
        { MattrOption.SectionDelimiter = "---"
          MattrOption.SectionParse = MyJsonParser() }
    )

mattrOPTION2 |> ignore
