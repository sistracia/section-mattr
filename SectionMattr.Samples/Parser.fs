module Parser

open System.Text.Json
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions
open SectionMattr

let stringContent =
    @"Content before the sections.

---

More content.

---one
title: One
---

This is section one."

// JSON Parse

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

let mattrJSON: Mattr<ParsedJsonData> =
    NewMattr.Sections(stringContent, MyJsonParser())

mattrJSON |> ignore

// YAML Parse

// `YamlDotNet` need this attribute to be able to deserialize
[<CLIMutable>]
type ParsedYamlData = { title: string }

type MyYamlParser() =
    interface IMattrParser<ParsedYamlData> with
        member _.Parse
            (
                section: MattrSection<string>,
                sections: MattrSection<ParsedYamlData> array
            ) : MattrSection<ParsedYamlData> =
            let data: ParsedYamlData =
                (new DeserializerBuilder())
                    .WithNamingConvention(UnderscoredNamingConvention.Instance)
                    .Build()
                    .Deserialize<ParsedYamlData>
                    section.Data

            NewMattr.AppendData data section

let mattrYAML: Mattr<ParsedYamlData> =
    NewMattr.Sections(stringContent, MyYamlParser())

mattrYAML |> ignore
