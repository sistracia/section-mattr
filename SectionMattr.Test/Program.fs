open Expecto
open SectionMattr
open System.IO
open System.Text.Json

type ParsedData = { title: string }

let readString (fixtureName: string) : string =
    File.ReadAllText(Path.Combine("../../..", "fixtures", fixtureName))

let readBuffer (fixtureName: string) : byte array =
    File.ReadAllBytes(Path.Combine("../../..", "fixtures", fixtureName))

type MyJsonParser() =
    interface IMattrParser<ParsedData> with
        member _.Parse(section: MattrSection<string>, _: MattrSection<ParsedData> array) : MattrSection<ParsedData> =
            NewMattr.AppendData (JsonSerializer.Deserialize<ParsedData>(section.Data)) section

[<Tests>]
let tests =
    testList
        "section-mattr"
        [

          test "should return a file object" {
              Expect.equal
                  (NewMattr.Sections "")
                  { Mattr.Content = ""
                    Mattr.Sections = [||] }
                  "should be equal"

              Expect.equal
                  (NewMattr.Sections "foo")
                  { Mattr.Content = "foo"
                    Mattr.Sections = [||] }
                  "should be equal"
          }

          test "should correctly parse non-sections" {
              Expect.equal
                  (NewMattr.Sections "foo\n---\nbar")
                  { Mattr.Content = "foo\n---\nbar"
                    Mattr.Sections = [||] }
                  "should be equal"

              Expect.equal
                  (NewMattr.Sections "foo\n---\nbar\n---")
                  { Mattr.Content = "foo\n---\nbar\n---"
                    Mattr.Sections = [||] }
                  "should be equal"
          }

          test "should parse front-matter without language" {
              Expect.equal
                  (NewMattr.Sections "---\ntitle: bar\n---\n\nfoo")
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "title: bar"
                           MattrSection.Content = "\nfoo" } |] }
                  "should be equal"

              Expect.equal
                  (NewMattr.Sections "---\nfoo\n---\nbar")
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "foo"
                           MattrSection.Content = "bar" } |] }
                  "should be equal"
          }

          test "should parse front-matter with language" {
              let input: string = "---json\n{\"title\": \"bar\"}\n---\n\nfoo"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = "json"
                           MattrSection.Data = "{\"title\": \"bar\"}"
                           MattrSection.Content = "\nfoo" } |] }
                  "should be equal"
          }

          test "should parse a section" {
              let input: string =
                  "---\ntitle: bar\n---\n\nfoo\n---one\ntitle: One\n---\nThis is one"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "title: bar"
                           MattrSection.Content = "\nfoo" }
                         { MattrSection.Key = "one"
                           MattrSection.Data = "title: One"
                           MattrSection.Content = "This is one" } |] }
                  "should be equal"
          }

          test "should use custom section_delimiter" {
              let input: string =
                  "~~~\ntitle: bar\n~~~\n\nfoo\n~~~one\ntitle: One\n~~~\nThis is one"

              Expect.equal
                  (NewMattr.Sections(input, (NewMattr.DefaultOption()).SetSectionDelimiter("~~~")))
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "title: bar"
                           MattrSection.Content = "\nfoo" }
                         { MattrSection.Key = "one"
                           MattrSection.Data = "title: One"
                           MattrSection.Content = "This is one" } |] }
                  "should be equal"
          }

          test "should use a custom parser on sections" {
              let input: string =
                  "---json\n{\"title\": \"bar\"}\n---\n\nfoo\n---json\n{\"title\": \"One\"}\n---\nThis is one"

              Expect.equal
                  (NewMattr.Sections(input, MyJsonParser()))
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = "json"
                           MattrSection.Data = { ParsedData.title = "bar" }
                           MattrSection.Content = "\nfoo" }
                         { MattrSection.Key = "json"
                           MattrSection.Data = { ParsedData.title = "One" }
                           MattrSection.Content = "This is one" } |] }
                  "should be equal"
          }

          test "should parse string with multiple sections" {
              let input: string = readString "multiple.md"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "title: bar"
                           MattrSection.Content = "\nfoo\n" }
                         { MattrSection.Key = "one"
                           MattrSection.Data = "title: One"
                           MattrSection.Content = "This is one\n" }
                         { MattrSection.Key = "two"
                           MattrSection.Data = "title: Two"
                           MattrSection.Content = "This is two\n" } |] }
                  "should be equal"
          }

          test "should not parse string non-sections" {
              let input: string = readString "hr.md"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = "yaml"
                           MattrSection.Data = "title: I'm front matter"
                           MattrSection.Content =
                             "\nThis page has front matter that should be parsed before the sections.\n" }
                         { MattrSection.Key = "aaa"
                           MattrSection.Data = "title: First section"
                           MattrSection.Content = "\nSection one.\n" }
                         { MattrSection.Key = "bbb"
                           MattrSection.Data = "title: Non-section horizontal rules"
                           MattrSection.Content = "\nPart 1.\n\n---\n\nPart 2.\n\n---\n\nPart 3.\n" }
                         { MattrSection.Key = "ccc"
                           MattrSection.Data = "title: Third section"
                           MattrSection.Content = "\nSection three.\n" } |] }
                  "should be equal"
          }

          test "should parse buffer with multiple sections" {
              let input: byte array = readBuffer "multiple.md"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = ""
                           MattrSection.Data = "title: bar"
                           MattrSection.Content = "\nfoo\n" }
                         { MattrSection.Key = "one"
                           MattrSection.Data = "title: One"
                           MattrSection.Content = "This is one\n" }
                         { MattrSection.Key = "two"
                           MattrSection.Data = "title: Two"
                           MattrSection.Content = "This is two\n" } |] }
                  "should be equal"
          }

          test "should not parse buffer non-sections" {
              let input: byte array = readBuffer "hr.md"

              Expect.equal
                  (NewMattr.Sections input)
                  { Mattr.Content = ""
                    Mattr.Sections =
                      [| { MattrSection.Key = "yaml"
                           MattrSection.Data = "title: I'm front matter"
                           MattrSection.Content =
                             "\nThis page has front matter that should be parsed before the sections.\n" }
                         { MattrSection.Key = "aaa"
                           MattrSection.Data = "title: First section"
                           MattrSection.Content = "\nSection one.\n" }
                         { MattrSection.Key = "bbb"
                           MattrSection.Data = "title: Non-section horizontal rules"
                           MattrSection.Content = "\nPart 1.\n\n---\n\nPart 2.\n\n---\n\nPart 3.\n" }
                         { MattrSection.Key = "ccc"
                           MattrSection.Data = "title: Third section"
                           MattrSection.Content = "\nSection three.\n" } |] }
                  "should be equal"
          }

          ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
