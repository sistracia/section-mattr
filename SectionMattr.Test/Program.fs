open Expecto
open SectionMattr
open System.IO
open System.Text.Json

type ParsedData = { title: string }

let read (fixtureName: string) : string =
    File.ReadAllText(Path.Combine("../../..", "fixtures", fixtureName))

[<Tests>]
let tests =
    testList
        "section-mattr"
        [

          test "should return a file object" {
              Expect.equal
                  (NewMattr.sections "")
                  { Mattr.content = ""
                    Mattr.sections = [||] }
                  "should be equal"

              Expect.equal
                  (NewMattr.sections "foo")
                  { Mattr.content = "foo"
                    Mattr.sections = [||] }
                  "should be equal"
          }

          test "should correctly parse non-sections" {
              Expect.equal
                  (NewMattr.sections "foo\n---\nbar")
                  { Mattr.content = "foo\n---\nbar"
                    Mattr.sections = [||] }
                  "should be equal"

              Expect.equal
                  (NewMattr.sections "foo\n---\nbar\n---")
                  { Mattr.content = "foo\n---\nbar\n---"
                    Mattr.sections = [||] }
                  "should be equal"
          }

          test "should parse front-matter without language" {
              Expect.equal
                  (NewMattr.sections "---\ntitle: bar\n---\n\nfoo")
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = ""
                           MattrSection.data = "title: bar"
                           MattrSection.content = "\nfoo" } |] }
                  "should be equal"

              Expect.equal
                  (NewMattr.sections "---\nfoo\n---\nbar")
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = ""
                           MattrSection.data = "foo"
                           MattrSection.content = "bar" } |] }
                  "should be equal"
          }

          test "should parse front-matter with language" {
              let input: string = "---json\n{\"title\": \"bar\"}\n---\n\nfoo"

              Expect.equal
                  (NewMattr.sections input)
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = "json"
                           MattrSection.data = "{\"title\": \"bar\"}"
                           MattrSection.content = "\nfoo" } |] }
                  "should be equal"
          }

          test "should parse a section" {
              let input: string =
                  "---\ntitle: bar\n---\n\nfoo\n---one\ntitle: One\n---\nThis is one"

              Expect.equal
                  (NewMattr.sections input)
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = ""
                           MattrSection.data = "title: bar"
                           MattrSection.content = "\nfoo" }
                         { MattrSection.key = "one"
                           MattrSection.data = "title: One"
                           MattrSection.content = "This is one" } |] }
                  "should be equal"
          }

          test "should use custom section_delimiter" {
              let input: string =
                  "~~~\ntitle: bar\n~~~\n\nfoo\n~~~one\ntitle: One\n~~~\nThis is one"

              Expect.equal
                  (NewMattr.sections (input, { MattrOption1.section_delimiter = "~~~" }))
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = ""
                           MattrSection.data = "title: bar"
                           MattrSection.content = "\nfoo" }
                         { MattrSection.key = "one"
                           MattrSection.data = "title: One"
                           MattrSection.content = "This is one" } |] }
                  "should be equal"
          }

          test "should use a custom parser on sections" {
              let input: string =
                  "---json\n{\"title\": \"bar\"}\n---\n\nfoo\n---json\n{\"title\": \"One\"}\n---\nThis is one"

              let parse (section: MattrSection<string>) (_: MattrSection<ParsedData> array) : MattrSection<ParsedData> =
                  Mattrial.appendData (JsonSerializer.Deserialize<ParsedData>(section.data)) section

              Expect.equal
                  (NewMattr.sections (input, parse))
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = "json"
                           MattrSection.data = { ParsedData.title = "bar" }
                           MattrSection.content = "\nfoo" }
                         { MattrSection.key = "json"
                           MattrSection.data = { ParsedData.title = "One" }
                           MattrSection.content = "This is one" } |] }
                  "should be equal"
          }

          test "should parse multiple sections" {
              let input: string = read "multiple.md"

              Expect.equal
                  (NewMattr.sections input)
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = ""
                           MattrSection.data = "title: bar"
                           MattrSection.content = "\nfoo\n" }
                         { MattrSection.key = "one"
                           MattrSection.data = "title: One"
                           MattrSection.content = "This is one\n" }
                         { MattrSection.key = "two"
                           MattrSection.data = "title: Two"
                           MattrSection.content = "This is two\n" } |] }
                  "should be equal"
          }

          test "should not parse non-sections" {
              let input: string = read "hr.md"

              Expect.equal
                  (NewMattr.sections input)
                  { Mattr.content = ""
                    Mattr.sections =
                      [| { MattrSection.key = "yaml"
                           MattrSection.data = "title: I'm front matter"
                           MattrSection.content =
                             "\nThis page has front matter that should be parsed before the sections.\n" }
                         { MattrSection.key = "aaa"
                           MattrSection.data = "title: First section"
                           MattrSection.content = "\nSection one.\n" }
                         { MattrSection.key = "bbb"
                           MattrSection.data = "title: Non-section horizontal rules"
                           MattrSection.content = "\nPart 1.\n\n---\n\nPart 2.\n\n---\n\nPart 3.\n" }
                         { MattrSection.key = "ccc"
                           MattrSection.data = "title: Third section"
                           MattrSection.content = "\nSection three.\n" } |] }
                  "should be equal"
          }

          ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
