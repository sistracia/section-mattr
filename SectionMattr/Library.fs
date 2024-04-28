namespace SectionMattr

open System.Text
open System.Text.RegularExpressions

[<Struct>]
type MattrSection<'TData> =
    { Key: string
      Data: 'TData
      Content: string }

type IMattrParser<'TData> =
    abstract member Parse: MattrSection<string> * MattrSection<'TData> array -> MattrSection<'TData>

type DefaultMattrParser() =
    interface IMattrParser<string> with
        member _.Parse(section: MattrSection<string>, _: MattrSection<string> array) : MattrSection<string> = section

[<Struct>]
type MattrOption<'TData> =
    { SectionDelimiter: string
      Parser: IMattrParser<'TData> }

    member this.SetDelimiter(delimiter: string) : MattrOption<'TData> =
        { Parser = this.Parser
          SectionDelimiter = delimiter }

    member this.SetParser<'TNewData>(parser: IMattrParser<'TNewData>) : MattrOption<'TNewData> =
        { SectionDelimiter = this.SectionDelimiter
          Parser = parser }

    static member Default: MattrOption<string> =
        { MattrOption.SectionDelimiter = "---"
          MattrOption.Parser = DefaultMattrParser() }

[<Struct>]
type Mattr<'TData> =
    { Content: string
      Sections: MattrSection<'TData> array }

type Parser<'TData> = MattrSection<string> -> MattrSection<'TData> array -> MattrSection<'TData>

module Mattrial =
    let createSection () : MattrSection<string> =
        { MattrSection.Key = ""
          MattrSection.Data = ""
          MattrSection.Content = "" }

    let stringToObject<'TData> (input: string) : Mattr<'TData> =
        { Mattr.Content = input
          Mattr.Sections = [||] }

    let bufferToObject<'TData> (input: byte array) : Mattr<'TData> =
        { Mattr.Content = Encoding.Default.GetString input
          Mattr.Sections = [||] }

    let identity (section: MattrSection<string>) (_: MattrSection<string> array) : MattrSection<string> = section

    let getKey (dataLine: string option) (delim: string) : string =
        match dataLine with
        | None -> ""
        | Some "" -> ""
        | Some(dataLine: string) -> dataLine.Substring(delim.Length).Trim()

    let isDelimiter (line: string) (delim: string) : bool =
        if (not (line.StartsWith delim)) then
            false
        else if (line.Length > delim.Length && line[delim.Length] = delim[delim.Length - 1]) then
            false
        else
            true

    let parse<'TData> (entity: Mattr<'TData>) (opts: MattrOption<'TData>) : Mattr<'TData> =
        let delim: string = opts.SectionDelimiter
        let lines: string array = Regex.Split(entity.Content, @"\r?\n")

        let mutable _entity: Mattr<'TData> = entity
        let mutable sections: MattrSection<'TData> array option = None
        let mutable content: string array = [||]
        let mutable section: MattrSection<string> = createSection ()
        let mutable stack: string array = [||]

        let initSections (contentStr: string) : unit =
            _entity <- { _entity with Content = contentStr }
            sections <- Some [||]
            content <- [||]

        let closeSection (contentStr: string) : unit =
            if (stack.Length <> 0) then
                section <-
                    { section with
                        Key = getKey (stack |> Array.tryItem 0) delim
                        Content = contentStr }

                sections <-
                    match sections with
                    | Some(sections: MattrSection<'TData> array) ->
                        Some(Array.append sections [| (opts.Parser.Parse(section, sections)) |])
                    | None -> None

                section <- createSection ()
                content <- [||]
                stack <- [||]

        for i in 0 .. lines.Length - 1 do
            let line = lines[i]
            let len = stack.Length
            let ln = line.Trim()

            let cond1 = isDelimiter ln delim
            let cond2 = ln.Length = 3 && i <> 0
            let cond3 = len = 0 || len = 2
            let cond4 = Option.isNone sections
            let cond5 = len = 2

            if (cond1 && cond2 && cond3) then
                content <- Array.append content [| line |]
            else if (cond1 && cond2) then
                stack <- Array.append stack [| ln |]

                section <-
                    { section with
                        MattrSection.Data = content |> String.concat "\n" }

                content <- [||]
            else if (cond1 && cond4) then
                initSections (content |> String.concat "\n")
                stack <- Array.append stack [| ln |]
            else if (cond1 && cond5) then
                closeSection (content |> String.concat "\n")
                stack <- Array.append stack [| ln |]
            else
                content <- Array.append content [| line |]

        if (Option.isNone sections) then
            initSections (content |> String.concat "\n")
        else
            closeSection (content |> String.concat "\n")

        match sections with
        | Some(sections: MattrSection<'TData> array) -> { _entity with Sections = sections }
        | None -> _entity

/// Ref: Defining static classes in F#
/// https://stackoverflow.com/q/13101995/12976234
[<AbstractClass; Sealed>]
type NewMattr private () =
    static member DefaultOption() : MattrOption<string> =
        { MattrOption.SectionDelimiter = "---"
          MattrOption.Parser = DefaultMattrParser() }

    static member AppendData<'TData> (data: 'TData) (section: MattrSection<string>) : MattrSection<'TData> =
        let parsed: MattrSection<'TData> =
            { MattrSection.Key = section.Key
              MattrSection.Content = section.Content
              MattrSection.Data = data }

        parsed

    static member Sections(input: string) : Mattr<string> =
        let file: Mattr<string> = Mattrial.stringToObject input
        let option: MattrOption<string> = NewMattr.DefaultOption()
        NewMattr.Sections(file, option)

    static member Sections<'TData>(input: string, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.stringToObject input
        let option: MattrOption<'TData> = (NewMattr.DefaultOption()).SetParser parser
        NewMattr.Sections(file, option)

    static member Sections<'TData>(input: string, option: MattrOption<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.stringToObject input
        NewMattr.Sections(file, option)

    static member Sections(input: byte array) : Mattr<string> =
        let file: Mattr<string> = Mattrial.bufferToObject input
        let option: MattrOption<string> = NewMattr.DefaultOption()
        NewMattr.Sections(file, option)

    static member Sections<'TData>(input: byte array, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.bufferToObject input
        let option: MattrOption<'TData> = (NewMattr.DefaultOption()).SetParser parser
        NewMattr.Sections(file, option)

    static member Sections<'TData>(input: byte array, option: MattrOption<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.bufferToObject input
        Mattrial.parse file option

    static member Sections(entity: Mattr<string>) : Mattr<string> =
        let option: MattrOption<string> = NewMattr.DefaultOption()
        NewMattr.Sections(entity, option)

    static member Sections<'TData>(entity: Mattr<'TData>, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let option: MattrOption<'TData> = (NewMattr.DefaultOption()).SetParser parser
        NewMattr.Sections(entity, option)

    static member Sections<'TData>(entity: Mattr<'TData>, option: MattrOption<'TData>) : Mattr<'TData> =
        Mattrial.parse entity option
