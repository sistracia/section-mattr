namespace SectionMattr

open System.Text
open System.Text.RegularExpressions

[<Struct>]
type MattrSection<'TData> =
    { key: string
      data: 'TData
      content: string }

type IMattrParser<'TData> =
    abstract member Parse: MattrSection<string> * MattrSection<'TData> array -> MattrSection<'TData>

type DefaultMattrParser() =
    interface IMattrParser<string> with
        member _.Parse(section: MattrSection<string>, _: MattrSection<string> array) : MattrSection<string> = section

[<Struct>]
type MattrOption<'TData> =
    { section_delimiter: string
      parser: IMattrParser<'TData> }

    member this.SetDelimiter(delimiter: string) =
        { parser = this.parser
          section_delimiter = delimiter }

    member this.SetParser(parser: IMattrParser<'TNewData>) =
        { section_delimiter = this.section_delimiter
          parser = parser }

    static member Default =
        { MattrOption.section_delimiter = "---"
          MattrOption.parser = DefaultMattrParser() }


[<Struct>]
type Mattr<'TData> =
    { content: string
      sections: MattrSection<'TData> array }

type Parser<'TData> = MattrSection<string> -> MattrSection<'TData> array -> MattrSection<'TData>

module Mattrial =
    let defaultOption () =
        { MattrOption.section_delimiter = "---"
          MattrOption.parser = DefaultMattrParser() }

    let createSection () : MattrSection<string> =
        { MattrSection.key = ""
          MattrSection.data = ""
          MattrSection.content = "" }

    let stringToObject<'TData> (input: string) : Mattr<'TData> =
        { Mattr.content = input
          Mattr.sections = [||] }

    let bufferToObject<'TData> (input: byte array) : Mattr<'TData> =
        { Mattr.content = Encoding.Default.GetString input
          Mattr.sections = [||] }

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

    let appendData<'TData> (data: 'TData) (section: MattrSection<string>) : MattrSection<'TData> =
        let parsed: MattrSection<'TData> =
            { MattrSection.key = section.key
              MattrSection.content = section.content
              MattrSection.data = data }

        parsed

    let parse<'TData> (entity: Mattr<'TData>) (opts: MattrOption<'TData>) =
        let delim: string = opts.section_delimiter
        let lines: string array = Regex.Split(entity.content, @"\r?\n")

        let mutable _entity: Mattr<'TData> = entity
        let mutable sections: MattrSection<'TData> array option = None
        let mutable content: string array = [||]
        let mutable section: MattrSection<string> = createSection ()
        let mutable stack: string array = [||]

        let initSections (contentStr: string) =
            _entity <- { _entity with content = contentStr }
            sections <- Some [||]
            content <- [||]

        let closeSection (contentStr: string) =
            if (stack.Length <> 0) then
                section <-
                    { section with
                        key = getKey (stack |> Array.tryItem 0) delim
                        content = contentStr }

                sections <-
                    match sections with
                    | Some(sections: MattrSection<'TData> array) ->
                        Some(Array.append sections [| (opts.parser.Parse(section, sections)) |])
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
                        MattrSection.data = content |> String.concat "\n" }

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
        | Some(sections: MattrSection<'TData> array) -> { _entity with sections = sections }
        | None -> _entity

[<Struct>]
type NewMattr =
    static member sections(input: string) : Mattr<string> =
        let file: Mattr<string> = Mattrial.stringToObject input
        let option: MattrOption<string> = Mattrial.defaultOption ()
        Mattrial.parse file option

    static member sections<'TData>(input: string, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.stringToObject input
        let option: MattrOption<'TData> = (Mattrial.defaultOption ()).SetParser parser
        Mattrial.parse file option

    static member sections<'TData>(input: string, option: MattrOption<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.stringToObject input
        Mattrial.parse file option

    static member sections(input: byte array) : Mattr<string> =
        let file: Mattr<string> = Mattrial.bufferToObject input
        let option: MattrOption<string> = Mattrial.defaultOption ()
        Mattrial.parse file option

    static member sections<'TData>(input: byte array, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.bufferToObject input
        let option: MattrOption<'TData> = (Mattrial.defaultOption ()).SetParser parser
        Mattrial.parse file option

    static member sections<'TData>(input: byte array, option: MattrOption<'TData>) : Mattr<'TData> =
        let file: Mattr<'TData> = Mattrial.bufferToObject input
        Mattrial.parse file option

    static member sections(entity: Mattr<string>) : Mattr<string> =
        let option: MattrOption<string> = Mattrial.defaultOption ()
        Mattrial.parse entity option

    static member sections<'TData>(entity: Mattr<'TData>, parser: IMattrParser<'TData>) : Mattr<'TData> =
        let option: MattrOption<'TData> = (Mattrial.defaultOption ()).SetParser parser
        Mattrial.parse entity option

    static member sections<'TData>(entity: Mattr<'TData>, option: MattrOption<'TData>) : Mattr<'TData> =
        Mattrial.parse entity option
