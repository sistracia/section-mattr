namespace SectionMattr

open System.Text.RegularExpressions

[<Struct>]
type MattrSection<'p> =
    { key: string
      data: 'p
      content: string }

[<Struct>]
type Mattr<'p> =
    { content: string
      sections: MattrSection<'p> array }

type Parser<'p> = MattrSection<string> -> MattrSection<'p> array -> MattrSection<'p>

[<Struct>]
type MattrOption1 = { section_delimiter: string }

[<Struct>]
type MattrOption2<'p> = { parse: Parser<'p> }

[<Struct>]
type MattrOption<'p> =
    { section_delimiter: string
      parse: Parser<'p> }

module Mattrial =
    let defaultOption<'p> (parser: Parser<'p>) : MattrOption<'p> =
        { MattrOption.section_delimiter = "---"
          MattrOption.parse = parser }

    let createSection () : MattrSection<string> =
        { MattrSection.key = ""
          MattrSection.data = ""
          MattrSection.content = "" }

    let toObject<'p> (input: string) : Mattr<'p> =
        { Mattr.content = input
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

    let appendData<'p> (data: 'p) (section: MattrSection<string>) : MattrSection<'p> =
        let parsed: MattrSection<'p> =
            { MattrSection.key = section.key
              MattrSection.content = section.content
              MattrSection.data = data }

        parsed

    let parse<'p> (entity: Mattr<'p>) (opts: MattrOption<'p>) =
        let delim: string = opts.section_delimiter
        let lines: string array = Regex.Split(entity.content, @"\r?\n")

        let mutable _entity: Mattr<'p> = entity
        let mutable sections: MattrSection<'p> array option = None
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
                    | Some(sections: MattrSection<'p> array) ->
                        Some(Array.append sections [| (opts.parse section sections) |])
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
        | Some(sections: MattrSection<'p> array) -> { _entity with sections = sections }
        | None -> _entity

[<Struct>]
type NewMattr =
    static member sections(input: string) : Mattr<string> =
        let file: Mattr<string> = Mattrial.toObject input
        let option: MattrOption<string> = Mattrial.defaultOption Mattrial.identity
        Mattrial.parse file option

    static member sections<'p>(input: string, parser: Parser<'p>) : Mattr<'p> =
        let file: Mattr<'p> = Mattrial.toObject input
        let option: MattrOption<'p> = Mattrial.defaultOption parser
        Mattrial.parse file option

    static member sections(input: string, option: MattrOption1) : Mattr<string> =
        let file: Mattr<string> = Mattrial.toObject input

        let option: MattrOption<string> =
            { MattrOption.section_delimiter = option.section_delimiter
              MattrOption.parse = Mattrial.identity }

        Mattrial.parse file option

    static member sections<'p>(input: string, option: MattrOption2<'p>) : Mattr<'p> =
        let file: Mattr<'p> = Mattrial.toObject input
        let option: MattrOption<'p> = Mattrial.defaultOption option.parse
        Mattrial.parse file option

    static member sections<'p>(input: string, option: MattrOption<'p>) : Mattr<'p> =
        let file: Mattr<'p> = Mattrial.toObject input
        Mattrial.parse file option

    static member sections(entity: Mattr<string>) : Mattr<string> =
        let option: MattrOption<string> = Mattrial.defaultOption Mattrial.identity
        Mattrial.parse entity option

    static member sections<'p>(entity: Mattr<'p>, parser: Parser<'p>) : Mattr<'p> =
        let option: MattrOption<'p> = Mattrial.defaultOption parser
        Mattrial.parse entity option

    static member sections(entity: Mattr<string>, option: MattrOption1) : Mattr<string> =
        let option: MattrOption<string> =
            { MattrOption.section_delimiter = option.section_delimiter
              MattrOption.parse = Mattrial.identity }

        Mattrial.parse entity option

    static member sections<'p>(entity: Mattr<'p>, option: MattrOption2<'p>) : Mattr<'p> =
        let option: MattrOption<'p> = Mattrial.defaultOption option.parse
        Mattrial.parse entity option

    static member sections<'p>(entity: Mattr<'p>, option: MattrOption<'p>) : Mattr<'p> = Mattrial.parse entity option
