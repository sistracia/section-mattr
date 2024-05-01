module Basic

open SectionMattr

let stringContent =
    @"Content before the sections.

---

More content.

---one
title: One
---

This is section one."

// string -> Mattr<string>
let mattr1: Mattr<string> = NewMattr.Sections stringContent
mattr1 |> ignore

// byte[] -> Mattr<string>
let mattr2: Mattr<string> =
    NewMattr.Sections(System.Text.Encoding.UTF8.GetBytes(stringContent))

mattr2 |> ignore

// Mattr<string> -> Mattr<string>
// `Mattr.Sections` will always be replaced by the section from `Mattr.Content`
// no matter there is section or not in `Mattr.Content`
let mattr3: Mattr<string> =
    NewMattr.Sections
        { Mattr.Content = stringContent
          Mattr.Sections = [||] }


mattr3 |> ignore
