open Expecto
open SectionMattr

[<Tests>]
let tests =
    test "A simple test" {
        let subject = Say.hello "World"
        Expect.equal subject "Hello World" "The strings should equal"
    }

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
