module ConventionalCommitTests

open Expecto
open Partas.Tools.ConventionalCommits

let emptyInput = "type: subject"
let emptyCommit = {
    Type = "type"
    Scope = ValueNone
    Footers = []
    Message = ValueNone
    Subject = "subject"
}

[<Tests>]
let tests =
    testList "Conventional Spec Type" [
        test "Returns Empty Unconventional on null input" {
            null
            |> ConventionalCommit.parse
            |> Expect.expect (Unconventional "")
        }
        test "Parses Simple Type" {
            emptyInput
            |> ConventionalCommit.parse
            |> Expect.expect (Conventional emptyCommit)
        }
        test "Space must follow ':' in Conventional Commit Type" {
            let input = "type:subject"
            let expected = Unconventional input
            input
            |> ConventionalCommit.parse
            |> function actual -> "" |> Expect.equal actual expected
        }
        test "Spaces can follow Type ':'" {
            let expected = Conventional emptyCommit
            "type:             subject"
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Spaces cannot precede Type" {
            let input = "type : subject"
            let expected = Unconventional input
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Type must be a noun" {
            let input = "typ3 : subject"
            let expected = Unconventional input
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Type is required" {
            let input = "(scope): subject"
            let expected = Unconventional input
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Capitalization of type is ignored" {
            let input = "TYPE: subject"
            let expected = Conventional emptyCommit
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Breaking change !" {
            let input = "type!: subject"
            let expected = Breaking emptyCommit
            input |> ConventionalCommit.parse
            |> Expect.expect expected
        }
    ]

[<Tests>]
let tests2 =
    testList "Conventional Spec Scope" [
        testTheory "Simple Scopes are parsed" [
            "type(scope): subject"
            "type(SCOPE): subject"
        ] <| fun input ->
            let expected =  Conventional { emptyCommit with Scope = ValueSome "scope" }
            input |> ConventionalCommit.parse
            |> Expect.expect expected
        test "Breaking change !" {
            let input = "type(scope)!: subject"
            let expected = Breaking { emptyCommit with Scope = ValueSome "scope" }
            input |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        testTheory "Spec correctly rejects these inputs" [
            "type(scope ): subject"
            "(scope): subject"
            "type( scope): subject"
            "type(_scope_): subject"
            "type(1cop): subject"
            "type(): subject"
            "type(*): subject"
            "type(😀): subject"
            "type(sc ope): subject"
            "type[scope]: subject"
            "type(scope)): subject"
            "type(scope) subject"
            "type(scope):: subject"
            "type(scop\ne): subject"
            "type(scope: subject"
        ] <| fun input ->
            input
            |> ConventionalCommit.parse
            |> Expect.expect (Unconventional input)
    ]

[<Tests>]
let tests3 =
    testList "Conventional Spec Message" [
        testTheory "Spec is enforced" [
            false,"type: subject
fail
"
            false,"type: subject
fail
fail
fail"
            false, "type: subject
fail

even though I'm here"
            true,"type: subject

Message"
            true, "type: subject

This is a correct message.

We can be over multiples lines.

Anything234 😀😀😁 goes!"
        ] <| fun (shouldBeConventional,input) ->
            let expected: ParsedCommit -> bool =
                if shouldBeConventional
                then _.IsConventional
                else _.IsUnconventional
            input |> ConventionalCommit.parse
            |> expected
            |> Flip.Expect.isTrue ""
        test "Message is correctly parsed" {
            let input = "type: subject

This is a message with something to say.

I want to say...

I'm sorry, but this is a terrible test."
            let expected = Conventional {
                emptyCommit with Message = ValueSome "This is a message with something to say.

I want to say...

I'm sorry, but this is a terrible test."
            }
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Excess newlines are stripped" {
            let input = "type: subject





This is a message with something to say.

I want to say...

I'm sorry, but this is a terrible test."
            let expected = Conventional {
                emptyCommit with Message = ValueSome "This is a message with something to say.

I want to say...

I'm sorry, but this is a terrible test."
            }
            input
            |> ConventionalCommit.parse
            |> Expect.expect expected            
        }
    ]

[<Tests>]
let tests4 =
    testList "Conventional Spec Footers" [
        test "Footer keys are case-insensitive" {
            let input = "type: subject

Message

Footer: value"
            let expected = Conventional {
                emptyCommit with
                    Message = ValueSome "Message"
                    Footers = [
                        Footer("footer","value")
                    ]
            }
            input |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        test "Footer keys allow ' #' separator" {
            let input = "type: subject

Message

footer #value"
            let expected = Conventional {
                emptyCommit with
                    Message = ValueSome "Message"
                    Footers = [
                        Footer("footer","value")
                    ]
            }
            input |> ConventionalCommit.parse
            |> Expect.expect expected
        }
        testTheory "Footer Keys Spec" [
            "footer:value"
            "footer#value"
            // TODO - is this 'technically' a valid key,val pair for a footer?
            // "Footer:  "
            ": value"
            "footer_Bogus: value"
            "foo ter: value"
            " footer: value"
            "footer:#value"
            "footer#: value"
        ] <| fun input ->
            let input' = input |> sprintf "type: subject

Message

%s"
            let expected = Conventional {
                emptyCommit with
                    Message =
                        input
                        |> sprintf "Message

%s"
                        |> ValueSome
            }
            input'
            |> ConventionalCommit.parse
            |> Expect.expect expected
            ()
    ]
