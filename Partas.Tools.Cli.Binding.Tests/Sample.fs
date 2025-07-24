module Tests

open Expecto
open Partas.Tools.Cli.Binding

[<CliBinding>]
type SomeUnion =
    | Run of command: string
    | Flag
    | KebabFlag
    | [<CompiledName("ToAst")>] Toast

[<CliBinding(Prefix = "-", Casing = Casing.None)>]
type OverridingUnion =
    | Baloney
    | [<CliBinding(Prefix="--")>] OverridenPrefix

[<CliBinding(BindingOptions = BindingOptions.StringLiteralFields)>]
type StringLiteralFields =
    | Format of formatting: string

[<Tests>]
let ``Binding Tests`` =
    testList "Serialization" [
        testCase "Simple Union with Field" <| fun _ ->
            let subject =
                Run "Test"
                |> CliBinding.toString
            "Single field DU renders to string"
            |> Expect.equal subject "--run=Test"
        test "Simple Union" {
            let subject = Flag |> CliBinding.toString
            Expect.equal subject "--flag" "Simple union renders to string"
        }
        test "Simple Union for Casing" {
            let subject = KebabFlag |> CliBinding.toString
            Expect.equal subject "--kebab-flag" "KebabFlag renders to --kebab-flag"
        }
        test "CompiledName on Case" {
            let subject = Toast |> CliBinding.toString
            Expect.equal subject "--to-ast" "CompiledName overrides the value"
        }
        test "CliBinding overriding" {
            let subjects =
                [
                    Baloney
                    OverridenPrefix
                ]
                |> List.map CliBinding.toString
            Expect.equal
                subjects
                [ "-Baloney"; "--OverridenPrefix" ]
                "CliBinding values default to inherited"
        }
        test "BindingOptions" {
            let subject = Format "%n" |> CliBinding.toString
            Expect.equal subject "--format=\"%n\"" "Fields are wrapped in strings"
        }
    ]
