module Main

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [
        CLIArguments.Colours 255
        CLIArguments.JoinWith "."
    ] argv
