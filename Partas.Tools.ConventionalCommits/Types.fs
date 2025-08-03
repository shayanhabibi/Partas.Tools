[<AutoOpen>]
module Partas.Tools.ConventionalCommits.AutoOpenTypes

// ======== Commits
type Footer =
    | Footer of key: string * value: string
    | BreakingChange of value: string
module Footer =
    [<Literal>]
    let BreakingChangeKey = "BREAKING-CHANGE"

type Footer with
    member this.Key =
        match this with
        | Footer(key, _) -> key
        | BreakingChange _ -> Footer.BreakingChangeKey
    member this.Value =
        match this with
        | BreakingChange value
        | Footer(_, value) -> value
    member this.Destructure =
        match this with
        | Footer(key,value) -> key,value
        | BreakingChange value -> Footer.BreakingChangeKey,value
    
type ConventionalCommit = {
    Type: string
    Scope: string voption
    Subject: string
    Message: string voption
    Footers: Footer list
}
type ParsedCommit =
    | Conventional of ConventionalCommit
    | Breaking of ConventionalCommit
    | Unconventional of string
module ConventionalCommit =
    let getFooterValue key: ConventionalCommit -> string ValueOption = function
        | { Footers = footers } ->
            footers
            |> List.tryPick(fun footer ->
                let key',value = footer.Destructure
                if key' = key then
                    Some value
                else None
                )
            |> ValueOption.ofOption
    let unsafeGetFooterValue key: ConventionalCommit -> string = function
        | { Footers = footers } ->
            footers
            |> List.pick(fun footer ->
                let key',value = footer.Destructure
                if key' = key then Some value else None)
