module Partas.Tools.GitNet.CommitCategorisation

open Partas.Tools.ConventionalCommits
open Partas.Tools.GitNet.Types

// Categorisation for Conventional Commits, or using XParsec.



[<AutoOpen>]
module private Helpers =
    let inline private isMatching gitNetList (value: string)=
        gitNetList |> List.contains value
    let isMinor (config: GitNetConfig) =
        isMatching config.SemverConfig.BumpMinorValues
        >> function true -> ValueSome BumpType.Minor | false -> ValueNone
    let isPatch (config: GitNetConfig) =
        isMatching config.SemverConfig.BumpPatchValues
        >> function true -> ValueSome BumpType.Patch | false -> ValueNone
    let isMajor (config: GitNetConfig) =
        isMatching config.SemverConfig.BumpMajorValues
        >> function true -> ValueSome BumpType.Major | false -> ValueNone
    let makeMatchers config = struct (isPatch config, isMinor config, isMajor config)

/// <summary>
/// Checks the <c>ParsedCommit</c> against the <c>GitNetConfig</c> to
/// determine if the commit would indicate a bump, and if so, of what type.
/// </summary>
/// <remarks>
/// <para>Automatically returns <c>ValueNone</c> for UnconventionalCommits currently.</para>
/// <para>Checks in order of precedence:<br/>
/// <c>Epoch > Major > Minor > Patch</c></para>
/// </remarks>
/// <param name="config">The provider for the values to match against the bump types.</param>
let willBump (config: GitNetConfig) =
    let struct (
        isPatch,
        isMinor,
        isMajor
    ) = makeMatchers config
    let footerContainsEpoch (footers: Footer list) =
        footers
        |> List.tryPick (function
            | Footer(key,value) when config.SemverConfig.BumpEpochValues |> List.contains key -> Some value
            | _ -> None)
    function
    | Conventional { Footers = footers } | Breaking { Footers = footers }
        when footerContainsEpoch footers |> _.IsSome ->
        footerContainsEpoch footers |> _.Value |> BumpType.Epoch |> ValueSome
    | Conventional { Type = typ } ->
        let orElse func = ValueOption.orElse (func typ)
        isMajor typ
        |> orElse isMinor
        |> orElse isPatch
    | Breaking _ ->
        BumpType.Major |> ValueSome
    // TODO - config needs optional regex for picking up unconventional commits
    | _ -> ValueNone
module ChangelogConfig =
    // TODO - add recognition for "KNOWN-BUG" footer.
    let private defaultGroupBy config: string voption * GitNetCommit -> _ =
        function
        | ValueNone, commit ->
            let group idx key = ValueSome $"<!-- {idx} --> {key}", commit
            let otherGroup = ValueSome config.UnmatchedGroupKey, commit
            match commit with
            | { ParsedCommit = ParsedCommit.Unconventional _} ->
                otherGroup
            | { ParsedCommit = ParsedCommit.Conventional commit } ->
                match commit with
                | { Footers = footers } when footers |> List.exists _.IsBreakingChange ->
                    group 0 "Breaking Changes"
                | { Type = "doc" | "docs" }
                | { Scope = ValueSome "doc" | ValueSome "docs" } ->
                    group 5 "Documentation"
                | { Type = "feat" | "added" | "new" } ->
                    group 1 "Added"
                | { Type = "changed" | "update" | "updated" | "change" } ->
                    group 3 "Changed"
                | { Type = "fix" | "bug" } ->
                    group 4 "Fixed"
                | { Type = "remove" | "removed" | "del" | "delete" } ->
                    group 3 "Removed"
                | { Type = "depr" | "deprecate" | "deprecated" } ->
                    group 2 "Deprecated"
                | { Type = "vuln" | "sec" | "security" | "vulnerability" } ->
                    group 6 "Security"
                | _ -> otherGroup
            | { ParsedCommit = Breaking _ } -> group 0 "Breaking Changes"
        | ValueSome _ as groupKey, commit ->
            groupKey, commit
    let groupBy config: (string voption * GitNetCommit) seq -> _ =
        match config with
        | { UsesGroupByDefault = false } ->
            Seq.map config.GroupBy
        | { GroupBy = func } ->
            Seq.map (defaultGroupBy config >> func)
let groupCommits (config: GitNetConfig) =
    Seq.map (fun pc -> ValueNone,pc)
    >> ChangelogConfig.groupBy config.ChangelogConfig
    >> fun groupKeyValue -> query {
        for _key,_value in groupKeyValue do
        groupValBy _value _key into groups
        select groups
    }
