module Partas.Tools.GitNet.CommitCategorisation

open ConventionalCommits
open Partas.Tools.GitNet.Types

// Only works with conventional commits, or with regex (because im lazy)

[<RequireQualifiedAccess>]
type BumpType =
    | Minor
    | Major
    | Patch
    | Epoch of string

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
        
        
        
