module Partas.Tools.GitNet.Types

open Fake.Core

type ParsedCommit = ConventionalCommits.Types.ParsedCommit
type GitNetCommit = {
    ParsedCommit: ParsedCommit
    Original: LibGit2Sharp.Commit
}

module GitNetCommit =
    open LibGit2Sharp.FSharp
    let private (|ConventionalOrBreaking|IsUnconventional|) = function
        | ParsedCommit.Conventional commit | ParsedCommit.Breaking commit -> ConventionalOrBreaking commit
        | ParsedCommit.Unconventional commit -> IsUnconventional commit
    let create parser commit =
        {
            ParsedCommit =
                commit
                |> Commit.message
                |> parser
            Original = commit
        }
    let original = function { Original = original } -> original
    let parsedCommit = function { ParsedCommit = parsed } -> parsed
    let sha = original >> Commit.hash
    let notes = original >> Commit.notes
    module Sha =
        /// <summary>
        /// 7 length sha used in links on github
        /// </summary>
        let abbreviated = sha >> _.Substring(0, 6)
        /// <summary>
        /// 5 length sha useful for use in documentation
        /// </summary>
        let humanFriendly = sha >> _.Substring(0,4)
    let author = original >> Commit.author
    let committer = original >> Commit.committer
    module Signatures =
        let private name = Signature.name
        let private email = Signature.email
        let private date = Signature.date
        let authorName = author >> name
        let committerName = committer >> name
        let authorEmail = author >> email
        let committerEmail = committer >> email
        let authorDate = author >> date
        let committerDate = committer >> date
    let parsedType = parsedCommit >> function
        | ConventionalOrBreaking { Type = typ } -> ValueSome typ
        | IsUnconventional _ -> ValueNone
    let subject = function
        | { ParsedCommit = ConventionalOrBreaking { Subject = subject } } -> subject
        | { Original = original } ->
            original |> Commit.subject
    let parsedSubject = function
        | { ParsedCommit = ParsedCommit.Unconventional _ } -> ValueNone
        | commit -> subject commit |> ValueSome
    let footers = function
        | { ParsedCommit = ConventionalOrBreaking { Footers = footers } } ->
            footers
        | _ -> []

// ========== Projects
type CrackedProject = {
    ProjectDirectory: string
    ProjectFileName: string
    SourceFiles: string list
    AssemblyFile: string voption
    Scope: string voption
    Epoch: string voption
}

// ========== Configs

/// <summary>
/// Configures the behaviour for parsing Sepochs in GitNet
/// </summary>
type SepochConfig = {
    IncludeScopes: string list
    ExcludeScopes: string list
    IncludeEpochs: string list
    ExcludeEpochs: string list
}
/// <summary>
/// Configures the behaviour for parsing triggers for bumps in GitNet
/// </summary>
type SemverConfig = {
    /// <summary>
    /// Types that would trigger a Patch bump.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "fix" ]</c>
    /// </remarks>
    BumpPatchValues: string list
    /// <summary>
    /// Types that would trigger a Minor bump.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "add"; "update"; "deprecate" ]</c>
    /// </remarks>
    BumpMinorValues: string list
    /// <summary>
    /// Types that would trigger a MAJOR bump. The use of <c>!</c> and
    /// the footer <c>BREAKING CHANGE</c> would still trigger MAJOR bumps
    /// as per Conventional Commits spec.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "feat" ]</c>
    /// </remarks>
    BumpMajorValues: string list
    /// <summary>
    /// The Footer key to match for Epoch bumps. The new epoch would be the
    /// value to that key.
    /// </summary>
    /// <remarks>
    /// <para>By default: <c>[ EPOCH ]</c></para>
    /// <para>Note: You must ensure values are valid Footer keys as per
    /// the Conventional Commits spec.</para>
    /// </remarks>
    BumpEpochValues: string list
}

type AutoScopeType =
    | NoScoping
    | Transform of transformer: (string -> string)

type ProjectConfig = {
    AutoScope: AutoScopeType
    UseEpochs: bool
    UseScopes: bool
    UpdateAssemblyFiles: bool
}

type ChangelogConfig = {
    UsesGroupByDefault: bool
    GroupBy:
        string voption *
        GitNetCommit
         -> string voption * GitNetCommit
    UnmatchedGroupKey: string
}

/// <summary>
/// Configures the behaviour for parsing commits and tags in GitNet
/// </summary>
type GitNetConfig = {
    SepochConfig: SepochConfig
    SemverConfig: SemverConfig
    RepositoryPath: string
    ProjectConfig: ProjectConfig
    ChangelogConfig: ChangelogConfig
}

module SepochConfig =
    let init = {
        SepochConfig.ExcludeEpochs = []
        IncludeEpochs = []
        IncludeScopes = []
        ExcludeScopes = []
    }
module SemverConfig =
    let init = {
        SemverConfig.BumpEpochValues = [ "EPOCH" ]
        BumpPatchValues = [ "fix" ]
        BumpMinorValues = [ "add"; "update"; "deprecate" ]
        BumpMajorValues = [ "feat" ]
    }
module ProjectConfig =
    let init = {
        ProjectConfig.AutoScope = AutoScopeType.NoScoping
        UseEpochs = true
        UseScopes = true
        UpdateAssemblyFiles = true
    }
module ChangelogConfig =
    let init: ChangelogConfig = { UsesGroupByDefault = true
                                  GroupBy = id
                                  UnmatchedGroupKey = "Other" }
module GitNetConfig =
    let init = {
        GitNetConfig.ProjectConfig = ProjectConfig.init
        SepochConfig = SepochConfig.init
        SemverConfig = SemverConfig.init
        RepositoryPath = __SOURCE_DIRECTORY__
        ChangelogConfig = ChangelogConfig.init
    }
    let autoScope = function
        { ProjectConfig = { AutoScope = value } } ->
            match value with
            | Transform transformer -> ValueSome transformer
            | NoScoping -> ValueNone

open LibGit2Sharp.FSharp
// ========= Workers
type GitNetRuntime(?config: GitNetConfig) =
    let config =
        lazy
        #if DEBUG
        if config.IsNone then
            Trace.log "GitNet running with default configuration."
        #endif
        defaultArg config GitNetConfig.init
    let repo =
        lazy
        let doLoad =
             lazy
             config.Value.RepositoryPath
             |> Repository.load
        #if DEBUG
        Trace.log $"GitNet loading repository from path: %s{config.Value.RepositoryPath}"
        try
        doLoad.Value
        with e -> failwith $"GitNet must be run from, or given a valid repository directory through creating the GitNetRuntime\
                            with a config that has RepositoryPath set correctly.\nException: {e}"
        #else
        doLoad.Value
        #endif
    let commits =
        lazy
        repo.Value
        |> Repository.commits
    let tags =
        lazy
        repo.Value
        |> Repository.tags
    let branches =
        lazy
        repo.Value
        |> Repository.branches
    /// All tags that are prescoped, or have been scoped
    /// according to the config settings.
    let scopedTags = lazy(
        let scoper =
            config.Value
            |> GitNetConfig.autoScope
            |> ValueOption.map(fun func -> func >> ValueSome)
            |> ValueOption.defaultValue (fun _ -> ValueNone)
        // query {
        //     for tag in tags.Value do
        //     
        // }
        )
    let tagCollection = lazy(
        5
        )
        
    member this.Tags = tags.Value
    member this.Branches = branches.Value
    member this.Commits = commits.Value
