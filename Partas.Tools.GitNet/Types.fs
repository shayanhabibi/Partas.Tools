module Partas.Tools.GitNet.Types

open System
open System.Collections.Frozen
open System.Collections.Generic
open System.Collections.Immutable
open Partas.Tools.ConventionalCommits
open Fake.Core
open Partas.Tools.SepochSemver

/// <summary>
/// Exposes LibGit2Sharp types to the namespace without
/// pollution from other library associated types and methods..
/// </summary>
[<AutoOpen>]
module Aliases =
    type DiffTargets = LibGit2Sharp.DiffTargets
    type ExplicitPathsOptions = LibGit2Sharp.ExplicitPathsOptions
    type TreeChanges = LibGit2Sharp.TreeChanges
    type Commit = LibGit2Sharp.Commit
    type Tag = LibGit2Sharp.Tag
    type GitObject = LibGit2Sharp.GitObject

/// <summary>
/// Discriminated union as a response to functions that determine
/// what the next version bump type would be, or should be.
/// </summary>
[<RequireQualifiedAccess>]
type BumpType =
    | Patch
    | Minor
    | Major
    | Epoch of string

/// <summary>
/// Container for commits; contains the original LibGit2Sharp
/// commit and the parsed commit.
/// </summary>
type GitNetCommit = {
    ParsedCommit: ParsedCommit
    Original: LibGit2Sharp.Commit
}

// ========== Projects
/// <summary>
/// Container type for the returned details of a parsed &amp;/or (Discovered)
/// project.
/// </summary>
/// <remarks>
/// The paths provided in the container are relative to the
/// working directory of the git repository. This is to make
/// operating with the Git tree simple and hassle free.<br/><br/>
/// If the <c>fsproj</c> file contains a <c>GitNetScope</c> or
/// <c>GitNetEpoch</c> value, then they are extracted.<br/><br/>
/// All compiled values are extracted, and the AssemblyFile is also
/// extracted.
/// </remarks>
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
    /// <summary>
    /// Specifies which scopes are included in parsing and traversal.<br/><br/>
    /// A non-empty value will restrict scopes to only the defined inclusions.
    /// </summary>
    IncludeScopes: string list
    /// <summary>
    /// Specifies a list of scopes that are excluded in parsing and traversal.<br/><br/>
    /// A non empty value will not restrict what is included.
    /// </summary>
    ExcludeScopes: string list
    /// <summary>
    /// Specifies a list of Epochs that are included in parsing and traversal.
    /// </summary>
    IncludeEpochs: string list
    /// <summary>
    /// Specifies a list of Epochs that are excluded in parsing and traversal.
    /// </summary>
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

/// <summary>
/// Union field used in <c>ProjectConfig</c> to determine the behaviour
/// of GitNet with regards to scoping projects that have not been scoped
/// previously.
/// </summary>
type AutoScopeType =
    | NoScoping
    /// <summary>
    /// The autoscoper is applied to project names and determines
    /// whether that project should be within a scope or not.
    /// Scoped projects do not get included in other tags.
    /// </summary>
    | Transform of transformer: (string -> string voption)

/// <summary>
/// Configuration for details regarding project interaction.
/// </summary>
type ProjectConfig = {
    AutoScope: AutoScopeType
    UseEpochs: bool
    UseScopes: bool
    UpdateAssemblyFiles: bool
}

/// <summary>
/// Configuration details regarding the changelog generation.
/// </summary>
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


/// <summary>
/// Pair of a git tag, and the parsed <c>SepochSemver</c>, when the
/// <c>Sepoch</c> value is not <c>None</c>.
/// </summary>
type GitNetSepochTag = {
    GitTag: LibGit2Sharp.Tag
    SepochSemver: SepochSemver
}
/// <summary>
/// Pair of a git tag, and the parsed <c>SemVersion</c>, when the
/// <c>Sepoch</c> value is <c>None</c>.
/// </summary>
type GitNetSemverTag = {
    GitTag: LibGit2Sharp.Tag
    Semver: Semver.SemVersion
}
/// <summary>
/// Discriminated union which provides cases to delineate tags
/// that are parseable as <c>SepochSemver</c> tags, <c>Semver</c> tags,
/// or neither.
/// </summary>
type GitNetTag =
    /// A SepochSemver and original LibGit2Sharp.Tag record.
    | GitNetTag of GitNetSepochTag
    /// A Semver and original LibGit2Sharp.Tag record
    | SemVerTag of GitNetSemverTag
    /// A tag which is not Semver compatible.
    | GitTag of LibGit2Sharp.Tag

module GitNetTag =
    open LibGit2Sharp.FSharp
    open System
    module Patterns =
        let (|Tag|) = function
            | GitTag tag | GitNetTag { GitTag = tag }
            | SemVerTag { GitTag = tag } ->
                tag
    let private fromTagImpl tag =
        tag |> Tag.name
        |> parseSepochSemver
        |> function
        | { Sepoch = Sepoch.None } as sepSemVer ->
            { GitTag = tag
              Semver = sepSemVer.SemVer }
            |> SemVerTag
        | sepSemVer ->
            { GitTag = tag
              SepochSemver = sepSemVer }
            |> GitNetTag
    let fromTag tag =
        // if the parse fails then we still pass it
        try
            fromTagImpl tag
        with
        | e ->
            #if DEBUG
            e |> printfn "%A"
            #endif
            GitTag tag
    /// <summary>
    /// Module for interaction with the <c>LibGit2Sharp.Tag</c> ellement
    /// of the <c>GitNetTag</c>
    /// </summary>
    module Git =
        /// <summary>
        /// Returns the <c>LibGit2Sharp.Tag</c> from the <c>GitNetTag</c>.
        /// </summary>
        let tag = function Patterns.Tag tag -> tag
        /// <summary>
        /// Returns the name of the <c>LibGit2Sharp.Tag</c> from the <c>GitNetTag</c>.
        /// </summary>
        let name = tag >> Tag.name
        /// <summary>
        /// Returns the full name of the <c>LibGit2Sharp.Tag</c> from the <c>GitNetTag</c>.
        /// </summary>
        let fullName = tag >> Tag.fullName
        /// <summary>
        /// Returns the sha from the <c>LibGit2Sharp.Tag</c> from the <c>GitNetTag</c>.
        /// </summary>
        let sha = tag >> Tag.target >> GitObject.sha
        /// <summary>
        /// Returns the target commit of the <c>LibGit2Sharp.Tag</c> from the <c>GitNetTag</c>.
        /// </summary>
        let commit = tag >> Tag.target >> GitObject.peel<LibGit2Sharp.Commit>
    /// Returns the string representation of the tag. If SemVer compatible, then this
    /// will be the SemVer formatted string. Otherwise it is the human friendly name
    /// of the tag.
    let toString = function
        | GitNetTag { SepochSemver = sepoch } ->
            sepoch.ToString()
        | SemVerTag { Semver = semver } ->
            semver.ToString()
        | GitTag tag ->
            // Shortname of tag
            tag |> Tag.name
    let toTitleString = function
        | SemVerTag { Semver = semver }
        | GitNetTag { SepochSemver = { SemVer = semver } } ->
            semver.ToString()
            |> Some
        | _ -> None
    /// <summary>
    /// Returns an option which contains either <c>SemVer</c> compatible tag cases from
    /// the passed <c>GitNetTag</c>, or returns <c>ValueNone</c>
    /// </summary>
    let chooseSemverCompatible = function
        | (GitNetTag _)
        | (SemVerTag _) as tag -> ValueSome tag
        | _ -> ValueNone
    /// <summary>
    /// Returns an error where the successful case matches <c>GitNetTag</c>s that are
    /// <c>SemVer</c> compatible, and the failure case contains the unwrapped tag.
    /// </summary>
    let chooseSemverCompatibleOrError = function
        | (GitNetTag _) | (SemVerTag _) as tag -> Ok tag
        | GitTag tag -> Error tag
    let getScope = function
        | GitNetTag { SepochSemver = { Sepoch = sepoch } } ->
            sepoch.GetScope
        | _ -> ValueNone
/// <summary>
/// Configuration details regarding the <c>SepochSemver</c> parsing
/// and other behaviours.
/// </summary>
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
    /// Retrieve the key that is used to group commits which do not match any other
    /// group.
    let unmatchedGroupKey = _.UnmatchedGroupKey
    /// Retrieves the function that is used to further modify the grouping of the commits.
    let groupBy = _.GroupBy
    /// Whether or not the default grouping path is used.
    let isGroupByDefault = _.UsesGroupByDefault
    /// Set the key that is used to group commits which do not match any other group.
    /// Default is "Other".
    let withUnmatchedGroupKey key config = { config with UnmatchedGroupKey = key }
    /// Set the function that is used to further modify the grouping of the commits.
    /// Default is 'id'.
    let withGroupBy handler changelogConfig = { changelogConfig with GroupBy = handler }
    /// Set whether or not to use the default grouping functions before passing to the
    /// grouping function contained within the config.
    let withUsesGroupByDefault value changelogConfig = { changelogConfig with UsesGroupByDefault = value }
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

/// Functions for GitNetCommits; mostly acts as shortcuts
/// to interacting with the embedded parsed commit, or the
/// original libgit2sharp commit.
module GitNetCommit =
    open LibGit2Sharp.FSharp
    /// <summary>
    /// Active pattern that delineates a parsed commit between
    /// conventional commits and unconventional commits.
    /// Conventional commits includes both the <c>Breaking</c> and
    /// standard cases.
    /// </summary>
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
    /// Returns the original commit object from the container.
    let original = function { Original = original } -> original
    /// Returns the parsed commit object from the container.
    let parsedCommit = function { ParsedCommit = parsed } -> parsed
    /// Retrieves the SHA key from the original commit object.
    let sha = original >> Commit.sha
    /// Retrieves any notes attached to the original commit object.
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
    /// <summary>
    /// Author of the original commit object in the container.
    /// </summary>
    /// <remarks>
    /// <para>The author indicates which person wrote and stages the commit.</para>
    /// <para>The committer is the individual that merged/committed the changes.</para>
    /// </remarks>
    let author = original >> Commit.author
    /// <summary>
    /// Committer of the original commit object in the container.
    /// </summary>
    /// <remarks>
    /// <para>The author indicates which person wrote and stages the commit.</para>
    /// <para>The committer is the individual that merged/committed the changes.</para>
    /// </remarks>
    let committer = original >> Commit.committer
    module Signatures =
        let private name = Signature.name
        let private email = Signature.email
        let private date = Signature.date
        /// Helper that gets the author name directly from the
        /// commit object container.
        let authorName = author >> name
        /// Retrieves the committer name directly from the
        /// commit object container.
        let committerName = committer >> name
        /// Retrieves the author email from the commit object
        /// container.
        let authorEmail = author >> email
        /// Retrieves the committer email from the commit object container.
        let committerEmail = committer >> email
        /// Retrieves the date that the code change was authored from the commit
        /// object container.
        let authorDate = author >> date
        /// Retrieves the date that the code was committed from the object f
        let committerDate = committer >> date
    let parsedType = parsedCommit >> function
        | ConventionalOrBreaking { Type = typ } -> ValueSome typ
        | IsUnconventional _ -> ValueNone
    /// <summary>
    /// Retrieves the subject from a <c>ParsedCommit</c>.
    /// </summary>
    /// <remarks>
    /// In the case of a non-conventional parsed commit, the subject
    /// of the original commit is retrieved.
    /// </remarks>
    let subject = function
        | { ParsedCommit = ConventionalOrBreaking { Subject = subject } } -> subject
        | { Original = original } ->
            original |> Commit.subject
    /// <summary>
    /// Will only return the subject of a parsed <c>Conventional</c>
    /// commit, otherwise returns <c>ValueNone</c>
    /// </summary>
    let parsedSubject = function
        | { ParsedCommit = ParsedCommit.Unconventional _ } -> ValueNone
        | commit -> subject commit |> ValueSome
    /// <summary>
    /// Returns the footers for a parsed commit.
    /// </summary>
    /// <remarks>
    /// If the target is an <c>Unconventional</c> Parsed Commit, then
    /// an empty list is returned.
    /// </remarks>
    let footers = function
        | { ParsedCommit = ConventionalOrBreaking { Footers = footers } } ->
            footers
        | _ -> []
