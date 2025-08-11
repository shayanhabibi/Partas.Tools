module Partas.Tools.GitNet.GitTraversal

open Fake.IO
open Partas.Tools.GitNet.Types
open System.Linq
open Microsoft.FSharp.Linq.RuntimeHelpers
open Partas.Tools
open Partas.Tools.GitNet.RepoCracker
open SepochSemver
open LibGit2Sharp.FSharp
open FSharp.Linq
open System
open System.Collections.Generic

// Functions for exploring the Git tree.

let tryParseSepochSemverFromTag (tag: LibGit2Sharp.Tag) =
    try
    tag
    |> Tag.name
    |> parseSepochSemver
    |> ValueSome
    with e -> ValueNone

[<AutoOpen>]
module internal Helpers =
    let private getCommitsAfter repo o =
        Repository.commits repo
        |> CommitLog.Query.until o
    let private getCommitsBetween repo o1 o2 =
        Repository.commits repo
        |> CommitLog.Query.between o1 o2
    let private getCommitsTo repo o =
        Repository.commits repo
        |> CommitLog.Query.since o
    let getCommitsAfterSepoch repo (sepoch: SepochSemver) =
        sepoch.ToString() |> getCommitsAfter repo
    let getCommitsBetweenSepochs repo (sepoch1: SepochSemver) (sepoch2: SepochSemver) =
        (sepoch1.ToString(), sepoch2.ToString()) ||> getCommitsBetween repo
    let getCommitsToSepoch repo (sepoch: SepochSemver) =
        sepoch.ToString() |> getCommitsTo repo
    let getCommitsAfterTag repo (tag: LibGit2Sharp.Tag) =
        tag |> getCommitsAfter repo
    let getCommitsBetweenTags repo (tag1: LibGit2Sharp.Tag) (tag2: LibGit2Sharp.Tag) =
        (tag1,tag2) ||> getCommitsBetween repo
    let getCommitsToTag repo (tag: LibGit2Sharp.Tag) =
        tag |> getCommitsTo repo

let isFirstCommitForAuthor (authorSet: HashSet<string>) (commit: LibGit2Sharp.Commit) =
    commit
    |> Commit.author
    |> Signature.name
    |> authorSet.Add
    |> not

let getFirstCommitsForAuthor (authorSet: HashSet<string>) (commits: LibGit2Sharp.ICommitLog) =
    commits
    |> Seq.filter (isFirstCommitForAuthor authorSet)
        
module Diff =
    let inline private diffCompareWithPathOptions (item: 'T) paths options = Repository.diff >> _.Compare<TreeChanges>(item, DiffTargets.Index, paths, options)
    let inline private diffCompareWithPaths (item: 'T) paths = Repository.diff >> _.Compare<TreeChanges>(item, DiffTargets.Index, paths)
    let inline private diffCompare (item: 'T)= Repository.diff >> _.Compare<TreeChanges>(item, DiffTargets.Index)
    let commit repo commit =
        let tree = commit |> Commit.tree
        repo |> diffCompare tree
    let tree repo tree = repo |> diffCompare tree
    let commitPaths repo paths commit =
        let tree = commit |> Commit.tree
        repo |> diffCompareWithPaths tree paths
    let commitPathsWith repo paths (handler: string -> unit) commit =
        let tree = commit |> Commit.tree
        repo
        |> diffCompareWithPathOptions
            tree
            paths
            (ExplicitPathsOptions(OnUnmatchedPath = handler))
    let treePaths repo paths tree =
        repo |> diffCompareWithPaths tree paths
    let treePathsWith repo paths handler tree =
        repo |> diffCompareWithPathOptions tree paths (ExplicitPathsOptions(OnUnmatchedPath = handler))