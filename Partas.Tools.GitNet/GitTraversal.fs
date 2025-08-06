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
let getTagCollection repo =
    query {
        for _tag in Repository.tags repo do
        let _maybeSepoch = tryParseSepochSemverFromTag _tag
        where (_maybeSepoch.IsValueSome)
        let sepoch = _maybeSepoch.Value
        sortBy sepoch
        groupValBy (_tag,sepoch) sepoch.Sepoch.GetScope
    }
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

let getCommitsFromTag config repo tag =
    let projects = crackRepo config
    let commits = getCommitsAfterTag repo tag
    match projects with
    | ValueNone -> query {
            for commit in commits do
            groupValBy (tag, commit) ValueNone
        }
    | ValueSome({ Projects = projects }) ->
        query {
            for project in projects do
            for commit in commits do
            where (
                repo.Diff.Compare<TreeChanges>(commit.Tree, DiffTargets.Index, [ project.ProjectDirectory ])
                |> Seq.isEmpty |> not
            )
            groupValBy (tag, commit) project.Scope
        }

let getCommitCollections config repo =
    // Utilised for grouping unreleased commits
    let getCommitsForTags ((tag1: LibGit2Sharp.Tag,_: SepochSemver),(tag2: LibGit2Sharp.Tag,_: SepochSemver))  : Result<_,string> =
        (tag1, tag2)
        ||> getCommitsBetweenTags repo
        |> Ok
    // group into scope and tagpairs
    let tagPairs = query {
        let _tagPairs = query {
            for group in getTagCollection repo do
            groupValBy (group |> Seq.pairwise) group.Key
        }
        for _group in _tagPairs do
        for _pair in _group |> Seq.tryExactlyOne |> Option.defaultValue (seq []) do
        groupValBy _pair _group.Key
    }
    // collection of commits for tags with scopes
    let batch = query {
        for _group in tagPairs do
        for _pairwiseTags in _group do
        let result = getCommitsForTags _pairwiseTags
        where result.IsOk
        select (_group.Key, _pairwiseTags, result)
    }
    let head = Seq.tryExactlyOne <| query {
            for _group in tagPairs do
            for _pairwiseTags in _group do
            select (_group.Key, _pairwiseTags)
            take 1
        }

    let tail = Seq.tryExactlyOne <| query {
        for _group in tagPairs do
        for _pairwiseTags in _group |> Seq.rev do
        select (_group.Key, _pairwiseTags)
        take 1
    }
    match head,tail with
    | Some(scope,tagArray), Some(_,tagArray2) when tagArray <> tagArray2 ->
        let head,tail =
            tagArray |> fst,
            tagArray2 |> snd
        let first =
            scope,
            (head, head),
            head |> fst
            |> getCommitsAfterTag repo
            |> Ok
        let second =
            scope,
            (tail,tail),
            tail |> fst
            |> getCommitsToTag repo
            |> Ok
        Some first, Some second
    | Some(scope,tagArray), Some(_)
    | Some(scope,tagArray), _ ->
        let head =
            fst tagArray
        let first =
            scope,
            (head, head),
            head |> fst
            |> getCommitsAfterTag repo
            |> Ok
        Some first, None
    | None, Some(scope,tagArray) ->
        let tail =
            tagArray
            |> snd
        let second =
            scope,
            (tail,tail),
            tail |> fst
            |> getCommitsToTag repo
            |> Ok
        None, Some second
    | _ -> None, None
    |> fun (first, second) ->
        seq {
            if first.IsSome then
                first.Value
            yield! batch
            if second.IsSome then
                second.Value
        }

let collectCommitsFor
    (scope: string voption)
    ( (fromTag: LibGit2Sharp.Tag, fromSepoch: SepochSemver),
      (toTag: LibGit2Sharp.Tag, toSepoch: SepochSemver) )
    (commits: Result<LibGit2Sharp.ICommitLog,string>) =
    {| From = {| Tag = fromTag
                 Sepoch = fromSepoch |}
       To = {| Tag = toTag
               Sepoch = toSepoch |}
       Scope = scope
       Commits =
           commits
           |> Result.defaultValue Unchecked.defaultof<_> |}

let isFirstCommitForAuthor (authorSet: HashSet<string>) (commit: LibGit2Sharp.Commit) =
    commit
    |> Commit.author
    |> Signature.name
    |> authorSet.Add
    |> not

let getFirstCommitsForAuthor (authorSet: HashSet<string>) (commits: LibGit2Sharp.ICommitLog) =
    commits
    |> Seq.filter (isFirstCommitForAuthor authorSet)


module Grouping =
    open LibGit2Sharp
    open FsToolkit.ErrorHandling
    [<RequireQualifiedAccess>]
    type CommitTagGrouping =
        | Head of Tag
        | Tag of Tag * Tag
        | Tail of Tag
    let getTagCollection repo =
        let tags = repo |> Repository.tags
        voption {
            let! head =
                tags |> Seq.tryHead
            let! tail =
                tags |> Seq.tryLast
            return seq {
                CommitTagGrouping.Head head
                CommitTagGrouping.Tail tail
                yield! tags |> Seq.pairwise |> Seq.map CommitTagGrouping.Tag
            }
        }
    let getCommitsForTags config repo (tags: CommitTagGrouping seq) =
        let commits = repo |> Repository.commits
        ()
        
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
