module Partas.Tools.GitNet.GitCollector

open System
open System.Collections.Frozen
open System.Collections.Generic
open System.Linq
open LibGit2Sharp
open GitTraversal
open Partas.Tools.GitNet
open Types
open RepoCracker
open LibGit2Sharp.FSharp
open FSharp.Control

(* COLLECTION

*)

let private defaultAutoScope =
    #if DEBUG
    AutoScopeType.Transform(fun projName ->
        projName.Split('.') |> Array.last)
    #else
    AutoScopeType.NoScoping
    #endif
let internal isAutoScope = function
    | { ProjectConfig = { AutoScope = autoScopeValue } } ->
        autoScopeValue.IsTransform
let private loadRepoFromConfig = function
    { GitNetConfig.RepositoryPath = repoPath } -> Repository.load repoPath

/// <summary>
/// Retrieves the possible fragments of the passed path as it traverses to the root.
/// </summary>
let private pathTraversal (path: string) =
    path
    |> Seq.mapi (fun i -> function
        | '/' | '\\' when i > 0 -> ValueSome <| i - 1
        | _ -> ValueNone)
    |> Seq.filter _.IsSome
    |> Seq.map (ValueOption.get >> fun idx -> path[0..idx])
    |> _.Append(path)

/// <summary>
/// Tags and Commits are 'cached' by their Sha in a <c>FrozenDictionary</c>, and
/// associated with an ordered array of the Shas.
/// </summary>
type FrozenOrderCollection<'T> = {
    ShaDictionary: FrozenDictionary<string, 'T>
    OrderedShaArray: string array
}
/// <summary>
/// Commits are 'cached' by their Sha in a <c>FrozenDictionary</c>, and
/// associated with an ordered array of the Shas.<br/><br/>The ordered array
/// is ordered by the <c>Committer</c> date.
/// </summary>
/// <remarks>
/// It is possible for traversal of commits to yield Shas which are not cached.
/// This is most likely due to the cached collection obfuscating parent commits.
/// <br/><br/>
/// This is relevant when sorting the commits or retrieving the cached commit data.
/// If we fail to find the commit, then we search the repository using a lookup,
/// and peel the <c>GitObject</c> to a <c>Commit</c>.<br/><br/>
/// It would be exceptional for an error to occur in this process, as there is no
/// logical reason for us to fail to find a <c>Commit</c>, implying the Sha we are
/// searching with somehow materialized out of thin air. We throw an exception in this
/// circumstance, and in this circumstance only.
/// </remarks>
type CommitCollection = FrozenOrderCollection<Commit>
/// <summary>
/// Tags are 'cached' by their Sha in a <c>FrozenDictionary</c>, and
/// associated with an ordered array of the Shas.<br/><br/>The ordered array
/// is sorted by the <c>Committer</c> date of the underlying commit for the tag.
/// </summary>
type TagCollection = FrozenOrderCollection<GitNetTag>
type CommitTreeChangeCollection = FrozenDictionary<string, FrozenSet<string>>
type ScopedCommitCollection = FrozenDictionary<string, FrozenSet<string>>
type TagCommitCollection = FrozenDictionary<string voption, string array>
type ScopedTagCommitCollection =
    FrozenDictionary<
        string voption, FrozenDictionary<string voption, FrozenSet<string>>
    >

module private CommitCollection =
    let init repo: CommitCollection  =
        let sortedCommits =
            Repository.commits repo
            |> Seq.sortByDescending (Commit.committer >> Signature.date)
            |> Seq.toArray
        {
            ShaDictionary = sortedCommits.ToFrozenDictionary(Commit.sha, id)
            OrderedShaArray = Array.map Commit.sha sortedCommits
        }
module private TagCollection =
    let init repo : TagCollection =
        let sortedTags =
            Repository.tags repo
            |> Seq.sortBy (
                Tag.target
                >> GitObject.peel<LibGit2Sharp.Commit>
                >> ValueOption.map (Commit.committer >> Signature.date)
                >> ValueOption.defaultValue DateTimeOffset.MaxValue
                )
            |> Seq.map GitNetTag.fromTag
            |> Seq.toArray
        {
            ShaDictionary = sortedTags.ToFrozenDictionary(GitNetTag.Git.sha, id)
            OrderedShaArray = Array.map GitNetTag.Git.sha sortedTags
        }

[<Struct>]
type private CommitPosition =
    | Latest of string
    | Pair of string * string
/// Computes the paths that were modified in each commit, and then collects them
/// into a dictionary with the commit Sha being the key.
let private computeCommitDiffPaths repo (commits: CommitCollection): CommitTreeChangeCollection =
    let commitPairs = commits.OrderedShaArray |> Array.pairwise
    seq {
        yield commitPairs |> Array.head |> fst |> Latest
        yield! commitPairs |> Array.map Pair
    }
    |> Seq.map (
        // The latest commit is compared to the current working tree
        function
        | Latest sha ->
            sha,
            sha
            |> commits.ShaDictionary.GetValueRefOrNullRef
            |> Diff.commit repo
        // Otherwise we compare sequential commit trees
        | Pair (newSha,oldSha) ->
            let oldCommitTree =
                commits.ShaDictionary.GetValueRefOrNullRef oldSha |> Commit.tree
            let newCommitTree =
                commits.ShaDictionary.GetValueRefOrNullRef newSha |> Commit.tree
            newSha,
            repo.Diff.Compare<TreeChanges>(oldCommitTree, newCommitTree)
        // We then extract the tree changes for the new commits, and compute
        // the affected paths into a frozen set
        >> fun (sha,changes) ->
            sha,
            seq { changes.Added; changes.Copied; changes.Deleted; changes.Modified }
            |> Seq.collect (
                Seq.collect (TreeEntryChanges.path >> pathTraversal)
                )
            |> _.ToFrozenSet()
        // We then pair that with the sha of the new commit as the key
        >> KeyValuePair.Create    
            )
    |> _.ToFrozenDictionary()
let private computeScopedCommits config (computedDiffPaths: CommitTreeChangeCollection): ScopedCommitCollection =
    crackRepo config
    |> ValueOption.get |> _.Projects
    |> Seq.choose (function
        | { Scope = ValueSome scope } as project ->
            (scope, [| for groups in computedDiffPaths do
                           if groups.Value.TryGetValue project.ProjectDirectory |> fst
                           then groups.Key |].ToFrozenSet())
            |> KeyValuePair
            |> Some
            // TODO - if multiple projects share the same scope, then we need to collect
            //        their scoped commits
        | _ -> None
        )
    |> _.ToFrozenDictionary()

[<Struct>]
type private TagPairType =
    | Last of string
    | First of string
    | Pair of string * string
// Collects an array of commit shas for each sequential tag in the repo.
// The commits since the latest tag are paired against ValueNone and reflect
// the UNRELEASED commits.
let private collectCommitsForTags repo (tagCollection: TagCollection): TagCommitCollection =
    let tagPairs = tagCollection.OrderedShaArray |> Array.pairwise
    seq {
        tagPairs |> Array.last |> snd |> Last // Last tag; AKA UNRELEASED
        yield! tagPairs |> Array.map Pair
        tagPairs |> Array.head |> fst |> First // First tag
    }
    |> Seq.map(function
        | Last lastTag ->
            let key = ValueNone
            let value =
                tagCollection.ShaDictionary[lastTag]
                |> GitNetTag.Git.tag
                |> getCommitsToTag repo
                |> Seq.map Commit.sha
                |> Seq.toArray
            key,value
        | Pair(oldTag, newTag) ->
            let getTag tagKey =
                tagCollection.ShaDictionary[tagKey]
                |> GitNetTag.Git.tag
            let key = ValueSome newTag
            let value = 
                (getTag oldTag, getTag newTag)
                ||> getCommitsBetweenTags repo
                |> Seq.map Commit.sha
                |> Seq.toArray
            key,value
        | First firstTag ->
            let key = ValueSome firstTag
            let value =
                tagCollection.ShaDictionary[firstTag]
                |> GitNetTag.Git.tag
                |> getCommitsAfterTag repo
                |> Seq.map Commit.sha
                |> Seq.toArray
            key,value
        >> KeyValuePair
        )
    |> _.ToFrozenDictionary()

let private collectScopedCommitsForTags (commitsForTags: TagCommitCollection) (scopedCommits: ScopedCommitCollection): ScopedTagCommitCollection =
    commitsForTags
    |> Seq.map (fun tagCommits ->
        let key = tagCommits.Key
        let unscopedCommits = HashSet()
        scopedCommits |> Seq.choose (fun scopedCommit ->
            let commitsForTag = tagCommits.Value
            let commitsForScope = scopedCommit.Value
            let intersection =
                commitsForScope.Intersect commitsForTag
                |> _.ToFrozenSet()
            unscopedCommits.SymmetricExceptWith
                (commitsForTag.Except commitsForScope)
            if intersection.Count() > 0 then
                KeyValuePair(ValueSome scopedCommit.Key, intersection)
                |> Some
            else None
            )
        |> fun scopedCommitCollection ->
            if Seq.length scopedCommitCollection = 0 then
                unscopedCommits.UnionWith tagCommits.Value
            scopedCommitCollection
        |> _.Append(KeyValuePair(ValueNone, unscopedCommits.ToFrozenSet()))
        |> _.ToFrozenDictionary()
        |> fun scopedCommitCollection ->
            KeyValuePair(key, scopedCommitCollection)
        )
    |> _.ToFrozenDictionary()

type ComputedScopeCommits = {
    Commits: Commit[]
    Scope: string voption
}
type ComputedTagScopedCommits = {
    Tag: GitNetTag voption
    ScopedCommits: ComputedScopeCommits list
}
type GitNetCollections = {
    CommitCollection: CommitCollection
    TagCollection: TagCollection
    CommitDiffs: CommitTreeChangeCollection
    ScopedCommits: ScopedCommitCollection
    TagCommits: TagCommitCollection
    ScopedTagCommits: ScopedTagCommitCollection
    Collection: ComputedTagScopedCommits array
}

let computeGitNetCollections config =
    let repo = Repository.load config.RepositoryPath
    let commitCollection = CommitCollection.init repo
    let tagCollection = TagCollection.init repo
    let commitDiffs = computeCommitDiffPaths repo commitCollection
    let scopedCommits = computeScopedCommits config commitDiffs
    let tagCommits = collectCommitsForTags repo tagCollection
    let scopedTagCommits = collectScopedCommitsForTags tagCommits scopedCommits
    let finalCollection =
        scopedTagCommits.OrderBy(fun keyVal ->
            if keyVal.Key.IsNone then 0
            else
            tagCollection.OrderedShaArray
            |> Array.findIndex((=) keyVal.Key.Value)
            |> (+) 1)
        |> Seq.map(
            fun tagScopeCommits ->
                tagScopeCommits.Value
                |> Seq.choose (
                    fun scopedCommits ->
                        if
                            tagScopeCommits.Key.IsSome
                            && tagCollection.ShaDictionary.Item(tagScopeCommits.Key.Value)
                               |> GitNetTag.getScope
                               |> ValueOption.isSome
                            // If this is a scoped tag, then we will skip
                            // any scopes that are not equal to the tags scope.
                            && tagCollection.ShaDictionary.Item(tagScopeCommits.Key.Value)
                               |> GitNetTag.getScope
                               |> (<>) scopedCommits.Key
                        then None
                        else
                        let scopeCommits = scopedCommits.Value
                        let orderedCommits =
                            scopeCommits.OrderBy(fun sha ->
                                commitCollection.OrderedShaArray
                                |> Array.tryFindIndex((=) sha)
                            )
                            |> Seq.map (fun sha ->
                                let isSuccessful,value =
                                    commitCollection.ShaDictionary.TryGetValue(sha)
                                if not isSuccessful then
                                    repo |> Repository.lookup sha
                                    |> ValueOption.map GitObject.peel<Commit>
                                    |> ValueOption.flatten
                                    |> ValueOption.defaultWith(fun () -> failwith "A commit has materialised out of nowhere")
                                else value
                                )
                            |> Seq.toArray
                        { Scope = scopedCommits.Key
                          Commits = orderedCommits }
                        |> Some
                    )
                |> fun computedScopeCommits ->
                    let tag =
                        tagScopeCommits.Key
                        |> ValueOption.map (fun tag -> tagCollection.ShaDictionary.Item(tag))
                    { Tag = tag
                      ScopedCommits = computedScopeCommits |> Seq.toList }
            )
    { CommitCollection = commitCollection
      TagCollection = tagCollection
      CommitDiffs = commitDiffs
      ScopedCommits = scopedCommits
      TagCommits = tagCommits
      ScopedTagCommits = scopedTagCommits
      Collection = finalCollection |> Seq.toArray }
