module Partas.Tools.GitNet.GitCollection

open System
open System.Collections.Frozen
open System.Collections.Generic
open LibGit2Sharp.FSharp
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.RepoCracker
open Partas.Tools.GitNet.Types
type Repository = LibGit2Sharp.Repository
type Commit = LibGit2Sharp.Commit
type Tag = LibGit2Sharp.Tag


[<Struct>]
type CommitSha = CommitSha of string
[<Struct>]
type TagSha = TagSha of string
[<Struct>]
type Scope = Scope of string

type CommitSha with
    member inline this.Value = let (CommitSha value) = this in value
type TagSha with
    member inline this.Value = let (TagSha value) = this in value
type Scope with
    member inline this.Value = let (Scope value) = this in value

module Commit =
    let sha = Commit.sha >> CommitSha
module GitNetTag =
    let getScope = GitNetTag.getScope >> ValueOption.map Scope
    module Git =
        let sha = GitNetTag.Git.sha >> TagSha

[<AutoOpen>]
module Helpers =
    let inline toFrozenSet<'a>: 'a seq -> 'a FrozenSet = _.ToFrozenSet()
    let inline toFrozenDictionary<'Key,'Value>: KeyValuePair<'Key, 'Value> seq -> FrozenDictionary<'Key,'Value> =
        _.ToFrozenDictionary()
    let inline findIndexes (item1: 'Item) (item2: 'Item) (collection: 'Item array) =
        let mutable idx1 = ValueNone
        let mutable idx2 = ValueNone
        let mutable idx = 0
        let length = collection.Length
        while idx < length && (idx1.IsNone || idx2.IsNone) do
            let current = &collection[idx]
            if current = item1 then
                idx1 <- ValueSome idx
            if current = item2 then
                idx2 <- ValueSome idx
            idx <- idx + 1
        idx1,idx2
    module Unsafe =
        let inline findIndexes item1 item2 collection =
            let item1,item2 = findIndexes item1 item2 collection
            item1.Value,item2.Value
    open System.Linq
    let pathTraversal (path: string) =
        path
        |> Seq.mapi (fun i -> function
            | '/' | '\\' when i > 0 -> ValueSome <| i - 1
            | _ -> ValueNone)
        |> Seq.filter _.IsSome
        |> Seq.map (ValueOption.get >> fun idx -> path[0..idx])
        |> _.Append(path)
        
type OrderedCollection<'Key, 'Value> = {
    OrderedKeys: 'Key[]
    KeyDictionary: FrozenDictionary<'Key, 'Value>
}

type OrderedCollection<'Key, 'Value> with
    member this.Get key = this.KeyDictionary[key]

module OrderedCollection =
    let inline private createImpl<'Source, 'Key, 'Item, 'Comparable when 'Comparable : comparison>
        ([<InlineIfLambda>] sortMethod: ('Source -> 'Comparable) -> 'Source seq -> 'Source seq)
        ([<InlineIfLambda>] keyFunc: 'Source -> 'Key)
        ([<InlineIfLambda>] itemFunc: 'Source -> 'Item)
        ([<InlineIfLambda>] sortBy: 'Source -> 'Comparable)
        (collection: 'Source seq) : OrderedCollection<'Key, 'Item> = {
        OrderedKeys =
            collection
            |> sortMethod sortBy
            |> Seq.map keyFunc
            |> Seq.toArray
        KeyDictionary =
            collection
            |> Seq.map (
                (fun item -> keyFunc item, itemFunc item)
                >> KeyValuePair
                )
            |> _.ToFrozenDictionary()
    }
    let create<'Source, 'Key, 'Item, 'Comparable when 'Comparable : comparison> keyFunc itemFunc sortBy collection = createImpl<'Source, 'Key, 'Item, 'Comparable> Seq.sortBy keyFunc itemFunc sortBy collection
    let createDesc<'Source, 'Key, 'Item, 'Comparable when 'Comparable : comparison> keyFunc itemFunc sortBy collection = createImpl<'Source, 'Key, 'Item, 'Comparable> Seq.sortByDescending keyFunc itemFunc sortBy collection
    let inline keyPairs<'Key, 'Value>: OrderedCollection<'Key,'Value> -> ('Key * 'Key) array = _.OrderedKeys >> Array.pairwise
    let inline get key (col: OrderedCollection<_, _>) = col.Get(key)
type TagCollection = OrderedCollection<TagSha, GitNetTag>
type CommitCollection = OrderedCollection<CommitSha, Commit>
module TagCollection =
    type GetCommitsResponse = {
        UnreleasedCommits: CommitSha FrozenSet
        TagCommits: FrozenDictionary<TagSha, CommitSha FrozenSet>
    }
    let getCommits repo (tagCollection: TagCollection) =
        let tagPairs = OrderedCollection.keyPairs tagCollection
        let (>->) (appl1,appl2) func = (func appl1, func appl2)
        {
            UnreleasedCommits =
                tagPairs
                |> Array.last |> snd
                |> tagCollection.Get
                |> GitNetTag.Git.tag
                |> getCommitsToTag repo
                |> Seq.map Commit.sha
                |> toFrozenSet
            TagCommits = 
                tagPairs
                |> Array.map Choice1Of2
                |> Array.insertAt
                       tagPairs.Length
                       (tagPairs |> Array.head |> fst |> Choice2Of2)
                |> Array.map(function
                    | Choice1Of2(oldTag,newTag) ->
                        let key = newTag
                        let value =
                            (oldTag, newTag)
                            >-> (tagCollection.Get >> GitNetTag.Git.tag)
                            ||> getCommitsBetweenTags repo
                            |> Seq.map Commit.sha
                            |> toFrozenSet
                        key,value
                    | Choice2Of2 firstTag ->
                        let key = firstTag
                        let value =
                            tagCollection.Get key
                            |> GitNetTag.Git.tag
                            |> getCommitsAfterTag repo
                            |> Seq.map Commit.sha
                            |> toFrozenSet
                        key,value
                    >> KeyValuePair
                        )
                |> toFrozenDictionary
        }
    let load repo =
        let sortedTags =
            Repository.tags repo
            |> Seq.sortBy (
                Tag.target
                >> GitObject.tryPeel<LibGit2Sharp.Commit>
                >> ValueOption.map (Commit.committer >> Signature.date)
                >> ValueOption.defaultValue DateTimeOffset.MaxValue
                )
            |> Seq.map GitNetTag.fromTag
            |> Seq.toArray
        {
            OrderedKeys = Array.map GitNetTag.Git.sha sortedTags
            KeyDictionary = sortedTags.ToFrozenDictionary(GitNetTag.Git.sha, id)
        }
module CommitCollection =
    let load repo =
        OrderedCollection.create
            Commit.sha
            id
            (Commit.committer >> Signature.date)
            (Repository.commits repo)
type CommitTreeChangeCollection = FrozenDictionary<CommitSha, FrozenSet<string>>
module CommitTreeChangeCollection =
    let load (repo: Repository) (commits: CommitCollection) : CommitTreeChangeCollection =
        let keyPairs = OrderedCollection.keyPairs commits
        keyPairs
        |> Seq.map Choice1Of2
        // latest commit
        |> Seq.insertAt 0 (keyPairs |> Seq.head |> fst |> Choice2Of2)
        |> Seq.map(function
            | Choice2Of2 sha ->
                // latest commit
                let key = sha
                let value = sha |> commits.Get |> Diff.commit repo
                key,value
            | Choice1Of2 (oldSha,newSha) ->
                let oldCommitTree =
                    commits.Get oldSha |> Commit.tree
                let newCommitTree =
                    commits.Get newSha |> Commit.tree
                let key = newSha
                let value = repo.Diff.Compare<TreeChanges>(oldCommitTree, newCommitTree)
                key,value
            >> fun (sha,changes) ->
                let key = sha
                let value =
                    seq { changes.Added; changes.Copied; changes.Deleted; changes.Modified }
                    |> Seq.collect (Seq.collect (TreeEntryChanges.path >> pathTraversal))
                    |> toFrozenSet
                key,value
            >> KeyValuePair.Create
                )
        |> toFrozenDictionary
type CommitScopeCollection = FrozenDictionary<CommitSha, Scope FrozenSet>
module CommitScopeCollection =
    let load (commitCollection: CommitTreeChangeCollection) (scopes: FrozenDictionary<Scope, string>) =
        let pathsDict = scopes.ToFrozenDictionary(_.Value,_.Key)
        let paths = scopes.Values
        commitCollection.ToFrozenDictionary(
            _.Key,
            (fun (keyValuePair: KeyValuePair<CommitSha,FrozenSet<string>>) -> keyValuePair.Value)
            >> fun treeChanges ->
                paths
                |> Seq.choose (fun path ->
                    if treeChanges.Contains path then
                        Some pathsDict[path]
                    else None
                    )
                |> toFrozenSet
            )
type TagCommitCollection =
    {
        TagCollection: TagCollection
        CommitCollection: CommitCollection
        TreeChangeCollection: FrozenDictionary<CommitSha, FrozenSet<string>>
        UntaggedCommits: FrozenSet<CommitSha>
        TagCommits: FrozenDictionary<TagSha, FrozenSet<CommitSha>>
        Scopes: FrozenDictionary<Scope, string>
        CommitScopes: FrozenDictionary<CommitSha, Scope FrozenSet>
    }

module TagCommitCollection =
    type ScopePathDictionary = FrozenDictionary<Scope, string>
    let load config =
        let repo = Repository.load config.RepositoryPath
        let crackedRepos = crackRepo config
        let scopes: ScopePathDictionary =
            crackedRepos
            |> ValueOption.map (
                _.Projects
                >> Seq.choose (fun proj ->
                    match proj.Scope with
                    | ValueSome scope ->
                        Some <| KeyValuePair(Scope scope, proj.ProjectDirectory)
                    | _ -> None)
                >> toFrozenDictionary
                )
            |> ValueOption.defaultValue ([] |> toFrozenDictionary)
        
        let tagCollection = TagCollection.load repo
        let commitCollection = CommitCollection.load repo
        let treeChangeCollection = CommitTreeChangeCollection.load repo commitCollection
        let commitScopes = CommitScopeCollection.load treeChangeCollection scopes
        let getCommitsResponse = tagCollection |> TagCollection.getCommits repo
        {
            TagCollection = tagCollection
            CommitCollection = commitCollection
            UntaggedCommits = getCommitsResponse.UnreleasedCommits
            TagCommits = getCommitsResponse.TagCommits
            TreeChangeCollection = treeChangeCollection
            Scopes = scopes
            CommitScopes = commitScopes
        }
    let inline private sliceTagsIndexImpl lowerIdx higherIdx collection=
        let inline tagCommitLookup sha = _.TagCommits[sha]
        let shas =
            collection.TagCollection.OrderedKeys[lowerIdx + 1..higherIdx].AsSpan()
        let shaCollection = HashSet(20)
        for sha in shas do
            shaCollection.UnionWith(tagCommitLookup sha collection)
        shaCollection
    let getCommitsBetween tag1 tag2 collection =
        let idx1,idx2 = Unsafe.findIndexes tag1 tag2 collection.TagCollection.OrderedKeys
        sliceTagsIndexImpl idx1 idx2 collection
        |> toFrozenSet
    let getUnreleasedCommits latestTag collection =
        let idx1 =
            collection.TagCollection.OrderedKeys
            |> Array.findIndex ((=) latestTag)
        let idx2 =
            collection.TagCollection.OrderedKeys.Length
            |> (+) -1
        let result = sliceTagsIndexImpl idx1 idx2 collection
        result.UnionWith(collection.UntaggedCommits)
        result
        |> toFrozenSet
    
    let collectScopes repo collection =
        let (|Scoped|) =
            collection.TagCollection.KeyDictionary.GetValueRefOrNullRef
            >> GitNetTag.getScope
        seq {
            for scope in collection.Scopes |> Seq.map _.Key do
                scope,
                collection.TagCollection.OrderedKeys
                |> Array.filter (function
                    | tagSha when collection.TagCollection.KeyDictionary.GetValueRefOrNullRef tagSha |> _.IsSemVerTag ->
                        true
                    | Scoped tagScope when tagScope |> ValueOption.contains scope ->
                        true
                    | _ -> false
                    )
                |> Array.pairwise
                |> fun pairs ->
                    pairs
                    |> Array.map Choice1Of3
                    |> Array.insertAt pairs.Length (
                        pairs |> Array.last |> snd
                        |> Choice2Of3
                        )
                    |> Array.insertAt 0 (
                        pairs |> Array.head |> fst
                        |> Choice3Of3
                        )
                |> Array.map (function
                    | Choice1Of3 tags ->
                        snd tags |> ValueSome, getCommitsBetween (fst tags) (snd tags) collection
                    | Choice2Of3 tag ->
                        ValueNone,
                        getUnreleasedCommits tag collection
                    | Choice3Of3 tag ->
                        tag |> ValueSome, getCommitsBetween tag tag collection
                    >> fun (tag,commits) ->
                        tag |> ValueOption.map (fun tag -> collection.TagCollection.KeyDictionary[tag]),
                        commits
                        |> Seq.filter (fun commit ->
                            try
                            collection.CommitScopes[commit].Contains scope
                            with :? KeyNotFoundException ->
                                Repository.lookup commit.Value repo
                                |> ValueOption.bind GitObject.tryPeel<Commit>
                                |> ValueOption.map (
                                    Diff.commitPaths repo [collection.Scopes[scope]]
                                    >> fun changes ->
                                        seq {
                                            changes.Added
                                            changes.Modified
                                            changes.Copied
                                            changes.Deleted
                                            changes.Renamed
                                        }
                                        |> Seq.collect(Seq.collect (TreeEntryChanges.path >> pathTraversal))
                                    )
                                |> ValueOption.exists(Seq.contains collection.Scopes[scope])
                            )
                        |> Seq.toArray
                    )
        }
    
