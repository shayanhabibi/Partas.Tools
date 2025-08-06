module Partas.Tools.GitNet.Runner

open System
open System.Collections.Frozen
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Linq
open LibGit2Sharp
open Partas.Tools.SepochSemver
open GitTraversal
open Partas.Tools.ConventionalCommits
open Partas.Tools.GitNet
open Types
open RepoCracker
open Fake.Core
open LibGit2Sharp.FSharp
open FSharp.Control

/// Initialise pre running gitnet.
type GitNetRuntime(?config: GitNetConfig) =
    let config = defaultArg config {
        GitNetConfig.init with
            GitNetConfig
                .ProjectConfig.AutoScope = AutoScopeType.Transform(fun projName ->
                projName.Split('.') |> Array.last
                )
    }
    let repo = Repository.load config.RepositoryPath
    let commits,commitShas =
        let commits = repo.Commits |> Seq.toArray
        let commitShas =
            commits
            |> Array.sortByDescending(_.Committer.When)
            |> Array.map _.Sha
        commits.ToFrozenDictionary(_.Sha, id),commitShas
    let diff = repo.Diff
    let tags, tagShas =
        let tags = repo.Tags |> Seq.toArray
        let tagShas =
            tags
            |> Array.sortBy (
                Tag.target
                >> GitObject.peel<Commit>
                >> ValueOption.map _.Committer.When
                >> ValueOption.defaultValue DateTimeOffset.MaxValue
                )
            |> Array.map _.Target.Sha
        tags.ToFrozenDictionary(_.Target.Sha, id), tagShas
    let commitPairs =
        commitShas
        |> Array.pairwise
    let tagPairs =
        tagShas |> Array.pairwise
    let commitDiffs =
        let makeKeyValue (newSha: string) (changes: TreeChanges) =
            KeyValuePair(
                newSha,
                seq {
                     let makePaths: TreeEntryChanges seq -> string seq =
                         Seq.map ( fun treeChange ->
                             let path = treeChange.Path
                                 
                             seq {
                                 let idxs = path |> Seq.mapi(fun i c ->
                                     if c = '/' && i > 0 then ValueSome (i - 1) else ValueNone
                                     )
                                 for i in idxs do
                                     if i.IsValueSome then
                                        path[0..i.Value]
                                 path
                             }
                             )
                         >> Seq.collect id
                     yield! makePaths changes.Added
                     yield! makePaths changes.Copied
                     yield! makePaths changes.Deleted
                     yield! makePaths changes.Modified
                }
                |> _.ToFrozenSet()
            )
        [|
            yield
                commitPairs |> Array.head
                |> fst
                |> fun newSha ->
                    newSha
                    |> commits.GetValueRefOrNullRef
                    |> Diff.commit repo
                    |> makeKeyValue newSha
            for newSha,oldSha in commitPairs do
                let newCommitTree = commits.GetValueRefOrNullRef newSha |> _.Tree
                let oldCommitTree = commits.GetValueRefOrNullRef oldSha |> _.Tree
                let changes = diff.Compare<TreeChanges>(oldCommitTree, newCommitTree)
                yield makeKeyValue newSha changes
        |].ToFrozenDictionary()
    let scopedCommits =
        crackRepo config
        |> ValueOption.get
        |> _.Projects
        |> Seq.filter (_.Scope.IsSome)
        |> Seq.map (fun project ->
            KeyValuePair(
                project.Scope.Value,
                [|
                    for groups in commitDiffs do
                        if groups.Value.TryGetValue project.ProjectDirectory |> fst
                        then groups.Key
                |].ToFrozenSet()
                )
            )
    let tagCommits =
        // let unreleasedCommits =
        let firstTag =
            tagPairs[0]
            |> fst
            |> fun key ->
                ValueSome key,
                tags[key]
                |> getCommitsAfterTag repo
        let unreleasedCommits =
            tagPairs
            |> Array.last
            |> snd
            |> fun key ->
                ValueNone,
                tags[key]
                |> getCommitsToTag repo
        let makeKeyValue (key: string voption) (commits: ICommitLog)=
            KeyValuePair(key, commits |> Seq.map _.Sha |> Seq.toArray)
        seq {
            yield unreleasedCommits ||> makeKeyValue
            yield!
                tagPairs
                |> Array.map (
                    fun (oldTag, newTag) ->
                        (ValueSome newTag,
                         (tags[oldTag], tags[newTag])
                         ||> getCommitsBetweenTags repo)
                        ||> makeKeyValue
                    )
            yield firstTag ||> makeKeyValue
        }
        |> _.ToFrozenDictionary()
    let scopedTagCommits =
        taskSeq {
            for tag in tagCommits do
                let scopedCommits =
                    seq {
                    let unscopedCommits = HashSet()
                    for scope in scopedCommits do
                        let tagCommits = tag.Value
                        let scopeCommits = scope.Value
                        let intersection =
                            scopeCommits.Intersect tagCommits
                            |> _.ToFrozenSet()
                        if intersection.Count() > 0 then
                            KeyValuePair(ValueSome scope.Key, intersection)
                        unscopedCommits.SymmetricExceptWith <| tagCommits.Except scopeCommits
                    KeyValuePair(
                        ValueNone,
                        unscopedCommits.ToFrozenSet()
                        )
                    }
                    |> _.ToFrozenDictionary()
                        
                KeyValuePair(tag.Key, scopedCommits)
        }
        |> TaskSeq.toArrayAsync
        |> Task.map _.ToFrozenDictionary()
    member this.GetEnumerator(): IEnumerator<_> =
            scopedTagCommits.Wait()
            let scopedTagCommits = scopedTagCommits.Result
            seq {
                let orderedScopeTagCommits =
                    scopedTagCommits.OrderBy(fun keyVal ->
                        if keyVal.Key.IsNone then 0
                        else
                        tagShas
                        |> Array.findIndex((=) keyVal.Key.Value)
                        |> (+) 1
                    )
                for tagScopeCommits in orderedScopeTagCommits do
                    let scopedCommitsDict = tagScopeCommits.Value
                    [
                        for scopedCommits in scopedCommitsDict do
                            let scopeCommits = scopedCommits.Value
                            let orderedCommits =
                                scopeCommits.OrderBy(fun sha ->
                                    commitShas
                                    |> Array.findIndex((=) sha)
                                    )
                            {| Scope = scopedCommits.Key
                               Commits = orderedCommits
                                         |> Seq.map (fun sha -> commits.Item(sha))
                                         |> Seq.toArray |}
                    ]
                    |> fun scopedCommits ->
                        {| Tag = tagScopeCommits.Key
                                 |> ValueOption.map(fun tag -> tags.Item(tag))
                           ScopedCommits = scopedCommits |}
            }
            |> _.GetEnumerator()
    interface IEnumerable<
        {| Tag: Tag voption
           ScopedCommits: {| Scope: string voption
                             Commits: Commit array |} list |} > with
        member this.GetEnumerator(): IEnumerator<_> =
            this.GetEnumerator()
        member this.GetEnumerator(): Collections.IEnumerator =
            this.GetEnumerator()
            
    // do
    //     for commit in scopedCommits do
    //         commit.Key |> printfn "%A"
    //         commit.Value.Items
    //         |> Seq.map (fun k -> commits[k].MessageShort)
    //         |> printfn "%A"
    //     scopedTagCommits.Wait()
    //     let scopedTagCommits = scopedTagCommits.Result
    //     for t in tagShas do
    //         for c in scopedTagCommits[ValueSome t] do
    //             (c.Key, c.Value.Items)
    //             |> printfn "%A"
    //
    //
    
