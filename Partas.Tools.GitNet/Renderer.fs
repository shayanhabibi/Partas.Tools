module Partas.Tools.GitNet.Renderer

open System.Collections.Frozen
open System.Collections.Generic
open LibGit2Sharp.FSharp
open Partas.Tools.ConventionalCommits
open Partas.Tools.GitNet.GitCollection
open Partas.Tools.GitNet.GitCollection.TagCommitCollection
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.Types

(*
Two phase collection to primitive records for writing.

PreRenderPhase ->
    Assembling the TagCommitCollection into an array that groups
    scopes to tags, and tags to commits.
    The tags and commits have been resolved to their GitNet prefix types.
RenderPhase ->
    Uses the output from the PreRenderPhase to 'render' the collection into
    records of strings.
    Ideally, there should be no optional types, these should be changed to
    DUs. This way we are explicit with the writer, and will not have to pass
    anything except the config item to it.
    The writing phase can then process these string records into the final
    output.
*)

/// <summary>
/// Module containing the types and functions relating to the PreRenderPhase.
/// </summary>
/// <remarks>
/// <para>This phase reflects the manipulation and collection of the <c>TagCommitCollection</c>
/// into their resolved <c>GitNet</c> types, with scopes matched to their collection of tags, and the
/// tags being matched to their collection of commits for that scope.</para>
/// </remarks>
module private PreRenderPhase =
    /// <summary>
    /// Represents the special state of the first and last tag when resolving the
    /// commits between the tags, as the first tag has no precursor, and the last tag
    /// is used to determine the unreleased commits.
    /// </summary>
    type TagPairPosition =
        /// <summary>
        /// The first Tag of the collection.
        /// This should be compared to the first commit to collect the commits for
        /// the first tag.
        /// </summary>
        | FirstTag of TagSha
        /// <summary>
        /// A pair of tags in the collection that are used as the upper and lower bounds
        /// when collecting the commits for the upper boundary tag using Gits Diffing ops.
        /// </summary>
        | PairTag of TagSha * TagSha
        /// <summary>
        /// The last tag in the collection that is used to collect the unreleased commits
        /// since the aforementioned tag.
        /// </summary>
        | LastTag of TagSha
        member inline this.PermuteToFirstTag =
            match this with
            | PairTag(tag,_) -> FirstTag tag
            | _ -> invalidOp ""
        member inline this.PermuteToLastTag =
            match this with
            | PairTag(_, tag) -> LastTag tag
            | _ -> invalidOp ""
    /// <summary>
    /// Functions and helpers operating on the TagCommitCollection.
    /// </summary>
    module TagCommitCollection =
        /// <summary>
        /// Functions and helpers operating on the TagCommitCollection relating
        /// to the Scope
        /// </summary>
        module Scope =
            let keys = _.Scopes >> Seq.map _.Key
            let dir = _.Scopes.GetValueRefOrNullRef
        /// <summary>
        /// Functions and helpers operating on the TagCommitCollection relating
        /// to Tags
        /// </summary>
        module Tags =
            let orderedShas = _.TagCollection.OrderedKeys
            let get sha = _.TagCollection.KeyDictionary[sha]
            let tryScope collection= collection.TagCollection.KeyDictionary.GetValueRefOrNullRef >> GitNetTag.getScope
            let scope collection = tryScope collection >> _.Value
            let (|IsUnscoped|_|) collection =
                let isScoped tag = collection |> get tag |> _.IsSemVerTag |> not
                function
                | tag when isScoped tag -> ValueSome ()
                | _ -> ValueNone
            let (|IsScoped|_|) collection = function
                | tag ->
                    tag
                    |> tryScope collection
                    |> ValueOption.map Scope
            
        /// <summary>
        /// Functions and helpers operating on the TagCommitCollection relating
        /// to Commits
        /// </summary>
        module Commits =
            let scopes sha = _.CommitScopes[sha]
            let containsScopeOrLookup collection repo scope sha =
                try (scopes sha >> _.Contains(scope)) collection
                with :? KeyNotFoundException ->
                    repo
                    |> Repository.lookup sha.Value
                    |> ValueOption.bind GitObject.tryPeel<Commit>
                    |> ValueOption.map (
                        Diff.commitPaths repo [Scope.dir collection scope]
                        >> fun changes ->
                            seq {
                                changes.Added
                                changes.Modified
                                changes.Copied
                                changes.Deleted
                                changes.Renamed
                            }
                            |> Seq.collect (Seq.collect (TreeEntryChanges.path >> pathTraversal))
                        )
                    |> ValueOption.exists(Seq.contains (Scope.dir collection scope))
            let get collection sha =
                collection.CommitCollection.Get sha
    
    /// <summary>
    /// Performs the pre render collection on a <c>TagCommitCollection</c>.
    /// </summary>
    /// <param name="repo">The repository, used for lookups in exceptional circumstances
    /// where a commit is not registered in the collection due to it being a parent.</param>
    /// <param name="collection">The <c>TagCommitCollection</c></param>
    let fromTagCommitCollection repo (collection: TagCommitCollection) =
        let (|Scoped|) =
            collection.TagCollection.KeyDictionary.GetValueRefOrNullRef
            >> GitNetTag.getScope
        collection
        |> TagCommitCollection.Scope.keys
        |> Seq.map (fun scope ->
            scope,
            collection
            |> TagCommitCollection.Tags.orderedShas
            |> Array.filter (function
                | tagSha when collection.TagCollection.KeyDictionary.GetValueRefOrNullRef tagSha |> _.IsSemVerTag ->
                    true
                | Scoped tagScope when tagScope |> ValueOption.contains scope.Value ->
                    true
                | _ -> false
                )
            |> Array.pairwise
            |> Array.map TagPairPosition.PairTag
            |> fun pairs ->
                pairs
                |> Array.insertAt
                       pairs.Length
                       (pairs |> Array.last |> _.PermuteToLastTag)
                |> Array.insertAt
                       0
                       (pairs |> Array.head |> _.PermuteToFirstTag)
            |> Array.map (function
                | PairTag(fstTag,sndTag) ->
                    ValueSome sndTag,
                    getCommitsBetween fstTag sndTag collection
                | LastTag tag ->
                    ValueNone,
                    getUnreleasedCommits tag collection
                | FirstTag tag ->
                    ValueSome tag,
                    getCommitsBetween tag tag collection // TODO
                >> fun (tag, commits) ->
                    tag
                    |> ValueOption.map (fun tag ->
                        collection
                        |> TagCommitCollection.Tags.get tag)
                    ,
                    commits
                    |> Seq.filter (TagCommitCollection.Commits.containsScopeOrLookup collection repo scope)
                    |> Seq.map (TagCommitCollection.Commits.get collection >> function
                        | commit -> { ParsedCommit = ConventionalCommit.parse commit.Message
                                      Original = commit })
                    |> Seq.toArray
                )
            )

/// <summary>
/// Functions, types and helpers for the <c>Render</c> Phase.
/// </summary>
/// <remarks>
/// This phase is responsible for rendering the computed <c>GitNet</c> tree
/// into primitive string records/DUs for the writer to then process into MD.
/// </remarks>
module Render =
    /// <summary>
    /// A rendered commit, for a tag, of a scope.
    /// </summary>
    type Commit = {
        Message: string
        CommitSha: string
        CommitAuthor: string
    }
    /// <summary>
    /// A rendered tag, of a scope.
    /// </summary>
    type Tag = {
        TagName: string
        TagUrl: string voption
        TagDate: string voption
        Commits: FrozenDictionary<string,Commit list>
    }
    /// <summary>
    /// A rendered scope, of a repository.
    /// </summary>
    type Scope = {
        ScopeName: string voption
        ScopeTags: Tag list
        ScopeUnreleased: Commit list
    }
    /// <summary>
    /// DU which indicates a failure in the rendering phase.
    /// </summary>
    type private Error =
        /// <summary>
        /// The rendering phase should only receive one unreleased tag
        /// per scope.
        /// </summary>
        | MoreThanOneUnreleasedTag
    /// <summary>
    /// Functions relating to Rendering of commits.
    /// </summary>
    module Commit =
        open LibGit2Sharp.FSharp
        let fromGitNetCommit ( { Original = gitCommit } as commit ) =
            {
                Message =
                    commit
                    |> GitNetCommit.subject
                CommitSha =
                    Commit.sha gitCommit
                CommitAuthor =
                    gitCommit
                    |> Commit.author
                    |> Signature.name
            }
    /// <summary>
    /// Functions relating to rendering of Tags
    /// </summary>
    module Tag =
        let fromGitNetTagWithCommits config commits (tag: GitNetTag) =
            {

                TagName =
                    tag
                    |> GitNetTag.toTitleString
                    |> _.Value
                TagUrl = ValueNone
                TagDate =
                    try
                    tag
                    |> GitNetTag.Git.commit
                    |> Commit.committer
                    |> Signature.date
                    |> _.ToString("yyyy-M-dd")
                    |> ValueSome
                    with e ->
                        ValueNone
                Commits =
                    commits
                    |> CommitCategorisation.groupCommits config
                    |> _.ToFrozenDictionary(
                        (fun group ->
                        group.Key |> ValueOption.defaultValue "Other"),
                        Seq.map Commit.fromGitNetCommit >> Seq.toList)
                    
            }
    /// <summary>
    /// Helpers that determines if a <c>tag * commits</c> tuple is an unreleased collection.
    /// This is determined by whether the tag is a true value, or null.
    /// </summary>
    /// <param name="tag"></param>
    /// <param name="_commits"></param>
    let private isUnreleasedTagCommitTuple (tag: GitNetTag voption, _commits: GitNetCommit array) = tag.IsNone
    /// <summary>
    /// Tries to partition a collection of <c>tag * commit</c> tuples, and extract
    /// the unreleased commits.<br/>
    /// </summary>
    let private tryPartitionUnreleased =
        Array.partition isUnreleasedTagCommitTuple
        >> fun (unreleased, tagged) ->
            try
            let result = 
                unreleased
                |> Array.exactlyOne
                |> snd,
                tagged |> Array.map (fun (tag,commits) ->
                    tag.Value,commits)
            Ok result
            with
            | :? System.ArgumentException as e ->
                Error Error.MoreThanOneUnreleasedTag
    /// <summary>
    /// Partitions a collection of <c>tag * commit</c> tuples, and extracts the unreleased commits.
    /// This will always succeed.
    /// </summary>
    let private partitionUnreleased =
        Array.partition isUnreleasedTagCommitTuple
        >> fun (unreleased,tagged) ->
            unreleased
            |> Array.tryHead
            |> Option.map snd
            |> Option.defaultValue [||]
            ,
            tagged
            |> Array.map(fun (tag,arr) -> tag.Value,arr)
    /// <summary>
    /// Functions relating to the rendering of scopes.
    /// </summary>
    module Scope =
        let fromPreRenderedCollectionEntry config (scopeName: GitCollection.Scope, entries: (GitNetTag voption * GitNetCommit array) array) =
            let unreleasedCommits, taggedCommits =
                entries |> partitionUnreleased 
            {
                ScopeName =
                    scopeName.Value
                    |> ValueSome
                ScopeTags =
                    taggedCommits
                    |> Array.map(fun (tag,commits) -> Tag.fromGitNetTagWithCommits config commits tag)
                    |> Array.toList
                ScopeUnreleased =
                    unreleasedCommits
                    |> Array.map Commit.fromGitNetCommit
                    |> Array.toList
            }
    /// <summary>
    /// Conducts the rendering of a <c>TagCommitCollection</c>.
    /// </summary>
    /// <param name="config">GitNetConfig - Provides details as to the categorisation behaviour of commits in tags.</param>
    /// <param name="repo">Repository - Used for lookups and diffing.</param>
    /// <param name="collection">TagCommitCollection - The collection to render.</param>
    let fromTagCommitCollection config repo collection =
        let collection' = PreRenderPhase.fromTagCommitCollection repo collection
        collection'
        |> Seq.map (Scope.fromPreRenderedCollectionEntry config)
        |> Seq.toArray
        
