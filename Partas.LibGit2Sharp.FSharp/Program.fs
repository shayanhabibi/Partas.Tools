/// <summary>
/// Contains FSharp friendly wrappers of LibGit2Sharp functions
/// </summary>
module LibGit2Sharp.FSharp

open LibGit2Sharp

[<RequireQualifiedAccess>]
module Repository =
    let inline private get func: Repository -> 'T = func
    let inline load path = new Repository(path)
    /// <summary>
    /// Loads a repository from <c>path</c> and completes <c>thunk</c>.
    /// </summary>
    /// <remarks>Disposes resources after <c>thunk</c> is complete.</remarks>
    /// <param name="thunk">Actions to perform within the resource usage scope.</param>
    /// <param name="path">Path or working directory of repo.</param>
    let doThenDispose (thunk: Repository -> unit) path  =
        use repo = load path
        thunk repo
    let dispose: Repository -> unit = get _.Dispose()
    let commits = get _.Commits
    let branches = get _.Branches
    let config = get _.Config
    let diff = get _.Diff
    let head = get _.Head
    let index = get _.Index
    let database = get _.ObjectDatabase
    let tags = get _.Tags
    let worktrees = get _.Worktrees
    let ignores = get _.Ignore
    let info = get _.Info
    let network = get _.Network
    let notes = get _.Notes
    let rebase = get _.Rebase
    let refs = get _.Refs
    let stashes = get _.Stashes
    let submodules = get _.Submodules
    let checkout options paths tree (repository: Repository) = repository.Checkout(tree,paths,options)
    let applyTag tagName = get _.ApplyTag(tagName)
    let lookup lookupString = get _.Lookup(lookupString : string) >> ValueOption.ofObj
    let isValid = Repository.IsValid
    let discover = Repository.Discover
    /// <remarks>
    /// Returns <c>ValueNone</c> if no annotated tag describing the commit
    /// is found.
    /// </remarks>
    /// <summary>
    /// Finds the most recent annotated tag that is reachable from a commit.
    /// If the tag points to the commit, then only the tag is shown. Otherwise, it suffixes the tag name with the number of additional commits on top of the tagged object and the abbreviated object name of the most recent commit.
    /// </summary>
    /// <param name="commit"></param>
    let describe commit =
        try get _.Describe(commit) >> ValueSome
        with e -> fun _ -> ValueNone
[<RequireQualifiedAccess>]
module ObjectDatabase =
    let inline private get func: ObjectDatabase -> 'T = func
    let hashCode: ObjectDatabase -> int = get _.GetHashCode()
    let calculateHistoryDivergence commit1 commit2 = get _.CalculateHistoryDivergence(commit1, commit2)
    let retrieveMetadata object = get _.RetrieveObjectMetadata(object)
    let shortenId object = get _.ShortenObjectId(object)
[<RequireQualifiedAccess>]
module RepositoryInformation =
    let inline private get func: RepositoryInformation -> 'T = func
    let message: RepositoryInformation -> string voption = get _.Message >> ValueOption.ofObj
    let path: RepositoryInformation -> string = get _.Path
    let currentOperation: RepositoryInformation -> CurrentOperation = get _.CurrentOperation
    let isBare: RepositoryInformation -> bool = get _.IsBare
    let isHeadDetached: RepositoryInformation -> bool = get _.IsHeadDetached
    let isHeadUnborn: RepositoryInformation -> bool = get _.IsHeadUnborn
    let isShallow: RepositoryInformation -> bool = get _.IsShallow
    /// <summary>
    /// Working Directory can return <c>null</c> if the repo is a <c>bare</c> repo. Use
    /// this when you are sure this is not the case.
    /// </summary>
    let unsafeWorkingDirectory: RepositoryInformation -> string = get _.WorkingDirectory
    let workingDirectory: RepositoryInformation -> string voption = get _.WorkingDirectory >> ValueOption.ofObj
    let hashCode: RepositoryInformation -> int = get _.GetHashCode()
    
[<RequireQualifiedAccess>]
module Commit =
    let inline private get (func: Commit -> 'T): Commit -> 'T = func
    let message: Commit -> string = get _.Message
    /// <summary>
    /// Alias for <c>MessageShort</c>
    /// </summary>
    let subject: Commit -> string = get _.MessageShort
    let messageShort = subject
    /// <summary>
    /// Is lazy loaded, and can throw an exception if the commit no longer exists.
    /// </summary>
    let unsafeParents: Commit -> seq<Commit> = get _.Parents
    /// <summary>
    /// Prevents exceptions if the commit does not exist anymore.
    /// </summary>
    let parents commit: seq<Commit> voption =
        try
        get _.Parents commit |> ValueOption.ofObj
        with e ->
            ValueNone
    let author: Commit -> Signature = get _.Author
    let committer: Commit -> Signature = get _.Committer
    let encoding: Commit -> string = get _.Encoding
    let isMissing: Commit -> bool = get _.IsMissing
    let notes: Commit -> seq<Note> = get _.Notes
    let prettifyMessage: Commit -> string = message >> fun msg -> Commit.PrettifyMessage(msg, '#')
    /// <summary>
    /// Alias for <c>Sha</c>
    /// </summary>
    let hash: Commit -> string = get _.Sha
    let sha: Commit -> string = get _.Sha
    let tree: Commit -> Tree = get _.Tree
    let id: Commit -> ObjectId = get _.Id
    let getTreeItem path = get _.Item(path) >> ValueOption.ofObj
[<RequireQualifiedAccess>]
module Branch =
    let inline private get func: Branch -> 'T = func
    let commits: Branch -> ICommitLog = get _.Commits
    let fullName: Branch -> string = get _.CanonicalName
    let name: Branch -> string = get _.FriendlyName
    let isHead: Branch -> bool = get _.IsCurrentRepositoryHead
    let isRemote: Branch -> bool = get _.IsRemote
    let isTracking: Branch -> bool = get _.IsTracking
    let getTreeItem (path: string) = get _.Item(path) >> ValueOption.ofObj
    let ref: Branch -> Reference = get _.Reference
    let tip: Branch -> Commit = get _.Tip
    let remoteName (branch: Branch) = if isRemote branch then ValueSome branch.RemoteName else ValueNone
    let trackedBranch: Branch -> Branch voption = get _.TrackedBranch >> ValueOption.ofObj
    let trackingDetails: Branch -> BranchTrackingDetails = get _.TrackingDetails
    let upstreamBranchFullName: Branch -> string = get _.UpstreamBranchCanonicalName
[<RequireQualifiedAccess>]
module ICommitLog =
    let inline private get func: ICommitLog -> 'T = func
    let sortedBy: ICommitLog -> CommitSortStrategies = get _.SortedBy
[<RequireQualifiedAccess>]
module BranchTrackingDetails =
    let inline private get func: BranchTrackingDetails -> 'T = func
    let aheadBy: BranchTrackingDetails -> int voption = get _.AheadBy >> ValueOption.ofNullable
    let behindBy: BranchTrackingDetails -> int voption = get _.BehindBy >> ValueOption.ofNullable
    let commonAncestor: BranchTrackingDetails -> Commit voption = get _.CommonAncestor >> ValueOption.ofObj
    let hashCode: BranchTrackingDetails -> int = get _.GetHashCode()
[<RequireQualifiedAccess>]
module Tag =
    let inline private get func: Tag -> 'T = func
    let fullName: Tag -> string = get _.CanonicalName
    let name: Tag -> string = get _.FriendlyName
    let ref: Tag -> Reference = get _.Reference
    let isAnnotated: Tag -> bool = get _.IsAnnotated
    let annotation: Tag -> TagAnnotation voption = get _.Annotation >> ValueOption.ofObj
    let peeled: Tag -> GitObject = get _.PeeledTarget
    let target: Tag -> GitObject = get _.Target
[<RequireQualifiedAccess>]
module TagAnnotation =
    let inline private get func: TagAnnotation -> 'T = func
    let message: TagAnnotation -> string = get _.Message
    let isMissing: TagAnnotation -> bool = get _.IsMissing
    let sha: TagAnnotation -> string = get _.Sha
    let id: TagAnnotation -> ObjectId = get _.Id
    let name: TagAnnotation -> string = get _.Name
    let target: TagAnnotation -> GitObject = get _.Target
    let tagger: TagAnnotation -> Signature = get _.Tagger
    let hashCode: TagAnnotation -> int = get _.GetHashCode()
    let peel<'T when 'T :> GitObject> tag =
        try
        get _.Peel<'T>() tag |> ValueSome
        with e -> ValueNone
    let unsafePeel<'T when 'T :> GitObject> = get _.Peel<'T>()
[<RequireQualifiedAccess>]
module Signature =
    let inline private get func: Signature -> 'T = func
    let name = get _.Name
    let email = get _.Email
    let date = get _.When
    let hashCode = get _.GetHashCode()
[<RequireQualifiedAccess>]
module Reference =
    let inline private get func: Reference -> 'T = func
    let fullName: Reference -> string = get _.CanonicalName
    let isLocalBranch: Reference -> bool = get _.IsLocalBranch
    let isNote: Reference -> bool = get _.IsNote
    let isRemoteTrackingBranch: Reference -> bool = get _.IsRemoteTrackingBranch
    let isTag: Reference -> bool = get _.IsTag
    let targetIdentifier: Reference -> string = get _.TargetIdentifier
    let resolve: Reference -> DirectReference = get _.ResolveToDirectReference()
    let hashCode: Reference -> int = get _.GetHashCode()
module DirectReference = Reference
[<RequireQualifiedAccess>]
module Tree =
    let inline private get' func: Tree -> 'T = func
    let get path: Tree -> _ = _.Item(path) >> ValueOption.ofObj
    let unsafeGet path = get' _.Item(path)
    let hash: Tree -> string = get' _.Sha
    let count: Tree -> int = get' _.Count
    let isMissing: Tree -> bool = get' _.IsMissing
    let hashCode: Tree -> int = get' _.GetHashCode()
    let enumerate: Tree -> System.Collections.Generic.IEnumerator<TreeEntry> = get' _.GetEnumerator()
    let id: Tree -> ObjectId = get' _.Id
[<RequireQualifiedAccess>]
module TreeEntry =
    let inline private get func: TreeEntry -> 'T = func
    let mode: TreeEntry -> Mode = get _.Mode
    let name: TreeEntry -> string = get _.Name
    let path: TreeEntry -> string = get _.Path
    let target: TreeEntry -> GitObject = get _.Target
    let targetType: TreeEntry -> TreeEntryTargetType = get _.TargetType
    let hashCode: TreeEntry -> int = get _.GetHashCode()
    let targetIsBlob: TreeEntry -> bool = targetType >> _.HasFlag(TreeEntryTargetType.Blob)
    let targetIsTree: TreeEntry -> bool = targetType >> _.HasFlag(TreeEntryTargetType.Tree)
    let targetIsLink: TreeEntry -> bool = targetType >> _.HasFlag(TreeEntryTargetType.GitLink)
    let private targetAsBlob: TreeEntry -> Blob = target >> _.Peel<Blob>()
    let private targetAsTree: TreeEntry -> Tree = target >> _.Peel<Tree>()
    let private targetAsLink: TreeEntry -> GitLink = target >> _.Peel<GitLink>()
    let getTarget = function
        | t when t |> targetIsBlob -> targetAsBlob t |> Choice1Of3
        | t when t |> targetIsTree -> targetAsTree t |> Choice2Of3
        | t when t |> targetIsLink -> targetAsLink t |> Choice3Of3
        | _ -> failwith "unreachable"
[<RequireQualifiedAccess>]
module Blob =
    let inline private get func: Blob -> 'T = func
    let isMissing: Blob -> bool = get _.IsMissing
    let sha: Blob -> string = get _.Sha
    let id: Blob -> ObjectId = get _.Id
    /// <summary>
    /// Wraps the operation with a <c>try ... with</c>
    /// </summary>
    let isBinary blob: bool voption =
        try
        get _.IsBinary blob |> ValueSome
        with e -> ValueNone
    let unsafeIsBinary: Blob -> bool = _.IsBinary
    /// <summary>
    /// Wraps the operation with a <c>try ... with</c>
    /// </summary>
    let size blob: int64 voption =
        try get _.Size blob |> ValueSome with e -> ValueNone
    let unsafeSize: Blob -> int64 = get _.Size
    let hashCode: Blob -> int = get _.GetHashCode()
    let peel<'T when 'T :> GitObject> blob =
        try get _.Peel<'T>() blob |> ValueSome with e -> ValueNone
    let unsafePeel<'T when 'T :> GitObject> = get _.Peel<'T>()
    let unsafeContentStream filteringOptions =
        get _.GetContentStream(filteringOptions)
    let contentStream filteringOptions blob =
        try unsafeContentStream filteringOptions blob |> ValueSome with _ -> ValueNone
    let unsafeContentText: Blob -> string = get _.GetContentText()
    let contentText blob: string voption =
        try unsafeContentText blob |> ValueSome with _ -> ValueNone
[<RequireQualifiedAccess>]
module GitLink =
    let inline private get func: GitLink -> 'T = func
    let isMissing: GitLink -> bool = get _.IsMissing
    let sha: GitLink -> string = get _.Sha
    let id: GitLink -> ObjectId = get _.Id
    let hashCode: GitLink -> int = get _.GetHashCode()
    let unsafePeel<'T when 'T :> GitObject> = get _.Peel<'T>()
    let peel<'T when 'T :> GitObject> gitLink =
        try unsafePeel<'T> gitLink |> ValueSome with _ -> ValueNone
[<RequireQualifiedAccess>]
module Note =
    let inline private get func: Note -> 'T = func
    let message: Note -> string = get _.Message
    let blobId: Note -> ObjectId = get _.BlobId
    let gitNamespace: Note -> _ = get _.Namespace
    let targetId: Note -> ObjectId = get _.TargetObjectId
    let hashCode: Note -> int = get _.GetHashCode()
[<RequireQualifiedAccess>]
module ObjectId =
    let inline private get func: ObjectId -> 'T = func
    let sha = get _.Sha
    let rawId = get _.RawId
    let hashCode = get _.GetHashCode()
[<RequireQualifiedAccess>]
module GitObject =
    let inline private get func: GitObject -> 'T = func
    let sha: GitObject -> string = get _.Sha
    let isMissing: GitObject -> bool = get _.IsMissing
    let id: GitObject -> ObjectId = get _.Id
    let unsafePeel<'T when 'T :> GitObject> = get _.Peel<'T>()
    let peel<'T when 'T :> GitObject> gitObj =
        try
            unsafePeel<'T> gitObj
            |> ValueSome
        with _ -> ValueNone
    let hashCode: GitObject -> int = get _.GetHashCode()
[<RequireQualifiedAccess>]
module Index =
    let inline private get' func: Index -> 'T = func
    let unsafeGet (path: string): Index -> _ = get' _.Item(path)
    let get path = unsafeGet path >> ValueOption.ofObj
    let count: Index -> int = get' _.Count
    let conflicts: Index -> ConflictCollection = get' _.Conflicts
    let isFullyMerged: Index -> bool = get' _.IsFullyMerged
    let hashCode: Index -> int = get' _.GetHashCode()
    let addFile path = get' _.Add(path: string)
    let addBlob mode path blob = get' _.Add(blob,path,mode)
    let enumerate: Index -> System.Collections.Generic.IEnumerator<IndexEntry> = get' _.GetEnumerator()
    let clear: Index -> unit = get' _.Clear()
    let remove path = get' _.Remove(path)
    let replaceWithCommit (commit: Commit) = get' _.Replace(commit)
    let replaceWithTree (tree: Tree) = get' _.Replace(tree)
    let replaceWithCommitPaths paths commit = get' _.Replace(commit,paths)
    let replace options paths commit = get' _.Replace(commit,paths,options)
    let write: Index -> unit = get' _.Write()
    let writeToTree: Index -> Tree = get' _.WriteToTree()
[<RequireQualifiedAccess>]
module Ignore =
    let inline private get func: Ignore -> 'T = func
    let hashCode: Ignore -> int = get _.GetHashCode()
    let addTemporaryRules rules = get _.AddTemporaryRules(rules)
    let isPathIgnore path = get _.IsPathIgnored(path)
    let resetTemporaryRules: Ignore -> unit = get _.ResetAllTemporaryRules()
[<RequireQualifiedAccess>]
module Mode =
    let inline private get func: Mode -> 'T = func
    let isField = get _.HasFlag(Mode.GitLink)
    let isDirectory = get _.HasFlag(Mode.Directory)
    let isExecutableFile = get  _.HasFlag(Mode.ExecutableFile)
    let isNonExecutableFile = get  _.HasFlag(Mode.NonExecutableFile)
    let isNonExecutableGroupWritableFile = get  _.HasFlag(Mode.NonExecutableGroupWritableFile)
    let isNonexistent = get  _.HasFlag(Mode.Nonexistent)
    let isSymbolicLink = get  _.HasFlag(Mode.SymbolicLink)
[<RequireQualifiedAccess>]
module Network =
    let inline private get func: Network -> 'T = func
    let remotes: Network -> RemoteCollection = get _.Remotes
    let hashCode: Network -> int = get _.GetHashCode()
    let listRefsForUrl url =
        get _.ListReferences(url: string) >> ValueOption.ofObj
    let listRefsForRemote remote =
        get _.ListReferences(remote: Remote) >> ValueOption.ofObj
    let fetch refSpecs path = get _.Fetch(path, refSpecs)
    let pushBranch branch  network=
        try get _.Push(branch: Branch) network |> Ok
        with :? LibGit2SharpException as e -> Error e
    let pushBranches branches  network=
        try get _.Push(branches: Branch seq) network |> Ok
        with :? LibGit2SharpException as e -> Error e
    let pushBranchWith (options: PushOptions) (branch: Branch) network =
        try get _.Push(branch, options) network |> Ok
        with :? LibGit2SharpException as e -> Error e
[<RequireQualifiedAccess>]
module CommitFilter =
    let inline private get func: CommitFilter -> 'T = func
    let init () = CommitFilter()
    module Get =
        let excludeReachableFrom = get _.ExcludeReachableFrom >> ValueOption.ofObj >> unbox<GitObject voption>
        let since = excludeReachableFrom
        let includeReachableFrom = get _.IncludeReachableFrom >> ValueOption.ofObj >> unbox<GitObject voption>
        let until = includeReachableFrom
        let firstParentOnly = get _.FirstParentOnly
        let sortBy (filter: CommitFilter) = filter.SortBy
    let excludeReachableFrom value (filter: CommitFilter) = filter.ExcludeReachableFrom <- value; filter
    let includeReachableFrom value (filter: CommitFilter) = filter.IncludeReachableFrom <- value; filter
    /// Alias for <c>excludeReachableFrom</c>
    let since = excludeReachableFrom
    /// Alias for <c>includeReachableFrom</c>
    let until = includeReachableFrom
    let firstParentOnly (filter: CommitFilter) = filter.FirstParentOnly <- true; filter
    let sortBy sortStrategy (filter: CommitFilter) = filter.SortBy <- sortStrategy; filter
    let hashCode = get _.GetHashCode()
    /// <summary>
    /// Initialises a commit filter for common patterns
    /// </summary>
    module Common =
        let between since until = init() |> excludeReachableFrom since |> includeReachableFrom until
        let until until = init() |> includeReachableFrom until
        let since since = init() |> excludeReachableFrom since
[<RequireQualifiedAccess>]
module CommitLog =
    let inline private get func: CommitLog -> 'T = func
    let sortedBy = get _.SortedBy
    let hashCode = get _.GetHashCode()
    let enumerate = get _.GetEnumerator()
    let queryBy filter : #IQueryableCommitLog -> _ = _.QueryBy(filter: CommitFilter)
    let queryByPath path: #IQueryableCommitLog -> _ = _.QueryBy(path: string)
    let queryByPathWith filter path: #IQueryableCommitLog -> _ = _.QueryBy(path,filter)
    module Query =
        let between since until = CommitFilter.Common.between since until |> queryBy
        let until until = CommitFilter.Common.until until |> queryBy
        let since since = CommitFilter.Common.since since |> queryBy
[<RequireQualifiedAccess>]
module LogEntry =
    let inline private get func: LogEntry -> 'T = func
    let path = get _.Path
    let commit = get _.Commit    
    let hashCode = get _.GetHashCode()

[<RequireQualifiedAccess>]
module CommitSortStrategies =
    let inline private get func: CommitSortStrategies -> 'T = func
    let inline private has flag: CommitSortStrategies -> bool = get _.HasFlag(flag)
    let isNone = has CommitSortStrategies.None
    let hasTime = has CommitSortStrategies.Time
    let hasTopological = has CommitSortStrategies.Topological
    let hasReverse = has CommitSortStrategies.Reverse

[<RequireQualifiedAccess>]
module DescribeOptions =
    let inline private get func: DescribeOptions -> 'T = func
    let init () = DescribeOptions()
    let alwaysRenderLongFormat options = (options: DescribeOptions).AlwaysRenderLongFormat <- true
    let neverRenderLongFormat options = (options: DescribeOptions).AlwaysRenderLongFormat <- false
    let minCommitIdAbbrevLen length options = (options: DescribeOptions).MinimumCommitIdAbbreviatedSize <- length
    let onlyFirstParent options = (options: DescribeOptions).OnlyFollowFirstParent <- true
    let followParents options = (options: DescribeOptions).OnlyFollowFirstParent <- false
    let commitIdFallback options = (options: DescribeOptions).UseCommitIdAsFallback <- true
    let noCommitIdFallback options = (options: DescribeOptions).UseCommitIdAsFallback <- false
    let strategy strategy options = (options: DescribeOptions).Strategy <- strategy
    let hashCode = get _.GetHashCode()

[<RequireQualifiedAccess>]
module HistoryDivergence =
    let inline private get func: HistoryDivergence -> 'T = func
    let aheadBy: HistoryDivergence -> int voption = get _.AheadBy >> ValueOption.ofNullable
    let behindBy: HistoryDivergence -> int voption = get _.BehindBy >> ValueOption.ofNullable
    let one: HistoryDivergence -> Commit voption = get _.One >> ValueOption.ofObj
    let another: HistoryDivergence -> Commit voption = get _.Another >> ValueOption.ofObj
    let commonAncestor: HistoryDivergence -> Commit voption = get _.CommonAncestor >> ValueOption.ofObj
    let hashCode: HistoryDivergence -> int = get _.GetHashCode()

[<RequireQualifiedAccess>]
module Worktree =
    let inline private get func: Worktree -> 'T = func
    let name: Worktree -> string = get _.Name
    let isLocked: Worktree -> bool = get _.IsLocked
    let lockReason: Worktree -> string = get _.LockReason
    let repository: Worktree -> Repository = get _.WorktreeRepository
    let hashCode: Worktree -> int = get _.GetHashCode()
    let lock reason = get _.Lock(reason)
    let unlock: Worktree -> unit = get _.Unlock()

[<RequireQualifiedAccess>]
module TreeChanges =
    let inline private get func: TreeChanges -> 'T = func
    let count: TreeChanges -> int = get _.Count
    let added: TreeChanges -> seq<TreeEntryChanges> = get _.Added
    let conflicted: TreeChanges -> seq<TreeEntryChanges> = get _.Conflicted
    let copied: TreeChanges -> seq<TreeEntryChanges> =  get _.Copied
    let deleted: TreeChanges -> seq<TreeEntryChanges> =  get _.Deleted
    let modified: TreeChanges -> seq<TreeEntryChanges> =  get _.Modified
    let renamed: TreeChanges -> seq<TreeEntryChanges> =  get _.Renamed
    let typeChanged: TreeChanges -> seq<TreeEntryChanges> =  get _.TypeChanged
    let unmodified: TreeChanges -> seq<TreeEntryChanges> =  get _.Unmodified
    let hashCode: TreeChanges -> int = get _.GetHashCode()
    let enumerate: TreeChanges -> System.Collections.Generic.IEnumerator<TreeEntryChanges> = get _.GetEnumerator()
    let dispose: TreeChanges -> unit = get _.Dispose()

[<RequireQualifiedAccess>]
module Remote =
    let inline private get func: Remote -> 'T = func
    let isAutomaticallyPruneOnFetch: Remote -> bool = get _.AutomaticallyPruneOnFetch
    let fetchRefSpecs: Remote -> seq<RefSpec> = get _.FetchRefSpecs
    let name: Remote -> string = get _.Name
    let pushRefSpecs: Remote -> seq<RefSpec> = get _.PushRefSpecs
    let pushUrl: Remote -> string = get _.PushUrl
    let tagFetchMode: Remote -> TagFetchMode = get _.TagFetchMode
    let url: Remote -> string = get _.Url
    let dispose: Remote -> unit = get _.Dispose()
    let hashCode: Remote -> int = get _.GetHashCode()
    let isValidName = Remote.IsValidName

[<RequireQualifiedAccess>]
module GitObjectMetadata =
    let inline private get func: GitObjectMetadata -> 'T = func
    let typ = get _.Type
    let size = get _.Size
    let isCommit = typ >> _.HasFlag(ObjectType.Commit)
    let isBlob = typ >> _.HasFlag(ObjectType.Blob)
    let isTree = typ >> _.HasFlag(ObjectType.Tree)
    let isTag = typ >> _.HasFlag(ObjectType.Tag)
    let getFlags o =
        [
            if isCommit o then ObjectType.Commit
            if isBlob o then ObjectType.Blob
            if isTree o then ObjectType.Tree
            if isTag o then ObjectType.Tag
        ]
/// <summary>
/// Note that the <c>Diff</c> methods are numerous and accept different types.<br/>
/// You can expect just a few common comparisons and helpers here.
/// </summary>
[<RequireQualifiedAccess>]
module Diff =
    let inline private get func: Diff -> 'T = func
    module Tree =
        let compare<'ChangeType
            when 'ChangeType: not struct and 'ChangeType :> IDiffResult
            > (old: Tree) (newTree: Tree) = get _.Compare<'ChangeType>(old, newTree)
        let compareWith<'ChangeType
            when 'ChangeType: not struct and 'ChangeType :> IDiffResult
            > (options: CompareOptions) (old: Tree) (newTree: Tree) = get _.Compare<'ChangeType>(old,newTree,options)
    module Commit =
        let compare<'ChangeType
            when 'ChangeType: not struct and 'ChangeType :> IDiffResult
            > (old: Commit) (newCommit: Commit) = get _.Compare<'ChangeType>(old.Tree, newCommit.Tree)
        let compareWith<'ChangeType
            when 'ChangeType: not struct and 'ChangeType :> IDiffResult
            > (options: CompareOptions) (old: Commit) (newCommit: Commit) = get _.Compare<'ChangeType>(old.Tree,newCommit.Tree,options)

[<RequireQualifiedAccess>]
module TreeEntryChanges =
    let inline private get func: TreeEntryChanges -> 'T = func
    let exists: TreeEntryChanges -> bool = get _.Exists
    let mode: TreeEntryChanges -> Mode = get _.Mode
    let oid: TreeEntryChanges -> ObjectId = get _.Oid
    let path: TreeEntryChanges -> string = get _.Path
    let status: TreeEntryChanges -> ChangeKind = get _.Status
    let oldExists: TreeEntryChanges -> bool = get _.OldExists
    let oldMode: TreeEntryChanges -> Mode = get _.OldMode
    let oldOid: TreeEntryChanges -> ObjectId = get _.OldOid
    let oldPath: TreeEntryChanges -> string = get _.OldPath
