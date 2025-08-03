namespace LibGit2Sharp.FSharpPatterns

open LibGit2Sharp
open LibGit2Sharp.FSharp

[<RequireQualifiedAccess>]
module GitObject =
    [<return: Struct>]
    let (|IsCommit|_|) gitObject =
        gitObject |> GitObject.peel<Commit>
    [<return: Struct>]
    let (|IsTree|_|) gitObject =
        gitObject |> GitObject.peel<Tree>
    [<return: Struct>]
    let (|IsBlob|_|) gitObject =
        gitObject |> GitObject.peel<Blob>

[<RequireQualifiedAccess>]
module TreeEntry =
    let (|IsBlob|IsTree|IsLink|) treeEntry =
        treeEntry |> TreeEntry.getTarget

[<RequireQualifiedAccess>]
module Branch =
    let (|IsHead|IsRemote|IsTracking|) = function
        | branch when Branch.isHead branch -> IsHead
        | branch when Branch.isRemote branch -> IsRemote
        | branch when Branch.isTracking branch -> IsTracking
