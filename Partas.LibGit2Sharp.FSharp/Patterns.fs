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
    [<return: Struct>]
    let (|IsHead|_|) = function
        | branch when Branch.isHead branch -> ValueSome ()
        | _ -> ValueNone
    
    [<return: Struct>]
    let (|IsRemote|_|) = function
        | branch when Branch.isRemote branch -> ValueSome ()
        | _ -> ValueNone
    
    [<return: Struct>]
    let (|IsTracking|_|) = function
        | branch when Branch.isTracking branch -> ValueSome ()
        | _ -> ValueNone
