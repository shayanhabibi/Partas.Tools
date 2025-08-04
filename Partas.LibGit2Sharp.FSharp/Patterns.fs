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
    let inline private isTyp<'T when 'T :> GitObject>: TreeEntry -> 'T voption = TreeEntry.target >> GitObject.peel<'T>
    [<return: Struct>]
    let (|IsBlob|_|): TreeEntry -> Blob voption = isTyp<Blob>
    [<return: Struct>]
    let (|IsTree|_|): TreeEntry -> Tree voption = isTyp<Tree>
    [<return: Struct>]
    let (|IsLink|_|): TreeEntry -> GitLink voption = isTyp<GitLink>

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
