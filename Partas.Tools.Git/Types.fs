module Partas.Tools.Git.Types

open Partas.Tools.Cli.Binding
open System
open BlackFox.CommandLine

type GitOption =
    interface
        static member inline op_Implicit(other: #GitOption): GitOption = other :> GitOption
    end

type GitCommand<'T> = {
    Options: GitOption list
    Args: string list
}

module GitCommand =
    let inline init<'T> = {
        Options = []
        Args = []
    }
    let toCmdLineWithCasing casing (command: GitCommand<'T>): CmdLine =
        let options =
            command.Options
            |> List.map CliBinding.toString
        CmdLine.empty
        |> CmdLine.append (typeof<'T>.Name |> Casing.toString casing)
        |> CmdLine.appendRaw (options |> String.concat " ")
        |> CmdLine.appendRaw (command.Args |> String.concat " ")
    let toCmdLine (command: GitCommand<'T>): CmdLine = toCmdLineWithCasing Casing.KebabCase command
    let toString<'T>: GitCommand<'T> -> string = toCmdLine >> CmdLine.toString
    
module Options =
    [<RequireQualifiedAccess>]
    type ExcludeHidden =
        | fetch
        | receive
        | uploadpack
[<CliBinding>]
type RevList =
    | MaxCount of int
    | Skip of int
    | Since of DateOnly
    | After of DateOnly
    | SinceAsFilter of DateOnly
    | Until of DateOnly
    | Before of DateOnly
    | MaxAge of DateTime
    | MinAge of DateTime
    | Author of string
    | Committer of string
    | GrepReflog of string
    | Grep of string
    | AllMatch
    | InvertGrep
    | RegexpIgnoreCase
    | BasicRegexp
    | ExtendedRegexp
    | FixedStrings
    | PearlRegexp
    | RemoveEmpty
    | Merges
    | NoMerges
    | MinParents of int
    | MaxParents of int
    | NoMinParents
    | NoMaxParents
    | FirstParent
    | ExcludeFirstParentOnly
    | Not
    | All
    | Branches of string
    | [<CompiledName "Tags">] Tags' of string
    | Remotes of string
    | Glob of string
    | Exclude of string
    | ExcludeHidden of Options.ExcludeHidden
    | Reflog
    | AlternateRefs
    | SingleWorktree
    | IgnoreMissing
    | Stdin
    | Quiet
    | DiskUsage
    | CherryMark
    | CherryPick
    | LeftOnly
    | RightOnly
    | Cherry
    | WalkReflogs
    | Merge
    | Boundary
    | UseBitmapIndex
    | Progress of string
    | [<CliBinding(Prefix = "-")>] Z
    | SimplifyByDecoration
    | ShowPulls
    | FullHistory
    | Dense
    | Sparse
    | SimplifyMerges
    | AncestryPath of string
    | ``FullHistory without parent rewriting``
    | ``FullHistory with parent rewriting``
    | Bisect
    | BisectVars
    | BisectAll
    | DateOrder
    | AuthorDateOrder
    | TopoOrder
    | Reverse
    | Objects
    | InCommitOrder
    | ObjectsEdge
    | ObjectsEdgeAggressive
    | IndexedObjects
    | Unpacked
    | ObjectNames
    | NoObjectNames
    | Filter of string
    | NoFilter
    | FilterProvidedObjects
    | FilterPrintOmitted
    | Missing of string
    | ExcludePromisorObjects
    | NoWalk of string
    | DoWalk
    | Pretty of string
    | [<
          CliBinding(BindingOptions = BindingOptions.StringLiteralFields)
      >] Format of string
    | AbbrevCommit
    | NoAbbrevCommit
    | OneLine
    | Encoding of string
    | ExpandTabs of int
    | NoExpandTabs
    | ShowSignature
    | RelativeDate
    | Date of string
    | Header
    | NoCommitHeader
    | CommitHeader
    | Parents
    | Children
    | Timestamp
    | LeftRight
    | Graph
    | ShowLinearBreak of string
    | Count
    interface GitOption

[<CliBinding(Equals = ":")>]
type PrettyFormats =
    | Oneline
    | Short
    | Medium
    | Full
    | Fuller
    | Reference
    | Email
    | Mboxrd
    | Raw
    | Format of string
    interface GitOption

[<CliBinding>]
type ShowRef =
    | Head
    | Branches
    | [<CompiledName "Tags">] Tags'
    | Dereference
    | Hash of int
    | Verify
    | Exists
    | Abbrev of int
    | Quiet
    | ExcludeExisting of pattern: string
    interface GitOption

[<CliBinding>]
type Log =
    | Follow
    | NoDecorate
    | Decorate of string
    | DecorateRefs of string
    | DecorateRefsExclude of string
    | ClearDecorations
    | Source
    | Mailmap
    | NoMailmap
    | UseMailmap
    | NoUseMailmap
    | FullDiff
    | LogSize
    | [<CliBinding(
        Prefix="-",
        Casing=Casing.None,
        Equals=":")
    >] L of funcName: string * file: string
    interface GitOption
[<CliBinding>]
type Tag =
    | List
    interface GitOption
[<CliBinding>]
type Branch =
    | List
    | Color
    | Remotes
    | Verbose
    | Abbrev
    | NoAbbrev
    interface GitOption
[<CliBinding>]
type ForEachRef =
    | Count of int
    | Stdin
    | Sort of string
    | Format of string
    | Color
    | Shell
    | Perl
    | Python
    | Tcl
    | PointsAt
    | Merged
    | NoMerged
    | Contains
    | NoContains
    | IgnoreCase
    | OmitEmpty
    | Exclude
    | IncludeRootRefs
    interface GitOption
[<CliBinding>]
type Config =
    | All
    | [<CliBinding(Prefix="")>] List
    interface GitOption
[<CliBinding(Prefix="")>]
type ConfigCommands =
    | List
    | Get
    | Set
    interface GitOption
