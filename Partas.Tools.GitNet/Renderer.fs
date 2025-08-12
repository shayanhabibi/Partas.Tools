module Partas.Tools.GitNet.Renderer

open System.Collections.Generic
open LibGit2Sharp.FSharp
open Partas.Tools.GitNet.Types

// Group our collection further into renderable groups.

(* RENDERING
*)

module Render =
    type Commit = {
        Message: string
        CommitSha: string
        CommitAuthor: string
    }
    type Tag = {
        TagName: string
        TagUrl: string
        TagDate: string voption
        Commits: Commit list
    }
    type Scope = {
        ScopeName: string voption
        ScopeTags: Tag list
    }
    type ScopeCollection = {
        Unscoped: Commit list
    }
