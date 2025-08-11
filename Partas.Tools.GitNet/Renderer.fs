module Partas.Tools.GitNet.Renderer

open System.Collections.Generic
open LibGit2Sharp.FSharp
open Partas.Tools.GitNet.GitCollector
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

module GitCollector =
    let makeRenders (config: GitNetConfig) (gitCollection: GitNetCollections) =
        let repo = Repository.load config.RepositoryPath
        let githubUrlRoot =
            GitHub.find repo
            |> ValueOption.map GitHub.urlRoot
        let initState: Render.Scope list = []
        ()
        