module Partas.Tools.GitNet.Action

open System
open System.Collections.Generic
open System.Linq
open GitTraversal
open Partas.Tools.ConventionalCommits
open Partas.Tools
open RepoCracker
open Fake.Core
open Fake.IO
open FSharp.Core
open MarkdownWriter
open CommitCategorisation
open Types
open LibGit2Sharp.FSharp
open SepochSemver
open FSharp.Formatting.Markdown

type private Tag = LibGit2Sharp.Tag
type private TagSepochPair = Tag * SepochSemver
type private TagPairs = TagSepochPair * TagSepochPair
type private ICommitLog = LibGit2Sharp.ICommitLog
type private Grouping = string voption * TagPairs * Result<ICommitLog, string>

let inline private debug o =
    #if DEBUG
    printfn $"%A{o}"
    #else
    ()
    #endif

[<AutoOpen>]
module internal Helpers =
    let getWorkDir =
        Repository.info >> RepositoryInformation.workingDirectory
    let getGithubRemote =
        Repository.network
        >> Network.remotes
        >> fun collection -> query {
            for remote in collection do
                where (
                    remote |> Remote.url
                    |> _.Contains("github.com"))
                take 1; exactlyOne
        }
    let getGithubUrl: LibGit2Sharp.Remote -> _ = function
        | remote when remote.Url |> String.endsWith ".git" ->
            remote.Url[..remote.Url.Length - 5]
        | remote ->
            remote.Url

let parseCommit (input: LibGit2Sharp.Commit) =
    let parsed = ConventionalCommit.parse input.Message
    {
        ParsedCommit = parsed
        Original = input
    }

let gitNetWithConfig (config: GitNetConfig -> GitNetConfig) =
    let config = GitNetConfig.init |> config
    let repo = config.RepositoryPath |> Repository.load
    let githubRemote = repo |> getGithubRemote
    let githubUrl = githubRemote |> getGithubUrl
    let commitCollection: Grouping seq = getCommitCollections config repo
    let links = Dictionary<string, string * string option> []
    let makeCommitLink (commit: LibGit2Sharp.Commit) =
        links.TryAdd(commit.Sha[0..4], ($"{githubUrl}/commit/{commit.Sha}", None)) |> ignore
        Md.iLink commit.Sha[0..4]
    let makeTagLink tag url =
        let key = tag |> snd |> _.ToString()
        links.TryAdd(key, (url, None)) |> ignore
        Md.Tag.header tag
    let bodyRenders =
        let renderCommits (commits: ICommitLog) =
            let groupedCommits =
                commits
                |> Seq.map parseCommit
                |> groupCommits config
                // filter the skips and empty groups
                |> fun groups -> query {
                    for group in groups do
                    for commit in group do
                    where (
                        commit
                        |> GitNetCommit.subject
                        |> _.StartsWith("[skip")
                        |> not
                        || commit
                        |> GitNetCommit.footers
                        |> List.exists (function
                            | Footer.Footer("changelog", "ignore") -> true
                            | _ -> false
                            )
                      
                        )
                    groupValBy commit group.Key
                }
                |> fun groups -> query {
                    for group in groups do
                    where (group |> Seq.isEmpty |> not)
                    sortBy group.Key
                }
            [ for group in groupedCommits do
                if group.Key.IsSome then
                    Md.rawHtml $"<h3>{group.Key.Value}</h3>\n"
                Md.unorderedList [
                    for commit in group do
                        [
                            para [] {
                                commit |> GitNetCommit.subject |> String.trim
                                " - "
                                makeCommitLink commit.Original
                                "@"
                                commit |> GitNetCommit.Signatures.authorName
                            }
                            #if DEBUG
                            commit |> GitNetCommit.parsedType
                            |> Md.paragraphComment
                            #endif
                        ]
                ]
            ]
        let renderForFirstTag (scope: string voption) (tags: TagPairs) (commits: Result<ICommitLog, string>) =
            let tag,sepoch = fst tags
            let commits = commits |> function
                Ok value -> value
                | Error msg -> failwith msg
            [
                makeTagLink (tag,sepoch) $"{githubUrl}/compare/{commits |> Seq.head |> _.Sha.Substring(0,7)}...{tag.FriendlyName}"
                yield! renderCommits commits
            ]
        let renderForTag (scope: string voption) (tags: TagPairs) (commits: Result<ICommitLog, string>) =
            let commits = commits |> function
                Ok value -> value
                | Error msg -> failwith msg
            let tag,semver = snd tags
            [
                makeTagLink (tag,semver) $"{githubUrl}/compare/{fst tags |> fst |> _.FriendlyName}...{tag.FriendlyName}"
                yield! renderCommits commits
            ]
        let renderForLastTag (scope: string voption) (tags: TagPairs) (commits: Result<ICommitLog, string>) =
            let commits = commits |> function
                | Ok value -> value
                | Error msg -> failwith msg
            let tag,_ = fst tags
            [
                "<h2><a href=\""
                + $"{githubUrl}/compare/{tag.FriendlyName}...HEAD"
                + "\">UNRELEASED</a></h2>"
                |> Md.rawHtml
                
                yield! renderCommits commits
            ]
        let renderForGroup (group: IGrouping<string voption,TagPairs * Result<ICommitLog, string>>) =
            [
                let head = group |> Seq.head
                let tail = group |> Seq.last
                let groups =
                    group |> Seq.skip 1
                    |> Seq.rev |> Seq.skip 1
                tail ||> renderForLastTag group.Key
                yield!
                    (groups
                     |> Seq.map (fun grouping -> grouping ||> renderForTag group.Key))
                head ||> renderForFirstTag group.Key
            ]
        query {
            for (_scope,_tags,_commits) in commitCollection do
            where _commits.IsOk
            groupValBy (_tags, (_commits)) _scope
        }
        |> Seq.map renderForGroup
        |> Seq.rev |> Seq.collect (Seq.collect id) |> Seq.toList
        |> List.append makeHeader
        |> fun l -> MarkdownDocument(l @ makeFooter,links) |> Markdown.ToMd |> File.writeString false "TEST.md"
    ()
