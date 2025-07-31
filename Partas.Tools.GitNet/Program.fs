open System.Linq
open Partas.Tools.GitNet.ProjectCracker
open Partas.Tools.GitNet.Types
open Partas.Tools.SepochSemver
open LibGit2Sharp
open LibGit2Sharp.FSharp
open FSharp.Linq

let getRepository path =
    let inline (!?) func target = if func target then ValueSome target else ValueNone
    Repository.discover path
    |> !? Repository.isValid
    |> ValueOption.map Repository.load
let tryParseSepochSemverFromTag (tag: Tag) =
    try
    tag
    |> Tag.name
    |> parseSepochSemver
    |> ValueSome
    with e -> ValueNone
let getTagCollection repo =
    let tags = Repository.tags >> Seq.choose (tryParseSepochSemverFromTag >> Option.ofValueOption)
    query {
        for tag in tags repo do
        sortBy tag
        groupBy tag.Sepoch.GetScope into group
        select group
    }

let getCommitsBetweenTags repo (sepoch1: SepochSemver) (sepoch2: SepochSemver) =
    Repository.commits repo
    |> CommitLog.Query.between (sepoch1.ToString()) (sepoch2.ToString())
let getCommitsToTag repo (sepoch: SepochSemver) =
    Repository.commits repo
    |> CommitLog.Query.between (sepoch.ToString()) "HEAD"

[<EntryPoint>]
let main args =
    let path = @"C:\Users\shaya\RiderProjects\Partas.Fake.Tools.GitCliff\"
    let repo = Repository.load path
    let tags =
        repo
        |> Repository.tags
        |> Seq.toList
        |> List.rev |> List.skip 1
    tags |> printfn "%A"
    let commitFilter =
        CommitFilter.Common.between
            (tags |> List.skip 1 |> List.head )
            (tags |> List.head)
    repo
    |> Repository.commits
    |> CommitLog.queryBy commitFilter
    |> Seq.map (Commit.prettifyMessage >> ConventionalCommits.parseCommit)
    |> Seq.length
    |> printfn "%A"
    
    repo
    |> Repository.commits |> CommitLog.Query.between "0.2.0" "HEAD"
    |> Seq.head |> printfn "%A"
    repo |> Repository.commits |> CommitLog.Query.between "0.0.1" (repo |> Repository.tags |> _.Item("0.2.0"))
    |> Seq.head |> Commit.message |> ConventionalCommits.parseCommit |> printfn "%A"
    repo |> Repository.tags |> _.Item("0.2.0")  |> printfn "%A"
    repo
    |> Repository.commits
    |> CommitLog.Query.since tags[1]
    |> Seq.length
    |> printfn "%A"
    "<bubbles> 1.3.0"
    |> parseSepochSemver
    |> printfn "%A"
    getTagCollection repo |> Seq.map Seq.toList |> Seq.toList |> printfn "%A"
    let tags = (getTagCollection repo)
    for groups in tags do
        groups
        |> Seq.toList
        |> function
            | value ->
                getCommitsToTag repo (value |> List.rev |> List.head)
                |> Seq.map (Commit.message >> ConventionalCommits.parseCommit)
                |> printfn "%A"
                getCommitsBetweenTags repo value[0] value[2]
        |> printfn "%A"
    repo |> Repository.info |> RepositoryInformation.workingDirectory |> printfn "%s"
    {
        GitNetConfig.init with
            RepositoryPath = @"C:\Users\shaya\RiderProjects\Partas.Solid.Plugin\"
    } |> crackRepository
    |> printfn "%A"
    0
