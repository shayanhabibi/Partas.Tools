open System.Linq
open Partas.Tools.GitNet.GitHubLinkParser
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.RepoCracker
open Partas.Tools.GitNet.GitCollector
open Partas.Tools.GitNet.Types
open Partas.Tools.SepochSemver
open LibGit2Sharp
open LibGit2Sharp.FSharp
open FSharp.Linq




[<EntryPoint>]
let main args =
    let path = @"/Users/leonielott/RiderProjects/Partas.Solid/"
    // let path = @"C:\Users\shaya\RiderProjects\Partas.Fake.Tools.GitCliff\"
    // let path = @"C:\Users\shaya\RiderProjects\Partas.Solid.Plugin\"
    let repo = Repository.load path

    {
        GitNetConfig.init with
            RepositoryPath = path
            ProjectConfig =
                {
                    ProjectConfig.init with
                        // AutoScope = AutoScopeType.NoScoping
                        AutoScope = AutoScopeType.Transform (fun inp ->
                            inp.Split('.').Last())
                }
    } |> fun config ->
        computeGitNetCollections(config).Collection
        |> Seq.iter (printfn "%A")
    repo
    |> Repository.tags
    |> Seq.map _.FriendlyName
    |> printfn "%A"
        
    0
