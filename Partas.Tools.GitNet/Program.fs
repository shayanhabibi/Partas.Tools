open System.Linq
open Partas.Tools.GitNet
open Partas.Tools.GitNet.GitHubLinkParser
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.RepoCracker
open Partas.Tools.GitNet.Types
open Partas.Tools.SepochSemver
open LibGit2Sharp
open LibGit2Sharp.FSharp
open FSharp.Linq



open GitCollection
[<EntryPoint>]
let main args =
    // let path = @"C:\Users\shaya\RiderProjects\Partas.Fake.Tools.GitCliff\"
    let path = @"C:\Users\shaya\RiderProjects\Partas.Solid.Plugin\"
    let repo = Repository.load path

    {
        GitNetConfig.init with
            RepositoryPath = path
            ProjectConfig =
                {
                    ProjectConfig.init with
                        // AutoScope = AutoScopeType.NoScoping
                        AutoScope = AutoScopeType.Transform (
                            _.Split('.').Last() >> ValueSome
                            )
                }
    }
    |> fun config ->
        // computeGitNetCollections(config).Collection
        TagCommitCollection.load config
        |> TagCommitCollection.collectScopes repo
        |> Seq.iter (printfn "%A")
    repo
    |> Repository.tags
    |> Seq.map _.FriendlyName
    |> printfn "%A"
        
    0
