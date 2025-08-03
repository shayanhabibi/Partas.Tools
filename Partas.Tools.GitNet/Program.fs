open System.Linq
open Partas.Tools.GitNet.Action
open Partas.Tools.GitNet.GitHubLinkParser
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.RepoCracker
open Partas.Tools.GitNet.Types
open Partas.Tools.SepochSemver
open LibGit2Sharp
open LibGit2Sharp.FSharp
open FSharp.Linq


let treeFiles (tree: Tree) =
    let peelToTree = TreeEntry.target >> GitObject.unsafePeel<Tree>
    let rec folder = fun (state: TreeEntry list) (tree: TreeEntry) ->
        if tree |> TreeEntry.targetIsTree then
            peelToTree tree |> Seq.fold folder state
        else tree :: state
    tree |> Seq.fold folder []
    |> List.map _.Name

[<EntryPoint>]
let main args =
    let path = @"C:\Users\shaya\RiderProjects\Partas.Solid.Plugin\"
    let repo = Repository.load path
    gitNetWithConfig (fun config -> {
        config with
            RepositoryPath = path
            ProjectConfig =
            {
                ProjectConfig.init with
                    AutoScope = AutoScopeType.Transform (fun inp ->
                        inp.Split('.').Last()
                        )
            }
    })
    0
