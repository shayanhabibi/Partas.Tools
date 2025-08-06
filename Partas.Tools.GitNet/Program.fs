open System.Linq
open Partas.Tools.GitNet.Action
open Partas.Tools.GitNet.GitHubLinkParser
open Partas.Tools.GitNet.GitTraversal
open Partas.Tools.GitNet.RepoCracker
open Partas.Tools.GitNet.Runner
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
    let path = @"C:\Users\shaya\RiderProjects\Partas.Fake.Tools.GitCliff\"
    // let path = @"C:\Users\shaya\RiderProjects\Partas.Solid.Plugin\"
    let repo = Repository.load path
    // let commit = repo |> Repository.commits |> Seq.skip 12 |> Seq.take 2 |> Seq.toList
    // commit |> function
    //     | [ i1; i2 ] ->
    //         repo |> Repository.diff
    //         |> _.Compare<TreeChanges>(i2.Tree, DiffTargets.Index, [ "Partas.Solid" ])
    //         |> _.Count |> printfn "%A"
    //         // |> _.Modified
    //         // |> Seq.iter ( _.OldPath >> printfn "%A")
    //     | _ -> ()
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
        GitNetRuntime(config)
        |> Seq.iter (printfn "%A")
    repo
    |> Repository.tags
    |> Seq.map _.FriendlyName
    |> printfn "%A"
        
                    
        
    0
