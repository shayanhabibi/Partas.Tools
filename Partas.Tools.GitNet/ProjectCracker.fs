module Partas.Tools.GitNet.ProjectCracker

open System.IO
open System.Xml.XPath
open Partas.Tools.GitNet.Types
open FsToolkit.ErrorHandling
// Globs the working directory for a repo to find all projects within
open LibGit2Sharp
open LibGit2Sharp.FSharp
open Fake.Core
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.DotNet

type CrackedProject = {
    ProjectDirectory: string
    ProjectFileName: string
    SourceFiles: string list
    Scope: string voption
    Epoch: string voption
}

let getRepository path =
    let inline (!?) func target = if func target then ValueSome target else ValueNone
    Repository.discover path
    |> !? Repository.isValid
    |> ValueOption.map Repository.load
    
let private loadRepo (config: GitNetConfig) = config.RepositoryPath |> getRepository

let crackRepository (config: GitNetConfig) =
    voption {
        use! repo = loadRepo config
        let getWorkPath = Repository.info >> RepositoryInformation.workingDirectory
        let workDir = repo |> getWorkPath
        return
            !! $"{workDir |> String.trimSlash}/**/*.fsproj"
            |> Seq.map (fun path ->
                let absDir = Path.getDirectory path
                let relativeToRepo =
                    Path.combine absDir
                    >> fun p -> Path.GetRelativePath(workDir, p)
                let dir = Path.GetRelativePath(workDir, absDir)
                let file = Path.GetFileName path |> relativeToRepo
                let project = MSBuild.loadProject path
                let sourceFiles =
                    project
                        .Document
                        .XPathSelectElements("//Compile[@Include]")
                    |> Seq.map (_.XPathEvaluate("string(@Include)") >> unbox<string> >> relativeToRepo)
                let scope =
                    project
                        .Document
                        .XPathSelectElement("//GitNetScope")
                    |> ValueOption.ofNull
                    |> ValueOption.map _.Value
                let epoch =
                    project
                        .Document
                        .XPathSelectElement("//GitNetEpoch")
                    |> ValueOption.ofNull
                    |> ValueOption.map _.Value
                {
                    ProjectDirectory = dir
                    ProjectFileName = file
                    SourceFiles = sourceFiles |> Seq.toList
                    Scope = scope
                    CrackedProject.Epoch = epoch
                }
            )
    }
