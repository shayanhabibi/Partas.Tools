module Partas.Tools.GitNet.RepoCracker

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

module private Repo =
    let private getRepository path =
        let inline (!?) func target = if func target then ValueSome target else ValueNone
        Repository.discover path
        |> !? Repository.isValid
        |> ValueOption.map Repository.load
        
    let private getWorkDir = Repository.info >> RepositoryInformation.workingDirectory
    let private getRepoFromConfig (config: GitNetConfig) = config.RepositoryPath |> getRepository
    let getPath =
        getRepoFromConfig
        >> ValueOption.bind (fun repo ->
            let result = getWorkDir repo
            repo |> Repository.dispose
            result
            )

module private Projects =
    let private findFsProjs rootDir =
        !! $"{rootDir |> String.trimSlash}/**/*.fsproj"
    let private extractElementsValues elementName: MSBuildProject -> string seq =
        _.Document
            .XPathSelectElements($"//{elementName}")
        >> Seq.map _.Value
    let private extractElementValue elementName: MSBuildProject -> string voption =
        _.Document
            .XPathSelectElement($"//{elementName}")
        >> ValueOption.ofObj
        >> ValueOption.map _.Value
    let private extractElementsAttributes elementName attributeName: MSBuildProject -> string seq =
        _.Document
            .XPathSelectElements($"//{elementName}[@{attributeName}]")
        >> Seq.map (_.XPathEvaluate($"string(@{attributeName})") >> unbox<string>)
    let private findProjects = findFsProjs >> seq
    let private loadProject = MSBuild.loadProject
    let findProjectsAndLoad =
        let makeResult path = loadProject path,path
        findProjects >> Seq.map makeResult
    let getSourceFiles: MSBuildProject -> string seq = extractElementsAttributes "Compile" "Include"
    
    let tryGetScope: MSBuildProject -> string voption = extractElementValue "GitNetScope"
    let tryGetEpoch: MSBuildProject -> string voption = extractElementValue "GitNetEpoch"

let crackRepo (config: GitNetConfig) = voption {
    let! repoDir = Repo.getPath config
    let projectDirectory = Path.getDirectory
    let relativeToRepoFromProject projectDir projectRelativePath =
        let absolutePath = Path.combine projectDir projectRelativePath
        Path.GetRelativePath(repoDir, absolutePath)
    let makeCrackedProj (proj: MSBuildProject, path: string) =
        let projectDirectory = path |> projectDirectory
        let relativeToRepo = relativeToRepoFromProject projectDirectory
        {
            CrackedProject.ProjectDirectory = projectDirectory |> relativeToRepo
            ProjectFileName = path |> relativeToRepo
            SourceFiles =
                proj
                |> Projects.getSourceFiles
                |> Seq.toList
            Scope = proj |> Projects.tryGetScope
            Epoch = proj |> Projects.tryGetEpoch
        }
    return
        repoDir,
        Projects.findProjectsAndLoad repoDir
        |> Seq.map makeCrackedProj
}
