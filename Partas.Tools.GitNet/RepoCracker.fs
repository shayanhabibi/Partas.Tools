module Partas.Tools.GitNet.RepoCracker

open System.IO
open System.Xml.XPath
open Partas.Tools.GitNet.Types
open FsToolkit.ErrorHandling
open LibGit2Sharp
open LibGit2Sharp.FSharp
open Fake.Core
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.DotNet

// Globs the working directory for a repo to find all projects within

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
    let tryGetTitle project: string voption =
        extractElementValue "Title" project
        |> ValueOption.orElse (extractElementValue "PackageId" project)

type CrackRepoResult = {
    RepoDirectory: string
    Projects: CrackedProject seq
}

let crackRepo (config: GitNetConfig) = voption {
    let! repoDir = Repo.getPath config
    let projectDirectory = Path.getDirectory
    let relativeToRepoFromProject projectDir projectRelativePath =
        let absolutePath = Path.combine projectDir projectRelativePath
        Path.GetRelativePath(repoDir, absolutePath)
    let makeCrackedProj (proj: MSBuildProject, path: string) =
        let projectDirectory = path |> projectDirectory
        let relativeToRepo = relativeToRepoFromProject projectDirectory
        let sourceFiles = proj |> Projects.getSourceFiles |> Seq.toList
        {
            CrackedProject.ProjectDirectory =
                projectDirectory
                |> relativeToRepo
                |> String.replace "\\" "/"
            ProjectFileName = path |> relativeToRepo
            SourceFiles = sourceFiles
            AssemblyFile =
                sourceFiles
                |> Seq.tryFind
                    (fun path ->
                    let path = Path.GetFileName path
                    path = "AssemblyFile.fs"
                    || path = "AssemblyInfo.fs"
                    )
                |> ValueOption.ofOption
            Scope =
                match config.ProjectConfig.AutoScope with
                | NoScoping -> ValueNone
                | Transform transformer ->
                    proj |> Projects.tryGetTitle
                    |> ValueOption.orElse (path |> Path.GetFileNameWithoutExtension |> ValueSome)
                    |> ValueOption.bind transformer
            Epoch = proj |> Projects.tryGetEpoch
        }
    return
        { RepoDirectory = repoDir
          Projects = Projects.findProjectsAndLoad repoDir
                     |> Seq.map makeCrackedProj }
}
