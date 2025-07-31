namespace Partas.Tools.Fake

open System.Xml.Linq
open FSharp.Json.Json
open Fake.Core
open Fake.IO
open Fake.Tools.GitCliff
open Fake.Tools
open Fake.DotNet
open Fake.DotNet.Testing
#nowarn 3391
[<RequireQualifiedAccess>]
type PathType =
    | Absolute of string
    | Relative of string

module PathType =
    let createRelative = PathType.Relative
    let createAbsolute = PathType.Absolute
    /// <summary>
    /// Resolves a path with the given root if it is a relative path,
    /// otherwise returns the contained absolute path as an 'Error'
    /// </summary>
    /// <param name="path">The root path</param>
    let resolvePath path = function
        | PathType.Relative relPath ->
            Ok <| Path.combine path relPath
        | PathType.Absolute path ->
            Error path
    let forceResolvePath path = resolvePath path >> function
        | Ok value | Error value -> value

type Project = {
    Description: string
    Name: string
    GitOwner: string
    RootPath: string
    ReleaseNotesPath: PathType
    AssemblyPath: PathType
}
open Fake.IO.Globbing.Operators

type ProjectContainer (description: string, name: string, gitOwner: string, rootPath: string, releaseNotesPath: PathType, assemblyPath: PathType) =
    let releaseNotesPath = PathType.forceResolvePath rootPath releaseNotesPath
    let assemblyPath = PathType.forceResolvePath rootPath assemblyPath
    let releaseNotes = lazy ReleaseNotes.load releaseNotesPath
    let mutable fantomasGlob: IGlobbingPattern =
        !! "**/*.fs" ++ "**/*.fsx"
        -- "packages/**/*.*"
        -- "paket-files/**/*.*"
        -- ".fake/**/*.*"
        -- "**/obj/**/*.*"
        -- "**/AssemblyInfo.fs"
    
    member _.SetFantomasGlob value = fantomasGlob <- value
    member _.Format() =
        fantomasGlob
        |> Seq.map (sprintf "\"%s\"")
        |> String.concat " "
        |> DotNet.exec id "fantomas"
    member this.FormatOrFail() =
        this.Format() |> function
            | { ExitCode = 0 } ->
                Trace.log "Formatting completed."
            | result ->
                failwithf $"Failed to format %s{name}.\nError code: %i{result.ExitCode}\n Messages: %A{result.Messages @ result.Errors}"
    member this.CheckFormat() =
        fantomasGlob
        |> Seq.map (sprintf "\"%s\"")
        |> String.concat " "
        |> sprintf "%s --check"
        |> DotNet.exec id "fantomas"
    member this.CheckFormatAndLog() =
        match this.CheckFormat() with
        | { ExitCode = 0 } ->
            Trace.log "No files need formatting"
        | { ExitCode = 99 } ->
            Trace.traceImportant "Some files need formatting, run `dotnet fsi build.fsx target Format` to format them"
        | result ->
            Trace.traceErrorfn $"Errors while formatting: %A{result.Errors}"
    member this.CheckFormatOrFail() =
        match this.CheckFormat() with
        | { ExitCode = 0 } ->
            Trace.log "No files need formatting"
        | { ExitCode = 99 } ->
            failwith "Some files need formatting, run fantomas to format them."
        | result ->
            Trace.logf $"Errors while formatting: %A{result.Errors}"
            failwith "Unknown errors while formatting"
    member this.CreateAssemblyInfo(assemblyInfo: AssemblyInfo.Attribute list -> AssemblyInfo.Attribute list) =
        let hasAssemblyInfo = File.exists assemblyPath
        let attributes = assemblyInfo <| [
            AssemblyInfo.Title name
            AssemblyInfo.Product name
            AssemblyInfo.Version releaseNotes.Value.AssemblyVersion
            AssemblyInfo.FileVersion releaseNotes.Value.AssemblyVersion
        ]
        AssemblyInfoFile.createFSharp
            assemblyPath
            attributes
        
        let projPath =
            rootPath
            |> sprintf "%s.fsproj"
            |> Path.combine rootPath
        let addToMsBuild () =
            projPath
            |> MSBuild.loadProject
            |> fun project ->
                match project.Root with
                | xElement when xElement.Name.LocalName = "Project" ->
                    xElement.Descendants()
                    |> Seq.last
                    |> _.AddAfterSelf(
                        let parentName = "ItemGroup"
                        let childName = "Compile"
                        let childAttribute = "Include"
                        let childAttributeValue = "AssemblyInfo.fs"
                        let parentElement = XElement(parentName)
                        parentElement |> _.Add(
                                XElement(childName)
                                |> _.Add(
                                    XAttribute(childAttribute, childAttributeValue)
                                    )
                            )
                        parentElement
                        )
                    project.Save projPath
                    Git.Staging.stageFile "" assemblyPath
                    |> ignore
                | null ->
                    projPath
                    |> failwithf "While creating AssemblyInfo, tried and failed to find a valid project at `%s.fsproj`. The project does not contain a valid node."
                | e ->
                    (projPath, e.Name.LocalName )
                    ||> failwithf "While creating AssemblyInfo, tried and failed to find a valid project at `%s.fsproj`.\
                                    Instead of an initial `Project` node, found a node called %s"
        if hasAssemblyInfo then ()
        else addToMsBuild()
    member this.Build(buildOptions: DotNet.BuildOptions -> DotNet.BuildOptions) =
        let setDefaults: DotNet.BuildOptions -> DotNet.BuildOptions = fun p -> {
            p with
                Configuration = DotNet.BuildConfiguration.Release
                DotNet.BuildOptions.MSBuildParams.DisableInternalBinLog = true
                DotNet.BuildOptions.MSBuildParams.Properties = [
                    "PackageVersion", releaseNotes.Value.AssemblyVersion
                    "Author", gitOwner
                    "Version", releaseNotes.Value.AssemblyVersion
                    "Description", description
                ]
        }
        name
        |> sprintf "%s.fsproj"
        |> Path.combine rootPath
        |> DotNet.build (setDefaults >> buildOptions)
    member this.Pack(packOptions: DotNet.PackOptions -> DotNet.PackOptions) =
        let defaultOptions: DotNet.PackOptions -> _ = fun p -> {
            p with
                NoRestore = true
                OutputPath = Some "bin"
                DotNet.PackOptions.MSBuildParams.DisableInternalBinLog = true
                DotNet.PackOptions.MSBuildParams.Properties = [
                    "PackageVersion", releaseNotes.Value.AssemblyVersion
                    "Author", gitOwner
                    "Version", releaseNotes.Value.AssemblyVersion
                    "Description", description
                ]
        }
        name
        |> sprintf "%s.fsproj"
        |> Path.combine rootPath
        |> DotNet.pack (defaultOptions >> packOptions)
    member this.Publish(publishOptions: DotNet.NuGetPushOptions -> DotNet.NuGetPushOptions) =
        let defaultOptions: DotNet.NuGetPushOptions -> _ = fun p -> {
            p with
                DotNet.NuGetPushOptions.PushParams.Source = Some "https://api.nuget.org/v3/index.json"
        }
        !! "bin/*.nupkg"
        |> Seq.iter (
            DotNet.nugetPush (defaultOptions >> publishOptions)
            )
    member this.CreateGitCliffConfig(options: ConfigHelper.Config -> ConfigHelper.Config ) =
        let defaultOptions: ConfigHelper.Config -> _ = fun p -> {
            p with
                Changelog.RenderAlways = Some true
                Git.IncludePaths = [| rootPath |]
                Changelog.Output = releaseNotesPath
                Git.CommitParsers = p.Git.CommitParsers |> Array.append [|
                    ConfigHelper.CommitParser.Create(message = "^\[skip ci\]", skip = true)
                |]
                Changelog.Body = "{%- macro remote_url() -%}
  https://github.com/{{ remote.github.owner }}/{{ remote.github.repo }}
{%- endmacro -%}

{% if version -%}
    ## [{{ version | trim_start_matches(pat=\"v\") }}] - {{ timestamp | date(format=\"%Y-%m-%d\") }}
{% else -%}
    <h2><a href=\"{{ self::remote_url() }}/compare/{{ release.previous.version }}..HEAD\" >Unreleased</a></h2>
{% endif -%}

{% for group, commits in commits | group_by(attribute=\"group\") %}
    <h3>{{ group | upper_first }}</h3>
    {%- for commit in commits %}
        - {{ commit.message | split(pat=\"\n\") | first | upper_first | trim }}\
            {% if commit.remote.username %} by @{{ commit.remote.username }}{%- endif -%}
            {% if commit.remote.pr_number %} in \
            [#{{ commit.remote.pr_number }}]({{ self::remote_url() }}/pull/{{ commit.remote.pr_number }}) \
            {%- endif -%}
    {% endfor %}
{% endfor %}

{%- if github.contributors | filter(attribute=\"is_first_time\", value=true) | length != 0 %}
  <h2>New Contributors</h2>
{%- endif -%}

{% for contributor in github.contributors | filter(attribute=\"is_first_time\", value=true) %}
  * @{{ contributor.username }} made their first contribution
    {%- if contributor.pr_number %} in \
      [#{{ contributor.pr_number }}]({{ self::remote_url() }}/pull/{{ contributor.pr_number }}) \
    {%- endif %}
{%- endfor %}\n

"
                Changelog.Footer = "
{%- macro remote_url() -%}
  https://github.com/{{ remote.github.owner }}/{{ remote.github.repo }}
{%- endmacro -%}
\n
{% for release in releases -%}
    {% if release.version -%}
        {% if release.previous.version -%}
            [{{ release.version | trim_start_matches(pat=\"v\") }}]: \
                {{ self::remote_url() }}/compare/{{ release.previous.version }}..{{ release.version }}
        {% endif -%}
    {% endif -%}
{% endfor %}
<!-- generated by git-cliff -->
<!-- using Partas.Fake.Tools.GitCliff -->"
                
        }
        ConfigHelper.writeConfiguration
            (fun _ -> ConfigHelper.Config.Default |> defaultOptions |> options)
            (Path.combine rootPath "cliff.toml")
    member this.RunGitCliff() =
        run id rootPath
        Git.Staging.stageFile "" releaseNotesPath |> ignore
    member this.RunGitCliffWithContext(contextModifiers: GitCliffContext.JsonContent -> GitCliffContext.JsonContent) =
        let contextPath = Path.combine rootPath "context.json"
        let runGenContext = fun cli -> {
            cli with
                CliParams.IncludePath = rootPath
                Output = contextPath
                Flags = [ CliFlags.Context ]
        }
        let runLoadContext () = File.readAsString contextPath|> GitCliffContext.Json.deserialize
        let saveContext = GitCliffContext.Json.serialize >> File.writeString false contextPath
        let runWriteContext = fun cli -> {
            cli with
                CliParams.Output = releaseNotesPath
                FromContext = contextPath
        }
        
        run runGenContext ""
        |> runLoadContext
        |> contextModifiers
        |> saveContext
        run runWriteContext ""
        Git.Staging.stageFile "" releaseNotesPath |> ignore
        File.delete contextPath

    new(project: Project) = ProjectContainer(project.Description, project.Name, project.GitOwner, project.RootPath, project.ReleaseNotesPath, project.AssemblyPath)

module Helpers =
    let setupStandardFsxEnvironment () =
        System.Environment.GetCommandLineArgs()
        |> Array.skip 2
        |> Array.toList
        |> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
        |> Fake.Core.Context.RuntimeContext.Fake
        |> Fake.Core.Context.setExecutionContext        
    let setGitBotUser () =
        [ "config --local user.email \"41898282+github-actions[bot]@users.noreply.github.com\""
          "config --local user.name \"GitHub Action\"" ]
        |> List.iter (Git.CommandHelper.directRunGitCommandAndFail "")
    let private defaultExpectoTestMods: Expecto.Params -> _ = fun p ->
        { p with
            Summary = true
            CustomArgs = "--colours 256" :: p.CustomArgs }
    let defaultExpectoTests () =
        !! "**/bin/**/*.Tests.dll"
        |> Expecto.run defaultExpectoTestMods
    let cleanBins () = !! "**/**/bin" |> Shell.cleanDirs
    let clean folders =
        folders |> Shell.cleanDirs
    let defaultClean () =
        [ "bin"; "temp" ] |> clean |> cleanBins
    let restoreTools () =
        DotNet.exec id "tool" "restore"
        |> function
            | result when result.OK ->
                Trace.log "Local tools restored."
            | result ->
                result.Errors
                |> failwith "Failed to restore dotnet tools: %A" 

type Project with
    member inline this.Finalize() = ProjectContainer(this)
    
module Project =
    let create name rootPath = {
        Description = ""
        Name = name
        GitOwner = ""
        RootPath = rootPath
        ReleaseNotesPath = PathType.Relative "RELEASE_NOTES.md"
        AssemblyPath = PathType.Relative "AssemblyInfo.fs"
    }
    let createSimple name = create name $"{name}{Path.directorySeparator}"
    let createSimpleSrc name = Path.combine "src" name |> create name
    let withDescription description project = { project with Project.Description = description }
    let withName name project = { project with Project.Name = name }
    let withGitOwner owner project = { project with Project.GitOwner = owner }
    let withRootPath path project = { project with Project.RootPath = path }
    let withReleaseNotesPath path project = { project with Project.ReleaseNotesPath = path }
    let withRelativeReleaseNotesPath path = withReleaseNotesPath (PathType.Relative path)
    let withAbsoluteReleaseNotesPath path = withReleaseNotesPath (PathType.Absolute path)
    let withAssemblyPath path project = { project with Project.AssemblyPath = path }
    let withRelativeAssemblyPath path = PathType.Relative path |> withAssemblyPath
    let withAbsoluteAssemblyPath path = PathType.Absolute path |> withAssemblyPath
    let finalise (project: Project) = project.Finalize()
    
    module Simple =
        let private apply func (project: ProjectContainer) = func project; project
        let build: ProjectContainer -> _ = apply _.Build(id)
        let pack: ProjectContainer -> _  = apply _.Pack(id)
        let publish: ProjectContainer -> _  = apply _.Publish(id)
        let checkFormatAndFail: ProjectContainer -> _  = apply _.CheckFormatOrFail()
        let checkFormat: ProjectContainer -> _  = apply _.CheckFormatAndLog()
        let format: ProjectContainer -> _  = apply _.FormatOrFail()
        let createAssemblyInfo: ProjectContainer -> _ = apply _.CreateAssemblyInfo(id)
        let createGitCliffConfig: ProjectContainer -> _ = apply _.CreateGitCliffConfig(id)
        let runGitCliff: ProjectContainer -> _ = apply _.RunGitCliff()
        let runGitCliffWithContext contextModifier: ProjectContainer -> _ = apply _.RunGitCliffWithContext(contextModifier)
    
    module Preconfigured =
        module private Targets =
            let prelude = "partas-prelude"
            let clean = "partas-clean"
            let init = "partas-init"
            let gitcliff = "partas-gitcliff"
            let build = "partas-build"
            let pack = "partas-pack"
            let publish = "partas-publish"
            let gitPush = "partas-git-push"
            let format = "partas-format"
            let checkFormat = "partas-check"
            let expecto = "partas-test"
        type TargetTuple<'T> = TargetTuple of name: string * funcToInit: ('T -> unit) with
            static member inline op_Implicit((name: string, funcToInit: 'T -> unit)): TargetTuple<'T> = TargetTuple(name, funcToInit)
            member this.Destructure() = let (TargetTuple (name, func)) = this in name,func
            member this.Target = this.Destructure() |> fst
            member this.TargetEnabler = this.Destructure() |> snd
        type PartasTargets = {
            prelude: TargetTuple<unit>
            clean: TargetTuple<unit>
            init: TargetTuple<unit>
            gitcliff: TargetTuple<ProjectContainer>
            build: TargetTuple<ProjectContainer>
            pack: TargetTuple<ProjectContainer>
            publish: TargetTuple<ProjectContainer>
            gitPush: TargetTuple<unit>
            format: TargetTuple<ProjectContainer>
            checkFormat: TargetTuple<ProjectContainer>
            expecto: TargetTuple<unit>
        }
        let private targets = {
            prelude = Targets.prelude, fun () ->
                Target.create Targets.prelude (fun args ->
                    if
                        args.Context.Arguments
                        |> List.contains "--local" 
                        |> not
                    then
                        Helpers.setGitBotUser()
                    )
            clean = Targets.clean, fun () ->
                Target.create Targets.clean (fun _ ->
                    Helpers.defaultClean()
                    )
            init = Targets.init, fun () ->
                Target.create Targets.init (fun _ ->
                    Helpers.restoreTools()
                    )
            gitcliff = Targets.gitcliff, fun (project: ProjectContainer) ->
                Target.create Targets.gitcliff (fun _ ->
                    Simple.createGitCliffConfig project
                    |> _.RunGitCliff()
                    )
            build = Targets.build, fun project ->
                Target.create Targets.build (fun ctx ->
                    if
                        ctx.Context.PreviousTargets
                        |> List.tryFind (fun target ->
                            [
                                "gitcliff"
                                "git-cliff"
                                "cliff"
                                Targets.gitcliff
                            ] |> List.contains target.Target.Name
                            )
                        |> Option.exists _.Error.IsNone
                    then Simple.createAssemblyInfo project
                    else project
                    |> Simple.build
                    |> ignore
                    )
            pack = Targets.pack, fun (project: ProjectContainer) ->
                Target.create Targets.pack (fun _ ->
                    Simple.pack project |> ignore
                    )
            publish = Targets.publish, fun (project: ProjectContainer) ->
                Target.create Targets.publish (fun args ->
                    if
                        args.Context.Arguments
                        |> List.contains "--local"
                        |> not
                    then
                        Simple.publish project
                        |> ignore
                    else Trace.log "--local: Skipping publish"
                )
                
            gitPush = Targets.gitPush, fun () ->
                Target.create Targets.gitPush (fun args ->
                    if
                        args.Context.Arguments
                        |> List.contains "--local"
                        |> not
                    then
                        Git.Branches.push ""
                    else Trace.log "--local: Skipping push"
                    )
            format = Targets.format, fun (project: ProjectContainer) ->
                Target.create Targets.format (fun _ ->
                    Simple.format project
                    |> ignore
                    )
            checkFormat = Targets.checkFormat, fun (project: ProjectContainer) ->
                Target.create Targets.checkFormat (fun args ->
                    if
                        args.Context.Arguments
                        |> List.contains "--fantomas-mercy"
                    then Simple.checkFormat project
                    else Simple.checkFormatAndFail project
                    |> ignore
                    )            
            expecto = Targets.expecto, fun () ->
                Target.create Targets.expecto (fun _ ->
                    Helpers.defaultExpectoTests()
                    )
        }
