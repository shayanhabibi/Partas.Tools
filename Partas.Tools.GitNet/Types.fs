module Partas.Tools.GitNet.Types

/// <summary>
/// Configures the behaviour for parsing Sepochs in GitNet
/// </summary>
type SepochConfig = {
    IncludeScopes: string list
    ExcludeScopes: string list
    IncludeEpochs: string list
    ExcludeEpochs: string list
}
/// <summary>
/// Configures the behaviour for parsing triggers for bumps in GitNet
/// </summary>
type SemverConfig = {
    /// <summary>
    /// Types that would trigger a Patch bump.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "fix" ]</c>
    /// </remarks>
    BumpPatchValues: string list
    /// <summary>
    /// Types that would trigger a Minor bump.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "add"; "update"; "deprecate" ]</c>
    /// </remarks>
    BumpMinorValues: string list
    /// <summary>
    /// Types that would trigger a MAJOR bump. The use of <c>!</c> and
    /// the footer <c>BREAKING CHANGE</c> would still trigger MAJOR bumps
    /// as per Conventional Commits spec.
    /// </summary>
    /// <remarks>
    /// By default: <c>[ "feat" ]</c>
    /// </remarks>
    BumpMajorValues: string list
    /// <summary>
    /// The Footer key to match for Epoch bumps. The new epoch would be the
    /// value to that key.
    /// </summary>
    /// <remarks>
    /// <para>By default: <c>[ EPOCH ]</c></para>
    /// <para>Note: You must ensure values are valid Footer keys as per
    /// the Conventional Commits spec.</para>
    /// </remarks>
    BumpEpochValues: string list
}

type AutoScopeType =
    | NoAutoScoping
    | Default
    | Transform of transformer: (string -> string)

type ProjectConfig = {
    AutoScope: AutoScopeType
    UseEpochs: bool
    UseScopes: bool
    UpdateAssemblyFiles: bool
}

/// <summary>
/// Configures the behaviour for parsing commits and tags in GitNet
/// </summary>
type GitNetConfig = {
    SepochConfig: SepochConfig
    SemverConfig: SemverConfig
    RepositoryPath: string
    ProjectConfig: ProjectConfig
}

module SepochConfig =
    let init = {
        SepochConfig.ExcludeEpochs = []
        IncludeEpochs = []
        IncludeScopes = []
        ExcludeScopes = []
    }
module SemverConfig =
    let init = {
        SemverConfig.BumpEpochValues = [ "EPOCH" ]
        BumpPatchValues = [ "fix" ]
        BumpMinorValues = [ "add"; "update"; "deprecate" ]
        BumpMajorValues = [ "feat" ]
    }
module ProjectConfig =
    let init = {
        ProjectConfig.AutoScope = AutoScopeType.NoAutoScoping
        UseEpochs = true
        UseScopes = true
        UpdateAssemblyFiles = true
    }
module GitNetConfig =
    let init = {
        GitNetConfig.ProjectConfig = ProjectConfig.init
        SepochConfig = SepochConfig.init
        SemverConfig = SemverConfig.init
        RepositoryPath = __SOURCE_DIRECTORY__
    }
    
