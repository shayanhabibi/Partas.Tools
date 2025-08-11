module Partas.Tools.GitNet.GitHub

open Fake.Core
open FSharp.Core
open LibGit2Sharp.FSharp

[<Struct>]
type GitHubRemote = GitHubRemote of LibGit2Sharp.Remote with
    member inline this.Value = let (GitHubRemote value) = this in value

/// Finds a github remote in a repository if it exists
let find =
    Repository.network
    >> Network.remotes
    >> Seq.tryFind(Remote.url >> _.Contains("github.com"))
    >> ValueOption.ofOption
    >> ValueOption.map GitHubRemote
/// Finds a github remote in a repository or fails
let unsafeFind =
    Repository.network
    >> Network.remotes
    >> Seq.find(Remote.url >> _.Contains("github.com"))
/// Extracts the url root from a github remote
let urlRoot = function
    GitHubRemote remote ->
        if remote.Url |> String.endsWith ".git"
        then remote.Url[..remote.Url.Length - 5]
        else remote.Url

module Commit =
    /// Creates a url fragment for a commit
    let urlFragment: LibGit2Sharp.Commit -> string =
        Commit.sha >> _.Substring(0,7)
    /// Alias for `Commit.sha`
    let urlFull = Commit.sha
module Tag =
    /// Creates a url fragment for a tag.
    /// Alias for `Tag.name`
    let urlFragment = Tag.name
    /// Same as urlFragment.
    /// Alias for `Tag.name`
    let urlFull = urlFragment
module Url =
    /// <summary>
    /// Makes a comparison url query link.
    /// </summary>
    /// <param name="urlRoot"></param>
    /// <param name="fragment1"></param>
    /// <param name="fragment2"></param>
    let makeCompare urlRoot fragment1 fragment2 =
        $"%s{urlRoot}/compare/%s{fragment1}...%s{fragment2}"
    let makeCommit urlRoot =
        Commit.urlFull
        >> sprintf "%s/commit/%s" urlRoot
    let makeTag urlRoot tag =
        makeCompare
            urlRoot
            (Tag.urlFragment tag)
            "HEAD"
