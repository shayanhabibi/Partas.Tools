module Partas.Tools.GitNet.GitHubLinkParser

// Autolinked references on github follow several forms.
// Issues and pull requests:
// #26 : https://github.com/author/repo/issues/26
// #26 : #26
// GH-26 : GH-26
// author/repository#26 : author/repository#26
// Labels (only works for repositories of the same repo):
// https://github.com/author/repo/labels/labelname
// Commit SHAs
// a5c3785 - https://github.com/author/repo/commit/a5c3785ed8d6a35868bc169f07e40e889087fd2e
// author@a5c3785
// username/repo@a5c3785

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Combinators

type IPullIssueTracker =
    abstract AddLink: position: int64 * link: string -> IPullIssueTracker
    abstract GetLinks: (int64 * string) list

let pPullIssue =
    let openDelimiter = pchar '#'
    let pLink = manyChars digit .>> notFollowedBy ((noneOf " \n\r\n" >>. preturn ()) <|> skipMany1 eof)
    parser {
        let! _ = openDelimiter
        let! linkVal = pLink
        let result = $"#{linkVal}"
        let! (state: IPullIssueTracker) = getUserState
        let! pos = getPosition
        let (newState: IPullIssueTracker) = state.AddLink(pos.Index, result)
        do! setUserState newState
        return result
    }
    
    

let examplePullIssue () =
    let rec makeState accumulator: IPullIssueTracker = {
        new IPullIssueTracker with
            member this.GetLinks = accumulator
            member this.AddLink(pos,link) =
                (pos, link) :: this.GetLinks
                |> makeState
    }
    makeState []
    |> Reader.ofString " asdf fdsf #26 fdf as"
    |> (manyChars ( pPullIssue >>. anyChar <|> anyChar ) .>>. getUserState)
    |> function
        | Ok { Parsed = value,i } ->
            value,i.GetLinks
        | Error value ->
            printfn $"%A{value}"
            "",[]
    
    
    
