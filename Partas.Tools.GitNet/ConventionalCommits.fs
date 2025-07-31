module ConventionalCommits

open XParsec
open XParsec.CharParsers
open XParsec.Parsers
type Footer =
    | Footer of key: string * value: string
    | BreakingChange of value: string
type ConventionalCommit = {
    Type: string
    Scope: string voption
    Subject: string
    Message: string voption
    Footers: Footer list
}
type ParsedCommit =
    | Conventional of ConventionalCommit
    | Breaking of ConventionalCommit
    | Unconventional of string
module private Footer =
    let create key value =
        match key with
        | "BREAKING CHANGE" | "BREAKING-CHANGE" ->
            BreakingChange value
        | key -> Footer(key,value)
module private ConventionalCommit =
    let create typ = {
        Type = typ
        Subject = ""
        Scope = ValueNone
        Message =  ValueNone
        Footers = []
    }
    let withScope scope commit = { commit with Scope = scope }
    let withSubject subject commit = { commit with Subject = subject }
    let withMessage (message: string voption) commit = { commit with Message = message |> ValueOption.map _.Trim() }
    let addFooter footer commit = { commit with Footers = footer :: commit.Footers }
    let addFooters footers commit = { commit with Footers = footers @ commit.Footers }
    let setFooters footers commit = { commit with Footers = footers }
module private ParseResult =
    let map func result =
        result
        |> Result.bind (function
            | { Parsed = value } -> func value |> Ok
            )
open FsToolkit.ErrorHandling
let private ws = skipMany (pchar ' ')
let private untilScopeOrTypeEnd =  lookAhead (skipAnyOf ":(")
let private checkBreaking input =
    input |> skipChar '!'
    |> function
        | Ok _ as value ->
            input
            |> setUserState true
            |> ignore
            value
        | value -> value
        
let private typeEnd = ws >>. optional checkBreaking >>.  skipChar ':'
let private pType = manyCharsTill (noneOf " ") untilScopeOrTypeEnd
let private pScope =
    let scope = skipItem '(' >>. manyCharsTill anyChar (skipItem ')')
    ws >>. opt scope .>> typeEnd
let private pSubject = ws >>. manyCharsTill anyChar (skipNewline <|> eof)
let private breakingChange input =
    input |> (pstring "BREAKING CHANGE" <|> pstring "BREAKING-CHANGE")
    |> function
        | Ok _ as value ->
            input
            |> setUserState true
            |> ignore
            value
        | value -> value
let private footerKey =
    let key = breakingChange <|> (many1Chars (noneOf ": \n\r\n"))
    skipNewline >>. skipNewline >>. key .>> typeEnd
let private untilFooter = (lookAhead footerKey) <|> (eof >>. preturn "")
let private footerValue = ws >>. manyCharsTill anyChar untilFooter
let private pFooter = footerKey .>>. footerValue
let private pFooters = many pFooter
let private pMessage = skipNewline >>. manyCharsTill anyChar untilFooter
let parseCommit input =
    let reader = Reader.ofString input false
    
    let commit = result {
        let! { Parsed = typ, _} =
            reader
            |> pType
        let! scope =
            reader
            |> pScope
            |> ParseResult.map (ValueOption.map fst)
        let! { Parsed = subject, _} =
            reader
            |> pSubject
        let message =
            reader
            |> pMessage
            |> ParseResult.map fst
            |> Result.toValueOption
        let footers =
            reader
            |> pFooters
            |> ParseResult.map (Seq.map (fun struct (key,(value,_)) -> Footer.create key value))
            |> Result.map Seq.toList
            |> Result.toList
            |> List.concat
        let res =
            ConventionalCommit.create typ
            |> ConventionalCommit.withScope scope
            |> ConventionalCommit.withSubject subject
            |> ConventionalCommit.withMessage message
            |> ConventionalCommit.setFooters footers
        return res
    }
    match commit,reader.AtEnd with
    | Ok value, true when value.Footers |> List.exists _.IsBreakingChange ->
        Breaking value
    | Ok value, true when reader.State ->
        Breaking value
    | Ok value, true ->
        Conventional value
    | _ -> Unconventional input
