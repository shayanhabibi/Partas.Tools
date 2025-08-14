[<AutoOpen>]
module Partas.Tools.ConventionalCommits.AutoOpenParser

open System
open System.Collections.Immutable
open XParsec
open XParsec.CharParsers
open XParsec.Combinators
open XParsec.Parsers

let private makeFooter struct (key, value) =
    match key with
    | "BREAKING-CHANGE" | "BREAKING CHANGE" ->
        Footer.BreakingChange(value)
    | _ -> Footer.Footer(key,value)

module ErrorFormatting =
    open System.Text
    open ErrorFormatting
    let formatStringError (input: string) (error: ParseError<char, _>) =
        let index = LineIndex.OfString input
        let readable = ReadableString input

        let formatOne (x: char) (sb: StringBuilder) = sb.Append(''').Append(x).Append(''')

        let formatSeq (xs: char seq) (sb: StringBuilder) =
            sb.Append('"') |> ignore
            (sb, xs) ||> Seq.fold (fun sb x -> sb.Append x) |> ignore
            sb.Append('"')


        StringBuilder()
        |> formatErrorsLine index readable error.Position.Index
        |> formatParseError formatOne formatSeq error
        |> _.ToString()

/// skip white string: spaces, new lines, returns etc
let ws = skipMany (anyOf ([ ' '; '\n'; '\r'; '\t' ]))
/// Parses a footer key
let private pFooterKey =
    let pFooterKeyBreakingChange = pstring "BREAKING CHANGE"
    newline >>.
    (pstring "BREAKING-CHANGE" .>> setUserState true) <|> (
        (many1Chars2 asciiLetter (asciiLetter <|> digit <|> pchar '-')
         |>> _.ToLower()) <|> (pFooterKeyBreakingChange .>> setUserState true)
    )
    .>> (
       ( satisfyL (function ':' -> true | _ -> false) "Footers keys must be followed by \": \" or \" #\" "
        >>. satisfyL (function ' ' -> true | _ -> false) "Footers keys must be followed by \": \" or \" #\" " )
       <|> (satisfyL (function ' ' -> true | _ -> false) "Footers keys must be followed by \": \" or \" #\" "
            >>. satisfyL (function '#' -> true | _ -> false) "Footers keys must be followed by \": \" or \" #\" ")
        )
/// Parses a footer value
let private pFooterValue =
    ws >>. manyCharsTill anyChar (lookAhead pFooterKey <|> (eof >>% "")) |>> fst

/// Parses a footer section
let private pFooters =
    (newline >>. many (tuple2 pFooterKey pFooterValue |>> makeFooter) <|> (eof >>% [||].ToImmutableArray()))

/// Parses '!' Breaking indication in type
let private pBreakingType =
    (pstring "!: " >>. setUserState true) <|> (pstring ": " >>% ())

/// Parses optional scope
let private pScope =
    let success =
        pitem '(' >>. many1Chars asciiLetter .>> pitem ')' |>> _.ToLower() |>> ValueSome
    let failure =
        lookAhead (pitem ':' <|> pitem '!') >>% ValueNone
    success <|> failure

/// Parses the type
let private pType = many1Chars asciiLetter |>> _.ToLower()

/// Strings type and scope parser together
let private pTypeAndScope =
    tuple3 pType pScope pBreakingType

/// Parses the description/subject
let private pDescription =
    manyCharsTill (noneOf [ '\n'; '\r' ]) ((skipNewline >>. skipNewline) <|> eof) |>> fst

/// Parses the body/content
let private pBody =
    manyCharsTill anyChar (lookAhead (skipNewline >>. pFooterKey) <|> (eof >>% Unchecked.defaultof<_>)) |>> fst

/// Parses a conventional commit
let private pConventionalCommit =
    tuple4 pTypeAndScope  pDescription pBody pFooters

module ConventionalCommit =
    let private parseImpl input =
        let reader = Reader.ofString input false
        let parseCommit = parser {
            let! struct (typ,scope,_) = pTypeAndScope
            let! description = ws >>. pDescription
            let! body = pBody
            let! footers = pFooters
            let! isBreaking = getUserState
            return
                {
                    Type = typ
                    Scope = scope
                    Subject = description.TrimEnd()
                    Message =
                        body
                        |> String.IsNullOrEmpty
                        |> function
                            true -> ValueNone
                          | _ -> body.Trim()
                                |> ValueSome
                    ConventionalCommit.Footers = footers |> Seq.toList
                }
                |> if isBreaking
                    then ParsedCommit.Breaking
                    else ParsedCommit.Conventional
        }
        parseCommit reader
    
    /// <summary>
    /// Parses conventional commits. A Discriminated Union is returned that can identify
    /// <c>ParsedCommit.Breaking</c> commits readily. Failure to parse returns the DU case
    /// <c>ParsedCommit.Unconventional</c> with the input string.
    /// </summary>
    /// <remarks>
    /// Use <c>parseConventionalOrError</c> to strictly parse conventional commits, and fail with
    /// a human readable error message that can be printed to identify why the commit was not parsed.
    /// </remarks>
    let parse input =
        if isNull input
        then Unconventional ""
        else
        parseImpl input
        |> function
            | Ok { Parsed = commit } -> commit
            | Error _ -> ParsedCommit.Unconventional input
    
    /// <summary>
    /// Strict parser that only succeeds on parsing conventional commits.
    /// </summary>
    /// <remarks>
    /// <para>This implies that that the <c>Ok</c> result can only be either <c>ParsedCommit.Conventional</c> or
    /// <c>ParsedCommit.Breaking</c></para>
    /// <para>On failure, it returns a human readable error message thanks to <c>XParsec</c>
    /// <c>ErrorFormatting</c> capabilities.</para>
    /// <example>
    /// Example of error formatting:
    /// <code>
    /// "typscope): asdomfe"
    /// |> parseConventionalOrError
    /// |> Result.mapError (printfn "%A)
    /// </code>
    /// <code>
    /// "typscope): asdomf
    ///         ^ At index 8 (Ln 1, Col 9)
    /// Both parsers failed.
    /// ├───Expected '('
    /// └───Both parsers failed.
    ///     ├───Expected ':'
    ///     └───Expected '!'"
    /// </code>
    /// </example>
    /// </remarks>
    let parseConventionalOrError input =
        if isNull input
        then Error "Input was `null`."
        else
        parseImpl input
        |> function
            | Ok { Parsed = commit } -> Ok commit
            | Error e ->
                ErrorFormatting.formatStringError input e
                |> Error
