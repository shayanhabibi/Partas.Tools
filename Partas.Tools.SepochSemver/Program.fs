module Partas.Tools.SepochSemver

open Fake.Core
open Semver
open XParsec
open XParsec.CharParsers
open XParsec.Parsers

[<RequireQualifiedAccess; Struct>]
type Sepoch =
    | Scope of scope: string
    | EpochScope of epoch: string * scope: string
    | Epoch of epoch: string
    | None

[<Struct>]
type SepochSemver = {
    SemVer: SemVersion
    Sepoch: Sepoch
}

/// <summary>
/// Parses a Epoch & Scope if it is present and consumes the input.
/// </summary>
/// <remarks>
/// <para>Epoch or Scope are a SemVer prefix bounded by <c>&lt;></c> angle brackets.</para>
/// <para>Scope is itself bounded within by parenthesis.</para>
/// </remarks>
let private parseSepoch input =
    // XParsec reader
    let reader = Reader.ofString input ()
    // Valid chars for a Sepoch
    let sepochChars = noneOf "1234567890<>+-. \n\r\n#"
    // The opening delimiter of a Sepoch
    let openBound = skipChar '<'
    // The closing delimiters of an EPOCH
    // Does not consume the closing delimiter for the epoch
    let terminator =
        lookAhead (skipChar '>' <|> skipChar '(')
    // Consumes an epoch
    let consumeEpoch =
        manyCharsTill sepochChars terminator
    // Consumes a scope
    let consumeScope =
        let terminator = skipChar ')'
        let consume = skipChar '(' >>. manyCharsTill sepochChars terminator
        (opt consume) .>> skipChar '>'
    // Combines the results of consumeEpoch and consumeScope
    let combinator = fun (epoch: string * unit) (scope: (string * unit) voption) ->
        fst epoch, scope |> ValueOption.map fst
    // The Sepoch parse result (optional)
    let sepoch = reader |> opt (openBound >>. pipe2 consumeEpoch consumeScope combinator)
    // The rest of the input to be fed into the SemVer parser
    let remainder = reader |> manyChars anyChar
    // Matching the results of the sepoch parse result and the rest of the input
    match sepoch,remainder with
    | Ok { Parsed = ValueSome sepoch }, Ok { Parsed = remainder } ->
        remainder |> String.trim,
        match sepoch with
        | epoch, ValueSome scope when epoch |> String.isNullOrWhiteSpace -> Sepoch.Scope scope
        | epoch, ValueNone when epoch |> String.isNullOrWhiteSpace -> Sepoch.None
        | epoch, ValueSome scope -> Sepoch.EpochScope(epoch,scope)
        | epoch, ValueNone -> Sepoch.Epoch(epoch)
    | _, Ok { Parsed = remainder } ->
        remainder |> String.trim, Sepoch.None
    | results -> failwithf $"%A{results}"

let inline private parseSemver input = SemVersion.Parse input

let parseSepochSemver input =
    let input,sepoch = parseSepoch input
    {
        Sepoch = sepoch
        SemVer = parseSemver input
    }
