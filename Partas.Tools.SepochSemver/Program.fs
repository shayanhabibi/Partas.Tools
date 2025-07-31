module Partas.Tools.SepochSemver

open Fake.Core
open Semver
open XParsec
open XParsec.CharParsers
open XParsec.Parsers

/// <summary>
/// Contains data for the optional Epoch/Scope as described in the
/// Spec.
/// </summary>
[<RequireQualifiedAccess; Struct>]
type Sepoch =
    | Scope of scope: string
    | EpochScope of epoch: string * scope: string
    | Epoch of epoch: string
    | None
    member this.GetScope =
        match this with
        | Scope value
        | EpochScope(_,value) -> value |> ValueSome
        | _ -> ValueNone
    member this.GetEpoch =
        match this with
        | EpochScope(value,_) | Epoch value -> ValueSome value
        | _ -> ValueNone
    member this.Destructured=
        this.GetEpoch,this.GetScope
    
    override this.ToString() =
        match this with
        | Scope scope -> $"<({scope})>" 
        | EpochScope(epoch, scope) -> $"<{epoch}({scope})>"
        | Epoch epoch -> $"<{epoch}>"
        | None -> ""

[<Struct>]
type SepochExceptionData<'T> = {
    LeftSepoch: Sepoch
    RightSepoch: Sepoch
    IgnoreSepochValue: 'T
}
type SepochExceptionEqualityData = SepochExceptionData<bool>
type SepochExceptionComparisonData = SepochExceptionData<int>
/// <summary>
/// An exception risen from performing a <c>.=</c> comparison which
/// provides this exception when the Sepoch are not of the same shape.
/// </summary>
/// <remarks>
/// The attached data provides a field which provides the value for the
/// parts of the shape that are present.
/// </remarks>
exception UnmatchedSepochEquality of SepochExceptionEqualityData
/// <summary>
/// An exception risen from performing a <c>.=</c> comparison on a <c>SepochSemver</c>
/// when the contained <c>Sepoch</c> are not of the same shape.
/// </summary>
/// <seealso cref="T:Partas.Tools.SepochSemver.UnmatchedSepochEquality"/>
exception UnmatchedSepochSemverComparison of SepochExceptionEqualityData * int
exception UnmatchedSepochSemverEquality of SepochExceptionEqualityData * bool
let inline internal unmatchedSepoch left right value = {
    LeftSepoch = left
    RightSepoch = right
    IgnoreSepochValue = value
}
let internal unmatchedSepochEquality left right =
    unmatchedSepoch left right >> UnmatchedSepochEquality 

type Sepoch with
    /// <summary>
    /// In the case where one Sepoch does not share a field, an exception
    /// is raised containing the data of both Sepochs with the value of the
    /// shared fields equality.
    /// </summary>
    /// <exception cref="T:Partas.Tools.SepochSemver.UnmatchedSepochEquality"></exception>
    static member op_DotEquals (this: Sepoch, other: Sepoch) =
        match this, other with
        | Epoch _, Epoch _
        | None, None
        | EpochScope _, EpochScope _
        | Scope _, Scope _ -> this = other
        | EpochScope(epoch, _), Epoch epoch1 
        | Epoch epoch, EpochScope(epoch1, _) ->
            epoch = epoch1
            |> unmatchedSepochEquality this other
            |> raise
        | EpochScope(_, scope), Scope scope1
        | Scope scope, EpochScope(_, scope1) ->
            scope = scope1
            |> unmatchedSepochEquality this other
            |> raise
        | _ -> false |> unmatchedSepochEquality this other |> raise

/// <summary>
/// Struct of a <c>SemVersion</c> with the optional <c>Sepoch</c> value.
/// </summary>
[<Struct; CustomComparison; CustomEquality>]
type SepochSemver =
    {
        SemVer: SemVersion
        Sepoch: Sepoch
    }
    override this.ToString() =
        match this.Sepoch with
        | Sepoch.None -> this.SemVer.ToString()
        | sepoch -> $"{sepoch} {this.SemVer}"
    static member op_DotEquals (this, other) =
        try
        this.Sepoch .= other.Sepoch
        && this.SemVer = other.SemVer
        with
        | UnmatchedSepochEquality data ->
            UnmatchedSepochSemverEquality(data, this.SemVer = other.SemVer)
            |> raise
    /// <summary>
    /// Structural comparison. Use <c>.=</c> to perform a more nuanced comparison which
    /// raises exceptions when the <c>Sepoch</c> value does not match.
    /// </summary>
    /// <param name="obj"></param>
    /// <seealso cref="M:Partas.Tools.SepochSemver.Sepoch.op_DotEquals(Partas.Tools.SepochSemver.Sepoch,Partas.Tools.SepochSemver.Sepoch)" />
    override this.Equals(obj) =
        let other = obj :?> SepochSemver
        this.SemVer = other.SemVer
        && this.Sepoch = other.Sepoch
    override this.GetHashCode() = struct (this.SemVer, this.Sepoch).GetHashCode()
    interface System.IComparable with
        override this.CompareTo (obj: obj): int =
            match obj with
            | :? SemVersion as other ->
                this.SemVer.CompareSortOrderTo other
            | :? SepochSemver as other ->
                this.SemVer.CompareSortOrderTo other.SemVer
            | _ ->
                sprintf $"Cannot compare %s{this.GetType().FullName} to %s{obj.GetType().FullName}"
                |> invalidArg "other"

/// <summary>
/// Parses an Epoch and Scope if it is present and consumes the input.
/// </summary>
/// <remarks>
/// <para>Epoch or Scope are a SemVer prefix bounded by <c>&lt;></c> angle brackets.</para>
/// <para>Scope is itself bounded within by parenthesis.</para>
/// </remarks>
let private pSepoch input =
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

let inline private pSemver input (style: SemVersionStyles) = SemVersion.Parse(input, style)
/// <summary>
/// Strict SemVer parsing; with no <c>v</c> prefix allowed.
/// </summary>
/// <param name="input"></param>
/// <exception cref="System.ArgumentException">style is not a valid Semver.SemVersionStyles value.</exception>
/// <exception cref="System.ArgumentNullException">version is null.</exception>
/// <exception cref="System.FormatException">The version is invalid or not in a format compliant with style.</exception>
let strictParseSepochSemver input =
    let input,sepoch = pSepoch input
    {
        Sepoch = sepoch
        SemVer = pSemver input SemVersionStyles.Strict
    }

/// <summary>
/// SemVer is parsed with an allowable leading whitespace and <c>V</c> or <c>v</c>
/// </summary>
/// <param name="input"></param>
/// <exception cref="System.ArgumentException">style is not a valid Semver.SemVersionStyles value.</exception>
/// <exception cref="System.ArgumentNullException">version is null.</exception>
/// <exception cref="System.FormatException">The version is invalid or not in a format compliant with style.</exception>
let parseSepochSemver input =
    let input,sepoch = input |> String.trim |> pSepoch
    {
        Sepoch = sepoch
        SemVer = pSemver input (SemVersionStyles.AllowLeadingWhitespace ||| SemVersionStyles.AllowV)
    }

/// <summary>
/// Parses only the Sepoch from the given value for testing purposes.
/// </summary>
[<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
let parseSepoch input = pSepoch input |> snd
