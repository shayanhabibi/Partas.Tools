# SepochSemver

My own flavor of Semver with a proposed marketing value `Epoch`, with an added optional `Scope` for
disambiguating tags for SemVer packages in a monorepo.

## Spec

### Semver

Semver spec follows the SemVer 2.0 spec and uses the [NuGet `Semver` package](https://semver-nuget.org/).

### Sepoch - Scoped Epoch

A Sepoch is an optional identifier preceding a Semver delimited by `<` angle brackets `>`.

It SHOULD have a single whitespace delineating it from the Semver

```
[<SEPOCH>] SEMVER
```

A Sepoch cannot contain spaces, **numbers**, and other special characters such as `.` `+` `-` which are used elsewhere in Semver.

> [!NOTE]
> The exclusion of numbers simplifies visual identification and disentanglement of the Sepoch from the Semver.
> 
> It also simplifies tooling by providing an entire region that can be skipped - identifying the semver by the
> presence of the first numeric character.

`(` Parenthesis `)` may only be used in Sepoch to delineate the Scope.

A Sepoch is made up of two optional identifiers in the following order:
1. Epoch
   - Alphabetical identifier
   - SHOULD be in uppercase
2. Scope
   - Alphabetical identifier bound by `(` parenthesis `)`
   - SHOULD be capitalized
   - SHOULD NOT be in uppercase

## Usage

```fsharp
open Partas.Tools.SepochSemver

let sepochSemver: SepochSemver = parseSepochSemver "<WILDLANDS(Core)> 1.4.0"
```

```fsharp
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
```

## Dependencies

- [XParsec] for Sepoch parsing
- [Semver] for Semver parsing
- [Fake.Core.Strings] for string helpers

[XParsec]:https://github.com/roboz0r/XParsec
[Semver]:https://semver-nuget.org/
[Fake.Core.Strings]:https://fake.build/

---

MIT License

Copyright (c) Shayan Habibi 2025
