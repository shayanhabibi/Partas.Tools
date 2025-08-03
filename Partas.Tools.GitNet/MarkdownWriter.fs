module Partas.Tools.GitNet.MarkdownWriter

open FSharp.Formatting.Markdown
open FSharp.Formatting.Common
open ConventionalCommits
open Partas.Tools
open SepochSemver
open LibGit2Sharp.FSharp
open LibGit2Sharp.FSharpPatterns

[<AutoOpen>]
module internal CEs =
    type MarkdownBuilder() =
        let mutable paragraphs = []
        let addParagraph (value: MarkdownParagraph) = paragraphs <- value :: paragraphs
        member _.Zero(): unit = ()
        member _.Yield(value: string): unit =
            addParagraph <| Paragraph([Literal(value, None)], None)
        member _.Combine(_: unit, _: unit) = ()
        member _.Delay f = f()
        member _.Run(_) = paragraphs |> List.rev
        member _.Yield(para: MarkdownParagraph) = addParagraph para

    type MarkdownParagraphBuilder(state: MarkdownSpan list) =
        let mutable spans = state |> List.rev
        member this.AddSpan(span: MarkdownSpan) =
            spans <- span :: spans
        member this.Spans = spans |> List.rev
        member inline this.Yield(span: MarkdownSpan) =
            span |> this.AddSpan
        member inline this.Yield(text: string) =
            Literal(text,None) |> this.AddSpan
        member inline this.YieldFrom(values: string seq) =
            values |> Seq.iter this.Yield
        member inline this.YieldFrom(values: MarkdownSpan seq) =
            values |> Seq.iter this.Yield
        member inline this.Combine(_:unit,_:unit) = ()
        member inline this.Delay f = f()

    type ParagraphBuilder(state: MarkdownSpan list) =
        inherit MarkdownParagraphBuilder(state)
        member this.Run(_) = Paragraph(base.Spans, None)

    type HeadingBuilder(state: MarkdownSpan list, heading: int) =
        inherit MarkdownParagraphBuilder(state)
        member this.Run(_) = Heading(heading, base.Spans, None)
    type SpanBuilder(state: MarkdownSpan list) =
        inherit MarkdownParagraphBuilder(state)
        member this.Run(_) = Span(base.Spans, None)
    type CodeBlockBuilder(language: string) =
        let mutable code: string list = []
        let mutable language = language
        let mutable fence = ValueNone
        let mutable ignoredLine = ValueNone
        let mutable executionCount = ValueNone
        member this.AddCodeLine(value: string) = code <- value :: code
        [<CustomOperation "language">]
        member this.LanguageOp(_, value: string) = language <- value
        [<CustomOperation "fence">]
        member this.FenceOp(_, value: string) =
            fence <- value |> ValueOption.ofObj
        [<CustomOperation "ignoredLine">]
        member this.IgnoredLineOp(_, value: string) =
            ignoredLine <- value |> ValueOption.ofObj
        [<CustomOperation "executionCount">]
        member this.ExecutionCountOp(_, value: int) =
            executionCount <- ValueSome value
        member this.Delay(f) = f()
        member this.Combine(_,_) = ()
        member this.Yield(value: string) = this.AddCodeLine value
        member this.Run(_) =
            let codeBlock =
                code |> List.rev
                |> String.concat "\n"
            let conv = Option.ofValueOption
            CodeBlock(
                codeBlock,
                conv executionCount,
                conv fence,
                language,
                ignoredLine |> ValueOption.defaultValue "",
                None
            )

    let code language = CodeBlockBuilder(language)
    let markdown () = MarkdownBuilder()
    let para state = ParagraphBuilder(state)
    let heading level state = HeadingBuilder(state, level)

module internal Md =
    let paragraphComment text = MarkdownParagraph.InlineHtmlBlock($"<!-- {text} -->", None, None)
    let literal text = MarkdownSpan.Literal(text,None)
    let aLink url =
        MarkdownSpan.AnchorLink(url, None)
    let iLink title =
        MarkdownSpan.IndirectLink(
            [ MarkdownSpan.Literal(title, None) ],
            title,title,None
            )
    let dLink title url =
        MarkdownSpan.DirectLink(
            [ MarkdownSpan.Literal(title, None) ],
            url, Some title, None
        )
    let unorderedList items =
        MarkdownParagraph.ListBlock(MarkdownListKind.Unordered, items, None)
    let orderedList items =
        MarkdownParagraph.ListBlock(MarkdownListKind.Ordered, items, None)
    let rawHtml value = MarkdownParagraph.InlineHtmlBlock(value, None, None)
    module Tag =
        let header (tag: LibGit2Sharp.Tag * SepochSemver) =
            let date =
                fst tag
                |> Tag.peeled
                |> function
                    GitObject.IsCommit value ->
                        value
                        |> Commit.committer
                        |> Signature.date
                        |> _.ToString("yyyy-M-dd")
                        |> ValueSome
                    | _ -> ValueNone
            
            let size = 2
            let body = [
                snd tag
                |> _.ToString()
                |> iLink
                if date.IsSome then
                    literal $" - {date.Value}"
                
            ]
            MarkdownParagraph.Heading(size,body,None)

let makeHeader =
    """
# RELEASE NOTES

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to a flavored version of [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


<details>
<summary>See the spec for this SemVer flavor.</summary>

<h3>Epoch Scoped Semver</h3>

This flavor adds an optional marketable value called an `EPOCH`.
There is also an optional disambiguating `SCOPE` identifier for delineating tags for packages in a mono repo.

<blockquote>The motivation for this is to prevent resistance to utilising SemVer major bumps
correctly, by allowing a separate marketable identifier which is easily compatible
with the current SemVer spec.</blockquote>


An Epoch/Scope (*Sepoch*) is an OPTIONAL prefix to a typical SemVer.

* A Sepoch MUST BE bounded by `<` angle brackets `>`.
* The identifiers MUST BE ALPHABETICAL (A-Za-z) identifiers.
* The Epoch SHOULD BE upper case
* The Epoch MUST come before the Scope, if both are present.
* The Scope MUST additionally be bounded by `(` parenthesis `)`.
* The Scope SHOULD BE capitalised/pascal cased.
* A Sepoch SHOULD BE separated from SemVer by a single white space.
* Epoch DOES NOT influence precedence.
* Scope MUST uniquely identify a single components versioning.
* Different scopes CANNOT BE compared for precedence.
* A SemVer without a Scope CAN BE compared to a Scoped SemVer for compatibility. But caution is advised.

> There is no enforcement for ordering EPOCHs in this spec, as it
would be overly restrictive and yield little value since we can delineate and
earlier EPOCH from a later EPOCH by the SemVers.

**Examples:**

> [!WARNING]
> The following examples ignore the usage of angle brackets due to formatting issues with Mermaid JS. 

```mermaid
gitGraph
commit tag: "ALPS 1.2.3"
branch develop
commit id: "add: ..."
commit
checkout main
cherry-pick id: "add: ..." tag: "ALPS 2.1.3"
checkout develop
commit
commit
checkout main
merge develop tag: "ALPS 3.4.5"
checkout develop
commit
commit
checkout main
merge develop tag: "BRAVO 4.0.0" type: HIGHLIGHT
```
_While there are breaking changes between versions 1 to 3, we expect that it is less than
from 3 to 4. We expect the API surface would change more dramatically, or there is some other significant
milestone improvement, in the change from version 3 epoch ALPS to version 4 epoch BRAVO._

```
<WILDLANDS(Core)> 4.2.0
<WILDLANDS(Engine)> 0.5.3
<DELTA(Core)> 5.0.0
<DELTA(Engine> 0.5.3
```
_Cannot be compared to `Core` versions. Both Engine versions are equal, we can identify that
the ecosystems marketed change does not change the Engine packages API_

</details>

""" |> Markdown.Parse |> _.Paragraphs

let makeFooter =
    """

<!-- generated by Partas.Tools.GitNet -->
""" |> Markdown.Parse |> _.Paragraphs
    
let makeGroups (groups: (string * ConventionalCommit list) list) =
    let text value = Literal(value,None)
    [
        for group in groups do
        Heading(2, [ fst group |> text ], None)
        for commit in group |> snd do
        Paragraph([
            
        ], None)
            
    ] 

module Mermaid =
    let [<Literal>] kanbanTicketStub = "#TICKET#"
    // if ticketUrl.IsSome then
    //     "---"
    //     "config:"
    //     "   kanban:"
    //     $"       ticketBaseUrl: '{ticketUrl.IsSome}'"
    //     "---"
    /// <summary>
    /// NOT THREAD SAFE.
    /// </summary>
    type GitGraphWriter(?mainBranchName: string, ?verticalOrientation: bool) =
        let mutable currentBranch: string = defaultArg mainBranchName "main"
        let branches: ResizeArray<string> = ResizeArray([currentBranch])
        let entries = ResizeArray([
            if mainBranchName.IsSome then
                "---"
                "config:"
                "   gitGraph:"
                $"       mainBranchName: '{mainBranchName.Value}'"
                "---"
            if verticalOrientation.IsSome && verticalOrientation.Value then
                "gitGraph TB:"
            else "gitGraph"
        ])
        let checkoutBranch branch =
            let writeCheckout (branch: string) =
                entries.Add($"checkout {branch}")
            let writeBranch (branch: string) =
                entries.Add($"branch {branch}")
            if
                branch <> currentBranch
                && not <| branches.Contains(branch)
            then
                currentBranch <- branch
                branches.Add branch
                writeBranch branch
            elif
                branch <> currentBranch
            then
                currentBranch <- branch
                writeCheckout branch
        let writeMerge branch =
            entries.Add($"merge {branch}")
        let writeMergeWithTag branch tag =
            writeMerge $"{branch} tag: \"{tag}\""
        member this.CommitToBranch (branch, commit: Types.GitNetCommit) =
            checkoutBranch branch
            commit
        member this.Commit (commit: Types.GitNetCommit) =
            commit
        member this.EmptyCommit () =
            entries.Add("commit")
        member this.EmptyCommits (count: int) =
            for _ in [0..count] do
                entries.Add("commit")
        member this.MergeBranch (tooBranch, fromBranch) =
            checkoutBranch tooBranch
            writeMerge fromBranch
        member this.Render(?withCodeBlock: bool) =
            let withCodeBlock = defaultArg withCodeBlock false
            [
                if withCodeBlock then
                    "```mermaid"
                yield! entries
                if withCodeBlock then
                    "```"
            ]
            |> String.concat "\n"
