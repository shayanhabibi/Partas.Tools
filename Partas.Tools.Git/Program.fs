module Partas.Tools.Git.Utils

open Partas.Tools.Cli.Binding
open Partas.Tools.Git.Types
open System
open SimpleExec
open BlackFox.CommandLine

module Formatting =
    let private p value = "%" + value
    let newline = p "n"
    let ``%`` = "%%"
    let literalFormatting value = "%x" + value
    module Color =
        let red = "%Cred"
        let green = "%Cgreen"
        let blue = "%Cblue"
        let reset = "%Creset"
    
    let boundary = "%m"
    let wrapSwitch = "%w"
    
module Placeholders =
    module Commit =
        let hash = "%H"
        module Abbrev =
            let hash = "%h"
    module Tree =
        let hash = "%T"
        module Abbrev =
            let hash = "%t"
    module Parent =
        let hashes = "%P"
        module Abbrev =
            let hash = "%p"
    module MailMap =
        module Author =
            let private make = (+) "%a"
            let name = make "N"
            let email = make "E"
            let localEmailPart = make "L"
    module Author =
        let private make = (+) "%a"
        let name = make "n"
        let email = make "e"
        let localEmailPart = make "l"
        module Date =
            let formatted = make "d"
            let rfc2822 = "D"
            let relative = make "r"
            let unixTimestamp = make "t"
            let isoLike = make "i"
            let strikeIso = make "R"
            let shortFormat = make "s"
            let humanStyle = make "h"
    module Committer =
        let private make = (+) "%c"
        let name = make "n"
        let email = make "e"
        let localEmailPart = make "l"
        module Date =
            let formatted = make "d"
            let rfc2822 = make "D"
            let relative = make "r"
            let unixTimestamp = make "t"
            let isoLike = make "i"
            let strictIso = make "I"
            let shortFormat = make "s"
            let humanStyle = make "h"
    let refNames = "%d"
    let refNamesWithoutWrap = "%D"
    let decorate value = failwith "TODO" // TODO
    let describe value = failwith "TODO" // TODO
    let clRefName = "%S"
    let encoding = "%e"
    let subject = "%s"
    let sanitizedSubjectLine = "%f"
    let body = "%b"
    let rawBody = "%B"
    let gpgRawVerificationMessage = "%GG"
    let signatureValidity = "%G?"
    let signatureName = "%GS"
    let signatureKey = "%GK"
    let signatureFingerprint = "%GF"
    let signatureFingerprintPrimarySubkey = "%GP"
    let signatureKeyTrustLevel = "%GT"
    module Reflog =
        let private make = (+) "%g"
        let selector = make "D"
        let abbrevSelector = make "d"
        let identityName = make "n"
        let mailMapIdentityName = make "N"
        let identityEmail = make "e"
        let mailMapIdentityEmail = make "E"
        let subject = make "s"
        // todo trailers

module Format =
    let simpleJson = $"""{{
    \"hash\": \"{Placeholders.Commit.Abbrev.hash}\",
    \"author\": {{ \
        \"name\": \"{Placeholders.Author.name}\",
        \"email\": \"{Placeholders.Author.email}\",
        \"date\": \"{Placeholders.Author.Date.shortFormat}\"
    }},
    \"committer\": {{ 
        \"name\": \"{Placeholders.Committer.name}\",
        \"email\": \"{Placeholders.Committer.name}\",
        \"date\": \"{Placeholders.Committer.Date.shortFormat}\"
    }},
    \"commit\": {{ 
        \"parents\": \"{Placeholders.Parent.Abbrev.hash}\",
        \"tree\": \"{Placeholders.Tree.Abbrev.hash}\"
    }},
    \"refs\": \"{Placeholders.refNames}\",
    \"subject\": \"{Placeholders.subject}\",
    \"body\": \"{Placeholders.body}\",
}}
$END$
"""


let run<'T> (args: GitCommand<'T> -> GitCommand<'T>) directory =
    let args = { Args = []; Options = [] } |> args
    let cmdLine = args |> GitCommand.toString
    let struct (shaStdout,_) =
        #if DEBUG
        Console.ForegroundColor <- ConsoleColor.DarkYellow
        CmdLine.empty
            |> CmdLine.appendRaw "git"
            |> CmdLine.appendRaw cmdLine
            |> CmdLine.toString
            |> sprintf "Partas.Tools.Git:\n%s"
            |> Console.WriteLine
        Console.ResetColor()
        #endif
        Command.ReadAsync(
            "git",
            cmdLine
            ,directory
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously
    shaStdout
