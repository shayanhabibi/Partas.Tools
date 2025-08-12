module SepochSemver.Tests

open Expecto
open Partas.Tools.SepochSemver

[<Tests>]
let SepochSemver =
    testList "SepochSemver" [
        testTheory "Simple Sepochs" [
            "<WILD>"
            "<(Scoped)>"
        ] <| fun input ->
            let sepoch = parseSepoch input
            Expect.equal
                $"{sepoch}"
                input
                $"Can convert {input} into and backfrom Sepoch"
        testTheory "Simple SepochSemvers" [
            "0.1.2"
            "1.5.3"
            "<WILD> 0.4.5"
            "<(Scoped)> 0.3.1"
            "<RAGE(Core)> 3.2.1"
            "<RAGE(Experimental)> 3.2.1+432ld-32"
            "<(Pdf)> 3.2.1-preview.1"
            "<(Pdf)> 0.0.2-preview5"
            "<EPOCH> 4.3.6-alpha.8.3+542iop00"
        ] <| fun input ->
            let sepochSemver = parseSepochSemver input
            Expect.equal
                $"{sepochSemver}"
                input
                $"Can convert {input} into and backfrom SepochSemver"
        
    ]
