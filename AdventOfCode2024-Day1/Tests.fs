module Tests

open Xunit
open AdventOfCode_Utils.Say

[<Fact>]
let ``My test`` () =
    Assert.Equal("Hello Ian", hello("Ian"))
