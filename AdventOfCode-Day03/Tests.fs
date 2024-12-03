module Tests

open Xunit
open Day3

[<Fact>]
let ``My test`` () =
    Assert.Equal(161, solve "Data3-Example.txt")

[<Fact>]
let ``My testx`` () =
    Assert.Equal(162813399, solve "Data3.txt")

[<Fact>]
let ``My testawd`` () =
    Assert.Equal(48, solveWithDeactivation "Data3-Example.txt")

[<Fact>]
let ``My testxaaa`` () =
    Assert.Equal(53783319, solveWithDeactivation "Data3.txt")
