module Tests

open Xunit
open Day10

let exampleFile = "Data10-Example.txt"
let file = "Data10.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(0, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(0, solvePart1 file)
