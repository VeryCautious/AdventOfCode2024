module Tests

open Xunit
open Day13

let exampleFile = "Data13-Example.txt"
let file = "Data13.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(480, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(27105, solvePart1 file)

[<Fact>]
let ``winnable example`` () =
    Assert.Equal(Some (80,40), winnable (Button (94,34), Button (22,67), PrizePosition (8400,5400)))