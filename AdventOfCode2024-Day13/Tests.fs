module Tests

open Xunit
open Day13

let exampleFile = "Data13-Example.txt"
let file = "Data13.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(480L, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(27105L, solvePart1 file)

[<Fact>]
let ``solvePart2 exampleFile`` () =
    Assert.Equal(875318608908L, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 file`` () =
    Assert.Equal(101726882250942L, solvePart2 file)

[<Fact>]
let ``winnable example`` () =
    Assert.Equal(Some (80L,40L), winnable (Button (94L,34L), Button (22L,67L), PrizePosition (8400L,5400L)))