module Tests

open Xunit
open Day16

let exampleFile = "Data16-Example.txt"
let exampleFile2 = "Data16-Example2.txt"
let file = "Data16.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(7036, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 exampleFile2`` () =
    Assert.Equal(11048, solvePart1 exampleFile2)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(82460, solvePart1 file)

[<Fact>]
let ``solvePart2 exampleFile`` () =
    Assert.Equal(45, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 exampleFile2`` () =
    Assert.Equal(64, solvePart2 exampleFile2)

[<Fact>]
let ``solvePart2 file`` () =
    Assert.Equal(590, solvePart2 file)