module Tests

open Xunit
open Day6

let exampleFile = "Data6-Example.txt"
let file = "Data6.txt"

[<Fact>]
let ``solvePart1 ExampleFile`` () =
    Assert.Equal(41, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 File`` () =
    Assert.Equal(5305, solvePart1 file)

[<Fact>]
let ``solvePart2 ExampleFile`` () =
    Assert.Equal(6, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 File`` () =
    let value = solvePart2 file
    Assert.Equal(true, 2000 < value)
    Assert.Equal(true, 9999999 > value)