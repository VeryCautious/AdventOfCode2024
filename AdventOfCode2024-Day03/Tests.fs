module Tests

open Xunit
open Day3

let exampleFile = "Data3-Example.txt"
let file = "Data3.txt"

[<Fact>]
let ``Part1 ExampleFile`` () =
    Assert.Equal(161, solvePart1 exampleFile)

[<Fact>]
let ``Part1 File`` () =
    Assert.Equal(162813399, solvePart1 file)

[<Fact>]
let ``Part2 ExampleFile`` () =
    Assert.Equal(48, solvePart2 exampleFile)

[<Fact>]
let ``Part2 File`` () =
    Assert.Equal(53783319, solvePart2 file)
