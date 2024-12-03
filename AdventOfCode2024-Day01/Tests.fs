module Tests

open Xunit
open Day1

let fileName = "Data1.txt"
let exampleFileName = "Data1-Example.txt"

[<Fact>]
let ``Part1 ExampleFile`` () =
    Assert.Equal(11, solvePart1 exampleFileName)

[<Fact>]
let ``Part1 File`` () =
    Assert.Equal(3246517, solvePart1 fileName)

[<Fact>]
let ``Part2 ExampleFile`` () =
    Assert.Equal(31, solvePart2 exampleFileName)

[<Fact>]
let ``Part2 File`` () =
    Assert.Equal(29379307, solvePart2 fileName)