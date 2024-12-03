module Tests

open Xunit
open Day2

let exampleFile = "Data2-Example.txt"
let file = "Data2.txt"

[<Fact>]
let ``Part1 ExampleFile`` () =
    Assert.Equal(2, solvePart1 exampleFile)

[<Fact>]
let ``Part1 File`` () =
    Assert.Equal(390, solvePart1 file)

[<Fact>]
let ``Part2 ExampleFile`` () =
    Assert.Equal(4, solvePart2 exampleFile)

[<Fact>]
let ``Part2 File`` () =
    Assert.Equal(439, solvePart2 file)
