module Tests

open Xunit
open Day12

let exampleFile = "Data12-Example.txt"
let file = "Data12.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(1930, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(1381056, solvePart1 file)

[<Fact>]
let ``expandToGroup exampleFile`` () =
    let map = readInput exampleFile
    Assert.Equal(12, expandToGroup map (0,0) |> Seq.length)
