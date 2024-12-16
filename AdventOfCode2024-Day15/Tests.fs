module Tests

open System
open Xunit
open Day15

let exampleFile = "Data15-Example.txt"
let exampleSmall = "Data15-ExampleSmall.txt"
let file = "Data15.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(10092, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 exampleSmall`` () =
    Assert.Equal(2028, solvePart1 exampleSmall)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(1486930, solvePart1 file)

[<Fact>]
let ``parseTile test`` () =
    Assert.Equal([Empty; Empty; Robot; Box; Empty], "#..@O.#".Trim('#').ToCharArray() |> Array.map parseTile)