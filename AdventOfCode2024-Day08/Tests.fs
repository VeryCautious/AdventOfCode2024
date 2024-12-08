module Tests

open Day8
open Xunit

let exampleFile = "Data8-Example.txt"
let file = "Data8.txt"

[<Fact>]
let ``SolvePart1 ExampleFile`` () =
    Assert.Equal(14, solvePart1 exampleFile)

[<Fact>]
let ``SolvePart1 File`` () =
    Assert.Equal(364, solvePart1 file)

[<Fact>]
let ``points -> findAntiNode -> antiNode`` () =
    Assert.Equal((0,8), findAntiNode (0, 5) (0, 2))