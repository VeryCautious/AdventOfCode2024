module Tests

open System
open Xunit
open Day7

let exampleFile = "Data7-Example.txt"
let file = "Data7.txt"

[<Fact>]
let ``solvePart1 ExampleFile`` () =
    Assert.Equal(3749L, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 File`` () =
    Assert.Equal(4122618559853L, solvePart1 file)

[<Fact>]
let ``solvePart2 ExampleFile`` () =
    Assert.Equal(11387L, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 File`` () =
    Assert.Equal(227615740238334L, solvePart2 file)

[<Fact>]
let ``numbers,ops -> eval -> result`` () =
    let value = eval [|2;3;4|] [Add;Mul]
    Assert.Equal((2L+3L)*4L, value)

[<Fact>]
let ``numbers,ops -> eval -> result 2`` () =
    let value = eval [|11;6;16;20|] [Add;Mul;Add]
    Assert.Equal(292L, value)
