module Tests

open System
open Xunit
open Day7

[<Fact>]
let ``My test`` () =
    Assert.Equal(3749L, solvePart1 "Data7-Example.txt")

[<Fact>]
let ``My testkmwalda`` () =
    let value = solvePart1 "Data7.txt"
    Assert.Equal(4122618559853L, value)

[<Fact>]
let ``My teawdwast`` () =
    Assert.Equal(11387L, solvePart2 "Data7-Example.txt")

[<Fact>]
let ``My testawdkmwalda`` () =
    let value = solvePart2 "Data7.txt"
    Assert.Equal(4122618559853L, value)

[<Fact>]
let ``adwMy testa`` () =
    let x = addOneMul [Add;Add;Add]
    Assert.Equal(3, x.Length)
    Assert.Equal([|Mul;Add;Add|], x[0])
    Assert.Equal([|Add;Mul;Add|], x[1])
    Assert.Equal([|Add;Add;Mul|], x[2])

[<Fact>]
let ``aval`` () =
    let value = eval [|2;3;4|] [Add;Mul]
    Assert.Equal((2L+3L)*4L, value)

[<Fact>]
let ``avalawd`` () =
    let value = eval [|11;6;16;20|] [Add;Mul;Add]
    Assert.Equal(292L, value)

[<Fact>]
let ``avawawdwal`` () =
    Assert.Equal(1, [[1;2];[1;2]] |> List.distinct |> List.length)

[<Fact>]
let ``avawdal`` () =
    let value = operatorCombinations 6
    Assert.Equal(64, value |> List.collect id |> List.distinct |> List.length)
    value |> List.iteri (fun i b -> Assert.True(List.forall (fun a -> a |> List.filter ((=) Mul) |> List.length |> ((=) i)) b))

[<Fact>]
let ``avawdawdal`` () =
    let value = generateLists 3
    Assert.Equal(27, value |> List.distinct |> List.length)