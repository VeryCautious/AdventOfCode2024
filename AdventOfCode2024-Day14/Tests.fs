module Tests

open System
open Xunit
open Day14

let exampleFile = "Data14-Example.txt"
let file = "Data14.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(12 , solvePart1 exampleFile 11 7)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(222208000 , solvePart1 file 101 103)

[<Fact>]
let ``simulate example`` () =
    let poses = readInput exampleFile |> Array.map (simulate 11 7 100)
    Assert.Contains((0,2) , poses)
    Assert.Contains((1,3) , poses)
    Assert.Contains((1,6) , poses)

[<Fact>]
let ``qadrant tests`` () =
    let quad = quadrant 9 9
    Assert.Equal(-1, quad (0,4))
    Assert.Equal(0, quad (1,2))
    Assert.Equal(1, quad (6,2))
    Assert.Equal(2, quad (1,6))
    Assert.Equal(3, quad (7,6))