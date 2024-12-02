module Tests

open Xunit
open Day1

let fileName = "Data1.txt"
let exampleFileName = "Data1-Example.txt"

[<Fact>]
let ``My test`` () =
    Assert.Equal(3246517, day1Result fileName)

[<Fact>]
let ``My test -ex`` () =
    Assert.Equal(11, day1Result exampleFileName)


[<Fact>]
let ``My test2`` () =
    Assert.Equal(29379307, day1Result2 fileName)