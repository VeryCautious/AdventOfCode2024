module Tests

open System
open Xunit
open Day2



[<Fact>]
let ``My test`` () =
    Assert.Equal(2, day2Result "Data2-Example.txt")

[<Fact>]
let ``My test 2`` () =
    Assert.Equal(390, day2Result "Data2.txt")

[<Fact>]
let ``My test 3`` () =
    Assert.Equal(4, day2ResultPart2 "Data2-Example.txt")

[<Fact>]
let ``My test 4`` () =
    Assert.Equal(439, day2ResultPart2 "Data2.txt")
