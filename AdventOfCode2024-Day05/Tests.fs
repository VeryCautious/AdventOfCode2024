module Tests

open Xunit
open Day5

let exampleFile = "Data5-Example.txt"
let file = "Data5.txt"

[<Fact>]
let ``solvePart1 ExampleFile`` () =
    let summedMedianOfSorted = solvePart1 exampleFile
    Assert.Equal(143, summedMedianOfSorted)


[<Fact>]
let ``solvePart1 File`` () =
    let summedMedianOfSorted = solvePart1 file
    Assert.Equal(5991, summedMedianOfSorted)

[<Fact>]
let ``solvePart2 ExampleFile`` () =
    let summedMedianOfSorted = solvePart2 exampleFile
    Assert.Equal(123, summedMedianOfSorted)


[<Fact>]
let ``solvePart2 File`` () =
    let summedMedianOfSorted = solvePart2 file
    Assert.Equal(5479, summedMedianOfSorted)

module SortingTests =

    [<Fact>]
    let ``1 3 -> compairTransitive -> -1`` () =
        let comp = compairTransitive (createRules [| (1,2); (2,3) |]) 1 3
        Assert.Equal(-1, comp)

    [<Fact>]
    let ``3 1 -> compairTransitive -> 1`` () =
        let comp = compairTransitive (createRules [| (1,2); (2,3) |]) 3 1
        Assert.Equal(1, comp)

    [<Fact>]
    let ``1 4 -> compairTransitive -> -1`` () =
        let comp = compairTransitive (createRules [| (1,2); (2,3); (3,4) |]) 1 4
        Assert.Equal(-1, comp)

    [<Fact>]
    let ``1 2 -> compairTransitive -> -1`` () =
        let comp = compairTransitive (createRules [| (1,2); (2,3) |]) 1 2
        Assert.Equal(-1, comp)

    [<Fact>]
    let ``rulesMap -> compairTransitive rulesMap 75 29 -> -1`` () =
        let rules, _ = readInput "Data5-Example.txt"
        let rulesMap = createRules rules
        let isSortedAr = compairTransitive rulesMap 75 29
        Assert.Equal(-1, isSortedAr)

    [<Fact>]
    let ``rulesMap -> sort firstRow -> firstRowSorted`` () =
        let rules, _ = readInput "Data5-Example.txt"
        let rulesMap = createRules rules
        let x = sort rulesMap [| 75;97;47;61;53 |]
        Assert.Equal([ 97;75;47;61;53 ], x)