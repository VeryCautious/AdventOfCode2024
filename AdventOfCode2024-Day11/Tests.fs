module Tests

open Xunit
open Day11

let exampleFile = "Data11-Example.txt"
let file = "Data11.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(55312UL, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(211306UL, solvePart1 file)

[<Fact>]
let ``solvePart2 exampleFile`` () =
    Assert.Equal(65601038650482UL, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 file`` () =
    Assert.Equal(250783680217283UL, solvePart2 file)

module AdditionalTests =

    [<Fact>]
    let ``1001 -> split -> 10 1`` () =
        Assert.Equal([10UL; 1UL], split 1001UL |> List.toArray)

    [<Fact>]
    let ``digitcount tests`` () =
        Assert.Equal(1, digitCount 1UL)
        Assert.Equal(1, digitCount 9UL)

        Assert.Equal(2, digitCount 10UL)
        Assert.Equal(2, digitCount 11UL)
        Assert.Equal(2, digitCount 99UL)

        Assert.Equal(3, digitCount 100UL)