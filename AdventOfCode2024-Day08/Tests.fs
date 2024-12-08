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
let ``SolvePart2 ExampleFile`` () =
    Assert.Equal(34, solvePart2 exampleFile)

[<Fact>]
let ``SolvePart2 File`` () =
    Assert.Equal(1231, solvePart2 file)

module findingTests =

    [<Fact>]
    let ``points -> findAntiNode -> antiNode`` () =
        Assert.Equal((0,8), findAntiNode (0, 5) (0, 2) |> Seq.exactlyOne)

    [<Fact>]
    let ``points -> findAntiRow -> antiRow`` () =
        let xs = findAntiRow (0, 5) (0, 2) |> Seq.take 6 |> Seq.toArray
        Assert.Equal((0,2), xs[0])
        Assert.Equal((0,3), xs[1])
        Assert.Equal((0,5), xs[3])
        Assert.Equal((0,7), xs[5])

    [<Fact>]
    let ``points rev -> findAntiRow -> antiRow rev`` () =
        let xs = findAntiRow (0, 2) (0, 5) |> Seq.take 6 |> Seq.toArray
        Assert.Equal((0,5), xs[0])
        Assert.Equal((0,4), xs[1])
        Assert.Equal((0,2), xs[3])
        Assert.Equal((0,0), xs[5])