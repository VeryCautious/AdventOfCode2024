module Tests

open Xunit
open Day10

let exampleFile = "Data10-Example.txt"
let file = "Data10.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(36, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(461, solvePart1 file)

[<Fact>]
let ``solvePart2 exampleFile`` () =
    Assert.Equal(81, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 file`` () =
    Assert.Equal(875, solvePart2 file)

module AdditionalTests =

    [<Fact>]
    let ``v`` () =
        let (field, map) = readInput exampleFile
        let startPositions = field |> Array.filter (snd >> (=) 0) |> Array.map fst |> Array.toList
        let options = startPositions |> List.map (paths map) |> List.toArray
        Assert.All(options |> Array.head, (fun path -> Assert.Equal([0..9] |> List.rev, path |> List.map snd |> List.toArray)))

    [<Fact>]
    let ``u`` () =
        let (field, map) = readInput exampleFile
        let startPositions = field |> Array.filter (snd >> (=) 0) |> Array.map fst |> Array.toList
        let options = startPositions |> List.map ((paths map) >> score) |> List.toArray
        Assert.Equal([5; 6; 5; 3; 1; 3; 5; 3; 5], options)

    [<Fact>]
    let ``x`` () =
        let map = Array2D.init 3 1 (fun x _ -> [1;0;1][x])
        let nextSteps = nextSteps map ((1,0), 0) |> Seq.toArray
        Assert.Contains(((0,0), 1), nextSteps)
        Assert.Contains(((2,0), 1), nextSteps)

    [<Fact>]
    let ``y`` () =
        let map = Array2D.init 3 1 (fun x _ -> [2;0;1][x])
        let nextSteps = nextSteps map ((1,0), 0) |> Seq.toArray
        Assert.DoesNotContain(((0,0), 1), nextSteps)
        Assert.Contains(((2,0), 1), nextSteps)

    [<Fact>]
    let ``z`` () =
        let map = Array2D.init 3 1 (fun x _ -> [0..9][x])
        let path = extendPath map [((0, 0),0)] |> Seq.exactlyOne |> List.toArray
        let expected = [0..1] |> List.rev |> List.map (fun x -> ((x, 0),x))
        Assert.Equal(2, path.Length)
        Assert.Equal(expected, path)

    [<Fact>]
    let ``a`` () =
        let map = Array2D.init 10 1 (fun x _ -> [0..9][x])
        let path = paths map (0, 0) |> Seq.exactlyOne |> List.toArray
        let expected = [0..9] |> List.rev |> List.map (fun x -> ((x, 0),x))
        Assert.Equal(10, path.Length)
        Assert.Equal(expected, path)
    
