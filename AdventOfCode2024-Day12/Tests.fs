module Tests

open Xunit
open Day12

let exampleFile = "Data12-Example.txt"
let file = "Data12.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(1930, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(1381056, solvePart1 file)

[<Fact>]
let ``solvePart2 exampleFile`` () =
    Assert.Equal(1206, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 file`` () =
    Assert.Equal(0, solvePart2 file)

[<Fact>]
let ``expandToGroup exampleFile`` () =
    let map = readInput exampleFile
    Assert.Equal(12, expandToGroup map (0,0) |> Seq.length)

[<Fact>]
let ``consolidatedEdges exampleFile`` () =
    let map = readInput exampleFile
    let consolidated = findGroups map |> Seq.map (fun g -> map[fst g.Head, snd g.Head] ,(area g) , (consolidatedEdges g |> List.length)) |> Seq.toArray
    Assert.Contains(('R', 12, 10), consolidated)
    Assert.Contains(('I', 4, 4), consolidated)
    Assert.Contains(('C', 14, 22), consolidated)

[<Fact>]
let ``chunkConsecutive integers`` () =
    let chunks = (chunkConsecutive [1;2;3;5;7;8]) |> List.rev |> List.toArray
    Assert.Equal(3, chunks.Length)
    Assert.Equal([1;2;3], chunks[0] |> List.rev |> List.toArray)
    Assert.Equal([5], chunks[1] |> List.rev |> List.toArray)
    Assert.Equal([7;8], chunks[2] |> List.rev |> List.toArray)