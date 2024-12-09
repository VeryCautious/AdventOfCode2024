module Tests

open Xunit
open Day9

let exampleFile = "Data9-Example.txt"
let file = "Data9.txt"

[<Fact>]
let ``solvePart1 exampleFile`` () =
    Assert.Equal(1928UL, solvePart1 exampleFile)

[<Fact>]
let ``solvePart1 file`` () =
    Assert.Equal(6301895872542UL, solvePart1 file)

[<Fact>]
let ``x`` () =
    Assert.Equal([0;1;1;1;2;2;2;2;2], extractValues [| 1; 2; 3; 4; 5 |] |> List.toArray)

[<Fact>]
let ``y`` () =
    Assert.Equal([0;2;2;1;1;1;2;2;2], compress [| 1; 2; 3; 4; 5 |] |> List.toArray)

[<Fact>]
let ``z`` () =
    Assert.Equal(1928UL, "0099811188827773336446555566" |> (fun s -> s.ToCharArray()) |> Array.map (fun c -> c |> string |> int) |> checkSum)