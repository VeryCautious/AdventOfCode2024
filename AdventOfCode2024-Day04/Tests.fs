module Tests

open Xunit
open Day4

let exampleFile = "Data4-Example.txt"
let file = "Data4.txt"

[<Fact>]
let ``Part1 ExampleFile`` () =
    Assert.Equal(18, solvePart1 exampleFile)

[<Fact>]
let ``Part1 File`` () =
    Assert.Equal(2644, solvePart1 file)

[<Fact>]
let ``Part2 ExampleFile`` () =
    Assert.Equal(9, solvePart2 exampleFile)

[<Fact>]
let ``Part2 File`` () =
    Assert.Equal(1952, solvePart2 file)


module IndexTests =

    [<Fact>]
    let ``2x2 Square -> getSouthEastIndicies -> 3 IndexSeries`` () =
        Assert.Equal(3, getSouthEastIndicies [|[|'a';'a'|]; [|'a';'a'|]|] |> Array.length)

    [<Fact>]
    let ``3x3 Square -> getSouthEastIndicies -> All Present`` () =
        let expected = [| [|(0,0); (1,1); (2,2)|] ; [|(1,0); (2,1)|] ; [|(2,0)|] ; [|(0,1); (1,2)|] ; [|(0,2)|] |]
        let actual = getSouthEastIndicies [|[|'a';'a';'a'|]; [|'a';'a';'a'|]; [|'a';'a';'a'|]|]
        Assert.Equal(5, expected.Length)
        expected
        |> Array.iter (fun x -> Assert.Contains(x, actual))

    [<Fact>]
    let ``2x3 Shape -> getSouthEastIndicies -> All Present`` () =
        let field = [| [|'a';'a';'a'|]; [|'a';'a';'a'|] |]
        let expected = [| [|(0,0); (1,1)|] ; [|(1,0)|] ; [|(0,1);(1,2)|] ; [|(0,2)|] |]
        let actual = getSouthEastIndicies field
        Assert.Equal(2, field.Length)
        Assert.Equal(3, field.[0].Length)
        Assert.Equal(4, actual.Length)
        expected
        |> Array.iter (fun x -> Assert.Contains(x, actual))

    [<Fact>]
    let ``2x3 Shape -> transpose -> transposed`` () =
        let field = [| [|'a';'b';'c'|]; [|'d';'e';'f'|] |]
        let expected = [| [|'a';'b'|]; [|'c';'d'|] ; [|'e';'f'|]|]
        let actual = Array.transpose field
        Assert.Equal(expected.Length, actual.Length)
        Assert.Equal(expected.[0].Length, actual.[0].Length)
        Assert.Equal(['a';'d'], actual[0])