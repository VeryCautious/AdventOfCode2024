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
let ``solvePart2 exampleFile`` () =
    Assert.Equal(2858UL, solvePart2 exampleFile)

[<Fact>]
let ``solvePart2 file`` () =
    let value = solvePart2 file
    Assert.Equal(6323761685944UL, value)

module AdditionalTests =

    [<Fact>]
    let ``extractValues test`` () =
        Assert.Equal([0;1;1;1;2;2;2;2;2], extractValues [| 1; 2; 3; 4; 5 |] |> List.toArray)

    [<Fact>]
    let ``compress test`` () =
        Assert.Equal([0;2;2;1;1;1;2;2;2], compress [| 1; 2; 3; 4; 5 |] |> List.toArray)

    [<Fact>]
    let ``checksum test`` () =
        Assert.Equal(1928UL, "0099811188827773336446555566" |> (fun s -> s.ToCharArray()) |> Array.map (fun c -> c |> string |> int) |> checkSum)

    [<Fact>]
    let ``contribution test`` () =
        Assert.Equal(1+2, gauss (3-1))
        Assert.Equal(1+2+3+4+5+6, gauss (3+4-1))
        Assert.Equal((3+4+5+6) * 1 |> uint64, contribution 1 3 4)

    [<Fact>]
    let ``test gauss`` () =
        Assert.Equal(1+2+3+4+5, gauss 5)

    [<Fact>]
    let ``gauss should handle n=1 correctly``() =
        Assert.Equal(1, gauss 1)
    
    [<Fact>]
    let ``gauss should handle larger values correctly``() =
        Assert.Equal(55, gauss 10)
        Assert.Equal(5050, gauss 100)

    [<Fact>]
    let ``contribution should handle first block case (i=0) correctly``() =
        Assert.Equal(30UL, contribution 2 0 5)

    [<Fact>]
    let ``contribution should handle general case correctly``() =
        Assert.Equal(36UL, contribution 4 2 3)

    [<Fact>]
    let ``toBucketed should assign Some(index/2) to even indices and None to odd indices``() =
        let input = [|10;20;30;40;50|]
        let result = toBucketed input
        Assert.Equal((10, Some 0), result.[0])
        Assert.Equal((20, None), result.[1])
        Assert.Equal((30, Some 1), result.[2])
        Assert.Equal((40, None), result.[3])
        Assert.Equal((50, Some 2), result.[4])

    [<Fact>]
    let ``mergedRemove should merge adjacent free blocks``() =
        let data = [|(3,None);(5,Some 1);(2,None);(4,None);(6,Some 2)|]
        let result = mergedRemove 1 data
        Assert.Equal(3, result.Length)
        Assert.Equal((10,None), result.[0])
        Assert.Equal((4,None), result.[1])
        Assert.Equal((6,Some 2), result.[2])

    [<Fact>]
    let ``mergedRemove at start should merge adjacent free blocks``() =
        let data = [|(3,Some 1);(4,None)|]
        let result = mergedRemove 0 data
        Assert.Equal(1, result.Length)
        Assert.Equal((7,None), result.[0])

    [<Fact>]
    let ``mergedRemove at end should merge adjacent free blocks``() =
        let data = [|(3,None);(4,Some 1)|]
        let result = mergedRemove 1 data
        Assert.Equal(1, result.Length)
        Assert.Equal((7,None), result.[0])

    [<Fact>]
    let ``mergedInsert should insert a block and adjust the remaining free space``() =
        let data = [|(10,None)|]
        let result = mergedInsert 0 (3, Some 99) data
        Assert.Equal(2, result.Length)
        Assert.Equal((3,Some 99), result.[0])
        Assert.Equal((7,None), result.[1])

    [<Fact>]
    let ``mergedInsert should replace block entirely if no space remains``() =
        let data = [|(5,None)|]
        let result = mergedInsert 0 (5, Some 42) data
        Assert.Equal(1, result.Length)
        Assert.Equal((5,Some 42), result.[0])

    [<Fact>]
    let ``moveForward should move a block forward if a suitable free space is before it``() =
        let data = [|(5,None);(3,Some 42);(5,None)|]
        let result = moveForward data (3,42)
        Assert.Equal(2, result.Length)
        Assert.Equal((3,Some 42), result.[0])
        Assert.Equal((10,None), result.[1])

    [<Fact>]
    let ``moveForward should not move a block if no suitable free space is available``() =
        let data = [|(3,Some 1);(2,Some 2);(5,None)|]
        let result = moveForward data (2,2)
        Assert.Equal(data[1], result[1])

    [<Fact>]
    let ``compressWhole should move all files as far forward as possible``() =
        let data = [|(3,None);(3,Some 1);(1,None);(4,Some 2)|]
        let result = compressWhole data
        Assert.Equal(3, result.Length)
        Assert.Equal((3,Some 1), result.[0])
        Assert.Equal((4,None), result.[1])
        Assert.Equal((4,Some 2), result.[2])

    [<Fact>]
    let ``compressedCheckSum should handle single-block arrays``() =
        let data = [|(4,Some 10)|]
        Assert.Equal(100UL, compressedCheckSum data)

    [<Fact>]
    let ``compressedCheckSum should sum contributions of multiple blocks``() =
        let data = [|(3,Some 1);(4,Some 2);(8,None)|]
        Assert.Equal(42UL, compressedCheckSum data)
