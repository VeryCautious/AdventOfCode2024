module Day4

type Indicies = (int*int) array
type Field = char array array

let read2DArray fileName = 
    System.IO.File.ReadAllLines(fileName)
    |> Array.map (fun line -> line.ToCharArray())

let getRowIndicies (array: Field) = 
    let row = [|0..(array.Length - 1)|]
    let col = [|0..(array.[0].Length - 1)|]
    row
    |> Array.map (fun x -> col |> Array.map (fun y -> (x, y)))

let getSouthEastIndicies (array: Field) = 
    let row = [|0..(array.Length - 1)|]
    let col = [|0..(array.[0].Length - 1)|]
    let upper = row |> Array.map (fun x -> col |> Array.map (fun y -> (x + y, y)))
    let lower = [|1..(array.[0].Length - 1)|] |> Array.map (fun y -> row |> Array.map (fun x -> (x, x + y)))
    Array.concat [|upper; lower|]
    |> Array.map (Array.filter (fun (x, y) -> x >= 0 && x < array.Length && y >= 0 && y < array.[0].Length))

let flipX (maxX: int) (ind: Indicies) = ind |> Array.map (fun (x, y) -> (maxX - x, y))
let transpose (x,y) = (y,x)

let getDiagonalIndicies (array: Field) = 
    let southEast = getSouthEastIndicies array
    let southWest = southEast |> Array.map (flipX (array.Length - 1))
    Array.concat [|southEast; southWest|]

let stringFrom (array: Field) (indicies: Indicies) = 
    indicies |> Array.map (fun (x, y) -> array.[x].[y]) |> System.String

let countWord (word: string) (hayStack:string) = 
    [|0..(hayStack.Length - word.Length)|]
    |> Array.map (fun x -> hayStack.Substring(x, word.Length))
    |> Array.filter ((=) word)
    |> Array.length

let getPossibleIndecies (array: Field) = 
    let rowIndicies = getRowIndicies array
    let reversedRowIndicies = rowIndicies |> Array.map Array.rev
    let colIndicies = getRowIndicies (Array.transpose array) |> Array.map (Array.map transpose)
    let reversedColIndicies = colIndicies |> Array.map Array.rev
    let diagonalIndicies = getDiagonalIndicies array
    let reversedDiagonalIndicies = diagonalIndicies |> Array.map Array.rev
    Array.concat [|rowIndicies; colIndicies; reversedRowIndicies; reversedColIndicies; diagonalIndicies; reversedDiagonalIndicies|]

let countWordInArray (word: string) (array: Field) =
    getPossibleIndecies array
    |> Array.map (stringFrom array) 
    |> Array.map (countWord word)
    |> Array.sum

let solvePart1 (fileName: string) = 
    read2DArray fileName
    |> countWordInArray "XMAS" 