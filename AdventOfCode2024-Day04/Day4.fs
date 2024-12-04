module Day4

type Indicies = (int*int) array
type XIndicies = ((int*int) array) * ((int*int) array)
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

let cartesian a b = a |> Array.collect (fun x -> b |> Array.map (fun y -> (x, y)))

let getXIndices = ([|(-1,-1); (0,0); (1,1)|], [|(-1,1); (0,0); (1,-1)|])
    
let getXIndeciesAround (x,y) =
    let (a,b) = getXIndices
    let c = a |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
    let d = b |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
    (c, d)

let matches (array: Field) (indicies: XIndicies) =
    let (a, b) = indicies
    let aStr = stringFrom array a
    let bStr = stringFrom array b
    let aFit = (aStr = "MAS") || (aStr = "SAM")
    let bFit = (bStr = "MAS") || (bStr = "SAM")
    aFit && bFit

let containedIn (array: Field) (indicies: XIndicies) =
    Array.concat [fst indicies; snd indicies]
    |> Array.exists (fun (x, y) -> x < 0 || x >= array.Length || y < 0 || y >= array.[0].Length)
    |> not

let getCrossIndicies (array: Field) =
    let coords = cartesian [|0..(array.Length - 1)|] [|0..(array.[0].Length - 1)|]
    coords
    |> Array.map (getXIndeciesAround)
    |> Array.filter (containedIn array)

let countWordInArray (word: string) (array: Field) =
    getPossibleIndecies array
    |> Array.map (stringFrom array) 
    |> Array.map (countWord word)
    |> Array.sum

let solvePart1 (fileName: string) = 
    read2DArray fileName
    |> countWordInArray "XMAS"

let solvePart2 (fileName: string) = 
    let field = read2DArray fileName
    field
    |> getCrossIndicies
    |> Array.filter (matches field)
    |> Array.length