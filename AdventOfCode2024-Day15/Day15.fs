module Day15

open AdventOfCode_Utils

type Direction = Up | Down | Left | Right
type Tile = Box | Empty | Robot | Wall

let parseDirection c =
    match c with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith("Unmatched")

let parseTile c =
    match c with
    | 'O' -> Box
    | '.' -> Empty
    | '@' -> Robot
    | '#' -> Wall
    | _ -> failwith("Unmatched")

let parseField (str:string) =
    let parseLine (s:string) = s.ToCharArray() |> Array.skip 1 |> (fun a -> Array.take (a.Length-1) a) |> Array.map parseTile
    let field = str.Split('\n') |> Array.skip 1 |> (fun a -> Array.take (a.Length-1) a) |> Array.map parseLine
    Array2D.init field[0].Length field.Length (fun x y -> field[y][x])

let isHorizontal dir = match dir with | Right -> true | Left -> true | _ -> false
let isBot tile = match tile with | Robot -> true | _ -> false
let isBox tile = match tile with | Box -> true | _ -> false
let isEmpty tile = match tile with | Empty -> true | _ -> false
let isWall tile = match tile with | Wall -> true | _ -> false
let findBot (field:Tile[,]) = SeqU.cartesian [0..field.GetLength(0)-1] [0..field.GetLength(1)-1] |> Seq.find (fun (x,y) -> isBot field[x,y])
let findBotArround (field:Tile[,]) (x,y) = 
    [(0,0); (0,1); (1,0); (0,-1); (-1,0)] 
    |> Seq.map (fun (u,v) -> (u+x,v+y)) 
    |> Seq.filter (Array2DU.isOnMap field)
    |> Seq.find (fun (x,y) -> isBot field[x,y])

let parseInput fileName =
    let chunks = System.IO.File.ReadAllText fileName |> StringU.chunkByNewLine
    let field = chunks[0] |> parseField
    let directions = chunks[1].Replace("\n", "").ToCharArray() |> Array.map parseDirection
    let robotPos = findBot field
    (field, directions, robotPos)

let moveRight row botX =
    let firstFree = row |> Seq.skip botX |> Seq.takeWhile (isWall >> not) |> Seq.tryFindIndex isEmpty
    if firstFree.IsNone then
        row
    else
        row |> Seq.removeAt (firstFree.Value + botX) |> Seq.insertAt botX Empty

let print (field:Tile[,]) =
    for y in 0..field.GetLength(1)-1 do
        for x in 0..field.GetLength(0)-1 do
            let c = 
                match field[x,y] with
                | Box -> 'O'
                | Empty -> '.'
                | Robot -> '@'
                | Wall -> '#'
            printf "%c" c
        printfn ""
    printfn ""
    printfn ""
    ()

let move (field:Tile[,]) dir (botX,botY) =
    let copy = Array2D.copy field

    match dir with
        | Right -> (copy[0..,botY] <- moveRight field[0..,botY] botX |> Seq.toArray)
        | Left -> (copy[0..,botY] <- moveRight (field[0..,botY] |> Seq.rev) (field.GetLength(0) - 1 - botX) |> Seq.rev |> Seq.toArray)
        | Down -> (copy[botX,0..] <- moveRight field[botX, 0..] botY |> Seq.toArray)
        | Up -> (copy[botX,0..] <- moveRight (field[botX, 0..] |> Seq.rev) (field.GetLength(1) - 1 - botY) |> Seq.rev |> Seq.toArray)
    
    (copy, findBotArround copy (botX,botY)) 

let score (field:Tile[,]) =
    SeqU.cartesian [0..field.GetLength(0)-1] [0..field.GetLength(1)-1] 
    |> Seq.filter (fun (x,y) -> isBox field[x,y]) 
    |> Seq.map (fun (x,y) -> (x+1) + 100*(y+1))
    |> Seq.sum

let solvePart1 fileName = 
    let (field, directions, robotPos) = parseInput fileName
    let folder = (fun (f, pos) dir -> move f dir pos)
    Array.fold folder (field,robotPos) directions
    |> fst
    |> score