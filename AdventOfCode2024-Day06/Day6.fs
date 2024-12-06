module Day6

open System.Collections.Generic

type Guard = {
    Faces: int*int
    Position: int*int
}
type Field = bool[,]
type MoveResult = Done | Walking | Looped

let turnRight (x,y) = (y,-x)
let add (x,y) (a,b) = (x+a, y+b)
let isOnFiled (field:Field) (x,y) = x >= 0 && x < field.GetLength(0) && y >= 0 && y < field.GetLength(1)
let addChecked (field:Field) (x,y) (a,b) = isOnFiled field (x+a, y+b), (x+a, y+b)
let cartesian a b = a |> Seq.collect (fun x -> b |> Seq.map (fun y -> (x, y)))

let findGuard fileName =
    let field = System.IO.File.ReadAllLines(fileName) |> Array.rev |> Array.map (fun line -> line.ToCharArray()) |> Array.transpose
    let guardChar = '^'
    let guardPos = field |> Array.mapi (fun x _ -> field[x] |> Array.mapi (fun y _ -> x,y,field[x][y])) |> Array.collect id |> Array.find (fun (_,_,c) -> c = guardChar) |> fun (x,y,_) -> x,y
    { Faces = (0,1); Position = guardPos }

let readInput fileName =
    let field = System.IO.File.ReadAllLines(fileName) |> Array.rev |> Array.map (fun line -> line.ToCharArray() |> Array.map ((=) '#')) |> Array.transpose
    let width = field |> Array.length
    let height = field.[0] |> Array.length
    let guard = findGuard fileName
    Array2D.init width height (fun x y -> field[x][y]), guard

let moveGuard (field: Field) (guard: Guard) =
    let (onBoard, (newX, newY)) = guard.Position |> addChecked field guard.Faces
    let nextIsObstical = (newX, newY) |> addChecked field guard.Faces |> (fun (onField,(obsX, obsY)) -> onField && field[obsX, obsY])
    let newFaces = if nextIsObstical then guard.Faces |> turnRight else guard.Faces

    if onBoard then 
        let newGuard = { guard with Position = (newX, newY); Faces = newFaces }
        let nextIsOnBoard = newGuard.Position |> addChecked field newGuard.Faces |> fst
        if nextIsOnBoard then Some (Walking, newGuard) else Some (Done, newGuard)
    else
        None

let addValue (visited:HashSet<Guard>) guard = 
    visited.Add guard |> ignore
    visited

let unfoldMovement field (visited:HashSet<Guard>, guard:Option<Guard>) =
    match guard with
    | None -> None
    | Some guard ->
        let movement = moveGuard field guard
        if movement.IsNone then
            None
        else
            if visited.Contains(snd movement.Value) then
                Some ((Looped, guard), (visited, None))
            else
                match movement with
                | None -> None
                | Some (fin, guard) -> Some ((fin, guard), (addValue visited guard, Some guard))

let placeObstacle (field:Field) (x,y) =
    let newFiled = Array2D.copy field
    newFiled[x,y] <- true
    newFiled

let moves field guard =
    let visited = new HashSet<Guard>()
    let moves = Seq.unfold (unfoldMovement field) (visited, Some(guard))
    Seq.concat [Seq.singleton (Walking, guard); moves]

let allObstacleOptions (field:Field) (guard:Guard) =
    let coords = cartesian [0..field.GetLength(0)-1] [0..field.GetLength(1)-1]
    let canOnlyHitCoords = 
        moves field guard
        |> Seq.map (fun (_,g) -> g.Position)
        |> Seq.distinct
        |> HashSet
    coords
    |> Seq.filter (fun (x,y) -> not field[x,y] && guard.Position <> (x,y))
    |> Seq.filter canOnlyHitCoords.Contains
    |> Seq.map (placeObstacle field)

let findFinishedReason guard field =
    moves field guard 
    |> Seq.map (fun (reason,_) -> reason)
    |> Seq.last

let solvePart1 fileName =
    let (field, guard) = readInput fileName
    moves field guard
    |> Seq.map (fun (_,g) -> g.Position)
    |> Seq.distinct
    |> Seq.length

let solvePart2 fileName =
    let (field, guard) = readInput fileName
    allObstacleOptions field guard
    |> Seq.map (findFinishedReason guard)
    |> Seq.filter ((=) Looped)
    |> Seq.length

let printSize fileName =
    let (field, guard) = readInput fileName
    let all = allObstacleOptions field guard
    printfn "Size: %d" (Seq.length all)