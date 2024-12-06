module Day6

open System.Collections.Generic

type Guard = {
    Faces: int*int
    Position: int*int
}
type Field = bool[,]
type MoveResult = Done | Walking | Looped | Turning

let turnRight (x,y) = (y,-x)
let add (x,y) (a,b) = (x+a, y+b)
let isOnFiled (field:Field) (x,y) = x >= 0 && x < field.GetLength(0) && y >= 0 && y < field.GetLength(1)
let addChecked (field:Field) (x,y) (a,b) = isOnFiled field (x+a, y+b), (x+a, y+b)
let cartesian a b = a |> Seq.collect (fun x -> b |> Seq.map (fun y -> (x, y)))
let findGuard charField = { Faces = (0,1); Position = charField |> Array.find (fun (_,_,c) -> c = '^') |> fun (x,y,_) -> x,y }
let addValue (visited:HashSet<Guard>) guard = visited.Add guard |> (fun _ -> visited)

let readInput fileName =
    let field = System.IO.File.ReadAllLines(fileName) |> Array.rev |> Array.map (fun line -> line.ToCharArray()) |> Array.transpose
    let width = field |> Array.length
    let height = field.[0] |> Array.length
    let coords = field |> Array.mapi (fun x _ -> field[x] |> Array.mapi (fun y _ -> x,y,field[x][y])) |> Array.collect id
    let guard = findGuard coords
    Array2D.init width height (fun x y -> field[x][y] = '#'), guard

let moveGuard (field: Field) (guard: Guard) =
    let (onField, (newX, newY)) = guard.Position |> addChecked field guard.Faces
    let nextIsObstical = onField && field[newX, newY]
    
    if nextIsObstical then
        Some (Turning,{ guard with Faces = guard.Faces |> turnRight })
    else 
        if onField then
            let newGuard = { guard with Position = (newX, newY) }
            let nextIsOnBoard = newGuard.Position |> addChecked field newGuard.Faces |> fst
            if nextIsOnBoard then 
                Some (Walking, newGuard) 
            else 
                Some (Done, newGuard)
        else
            None
    
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
