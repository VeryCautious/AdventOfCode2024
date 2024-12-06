module Day6

type Guard = {
    Faces: int*int
    Position: int*int
}
type Field = bool[,]

let turnRight (x,y) = (y,-x)
let add (x,y) (a,b) = (x+a, y+b)
let isOnFiled (field:Field) (x,y) = x >= 0 && x < field.GetLength(0) && y >= 0 && y < field.GetLength(1)
let addChecked (field:Field) (x,y) (a,b) = isOnFiled field (x+a, y+b), (x+a, y+b)
let spreaded f (a, b) = f a b
let duplicate x =
    match x with
    | Some x -> Some (x, x)
    | None -> None

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

    match onBoard with
    | false -> None
    | true -> Some { guard with Position = (newX, newY); Faces = newFaces }

let moves field guard =
    let moves = Seq.unfold ((fun x -> moveGuard field x) >> duplicate) guard
    Seq.concat [Seq.singleton guard; moves]

let solvePart1 fileName =
    let (field, guard) = readInput fileName
    (field, guard)
    |> spreaded moves
    |> Seq.map (fun g -> g.Position)
    |> Seq.distinct
    |> Seq.length
    
