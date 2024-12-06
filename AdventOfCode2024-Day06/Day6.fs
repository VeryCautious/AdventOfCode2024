module Day6

type Guard = {
    Faces: int*int
    Position: int*int
}
type Field = bool[,]

let turnRight (x,y) = (y,-x)

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

let moveGuard (field: Field, guard: Guard) =
    true

