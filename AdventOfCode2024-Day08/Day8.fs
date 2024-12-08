module Day8

open AdventOfCode_Utils

let readInput fileName =
    let field = 
        System.IO.File.ReadAllLines fileName
        |> Array.mapi (fun x line -> line.ToCharArray() |> Array.mapi (fun y c -> (c, x, y)))
        |> Array.collect id
    let width = field |> Array.map (fun (_,x,_) -> x) |> Array.max |> (+) 1
    let height = field |> Array.map (fun (_,_,y) -> y) |> Array.max |> (+) 1
    (field, width, height)

let findAntiNode (x,y) (a,b) = [|(x+(x-a), y+(y-b))|]
let rec gcd x y =
    if y = 0 then abs x
    else gcd y (x % y)
let normalize (x,y) =
    let divider = gcd x y
    (x/divider, y/divider)
let generator ((x,y), (u,v)) =
    let newPos = (x+u,y+v)
    Some ((x,y), (newPos, (u,v)))
let findAntiRow (x,y) (a,b) =
    let dir = normalize (x-a, y-b)
    Seq.unfold generator ((a,b), dir)
let isOnFiled width heigth (x,y) = 0 <= x && x < width && 0 <= y && y < heigth 

let findAntiNodes antiNodeCalculator width height (antennaLocations: (int*int) array) =
    SeqU.cartesian antennaLocations antennaLocations
    |> Seq.filter (fun (loc1, loc2) -> loc1 <> loc2)
    |> Seq.map (fun (loc1, loc2) -> antiNodeCalculator loc1 loc2 |> Seq.takeWhile (isOnFiled width height))
    |> Seq.collect id
    |> Seq.toArray

let solve fileName antiNodeCalculator =
    let (field, width, height) = readInput fileName
    field
    |> Array.groupBy (fun (c,_,_) -> c)
    |> Array.filter (fst >> (<>) '.')
    |> Array.map (fun (_, g) -> g |> Array.map (fun (_,a,b) -> (a,b)))
    |> Array.collect (findAntiNodes antiNodeCalculator width height)
    |> Array.distinct
    |> Array.length

let solvePart1 fileName =
    solve fileName findAntiNode

let solvePart2 fileName =
    solve fileName findAntiRow