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

let findAntiNode (x,y) (a,b) = (x+(x-a), y+(y-b))

let isOnFiled width heigth (x,y) = 0 <= x && x < width && 0 <= y && y < heigth 

let findAntiNodes (antennaLocations: (int*int) array) =
    SeqU.cartesian antennaLocations antennaLocations
    |> Seq.filter (fun (loc1, loc2) -> loc1 <> loc2)
    |> Seq.map (fun (loc1, loc2) -> findAntiNode loc1 loc2)
    |> Seq.toArray

let solvePart1 fileName =
    let (field, width, height) = readInput fileName
    field
    |> Array.groupBy (fun (c,_,_) -> c)
    |> Array.filter (fst >> (<>) '.')
    |> Array.map (fun (_, g) -> g |> Array.map (fun (_,a,b) -> (a,b)))
    |> Array.collect findAntiNodes
    |> Array.filter (isOnFiled width height)
    |> Array.distinct
    |> Array.length
