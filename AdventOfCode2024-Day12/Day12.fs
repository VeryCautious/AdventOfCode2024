module Day12

open AdventOfCode_Utils
open System.Collections.Generic

let readInput = (FileU.read2DChars id) >> snd

let adjecentCoord (x, y) = [(-1,0);(1,0);(0,-1);(0,1)] |> List.map (fun (dx, dy) -> (x+dx, y+dy))

let adjecentSimilarFileds (field:char[,]) (x,y) = adjecentCoord (x,y) |> List.filter (fun (a,b) -> Array2DU.isOnMap field (a,b) && field[a,b] = field.[x,y])
let fencesNeededFor (field:char[,]) (x,y) = adjecentSimilarFileds field (x,y) |> List.length |> (-) 4

let expandToGroup (field:char[,]) (x,y) =
    let q = new Queue<(int*int)>([(x,y)])
    let visited = new HashSet<(int*int)>(q)
    while q.Count > 0 do
        let found = q.Dequeue() |> adjecentSimilarFileds field |> List.filter (fun c -> not (visited.Contains c))
        visited.UnionWith(found)
        found |> List.iter q.Enqueue
    visited
    

let area (group:(int*int)list) = group |> List.length
let fences (field:char[,]) (group:(int*int)list) = group |> List.sumBy (fencesNeededFor field)

let findGroups (field:char[,]) =
    seq {
        let unVisited = new HashSet<(int*int)>(SeqU.cartesian [0..field.GetLength(0)-1] [0..field.GetLength(1)-1])
        while unVisited.Count > 0 do
            let next = Seq.head unVisited
            let group = expandToGroup field next
            unVisited.ExceptWith(group)
            yield Seq.toList group
    }

let solvePart1 fileName =
    let map = readInput fileName
    findGroups map 
    |> Seq.map (fun g -> (area g) * (fences map g))
    |> Seq.sum