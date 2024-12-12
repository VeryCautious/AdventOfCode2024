module Day12

open AdventOfCode_Utils
open System.Collections.Generic

type Edge = Lower of (int*int) | Right of (int*int)

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

let getEdges (x,y) =  [Lower (x,y); Lower (x,y-1); Right (x,y); Right (x-1,y)]

let findEdges (group:(int*int)list) =
    group
    |> List.collect getEdges
    |> List.groupBy id
    |> List.filter (fun (_,v) -> v |> List.length = 1)
    |> List.map fst

let chunkConsecutive (list:int list) =
    let folder = (fun (last, prev) v ->
        let isConseq = last+1 = v
        match (isConseq, prev) with
        | (true, x::xs) -> (v, (List.insertAt 0 v x)::xs)
        | (false, xs) -> (v, [v]::xs)
        | _ -> failwith "Error")
    list
    |> List.sort
    |> List.fold folder (-42, [])
    |> snd

let consolidateLower (edges: Edge list) =
    let consolidateRow (_:int, cs:((int*int)list)) = cs |> List.map fst |> chunkConsecutive
    edges
    |> List.map (fun e -> 
        match e with
        | Lower c -> c
        | _ -> failwith "Not a lower edge")
    |> List.groupBy snd
    |> List.collect consolidateRow

let consolidateRight (edges: Edge list) =
    let consolidateColumn (_:int, cs:((int*int)list)) = cs |> List.map snd |> chunkConsecutive
    edges
    |> List.map (fun e -> 
        match e with
        | Right c -> c
        | _ -> failwith "Not a right edge")
    |> List.groupBy fst
    |> List.collect consolidateColumn

let consolidatedEdges (group:(int*int)list) = 
    group
    |> findEdges
    |> List.partition (fun e -> 
        match e with
        | Lower _ -> true
        | Right _ -> false)
    |> fun (lowers, rights) -> [lowers |> consolidateLower; rights |> consolidateRight]
    |> List.collect id


let solvePart1 fileName =
    let map = readInput fileName
    findGroups map 
    |> Seq.map (fun g -> (area g) * (fences map g))
    |> Seq.sum

let solvePart2 fileName =
    let map = readInput fileName
    findGroups map 
    |> Seq.map (fun g -> (area g) * (consolidatedEdges g |> List.length))
    |> Seq.sum