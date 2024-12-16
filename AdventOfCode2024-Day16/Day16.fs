module Day16

open AdventOfCode_Utils
open System.Collections.Generic

type Pos = (int*int)*(int*int)
type State = Pos*int*(Pos list)

let rotate90clockw (x,y) = (y,-x)
let rotate90cclockw (x,y) = (-y,x)
let add (x,y) (a,b) = (a+x,b+y)
let third (_, _, c) = c
let second (_, b, _) = b
let pos (a,_,_) = a

let parseInput fileName =
    let movementStenicle = FileU.read2DChars ((<>) '#') fileName |> snd
    let tiles = FileU.read2DChars id fileName |> fst
    let startPos = tiles |> Array.find (snd >> ((=) 'S')) |> fst
    let endPos = tiles |> Array.find (snd >> ((=) 'E')) |> fst
    (startPos, endPos, movementStenicle)

let canMoveTo movementStenicle (x,y) = Array2DU.isOnMap movementStenicle (x,y) && movementStenicle[x, y]
let extendPath (path:Pos list) ((p,i):Pos*int) = (p,i, p::path)

let findNextFor (movementStenicle:bool[,]) (((pos, dir), score, path):State) =
    let rots = 
        [extendPath path ((pos, rotate90clockw dir), score+1000); extendPath path ((pos, rotate90cclockw dir), score+1000)]
        |> List.filter (fun ((pos, dir),_, _) -> canMoveTo movementStenicle (add pos dir))
    let newPos = add pos dir
    if canMoveTo movementStenicle newPos then
        (extendPath path ((newPos, dir), score+1))::rots
    else
        rots

let collectPaths (visited:Dictionary<(int*int)*(int*int), List<State>>) (endState:State) =
    let q = new Queue<State list>([[endState]])
    let mutable resPaths = []
    while q.Count > 0 do
        let path = q.Dequeue()
        if visited.ContainsKey(path.Head |> pos) then
            let preds = visited[path.Head |> pos] |> Seq.toList
            let minScore = preds |> Seq.map third |> Seq.min
            preds 
            |> Seq.filter (third >> ((=) minScore)) 
            |> Seq.map (fun next -> (next::path)) 
            |> Seq.iter (q.Enqueue)
        else
            resPaths <- path::resPaths
    resPaths

let solve startPos endPos movementStenicle =
    let queued = new Dictionary<(int*int)*(int*int), int>()
    let q = new PriorityQueue<State,int>()
    let endStates = new HashSet<State>()
    let mutable minScore = Microsoft.FSharp.Core.int.MaxValue
    q.Enqueue(((startPos, (1,0)), 0, []), 0)

    while q.Count > 0 && ((q.Peek() |> second) <= minScore) do
        let ((pos, dir), score, path) = q.Dequeue()

        if pos = endPos then
            endStates.Add(((pos, dir), score, path)) |> ignore
            minScore <- min score minScore
        else
            let next = findNextFor movementStenicle ((pos, dir), score, path)
            next |> Seq.iter (fun ((nextPos, nextDir), nextScore, nextPath) -> 
                if queued.ContainsKey((nextPos, nextDir)) |> not || queued[(nextPos, nextDir)] >= nextScore then
                    q.Enqueue(((nextPos, nextDir), nextScore, nextPath), nextScore)
                 
                )

    let paths = endStates |> Seq.map third
    (minScore, paths)

let solvePart1 fileName = 
    parseInput fileName
    |||> solve
    |> fst

let solvePart2 fileName =
    parseInput fileName
    |||> solve
    |> snd
    |> Seq.collect id
    |> Seq.map fst
    |> Seq.distinct
    |> Seq.length