module Day16

open AdventOfCode_Utils
open System.Collections.Generic

type Pos = (int*int)*(int*int)
type State = Pos*int

let rotate90clockw (x,y) = (y,-x)
let rotate90cclockw (x,y) = (-y,x)
let add (x,y) (a,b) = (a+x,b+y)

let parseInput fileName =
    let movementStenicle = FileU.read2DChars ((<>) '#') fileName |> snd
    let tiles = FileU.read2DChars id fileName |> fst
    let startPos = tiles |> Array.find (snd >> ((=) 'S')) |> fst
    let endPos = tiles |> Array.find (snd >> ((=) 'E')) |> fst
    (startPos, endPos, movementStenicle)

let canMoveTo movementStenicle (x,y) = Array2DU.isOnMap movementStenicle (x,y) && movementStenicle[x, y]
let extendPath (path:Pos list) ((p,i):Pos*int) = (p,i, p::path)

let findNextFor (movementStenicle:bool[,]) (((pos, dir), score):State) =
    let rots = 
        [((pos, rotate90clockw dir), score+1000); ((pos, rotate90cclockw dir), score+1000)]
        |> List.filter (fun ((pos, dir),_) -> canMoveTo movementStenicle (add pos dir))
    let newPos = add pos dir
    if canMoveTo movementStenicle newPos then
        (((newPos, dir), score+1))::rots
    else
        rots

let collectPaths (visited:Dictionary<Pos, List<Pos>>) (endState: State seq)  = 
    let q = new Queue<Pos>(endState |> Seq.map fst)
    let visitedPos = new HashSet<Pos>(endState |> Seq.map fst);
    while q.Count > 0 do
        let pos = q.Dequeue()
        if visited.ContainsKey(pos) && visited[pos].Count > 0 then
            visited[pos] |> Seq.iter (fun x -> if visitedPos.Add(x) then q.Enqueue(x))
    visitedPos |> Seq.map(fst)

let solve startPos endPos movementStenicle =
    let prev = new Dictionary<Pos, List<Pos>>()
    let queued = new Dictionary<Pos, int>()
    let q = new PriorityQueue<State*State option,int>()
    let endStates = new HashSet<State>()
    let mutable minScore = Microsoft.FSharp.Core.int.MaxValue
    q.Enqueue((((startPos, (1,0)), 0), None), 0)
    queued.Add((startPos, (1,0)),0)

    while q.Count > 0 && ((q.Peek() |> (fst>>snd)) <= minScore) do
        let (((pos, dir), score), prevState) = q.Dequeue()

        if score <= queued[(pos, dir)] then
            if prev.ContainsKey((pos, dir)) |> not then prev.Add((pos, dir), new  List<Pos>())
            if prevState.IsSome then prev[(pos, dir)].Add(fst prevState.Value)

            if pos = endPos then
                endStates.Add(((pos, dir), score)) |> ignore
                minScore <- min score minScore
            else
                let next = findNextFor movementStenicle ((pos, dir), score)
                next |> Seq.iter (fun ((nextPos, nextDir), nextScore) ->
                    let alreadyQueued = queued.ContainsKey((nextPos, nextDir))
                    if alreadyQueued |> not || queued[(nextPos, nextDir)] >= nextScore then
                        q.Enqueue((((nextPos, nextDir), nextScore), Some ((pos, dir), score)), nextScore)
                        if alreadyQueued then 
                            queued.Remove((nextPos, nextDir)) |> ignore
                        queued.Add((nextPos, nextDir), nextScore)    
                    )

    (minScore, (collectPaths prev endStates) |> Seq.distinct |> Seq.length )

let solvePart1 fileName = 
    parseInput fileName
    |||> solve
    |> fst

let solvePart2 fileName =
    parseInput fileName
    |||> solve
    |> snd