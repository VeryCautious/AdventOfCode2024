module Day11

open System.Collections
open System.Collections.Generic

let readInput fileName = (System.IO.File.ReadAllText fileName).Split(" ") |> Array.map (fun n -> (uint64 n, 1UL)) |> Map

let digitCount (n:uint64) = n |> float |> log10 |> floor |> int |> (+) 1
let hasEvenDigitCount (n:uint64) = (digitCount n) % 2 = 0
let split (n:uint64) = n |> string |> (fun s -> [s.Substring(0, s.Length / 2); s.Substring(s.Length / 2)]) |> List.map uint64

let nextStep n =
    match n with
    | 0UL -> [1UL]
    | _ -> if hasEvenDigitCount(n) then split n else [n * 2024UL]

let add (stones: Generic.IDictionary<uint64, uint64>) (v:uint64) (c:uint64) = if stones.ContainsKey(v) then stones[v] <- (c + stones[v]) else stones.Add(v, c)
let simulateStep (stones: IDictionary<uint64, uint64>) =
    let newStones = new Dictionary<uint64, uint64>()
    Seq.iter (fun k -> nextStep k |> List.iter (fun newV -> add newStones newV stones[k])) stones.Keys
    newStones :> IDictionary<uint64, uint64>

let simulate (steps:int) (stones: IDictionary<uint64, uint64>) =
    List.fold (fun s _ -> simulateStep s) stones [1..steps]

let amount (stones: Generic.IDictionary<uint64, uint64>) = stones.Values |> Seq.map uint64 |> Seq.sum

let solvePart1 fileName =
    readInput fileName
    |> simulate 25
    |> amount

let solvePart2 fileName =
    readInput fileName
    |> simulate 75
    |> amount