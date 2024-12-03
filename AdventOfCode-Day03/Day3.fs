module Day3

open System.Text.RegularExpressions

type Operation = Mul of int * int | Do | Dont

let readMemory fileName = System.IO.File.ReadAllText(fileName).Replace("\n", "")

let enabledMulRegex = new Regex("(?:mul\((\d{1,3}),(\d{1,3})\))|(?:do\(\))|(?:don't\(\))")

let groupValues (m: Match) = m.Groups.Values |> Seq.tail |> Seq.map (fun g -> int g.Value)
let mapToOperation (m: Match) = 
    match m.Groups.[0].Value with
    | "don't()" -> Dont
    | "do()" -> Do
    | _ -> Mul(groupValues m |> Seq.head, groupValues m |> Seq.skip 1 |> Seq.head)

let extractMuls str = 
    str
    |> enabledMulRegex.Matches
    |> Seq.map mapToOperation

let int (b:bool) = if b then 1 else 0
let applyOperations noDeactivation ops =
    let rec applyOperations' ops (acc, enabled) = 
        match ops with
        | [] -> (acc, enabled)
        | Do::tl -> applyOperations' tl (acc, true)
        | Dont::tl -> applyOperations' tl (acc, false || noDeactivation)
        | Mul(a, b)::tl -> applyOperations' tl (acc + a * b * (int enabled), enabled)
    applyOperations' (Seq.toList ops) (0, true)
    |> fst

let solve fileName =
    readMemory fileName
    |> extractMuls
    |> applyOperations true

let solveWithDeactivation fileName =
    readMemory fileName
    |> extractMuls
    |> applyOperations false