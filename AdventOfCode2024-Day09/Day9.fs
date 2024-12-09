module Day9

open System.Collections

let readInput fileName = System.IO.File.ReadAllText fileName |> (fun s -> s.ToCharArray()) |> Array.map (fun c -> c |> string |> int)
let getEvenIndexesOf data = data |> Array.mapi (fun i v -> if (i % 2 = 0) then Some(v) else None) |> Array.choose id
let fileSizes = getEvenIndexesOf

let extractValues data = 
    let values = fileSizes data
    let folder xs (i, v) = (xs @ (List.replicate v i))
    Seq.fold folder List.empty (values |> Array.mapi (fun i x -> (i, x)))

let take (q:Queue) n = seq { for i in 1..n do yield q.Dequeue() :?> int } |> List.ofSeq


let compress data =
    let values = new Queue(extractValues data |> List.toArray)
    let revValues = new Queue(extractValues data |> List.rev |> List.toArray)
    let folder q1 q2 xs (even, segSize) =
        let q = if even then q1 else q2
        xs @ (take q segSize)
    let compressed = Seq.fold (folder values revValues) [] (data |> Array.mapi (fun i x -> (i % 2 = 0, x)))
    let totalFileSize = fileSizes data |> Array.sum
    compressed |> List.take totalFileSize

let checkSum data = data |> Seq.mapi (fun i v -> uint64 (i * v)) |> Seq.sum

let solvePart1 fileName =
    readInput fileName
    |> compress
    |> checkSum
    