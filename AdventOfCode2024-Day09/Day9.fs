module Day9

open System.Collections

let gauss n = (n * (n + 1)) / 2
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

let toBucketed data =
    data |> Array.mapi (fun i v -> (v, if i % 2 = 0 then Some (i / 2) else None)) |> Array.where (fun (x, _) -> x > 0)

let mergedRemove (index:int) (data: (int*(int option)) array) =
    let (currSize, _) = data[index]
    let beforeSize = if index > 0 && snd(data[index - 1]).IsNone then fst data.[index - 1] |> Some else None
    let afterSize = if index < (data.Length - 1) && snd(data[index + 1]).IsNone then fst data[index + 1] |> Some else None
    let wholeSize = [beforeSize; afterSize; Some currSize ] |> List.choose id |> List.sum
    let newFree = (wholeSize, None)
    let updated = data |> Array.updateAt index newFree
    let x = if afterSize.IsSome then updated |> Array.removeAt (index + 1) else updated
    if beforeSize.IsSome then x |> Array.removeAt (index - 1) else x

let mergedInsert (index:int) (newSize:int, newValue:int option) (data: (int*(int option)) array) =
    let (size, _) = data[index]
    let remaining = size - newSize
    if remaining > 0 then
        data |> Array.updateAt index (remaining, None) |> Array.insertAt index (newSize, newValue)
    else
        data |> Array.updateAt index (newSize, newValue)

let moveForward (data: (int*(int option)) array) (size, id: int) =
    data |> Array.iter (fun x -> assert (fst(x) > 0))
    let newIndex = data |> Seq.tryFindIndex (fun (x, id) -> id = None && x >= size)
    let oldIndex = data |> Seq.findIndex (fun (_, i) -> i = Some id)
    if newIndex.IsNone || newIndex.Value >= oldIndex then data else
    data |> mergedRemove oldIndex |> mergedInsert newIndex.Value (size, Some id)

let compressWhole (data: (int*(int option)) array) =
    let fileIds = data |> Array.where (fun (_, i) -> i.IsSome) |> Array.map (fun (size, i) -> (size, i.Value)) |> Array.rev
    let res = Array.fold moveForward data fileIds
    res |> Array.windowed 2 |> Array.iter (fun x -> assert not (snd(x[0]).IsNone && snd(x[1]).IsNone))
    res

let contribution (v:int) (i:int) (size:int) =
    if i = 0 then
        (gauss size) * v |> uint64
    else
        (gauss (i+size-1)) - (gauss (i-1))
        |> (*) v
        |> uint64

let compressedCheckSum (data: (int*(int option)) array) =
    Array.fold (fun (i, acc) (size, v: int option) -> (i+size, if v.IsSome then acc + contribution v.Value i size else acc)) (0, 0UL) data |> snd

let solvePart2 fileName =
    readInput fileName
    |> toBucketed
    |> compressWhole
    |> compressedCheckSum
    