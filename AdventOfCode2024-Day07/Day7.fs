module Day7

type Operation = Add | Mul | Concat

let prepend xs x = x::xs

let rec generateLists (operators: Operation list) n =
    if n = 0 then [[]] else generateLists operators (n - 1) |> List.collect (fun lst -> operators |> List.map (prepend lst))

let eval (values:int64 array) (operations:Operation list) =
    Array.zip values (List.toArray (Add::operations))
    |> Array.fold (fun acc (v, o) -> 
        match o with
            | Add -> acc + v
            | Mul -> acc * v
            | Concat -> string(acc) + string(v) |> int64
        ) 0L

let isSolvable (operators:Operation list) (target: int64) (values: int64 array) =
    generateLists operators (values.Length-1)
    |> List.map (fun ops -> (eval values ops, Array.contains 1L values))
    |> List.map fst
    |> List.exists ((=) target)

let parseLine (line:string) =
    let split = line.Split(": ")
    match split with
        | [|target;values|] -> 
            (int64 target, values.Split(' ') |> Array.map int64)
        | _ -> failwith "Invalid input"

let parseInput fileName =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map parseLine

let calibrateWith (operators:Operation list) fileName =
    parseInput fileName
    |> Array.filter (fun x -> x ||> (isSolvable operators))
    |> Array.map fst
    |> Array.sum

let solvePart1 = calibrateWith [Add;Mul]
let solvePart2 = calibrateWith [Add;Mul;Concat]