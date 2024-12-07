module Day7

type Operation = Add | Mul

let addOneMul (operations:Operation List) =
    let n = operations.Length
    [0..n-1]
    |> List.filter (fun i -> operations[i] <> Mul)
    |> List.map (fun i -> List.updateAt i Mul operations)

let operatorCombinations (n:int) =
    let rec combinations (n:int) (mMuls:int) =
        if mMuls = 0 then
            [[List.init n (fun _ -> Add)]]
        else
            let old = combinations n (mMuls-1)
            let newComb = old |> List.last |> List.collect addOneMul |> List.distinct
            old @ [newComb]
    combinations n n

let eval (values:int64 array) (operations:Operation list) =
    let zip = Array.zip values (List.toArray (Add::operations))
    zip
    |> Array.fold (fun acc (v, o) -> 
        match o with
            | Add -> acc + v
            | Mul -> acc * v
        ) 0L

let isSolvable (values: int64 array) (target: int64) =
    operatorCombinations (values.Length-1)
    |> List.map (List.map (fun ops -> (eval values ops, Array.contains 1L values)))
    |> List.takeWhile (List.exists (fun (ev,hasOne) -> ev <= target || hasOne))
    |> List.map (List.map fst)
    |> List.exists (List.exists ((=) target))

let parseLine (line:string) =
    let split = line.Split(": ")
    match split with
        | [|target;values|] -> 
            (int64 target, values.Split(' ') |> Array.map int64)
        | _ -> failwith "Invalid input"

let parseInput fileName =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map parseLine

let solvePart1 fileName =
    parseInput fileName
    |> Array.filter (fun (target, values) -> isSolvable values target)
    |> Array.map fst
    |> Array.sum