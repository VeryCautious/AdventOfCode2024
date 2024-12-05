module Day5

type SortRules = Map<int, int array>

let parseRule (rule: string) = 
    let parts = rule.Split("|") |> Array.map int
    match parts with
    | [|lower; higher|] -> (lower, higher)
    | _ -> failwith "Invalid rule"

let parseList (list: string) = 
    list.Split(",") |> Array.map int

let readInput fileName = 
    let chunks = System.IO.File.ReadAllText(fileName).ReplaceLineEndings("$").Split("$$")
    match chunks with
    | [|chunk1; chunk2|] -> (chunk1, chunk2)
    | _ -> failwith "Invalid input"
    |> (fun (c1, c2) -> c1.Split "$" |> Array.map parseRule, c2.Split "$" |> Array.map parseList)

let createRules (rules: (int * int) array) = 
    rules  |> Array.groupBy fst |> Array.map (fun (k, v) -> k, v |> Array.map snd) |> Map.ofArray

let compair (rules: SortRules) (a: int) (b: int) =
    let aSmaller = rules.ContainsKey a && Array.contains b rules[a]
    let bSmaller = rules.ContainsKey b && Array.contains a rules[b]
    match aSmaller, bSmaller with
    | true, false -> -1
    | false, true -> 1
    | false, false -> 0
    | true, true -> failwith "Invalid rules"

let rec compairTransitive (rules: SortRules) (a: int) (b: int) = 
    let rec localCompairTransitive (rules: SortRules) (x: int) (y: int) =
        let comp = compair rules x y
        match comp with
        | 0 -> 
            if not (rules.ContainsKey x) then
                None
            else
                rules[x] 
                |> Array.map (fun nx -> localCompairTransitive rules nx y)
                |> Array.tryFind (fun x -> x.IsSome && x.Value <> 0)
                |> (fun opt -> if opt.IsSome then opt.Value else None)
        | _ -> Some(comp)
    let normalComp = localCompairTransitive rules a b
    let reverseComp = localCompairTransitive rules b a
    match normalComp, reverseComp with
    | Some(comp), _ -> comp
    | _, Some(comp) -> -comp
    | _, _ -> 0

let sort (rules: SortRules) (values: int array) =
    values |> Array.sortWith (compairTransitive rules)

let isSorted (rules: SortRules) (values: int array) = 
    let sorted = sort rules values
    values = sorted

let getMedianOfSorted (values: int array) = 
    let length = values.Length
    let middle = length / 2
    values[middle]

let solvePart1 fileName = 
    let rules, values = readInput fileName
    let rulesMap = createRules rules
    values
    |> Array.filter (isSorted rulesMap)
    |> Array.map getMedianOfSorted
    |> Array.sum

let solvePart2 fileName = 
    let rules, values = readInput fileName
    let rulesMap = createRules rules
    values
    |> Array.filter (isSorted rulesMap >> not)
    |> Array.map (sort rulesMap)
    |> Array.map getMedianOfSorted
    |> Array.sum