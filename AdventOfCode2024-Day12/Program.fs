open Day12

module Program = 
    
    let matches (x,y) (edge:Edge) =
        match edge with
        | Lower (a,b) -> x = a && y = b
        | Right (a,b) -> x = a && y = b

    let printEdges width heigth (edges:Edge list) =
        for y in [-1..heigth] do
            for x in [-1..width] do
                let c = List.tryFind (matches (x,y)) edges
                let str = 
                    match c with
                    |None -> "  "
                    | Some (Lower _) -> "__"
                    | Some (Right _) -> " |"
                printf "%s" str
            printfn ""          
        ()
    
    let [<EntryPoint>] main _ = 
        let map = readInput "Data12-Example.txt"
        let print = printEdges (map.GetLength 0) (map.GetLength 1)
        let groups = findGroups map |> Seq.map findEdges
        groups |> Seq.iter print
        0
