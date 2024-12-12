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
        let map = readInput "Data12-Example4.txt"
        let print = printEdges (map.GetLength 0) (map.GetLength 1)
        let groups = 
            findGroups map 
            |> Seq.map (fun g -> (findEdges g, g))
            |> Seq.map (fun (e,g) -> (e, consolidatedEdges g))
        groups |> Seq.iter (fun (x,y) -> 
            print x
            printfn "%d" y
            printfn "<->")
        0
