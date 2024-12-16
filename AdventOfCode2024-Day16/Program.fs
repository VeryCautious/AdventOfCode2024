open Day16

module Program = 
    let [<EntryPoint>] main _ = 
        let res = solvePart2 "Data16.txt"
        printfn "final res: %d" res
        0
