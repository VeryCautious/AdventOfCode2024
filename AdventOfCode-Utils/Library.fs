namespace AdventOfCode_Utils

module SeqU =
    let cartesian a b = a |> Seq.collect (fun x -> b |> Seq.map (fun y -> (x, y)))
