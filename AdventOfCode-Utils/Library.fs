namespace AdventOfCode_Utils

module SeqU =
    let cartesian a b = a |> Seq.collect (fun x -> b |> Seq.map (fun y -> (x, y)))

module FileU =
    let read2DChars charMapper fileName =
        let field =
            System.IO.File.ReadAllLines fileName
            |> Array.mapi (fun x line -> line.ToCharArray() |> Array.mapi (fun y c -> ((x, y), charMapper c)))
            |> Array.collect id
        let width = field |> Array.map (fun ((x,_),_) -> x) |> Array.max |> (+) 1
        let height = field |> Array.map (fun ((_,y),_) -> y) |> Array.max |> (+) 1
        let map = Array2D.init width height (fun x y -> field |> Array.find (fun ((x', y'), _) -> x = x' && y = y') |> snd)
        (field, map)

module Array2DU =
    let isOnMap (map:'a[,]) (x,y) = x >= 0 && x < map.GetLength(0) && y >= 0 && y < map.GetLength(1)