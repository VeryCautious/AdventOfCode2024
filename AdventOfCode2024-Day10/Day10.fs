module Day10

open AdventOfCode_Utils

type Map = int[,]
type Coord = (int*int)*int

let readInput = FileU.read2DChars (string >> int)

let nextSteps (map:Map) (((x, y), h):Coord) =
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> Seq.map (fun (dx, dy) -> (x+dx, y+dy))
    |> Seq.filter (Array2DU.isOnMap map)
    |> Seq.map (fun (x, y) -> ((x, y), map[x,y]))
    |> Seq.filter (fun (_, h') -> h' = h+1)

let extendPath (map:Map) (path:Coord list) = nextSteps map (List.head path) |> Seq.map (fun step -> step::path) |> Seq.toList

let paths (map:Map) (startX, startY) =
    let startPath = [(startX, startY), map[startX, startY]]
    let folder =  (fun paths _ -> paths |> List.collect (extendPath map))
    Seq.fold folder [startPath] [1..9]

let score (paths: Coord list seq) = paths |> Seq.map (Seq.head >> fst) |> Seq.distinct |> Seq.length 

let solvePart1 fileName =
    let (field, map) = readInput fileName
    let startPositions = field |> Array.filter (snd >> (=) 0) |> Array.map fst |> Array.toList
    startPositions 
    |> List.map ((paths map) >> score)
    |> Seq.sum
