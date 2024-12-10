module Day10

type Map = int[,]
type Coord = (int*int)*int

let isOnMap (map:Map) (x,y) = x >= 0 && x < map.GetLength(0) && y >= 0 && y < map.GetLength(1)

let nextSteps (map:Map) (((x, y), h):Coord) =
    [(0,1);(0,-1);(1,0);(-1,0)]
    |> Seq.map (fun (dx, dy) -> (x+dx, y+dy))
    |> Seq.filter (isOnMap map)
    |> Seq.map (fun (x, y) -> ((x, y), map[x,y]))
    |> Seq.filter (fun (_, h') -> h' = h+1)

let extendPath (map:Map) (path:Coord list) = nextSteps map (List.head path) |> Seq.map (fun step -> step::path) |> Seq.toList

let paths (map:Map) (startX, startY) =
    let startCoord = (startX, startY), map[startX, startY]
    let startPath = [startCoord]
    let generator =  (fun paths x -> paths |> List.map (extendPath map) |> List.collect id)
    //Simulate 10 steps
    0

let solvePart1 fileName = 0