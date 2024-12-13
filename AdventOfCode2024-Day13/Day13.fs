module Day13

open System.Text.RegularExpressions
open System.IO

type Button = int64 * int64
type PrizePosition = int64 * int64
type Machine = Button * Button * PrizePosition

let readInput fileName (dx,dy) =
    let btnPattern = new Regex(@"^Button .: X\+(\d+), Y\+(\d+)$")
    let prizePattern = new Regex(@"^Prize: X=(\d+), Y=(\d+)$")
    let parseButton (s:string) = 
        let m = btnPattern.Match(s)
        (int64 m.Groups[1].Value, int64 m.Groups[2].Value)
    let parsePrize (s:string) =
        let m = prizePattern.Match(s)
        (int64 m.Groups[1].Value |> (+) dx, int64 m.Groups[2].Value |> (+) dy)
    File.ReadAllLines(fileName)
    |> Array.chunkBySize 4
    |> Array.map (fun a -> Machine (Button (parseButton a[0]), Button (parseButton a[1]), parsePrize a[2]))

let winnable (((x1, x2), (y1, y2), (c1, c2)):Machine) = 
    let det = x1*y2 - x2*y1
    if det = 0L then None else
        let x = (c1*y2 - c2*y1)
        let y = (x1*c2 - x2*c1)
        if x % det <> 0L || y % det <> 0L then None else Some (x / det, y / det)

let solve fileName offset = 
    readInput fileName offset
    |> Array.choose winnable
    |> Array.map (fun (x, y) -> 3L * x + y)
    |> Array.sum

let solvePart1 fileName = solve fileName (0L,0L)
let solvePart2 fileName = solve fileName (10000000000000L,10000000000000L)