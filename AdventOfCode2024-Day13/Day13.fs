module Day13

open System.Text.RegularExpressions
open System.IO

type Button = int * int
type PrizePosition = int * int
type Machine = Button * Button * PrizePosition

let readInput fileName =
    let btnPattern = new Regex(@"^Button .: X\+(\d+), Y\+(\d+)$")
    let prizePattern = new Regex(@"^Prize: X=(\d+), Y=(\d+)$")
    let parseButton (s:string) = 
        let m = btnPattern.Match(s)
        (int m.Groups[1].Value, int m.Groups[2].Value)
    let parsePrize (s:string) =
        let m = prizePattern.Match(s)
        (int m.Groups[1].Value, int m.Groups[2].Value)
    File.ReadAllLines(fileName)
    |> Array.chunkBySize 4
    |> Array.map (fun a -> Machine (Button (parseButton a[0]), Button (parseButton a[1]), parsePrize a[2]))

let winnable (((x1, x2), (y1, y2), (c1, c2)):Machine) = 
    let det = x1*y2 - x2*y1
    if det = 0 then None else
        let x = (c1*y2 - c2*y1)
        let y = (x1*c2 - x2*c1)
        if x % det <> 0 || y % det <> 0 then None else Some (x / det, y / det)

let solvePart1 fileName = 
    readInput fileName
    |> Array.choose winnable
    |> Array.map (fun (x, y) -> 3 * x + y)
    |> Array.sum