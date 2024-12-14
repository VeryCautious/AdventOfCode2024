module Day14

open System.Text.RegularExpressions
open System.IO
open AdventOfCode_Utils

type Bot = (int*int)*(int*int)

let readInput fileName : Bot array =
    let regex = new Regex("p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
    File.ReadAllLines fileName
    |> Array.map (fun l -> regex.Match l)
    |> Array.map (fun m -> ((int m.Groups[1].Value, int m.Groups[2].Value), (int m.Groups[3].Value, int m.Groups[4].Value)))

let simulate (width:int) (height:int) (steps:int) (((x,y),(vx, vy)):Bot) : Bot = 
    let newX = int ((int64 x+ int64 steps * int64 vx) % int64 width)
    let newY = int ((int64 y+ int64 steps* int64 vy) % int64 height)
    let fX = if newX >= 0 then newX else newX + width
    let fY = if newY >= 0 then newY else newY + height
    assert (fX >= 0)
    assert (fY >= 0)
    ((fX, fY),(vx, vy))

let quadrant (width:int) (height:int) (x:int,y:int) = 
    let halfW = width / 2
    let halfH = height / 2
    if x = halfW || y = halfH then
        -1
    else
        let a = if x < halfW then 0 else 1
        let b = if y < halfH then 0 else 2
        a + b

let solvePart1 fileName width height =
    readInput fileName
    |> Array.map (simulate width height 100 >> fst)
    |> Array.groupBy (quadrant width height)
    |> Array.filter (fst >> ((<=) 0))
    |> Array.map (snd >> Array.length)
    |> Array.fold (*) 1

let variance (bots:Bot array) =
    SeqU.cartesian bots bots
    |> Seq.map (fun (((x,y),_), ((x2,y2),_)) -> (x-x2) * (x-x2) + (y-y2) * (y-y2))
    |> Seq.sum

let solvePart2 fileName width height =
    let folder s = 
        let res = Array.map (simulate width height 1) s
        Some (res, res)
    readInput fileName
    |> Seq.unfold (folder)
    |> Seq.take 10000
    |> Seq.indexed
    |> Seq.minBy (fun (_, bots) -> variance bots)
    |> (fst >> ((+) 1))
    