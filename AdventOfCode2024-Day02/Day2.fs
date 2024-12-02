module Day2

open System.IO

let toPair xs = 
    match xs with
    | (x :: y :: _) -> (x, y)
    | _ -> failwith "Is not a 2 window"
let allMatch p xs = Seq.filter p xs |> Seq.length |> (=) (Seq.length xs)
let anyMatch p xs = Seq.filter p xs |> Seq.length |> (<) 0
let pairWindow xs = 
    xs
    |> Seq.windowed 2
    |> Seq.map (Seq.toList >> toPair)
let valuesAreBetween lower upper xs =
    Seq.max xs <= upper && Seq.min xs >= lower

type Report = 
    { Levels: int seq }
    static member getLevels (report: Report) = report.Levels
    static member isSafe (report: Report) = Report.isMonoton report && Report.hasSlowChange report
    static member isMonoton (report: Report) =
        report.Levels
        |> pairWindow
        |> Seq.map (fun (x,y) -> x < y)
        |> Seq.distinct
        |> Seq.length
        |> (=) 1
    static member hasSlowChange (report: Report) =
        report.Levels
        |> pairWindow
        |> Seq.map (fun (x,y) -> abs(x - y))
        |> valuesAreBetween 1 3
    static member fromLine (str:string) = str.Split(" ") |> Seq.map int |> (fun lvls -> { Levels = lvls })
    static member dampeningOptions (report:Report) =
        report.Levels
        |> Seq.mapi (fun i _ -> { Levels = Seq.removeAt i report.Levels })

let day2Result name = 
    File.ReadAllLines name 
    |> Seq.map Report.fromLine
    |> Seq.filter Report.isSafe
    |> Seq.length

let day2ResultPart2 name = 
    File.ReadAllLines name 
    |> Seq.map Report.fromLine
    |> Seq.map (Report.dampeningOptions >> (anyMatch Report.isSafe))
    |> Seq.filter id
    |> Seq.length