module Day1

open System.IO

let mapElem f (a, b) = (f a, f b)

let spreaded f (a,b) = f a b  

let zipWith f t =
    t
    |> spreaded Array.zip
    |> Array.map (spreaded f)


let parseLine (line:string) =
    let splitLine = line.Replace("   ", "%").Split("%")
    (int splitLine[0], int splitLine[1])


let count xs x =
    xs
    |> Array.filter ((=) x)
    |> Array.length


let day1Result name = 
    File.ReadAllLines name 
    |> Array.map parseLine
    |> Array.unzip
    |> (mapElem Array.sort)
    |> zipWith (-)
    |> Array.map abs
    |> Array.sum

let day1Result2 name = 
    File.ReadAllLines name
    |> Array.map parseLine
    |> Array.unzip
    |> (fun (x,y) -> x |> Array.map (fun z -> count y z |> (*) z ))
    |> Array.sum
