namespace AdventOfCode_Utils

module SeqU =
    let cartesian a b = a |> Seq.collect (fun x -> b |> Seq.map (fun y -> (x, y)))
   
module StringU =
    let chunkByNewLine (str:string) = str.Replace("\r","").Split("\n\n")

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

type Vector(x: int, y : int) =
    member this.x = x
    member this.y = y
    static member (~-) (v : Vector) =
        Vector(-v.x, -v.y)
    static member (*) (v : Vector, a) =
        Vector(a * v.x, a * v.y)
    static member (*) (a, v: Vector) =
        Vector(a * v.x, a * v.y)
    static member (+) (v1 : Vector, v2 : Vector) =
        Vector(v1.x + v2.x, v1.y + v2.y)
    static member (-) (v1 : Vector, v2 : Vector) =
        Vector(v1.x - v2.x, v1.y - v2.y)
    static member from (x, y) = Vector(x, y)
    override this.Equals (obj: obj): bool =
        match obj with
        | :? Vector as v -> v.x = this.x && v.y = this.y
        | _ -> false
    override this.GetHashCode() =
        this.x.GetHashCode() + 7 * this.y.GetHashCode()
    override this.ToString() =
        this.x.ToString() + " " + this.y.ToString()