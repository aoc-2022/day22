open System.IO
open System

type Foo(s:string) =
    member this.S = s
    override this.ToString() = $"Foo({s})"

let parse (s:string) : Foo =
    match s.Split [|'ø'|] |> Array.toList with
    | s :: rest -> Foo(s)

let input = File.ReadAllLines "/tmp/aoc/input" |> Seq.map parse |> Seq.toList
            
input |> List.map (printfn "%A")    
    