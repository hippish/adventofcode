module AdventOfCode.Main

open AdventOfCode
open Day3

let binaryDisplayTuple tuple =
    match tuple with
    | (a,b) -> printfn "%B, %B" a b

let  [<EntryPoint>] main args =
    "day3" |> part1 |> printfn "power consumption: %A"
    0