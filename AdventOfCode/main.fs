module AdventOfCode.Main

open AdventOfCode
open Day3

let binaryDisplayTuple tuple =
    match tuple with
    | (a,b) -> printfn "%B, %B" a b

let  [<EntryPoint>] main args =
    "day3" |> Day3.part1 |> printfn "power consumption: %A"
    "day3" |> Day3.part2 |> printfn "life support rating %A"
    "day4" |> AdventOfCode.day4.part1 |> printfn "bingo score %A"
    "day4" |> AdventOfCode.day4.part2 |> printfn "bingo loser score %A"
    "day5" |> day5.part1 |> printfn "d5 %A"
    0