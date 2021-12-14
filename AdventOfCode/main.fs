module AdventOfCode.Main

open AdventOfCode
open Day3

let binaryDisplayTuple tuple =
    match tuple with
    | (a,b) -> printfn "%B, %B" a b

let  [<EntryPoint>] main args =
//    "day3" |> Day3.part1 |> printfn "power consumption: %A"
//    "day3" |> Day3.part2 |> printfn "life support rating %A"
//    "day4" |> AdventOfCode.day4.part1 |> printfn "bingo score %A"
//    "day4" |> AdventOfCode.day4.part2 |> printfn "bingo loser score %A"
//    "day5-test" |> day5.part1 |> printfn "d5 points p1 %A"
//    "day5" |> day5.part2 |> printfn "d5 points p2 %A"
//    "day6" |> day6.part1 |> printfn "d6 fishies %A"
//    "day6" |> day6.part2 |> printfn "d6 lotsafishies %A"
//    "day7" |> day7.part1 |> printfn "d7 crabs %A"
//    "day7" |> day7.part2 |> printfn "d7 costly crabs %A"
//    "day8" |> day8.part1 |> printfn "d8 %A"
//    "day8" |> day8.part2 |> printfn "d8 %A"
    "day9" |> day9.part1 |> printfn "d9 %A"
    "day9" |> day9.part2 |> printfn "d9 p2 %A"
    0