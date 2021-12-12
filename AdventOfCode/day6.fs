module AdventOfCode.day6

open System


type Fish = { timer: int }

let makeFish time = { timer = time }

let parseFishies filename =
    filename
    |> ReadFile.readLines
    |> Array.map (fun f -> f.Split ',' |> Array.map int)
    |> Array.concat
    |> Array.map makeFish
    |> Array.toList

let ageFish fish =
    match fish with
    | f when f.timer = 0 -> [{f with timer = 6}; {timer = 8}]
    | f -> [{f with timer = f.timer - 1}]

let rec breedFish times fish =
    let bred = fish |> List.map ageFish |> List.concat
    match times with
    | 0 -> fish
    | _ -> breedFish (times - 1) bred

let calculateFishes filename times = filename |> parseFishies |> breedFish times |> List.length
let part1 filename = calculateFishes filename 80
let part2 filename = calculateFishes filename 256