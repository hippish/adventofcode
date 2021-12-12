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


let updateFishes (fishes: int64[]) =
    let breeding = fishes[0]
    let mutable updated = [|0L..8L|];
    for i = 1 to 8 do
        updated[i-1] <- fishes[i]

    updated[6] <- breeding + updated[6]
    updated[8] <- breeding

    updated

let createInitialArray (fishies: (int * int)[]) =
    let getCount index =
        fishies
        |> Array.tryFind (fun (age, count) -> index = age)
        |> function
        | Some (age, count) -> count |> int64
        | None -> 0L

    [|0..8|]
    |> Array.map getCount

let parseFish filename =
    filename
    |> ReadFile.readLines
    |> Array.map (fun f -> f.Split ',' |> Array.map int)
    |> Array.concat
    |> Array.countBy id
    |> createInitialArray

let rec calculateFish times fish =
    let bred = fish |> updateFishes
    match times with
    | 0 -> fish
    | _ -> calculateFish (times - 1) bred

let part2 filename = filename |> parseFish |> calculateFish 256 |> Array.sum