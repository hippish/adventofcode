module AdventOfCode.day7

let parseCrabs filename =
    filename
    |> ReadFile.readLines
    |> Array.map (fun l -> l.Split ',' |> Array.map int)
    |> Array.concat
    |> Array.countBy id

let getPositions crabs =
    let min = crabs |> Array.minBy fst |> fst
    let max = crabs |> Array.maxBy fst |> fst

    [ for i = min to max do
          yield i ]

let calculateFuel costfn crabs position =
    crabs
    |> Array.fold (fun acc (pos, count) -> (costfn pos position) * count + acc) 0

let calculateIncFuelCost start goal =
    let min, max = if start > goal then goal, start else start, goal
    let mutable cost = 0
    for i = min to max-1 do
        cost <- cost + i-min+1
    cost

let calculateMinFuel costfn crabs =
    crabs
    |> getPositions
    |> List.map (calculateFuel costfn crabs)
    |> List.min


let part1 filename = filename |> parseCrabs |> calculateMinFuel (fun a b -> abs(a-b))

let part2 filename = filename |> parseCrabs |> calculateMinFuel calculateIncFuelCost