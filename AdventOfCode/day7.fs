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

let calculateFuel crabs position =
    crabs
    |> Array.fold (fun acc (pos, count) -> abs(pos - position) * count + acc) 0

let calculateMinFuel crabs =
    crabs
    |> getPositions
    |> List.map (calculateFuel crabs)
    |> List.min


let part1 filename = filename |> parseCrabs |> calculateMinFuel
