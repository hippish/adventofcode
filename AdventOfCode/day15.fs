module AdventOfCode.day15

open AdventOfCode.shared

let parseMap filename =
    filename
    |> ReadFile.readToIntChars
    |> toLocationMap

let getKeyMax map fn =
    map
    |> Map.keys
    |> Seq.map fn
    |> Seq.sort
    |> Seq.last

let getEndPoint map =
    let x = getKeyMax map fst
    let y = getKeyMax map snd

    Location (x, y)

let findPath map =
    let start = 0,0
    let goal = getEndPoint map
    let costFn loc = Map.tryFind loc map |> Option.defaultValue 0
    let estimatedCost loc = getManhattanDistance loc goal

    aStar map start goal costFn estimatedCost

let part1 filename = filename |> parseMap |> findPath