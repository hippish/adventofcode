module AdventOfCode.day11

open AdventOfCode.shared

let bump (v: int option) =
    match v with
    | Some x -> Some(x + 1)
    | None -> None

let flash (map: Map<Location, int>) location =

    let rec inner (imap: Map<Location, int>) sparkling (remaining: Location list) =
        match remaining with
        | [] -> ( imap, sparkling)
        | next :: rem ->
            let updatedMap = Map.change next bump imap

            inner
                updatedMap
                (sparkling
                 @ if updatedMap.[next] = 10 then // append new flashés only when they first hit 10
                       [ next ]
                   else
                       [])
                rem

    inner map [] (getLocationNeighbors map location)


let flashOctopi map =
    let flashingLocations =
        map
        |> Map.filter (fun _ value -> value > 9)
        |> Map.toList
        |> List.map fst

    let rec inner imap flashes locations =
        match locations with
        | [] -> (flashes, imap)
        | oct :: remaining ->
            let (m, moar) = flash imap oct
            inner m (flashes+1) (remaining @ moar)

    inner map 0 flashingLocations

let resetFlashed map =
    map
    |> Map.map (fun loc value -> if value > 9 then 0 else value)

let updateOctopi prevFlashed (map: Map<Location, int>) =

    let (flashes, updatedMap) =
        map
        |> Map.map (fun _ v -> v + 1)
        |> flashOctopi

    flashes + prevFlashed, (updatedMap |> resetFlashed)

let doTheFlash array =
    let rec inner times (flashes, map) =
        match times with
        | x when times < 100 ->
            inner (x+1) (map |> updateOctopi flashes)
        | _ -> (flashes, map)

    let map = array |> toLocationMap
    inner 0 (0, map)

let part1 filename = filename |> ReadFile.readToIntChars |> doTheFlash