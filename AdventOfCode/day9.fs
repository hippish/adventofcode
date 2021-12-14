module AdventOfCode.day9

open System.Collections.Generic

let parse filename =
    filename
    |> ReadFile.readMap
        (fun line ->
            line.ToCharArray()
            |> Array.map string
            |> Array.map int)

let tryGetIndex i (array: 'a []) =
    match i with
    | x when 0 <= i && i < array.Length -> Some array.[x]
    | _ -> None

let tryGetNeighbor array (i, j) =
    match tryGetIndex i array with
    | Some x -> tryGetIndex j x
    | None -> None

let findNeighbors y x (matrix: int [] []) =
    let indexes =
        [ (y - 1, x) //up
          (y + 1, x) //down
          (y, x - 1) //left
          (y, x + 1) ] //right

    indexes
    |> List.map (tryGetNeighbor matrix)
    |> List.choose id

let getLocalCost i j (matrix: int [] []) =
    let local = matrix.[i].[j]

    findNeighbors i j matrix
    |> List.filter (fun x -> x <= local)
    |> function
        | [] -> Some(local + 1)
        | _ -> None

let calculateMinRisk (array: int [] []) =
    array
    |> Array.mapi
        (fun i arr ->
            arr
            |> Array.mapi (fun j _ -> getLocalCost i j array)
            |> Array.choose id)
    |> Array.concat
    |> Array.sum

let part1 filename = filename |> parse |> calculateMinRisk

type Location = int * int

let toLocationMap array =
    array
    |> Array.mapi (fun i arr -> arr |> Array.mapi (fun j value -> (i,j), value))
    |> Array.collect id
    |> Map

let neighbors (y, x) =
        [ (y - 1, x) //up
          (y + 1, x) //down
          (y, x - 1) //left
          (y, x + 1) ] //right

let rec fillBasin (map: Map<Location, int>) (visited: Set<Location>) location =
    let updated = Set.add location visited

    match location with
    | loc when map.ContainsKey loc |> not -> visited
    | loc when map.[loc] >= 9 -> visited
    | loc -> loc
             |> neighbors
             |> List.filter (fun l -> (visited.Contains l) |> not && map.ContainsKey l)
             |> List.map (fillBasin map updated)
             |> List.fold Set.union updated

let findBasins (map: Map<Location, int>) =
    let mutable basins: Set<Location> list = [];
    for k in map.Keys do
        if not <| List.exists (Set.contains k) basins then
            basins <- (fillBasin map Set.empty k) :: basins
    basins

let getBasinProduct basins =
    basins
    |> List.map Set.count
    |> List.sortDescending
    |> List.take(3)
    |> List.fold (*) 1

let part2 filename = filename |> parse |> toLocationMap |> findBasins |> getBasinProduct