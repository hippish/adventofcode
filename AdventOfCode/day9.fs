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
