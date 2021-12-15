module AdventOfCode.shared

type Location = int * int

let toLocationMap array =
    array
    |> Array.mapi (fun i arr -> arr |> Array.mapi (fun j value -> (i,j), value))
    |> Array.collect id
    |> Map

let getLocationNeighbors (map: Map<Location, 'a>) (location: Location) =
    let coords = [-1;0;+1]
    let (x,y) = location

    coords
    |> List.map (fun a -> coords |> List.map (fun b -> (a+x, b+y)))
    |> List.concat
    |> List.filter (fun loc -> (loc = location) |> not)
    |> List.filter map.ContainsKey