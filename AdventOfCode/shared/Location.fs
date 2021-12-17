module AdventOfCode.shared

open Microsoft.FSharp.Core

type Location = int * int

let toLocationMap array =
    array
    |> Array.mapi (fun i arr -> arr |> Array.mapi (fun j value -> (i, j), value))
    |> Array.collect id
    |> Map

let getLocationNeighbors (map: Map<Location, 'a>) (location: Location) =
    let coords = [ -1; 0; 1 ]
    let (x, y) = location

    coords
    |> List.map (fun a -> coords |> List.map (fun b -> (a + x, b + y)))
    |> List.concat
    |> List.filter (fun loc -> (loc = location) |> not)
    |> List.filter map.ContainsKey

let getLocationCardNeighbors (map: Map<Location, 'a>) (y, x) =
    let indexes =
        [ (y - 1, x) //up
          (y + 1, x) //down
          (y, x - 1) //left
          (y, x + 1) ] //right

    indexes |> List.filter map.ContainsKey

let square x = x * x

let getDistance ((y, x): Location) ((gy, gx): Location) =
    sqrt (float (square (gx - x)) + float (square (gy - y)))

let getManhattanDistance ((y, x): Location) ((gy, gx): Location) = abs (gx - x) + abs (gy - y)

type CostFn = Location -> int
type guessCostFn = Location -> int

type Path = Map<Location, Location * int>

let aStar (map: Map<Location, 'a>) (start: Location) (goal: Location) (costFn: CostFn) (guessCostFn: guessCostFn) =

    let addIfLower (value: int) (map: Map<Location, int>) location =
        Map.change
            location
            (fun v ->
                match v with
                | None -> Some value
                | Some x -> if value < x then Some value else v)
            map

    let reduceScores scoreFn (scoreMap: Map<Location, int>) (nodes: Location list) =
        nodes
        |> List.fold (fun m n -> addIfLower (scoreFn n) m n) scoreMap

    let getCost map node = Map.tryFind node map

    let addPath (map: Path) node parent cost =
        Map.change
            node
            (fun v ->
                match v with
                | None -> Some(parent, cost)
                | Some (_, value) ->
                    if value > cost then
                        Some(parent, cost)
                    else
                        v)
            map

    let rec travel (path: Path) edges (scores: Map<Location, int>) estimatedScores current =
        let neighbors = getLocationCardNeighbors map current
        let cost n = costFn n + scores.[current]

        let scores' = neighbors |> (reduceScores cost scores)

        let estimatedScores' =
            neighbors
            |> reduceScores (fun n -> (cost n) + (guessCostFn n)) estimatedScores

        let edges' =
            neighbors
            |> List.append edges
            |> List.filter (fun n -> n <> current && n <> start)
            |> List.filter (fun n -> Map.containsKey n path |> not)
            |> List.sortBy (getCost estimatedScores')

        match edges' with
        | head :: _ when head = goal -> path, scores'.[head]
        | head :: rem -> travel (addPath path head current scores'.[head]) rem scores' estimatedScores' head
        | [] -> failwith "no path found :("

    travel (Map [] ) [ start ] (Map [ start, 0 ]) (Map [ start, guessCostFn (start) ]) start
