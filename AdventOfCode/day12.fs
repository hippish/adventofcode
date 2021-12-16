module AdventOfCode.day12

open System

//module LargeCave =
//    type T = LargeCave of string
//    let create (s:string) =
//        if s.ToCharArray() |> Array.forall Char.IsUpper then
//            Some (LargeCave s)
//        else
//            None
//    let apply f (LargeCave s) = f s
//    let value s = apply id s
//
//open LargeCave
//
//module SmallCave =
//    type T = SmallCave of string
//    let create (s:string) =
//        if s.ToCharArray() |> Array.forall Char.IsUpper then
//            None
//        else
//            Some (SmallCave s)
//    let apply f (SmallCave s) = f s
//    let value s = apply id s

//type SmallCave = { value: string; Small: int }
//type LargeCave = { value: string; Large: int }
type Node =
    | SmallCave of string
    | LargeCave of string

type Edge = Node * Node
type Graph = Set<Node> * Edge list

let makeNode (s: string) =
    if s.ToCharArray() |> Array.forall Char.IsUpper then
        LargeCave s
    else
        SmallCave s

let lineToGraph (line: string): Graph =
    match List.ofArray (line.Split '-') with
    | a::b::_ ->
        let a' = makeNode a
        let b' = makeNode b
        (Set.ofList [a';b']), [(a',b')]
    | _ -> failwith "woops"

let parseGraph filename =
    filename
    |> ReadFile.readMap lineToGraph
    |> Array.fold (fun (nodes, edges) (n,e) -> (Set.union nodes n), edges @ e) (Set.ofList [],[])

let findPaths (edges: Edge list) (node: Node) =
    edges
    |> List.filter (fun (a,b) -> a = node || b = node)
    |> List.map (fun (a,b) -> if a = node then b else a)

let isValidPath (visited: Node list) (node: Node) =
    match node with
    | SmallCave s when s = "start" -> List.contains (SmallCave s) visited |> not
    | SmallCave s when List.contains (SmallCave s) visited -> false
    | _ -> true

let travel (graph: Graph) =
    let (nodes, edges) = graph
    let start = nodes |> Seq.find (fun n -> n = SmallCave "start")

    let rec walk visited node = [
        let paths =
            node
            |> findPaths edges
            |> List.filter (isValidPath visited)
            |> Set.ofList
            |> Set.toList
        match node with
        | SmallCave s when s = "end" -> yield node::visited
        | _ ->
            match paths with
            | [] -> yield []
            | _ ->
                for p in  paths |> List.map (walk (node::visited)) do
                    for p' in p do
                        yield p'

    ]

    walk [] start

let findAllPaths graph =
    let paths =
        graph
        |> travel
        |> List.filter (fun l -> List.isEmpty l |> not)
        |> List.map List.rev

    paths.Length, paths
let part1 filename = filename |> parseGraph |> findAllPaths