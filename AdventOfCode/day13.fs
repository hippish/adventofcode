module AdventOfCode.day13

open System.Text.RegularExpressions


let parseInstruction line =
    let regex = Regex("^fold along (\\w)=(\\d+)$")
    let matches = regex.Matches line

    matches.[0].Groups.[1].Value, matches.[0].Groups.[2].Value |> int

let parseMap (points: (int * int) list) (line: string) =
    match line.Split ',' |> Array.map int |> Array.toList with
    | x :: y :: _ -> (x, y) :: points
    | _ -> failwith "boom"

let parse filename =
    let lines =
        filename |> ReadFile.readLines |> Array.toList

    let i =
        lines |> List.findIndex (fun l -> l = "")

    let m, (_ :: ins) = List.splitAt i lines

    m |> List.fold parseMap [], ins |> List.map parseInstruction

let foldAt instruction set (x, y) =
    match instruction with
    | at, fx when at = "x" ->
        if fx > x then
            Set.add (x, y) set
        else
            Set.add (abs (x - fx * 2), y) set
    | at, fy when at = "y" ->
        if fy > y then
            Set.add (x, y) set
        else
            Set.add (x, abs (y - fy * 2)) set
    | _ -> failwith "wat"

let foldFirst (map, instructions: (string * int) list) =
    List.fold (foldAt instructions.[0]) Set.empty map

let part1 filename =
    filename |> parse |> foldFirst |> Set.count

let foldIns map instruction =
    map |> Set.fold (foldAt instruction) Set.empty

let fold (map: (int * int) list, instructions) =
    instructions |> List.fold foldIns (Set.ofList map)

let getMax fn set =
    set
    |> Set.toList
    |> List.map fn
    |> List.sort
    |> List.last

let getChar set (x, y) =
    if Set.contains (x, y) set then
        "#"
    else
        " "

let draw set =
    let xMax = set |> getMax fst
    let yMax = set |> getMax snd

    [ 0 .. yMax ]
    |> List.fold
        (fun lines y ->
            (List.fold (fun acc x -> $"{acc}{getChar set (x, y)}") "" [ 0 .. xMax ])
            :: lines)
        []
    |> List.rev

let part2 fn = fn |> parse |> fold |> draw
