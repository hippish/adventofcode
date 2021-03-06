module AdventOfCode.day5

open System.Text.RegularExpressions

type Point = { x: int; y: int }
type Line = Point * Point

let mapLine line: Line =
    let replaced =
        Regex.Replace(line, "(\d+),(\d+) -> (\d+),(\d+)", "$1,$2,$3,$4")

    let numbers =
        replaced.Split ','
        |> Array.map int
        |> Array.toList

    numbers
    |> List.chunkBySize 2
    |> List.map (fun nums -> { x = nums[0]; y = nums[1] })
    |> function
        | a :: b :: _ -> a, b
        | _ -> failwith "unexpected list length"

let parseInputs filename =
    filename
    |> ReadFile.readLines
    |> Array.map mapLine


let isStraightLine line =
    match line with
    | (p1, p2) when p1.y = p2.y -> true
    | (p1, p2) when p1.x = p2.x -> true
    | _ -> false

let getDelta x y =
    if x > y then 1
    else if x < y then -1
    else 0

let rec getCoveredPoints points line =
    let p1,p2 = line
    let hits = p2 :: points
    let dx = getDelta p1.x p2.x
    let dy = getDelta p1.y p2.y

//    printfn "covered points %A" p2
    match line with
    | _ when p1 = p2 -> hits
    | _ -> getCoveredPoints hits (p1, { x= p2.x + dx; y = p2.y + dy})


let calculateOverlappingPoints filter lines =
    lines
    |> List.filter filter
    |> List.map (getCoveredPoints List.Empty)
    |> List.concat
    |> List.countBy id
    |> List.filter (fun (point, count) -> count > 1)
    |> List.length

let calc filename filter = filename |> parseInputs |> Array.toList |> calculateOverlappingPoints filter
let part1 filename = calc filename isStraightLine

let part2 filename = calc filename (fun _ -> true)
