module AdventOfCode.day14

open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

type Instruction = string * char

let parseInsertions (lines: string list) =
    let regex = Regex("(\\w+) -> (\\w)")

    let text =
        lines
        |> List.fold (fun (acc: StringBuilder) -> acc.AppendLine) (StringBuilder())

    let matches = regex.Matches(text.ToString())

    [ for hit in matches do
          yield Instruction(hit.Groups.[1].Value, hit.Groups.[2].Value.ToCharArray().[0]) ]

let parseFile filename =
    let lines =
        ReadFile.readLines filename |> List.ofArray

    match lines with
    | head :: _ :: instructions -> (head, parseInsertions instructions)
    | _ -> failwith "unexpected format"

let findInstruction (instructions: Instruction list) (a, b) =
    instructions
    |> List.tryFind (fun (h, _) -> h = $"{a}{b}")

let applyFormula (forms: Instruction list) (current: string) =
    let updated =
        current.ToCharArray()
        |> Array.pairwise
        |> Array.fold
            (fun acc (a, b) ->
                match findInstruction forms (a, b) with
                | Some (_, i) -> $"{acc}{a}{i}"
                | None -> $"{acc}{a}")
            ""

    $"{updated}{current.ToCharArray() |> Array.last}"

let applySteps steps (start, forms) =
    [ 1 .. steps ]
    |> List.fold (fun acc _ -> applyFormula forms acc) start

let calculateScore (polymer: string) =
    let occurrences =
        polymer.ToCharArray()
        |> Array.countBy id
        |> Array.map snd
        |> Array.sort

    Array.last occurrences - Array.head occurrences

let part1 filename =
    filename
    |> parseFile
    |> applySteps 10
    |> calculateScore

let getPairMap (start: string) =
    start
    |> Seq.pairwise
    |> Seq.countBy id
    |> Seq.map (fun ((a,b), c) -> $"{a}{b}", int64 c)
    |> Map

let updateMap key value (map: Map<'k, int64>) =
    let v =
        map.TryFind key |> Option.defaultValue 0L

    map.Add(key, value + v)

let getPairs (key: string) (insert: char) =
    let  a :: b :: _ = key.ToCharArray() |> Array.toList
    let ap = $"{a}{insert}"
    let bp = $"{insert}{b}"

    [ ap; bp ]

let applyInstruction (p, insert) value chars pairs =
    let ps = getPairs p insert

    let uPairs =
        ps
        |> List.fold (fun map pair -> updateMap pair value map) pairs

    (chars |> updateMap insert value), uPairs

let applyInstructions instructions (chars, pairs: Map<string, int64>) =
    pairs
    |> Map.fold
        (fun (c, p) k v ->
            match findInstruction instructions (k.[0], k.[1]) with
            | None -> (c, p)
            | Some ins -> applyInstruction ins v c p)
        (chars, Map.empty)

let solveP2 steps (start, instructions) =
    let pairmap = getPairMap start
    let chars = start.ToCharArray() |> Array.countBy id |> Array.map (fun (c,v) -> c, int64 v) |> Map
    [1..steps]
    |> List.fold (fun charsPairs _ -> applyInstructions instructions charsPairs) (chars, pairmap)

let calculateP2Score (chars, _) =
    let count = chars |> Map.values

    (count |> Seq.max) - (count |> Seq.min)
let part2 filename =
    filename
    |> parseFile
    |> solveP2 40
    |> calculateP2Score