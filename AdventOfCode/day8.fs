module AdventOfCode.day8

open Microsoft.FSharp.Collections

type Segment =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

type Digit = { num: int; segments: Segment list }

let parse filename =
    filename
    |> ReadFile.readMap
        (fun l ->
            l.Split '|'
            |> Array.map (fun i -> i.Trim())
            |> Array.toList)
    |> Array.toList

let isEasyDigit (input: string) =
    match input.Length with
    | 2 -> true // 1
    | 4 -> true // 4
    | 3 -> true // 7
    | 7 -> true // 8
    | _ -> false

let getDisplays (inputs: string list list) =
    inputs
    |> List.map (fun [ _; s ] -> s.Split ' ' |> Array.toList)
    |> List.concat
    |> List.filter isEasyDigit

let part1 filename =
    filename |> parse |> getDisplays |> List.length

let mapInputs (input: string) =
    input.Split ' '
    |> Array.map (fun i -> i.ToCharArray() |> Set.ofArray)


let getKeysVals line =
    match line with
    | key::vals::_ -> (key |> mapInputs, vals |> mapInputs)
    |_ -> failwith "invalid array"


let findSetByLength length (sets: Set<char> list) =
    sets |> List.find (fun set -> set.Count = length)

let findSetsByLength length (sets: Set<char> list) =
    sets |> List.filter (fun set -> set.Count = length)

let findPattern keys =
//    let cf = keys |> findSetByLength 2 // 1
    let acf = keys |> findSetByLength 3 // 7
    let bcdf = keys |> findSetByLength 4 // 4
    let abcdefg = keys |> findSetByLength 7 // 8

//    let six = keys |> findSetsByLength 6 |> List.find (fun set -> not <| Set.isProperSubset acf set)
    let ZeroNine = keys |> findSetsByLength 6 |> List.filter (Set.isProperSubset acf)
    let nine = ZeroNine |> List.find (Set.isProperSubset bcdf)
    let zero = ZeroNine |> List.find (fun set -> set <> nine)

//    let a = Set.difference acf cf
    let e = Set.difference abcdefg nine
    let d = Set.difference abcdefg zero

    (e, d, acf)


let decodeDigit pattern (digit: Set<char>) =
    let (e,d,acf) = pattern
    match digit.Count with
    | 2 -> "1"
    | 3 -> "7"
    | 4 -> "4"
    | 5 -> match digit with
            | d when Set.isSubset acf d -> "3"
            | d when Set.isSubset e d -> "2"
            | _ -> "5"
    | 6 -> match digit with
            | d when not <| Set.isSubset e d -> "9"
            | h when not <| Set.isSubset d h -> "0"
            | _ -> "6"
    | 7 -> "8"

let decode line =
    let keys, vals =
        line
        |> getKeysVals

    let pattern = keys |> Array.toList |>findPattern

    vals |> Array.map (decodeDigit pattern) |> Array.reduce (+)

let part2 filename = filename |> parse |> List.map decode |> List.map int |> List.sum


