module AdventOfCode.Day3

open System
open AdventOfCode
open ReadFile

let textToIntArray (text: string) =
    text.ToCharArray()
    |> Array.map string
    |> Array.map Convert.ToInt32

let getInput filename =
    filename |> readLines |> Array.map textToIntArray

let flipmask = 0b111111111111
let getSignificantBit length sum = if sum * 2 >= length then 1 else 0

let bitsToInt (bits) =
    bits
    |> Array.fold (fun acc inp -> (acc <<< 1) + inp) 0

let findCommonBit (inputs: int [] []) =
    let length = inputs.Length

    inputs |> Array.transpose |> Array.map Array.sum
    |> Array.map (getSignificantBit length)

let findGammaEpsilon inputs =
    let gamma = inputs |> findCommonBit |> bitsToInt
    let epsilon = flipmask ^^^ gamma
    gamma, epsilon


let getPowerConsumption (gamma,epsilon) = gamma * epsilon

let part1 filename = filename |> getInput |> findGammaEpsilon |> getPowerConsumption


let bitComparator rating =
    match rating with
    | "Oxygen" -> fun a b -> a = b
    | "CO2" -> fun a b -> a <> b
    | _ -> failwith "invalid rating type"

// part2
let rec findCandidates comparator position candidates =
    match candidates with
    | [||] -> failwith "no candidates left"
    | [| result |] -> result
    | candidates ->
        let commonBits = candidates |> findCommonBit
//        printfn "position %A, common %A, candidates %A" position commonBits candidates
        candidates |> Array.filter (fun candidate -> comparator candidate[position] commonBits[position])
        |> findCandidates comparator (position + 1)

let findRating rating inputs =
    inputs
    |> findCandidates (bitComparator rating) 0
    |> bitsToInt


let findLifeSupportRating inputs =
    (inputs |> findRating "CO2") * (inputs |> findRating "Oxygen")
let part2 filename = filename |> getInput |> findLifeSupportRating