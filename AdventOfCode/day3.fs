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
let getSignificantBit length sum = if sum > length / 2 then 1 else 0

let bitsToInt (bits) =
    bits
    |> Array.fold (fun acc inp -> (acc <<< 1) + inp) 0

let findCommonBit (inputs: int [] []) =
    let lenght = inputs.Length

    inputs |> Array.transpose |> Array.map Array.sum
    |> Array.map (getSignificantBit lenght)

let findGammaEpsilon inputs =
    let gamma = inputs |> findCommonBit |> bitsToInt
    let epsilon = flipmask ^^^ gamma
    gamma, epsilon


let getPowerConsumption (gamma,epsilon) = gamma * epsilon
let part1 filename = filename |> getInput |> findGammaEpsilon |> getPowerConsumption
