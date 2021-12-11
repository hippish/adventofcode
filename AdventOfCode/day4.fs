module AdventOfCode.day4

open System
open System.Text.RegularExpressions


type BingoBoard = int option list list

let parseDrawnNumbers (numbers: string) =
    numbers.Split ',' |> Array.toList |> List.map int

let parseRow (row: string) =
    Regex.Split(row.Trim(), "\s+")
    |> Array.toList
    |> List.map int
    |> List.map Some

let parseBoard lines : BingoBoard = lines |> List.map parseRow

let getInputs filename =
    let lines =
        filename |> ReadFile.readLines |> Array.toList

    let first::rest = lines
    let drawnNumbers = parseDrawnNumbers first

    let boards =
        rest
        |> List.filter (fun line -> not <| String.IsNullOrEmpty(line))
        |> List.chunkBySize 5
        |> List.map parseBoard

    drawnNumbers, boards

let markHit drawnNumber boardNumber =
    match boardNumber with
    | None -> None
    | Some number ->
        if number = drawnNumber then
            None
        else
            Some number

let markRow number row =
    let mapper = markHit number
    row |> List.map mapper

let markBoard (number: int, board: BingoBoard) : BingoBoard =
    let mapper = markRow number
    board |> List.map mapper

let hasBingo rows =
    let cols = rows |> List.transpose
    let isBingo rc = rc |> List.choose id |> List.isEmpty

    let anyBingo rc =
        rc |> List.map isBingo |> List.contains true

    anyBingo rows || anyBingo cols

let calculateScore finalCall board =
    let result =
        board |> List.concat |> List.choose id |> List.sum

    result * finalCall

let rec playRound (draw: int list, bingoBoards: BingoBoard list) =
    let number::draws = draw

    let boards =
        bingoBoards
        |> List.map (fun board -> markBoard (number, board))

    let winners = boards |> List.filter hasBingo

    match winners with
    | [ winner ] -> calculateScore number winner
    | [] -> playRound (draws, boards)
    | _ -> failwith "multiple winners?"


let part1 filename = filename |> getInputs |> playRound
