module AdventOfCode.day10

type Chunk = { Open: char; Close: char; Value: int }

let chunk o c v = { Open = o; Close = c; Value = v }

let chunks =
    [ chunk '(' ')' 3
      chunk '[' ']' 57
      chunk '{' '}' 1197
      chunk '<' '>' 25137 ]

let findChunk ch =
    chunks
    |> List.find (fun c -> ch = c.Open || ch = c.Close)

let isCloser ch c = ch.Close = c
let isOpener ch c = ch.Open = c
let rec findFailValue (opened: Chunk list) remaining =
    match remaining with
    | [] -> 0 // we done
    | next::rem ->
        match findChunk next with
//        | None -> failwith "invalid char"
        |  ch when isOpener ch next -> findFailValue (ch::opened) rem
        |  ch when isCloser ch next ->
            match opened with
            | [] -> ch.Value // closing tag but nothings open
            | current::rest when current = ch -> findFailValue rest rem
            | _ -> ch.Value

let getValue (lines: string[]) =
    lines
    |> Array.map (fun l -> l.ToCharArray() |> Array.toList)
    |> Array.map (findFailValue [])
    |> Array.sum
let part1 filename = filename |> ReadFile.readLines |> getValue
