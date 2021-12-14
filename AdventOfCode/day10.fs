module AdventOfCode.day10

type Chunk =
    { Open: char
      Close: char
      Value: int
      Score: int64 }

let chunk o c v s =
    { Open = o
      Close = c
      Value = v
      Score = s }

let chunks =
    [ chunk '(' ')' 3 1
      chunk '[' ']' 57 2
      chunk '{' '}' 1197 3
      chunk '<' '>' 25137 4 ]

let findChunk ch =
    chunks
    |> List.find (fun c -> ch = c.Open || ch = c.Close)

let isCloser ch c = ch.Close = c
let isOpener ch c = ch.Open = c

let rec findFailValue (opened: Chunk list) remaining =
    match remaining with
    | [] -> (opened, 0) // we done
    | next :: rem ->
        match findChunk next with
        //        | None -> failwith "invalid char"
        | ch when isOpener ch next -> findFailValue (ch :: opened) rem
        | ch when isCloser ch next ->
            match opened with
            | [] -> (opened, ch.Value) // closing tag but nothings open
            | current :: rest when current = ch -> findFailValue rest rem
            | _ -> (opened, ch.Value)

let processLines filename =
    filename
    |> ReadFile.readLines
    |> Array.map (fun l -> l.ToCharArray() |> Array.toList)
    |> Array.map (findFailValue [])

let part1 filename =
    filename |> processLines |> Array.sumBy snd

let calculateScore (opened: Chunk list) =
    opened
    |> List.fold (fun acc chunk -> acc * 5L + chunk.Score) 0L

// SO cheating onoes
let secondHalf l =
    let rec aux xs ctr =
        match ctr with
        | _ :: _ :: ctr' ->
            match xs with
            | _ :: xs' -> aux xs' ctr'
            | [] -> failwith "should never happen!"
        | _ -> xs

    aux l l

let sortedMedian xs = let (m :: _) = secondHalf xs in m

let part2 filename =
    filename
    |> processLines
    |> Array.filter (fun (_, v) -> v = 0)
    |> Array.map fst
    |> Array.map calculateScore
    |> Array.sort
    |> Array.toList
    |> sortedMedian
