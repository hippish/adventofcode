module AdventOfCode.ReadFile

open System.IO

let readLines filename =
    Path.Combine("./Input/", filename)
    |> File.ReadAllLines

let readMap mapper filename =
    filename |> readLines |> Array.map mapper