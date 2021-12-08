module AdventOfCode2021.Day5

open System.IO

type Coordinate = {
    X: int
    Y: int
}

module Coordinate =
    let create (string: string) =
        match string.Split(",") with
        | [|x; y|] -> { X = int x; Y = int y }
        | _ -> failwith $"unexpected input for coordinate {string}"
    let fromTuple (x, y) = { X = x; Y = y }

type Line = {
    Start: Coordinate
    End: Coordinate
}

module Line =
    let create (string: string) =
        match string.Split(" -> ") with
        | [|startC; endC|] ->
            { Start = Coordinate.create startC; End = Coordinate.create endC }
        | arr -> failwith $"unexpected input {arr}"

    let range start ``end`` =
        match start <= ``end`` with
        | true -> [start..``end``]
        | false -> [``end``..start] |> List.rev

    let isDiagonal line =
        not (line.Start.X = line.End.X)
        && not (line.Start.Y = line.End.Y)

    let isHorizontal line =
        line.Start.Y = line.End.Y

    let coordinates line =
        match isDiagonal line, isHorizontal line with
        | true, _ ->
            Seq.zip
                (range line.Start.X line.End.X)
                (range line.Start.Y line.End.Y)
        | _, true ->
            range line.Start.X line.End.X
            |> Seq.map (fun x -> x, line.Start.Y)
        | _, _ -> 
            range line.Start.Y line.End.Y
            |> Seq.map (fun y -> line.Start.X, y)
        |> Seq.map Coordinate.fromTuple

let lines =
    File.ReadAllLines "src/AdventOfCode2021/Day5-input.txt"
    |> Seq.map (
        Line.create
        >> Line.coordinates
    )
    |> Seq.concat
    |> Array.ofSeq
    |> Array.groupBy id
    |> Array.filter (snd >> Array.length >> ((<) 1))
    |> Array.length