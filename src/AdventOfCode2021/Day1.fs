module AdventOfCode2021.Day1

open System.IO

let measurements = File.ReadAllLines "Day1-input.txt"
let measurementWindows =
    measurements
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Array.sum
let result =
    measurementWindows
    |> Seq.fold (fun (lastDepth, increasedCount) depth ->
        match depth > lastDepth with
        | true -> (depth, increasedCount + 1)
        | false -> (depth, increasedCount)
        ) (Seq.head measurementWindows, 0)
    |> snd
printf $"%i{result}"