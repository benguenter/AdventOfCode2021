module AdventOfCode2021.Day2

open System
open System.IO

type Instruction =
    | Forward of int
    | Down of int
    | Up of int

module Instruction =
    let fromString (str: String) =
        let tmp = (str.Split [|' '|])
        match tmp with
        | [|command; value|] ->
            match command.ToLower() with
            | "forward" -> Forward <| int value
            | "down" -> Down <| int value
            | "up" -> Up <| int value
            | _ -> failwith $"Unexpected command: %s{command}"
        | _ -> failwith $"Exactly two parts are required for an Instruction to %s{str}"

type Position = {
    Horizontal: int
    Depth: int
    Aim: int
}

module Position =
    let change position = function
        | Forward i ->
            {
                position with
                    Horizontal = position.Horizontal + i
                    Depth = position.Depth + (position.Aim * i)
            }
        | Down i -> { position with Aim = position.Aim + i }
        | Up i -> { position with Aim = position.Aim - i }

let initialPosition = {
    Horizontal = 0
    Depth = 0
    Aim = 0
}

let instructions =
    File.ReadAllLines "src/AdventOfCode2021/Day2-input.txt"
    |> Seq.map Instruction.fromString

let finalPosition =
    instructions
    |> Seq.fold Position.change initialPosition

finalPosition.Horizontal * finalPosition.Depth