module AdventOfCode2021.Day3


open System
open System.IO

type Bit = Zero | One

module Bit =
    let fromInt = function
        | 0 -> Zero
        | 1 -> One
        | i -> failwith $"unexpected input %i{i}"

type Binary = string

module Binary =
    let toInt i = Convert.ToInt32(i, 2)

type BitCount = {
    BitZeroCount: int
    BitOneCount: int
}

module BitCount =
    let gamma bitCount =
        if bitCount.BitZeroCount > bitCount.BitOneCount then
            Zero else One

    let epsilon bitCount =
        if bitCount.BitZeroCount <= bitCount.BitOneCount then
            Zero else One

    let create index strings =
        let atIndex i (str: String) = str.Substring(i, 1)
        let bits =
            strings
            |> Seq.map ((atIndex index) >> int >> Bit.fromInt)
            |> List.ofSeq
        let zeroes, ones = bits |> List.partition (function | Zero -> true | One -> false)
        {
            BitZeroCount = List.length zeroes
            BitOneCount = List.length ones
        }

type DiagnosticReport = Binary list

module DiagnosticReport =
    let atIndex i (str: String) = str.Substring(i, 1)
    let rec filter index report bitToMatch =
        let matchingBit = report |> BitCount.create index |> bitToMatch
        let result =
            report
            |> Seq.filter (
                (atIndex index)
                >> int
                >> Bit.fromInt
                >> ((=) matchingBit))
        result
        |> Seq.tryExactlyOne
        |> Option.defaultWith
            (fun () -> filter (index + 1) result bitToMatch)
        
        
    let rating report bitToMatch =
        filter 0 report bitToMatch
        |> Binary.toInt


let binaryNumbers = File.ReadAllLines "src/AdventOfCode2021/Day3-input.txt"

let gammaRate = DiagnosticReport.rating binaryNumbers BitCount.gamma
let epsilonRate = DiagnosticReport.rating binaryNumbers BitCount.epsilon

gammaRate * epsilonRate
