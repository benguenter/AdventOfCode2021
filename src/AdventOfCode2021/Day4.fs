module AdventOfCode2021.Day4

open System.IO

let zip5 (input: 'a[][]) =
    let res = Array.zeroCreate input.Length
    for i = 0 to res.Length-1 do 
        res.[i] <- Array.map (fun arr -> Array.get arr i) input
    res
        

type BingoNumber = {
    Value: int
    Marked: bool
}

module BingoNumber =
    let isMarked number = number.Marked
    let value number = number.Value
    let create number = { Value = number; Marked = false }
    let maybeMark numberToMark bn =
        if bn.Value = numberToMark then
            { bn with Marked = true }
        else bn

type BingoBoard = {
    Rows: BingoNumber [][]
    Columns: BingoNumber [][]
}

module BingoBoard =
    let create (numbers: string[][]) =
        let rows =
            numbers
            |> Array.map (Array.map (int >> BingoNumber.create))
        let columns = zip5 rows
        { Rows = rows; Columns = columns; }

    let checkWinner board =
        board.Rows |> Array.exists (Array.forall BingoNumber.isMarked)
        || board.Columns |> Array.exists (Array.forall BingoNumber.isMarked)
        
    let calculateWinningValue board finalNumber =
        board.Rows
        |> Seq.concat
        |> Seq.filter (BingoNumber.isMarked >> not)
        |> Seq.sumBy BingoNumber.value
        |> (*) finalNumber

    let markNumbers set board =
        let rows =
            board.Rows
            |> Array.map (Array.map (BingoNumber.maybeMark set))
        let columns = zip5 rows
        { Rows = rows; Columns = columns }

let lines = File.ReadAllLines "src/AdventOfCode2021/Day4-input.txt"

let numberSets =
    Array.head lines
    |> (fun s -> s.Split(','))
    |> Array.map int
let rawBoards =
    lines
    |> Array.skip 1
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map (fun l ->
        l.Split(' ')
        |> Array.filter (System.String.IsNullOrWhiteSpace >> not))
    |> Seq.chunkBySize 5
    |> Array.ofSeq
let boards =
    rawBoards
    |> Array.ofSeq
    |> Array.map BingoBoard.create

let rec apply boards drawnNumbers index =
    let numberDrawn = Array.get drawnNumbers index
    let boards = boards |> Array.map (BingoBoard.markNumbers numberDrawn)
    let notWinningBoards = boards |> Array.filter (BingoBoard.checkWinner >> not)

    match Seq.isEmpty notWinningBoards with
    | true -> Array.head boards, numberDrawn
    | false -> apply notWinningBoards drawnNumbers (index + 1)

apply boards numberSets 0
||> BingoBoard.calculateWinningValue