#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO

let inputText =
    // Path.Combine(__SOURCE_DIRECTORY__, "test-inputs-2.txt") |> File.ReadAllText
    Path.Combine(__SOURCE_DIRECTORY__, "inputs.txt") |> File.ReadAllLines

module Common =

    let get2DArray array =
        let columnsCount = array |> Array.head |> Seq.length
        let rowsCount = Array.length inputText

        Array2D.init rowsCount columnsCount (fun ridx cidx -> inputText[ridx][cidx])

    let wordAndRev array =
        [| array; Array.rev array |] |> Array.map String

    let getSquareWords (word: string) (array: char[,]) =
        let wordLengthMinus1 = word.Length - 1

        let first =
            [| for n in 0 .. word.Length - 1 do
                   array[n, n] |]

        let second =
            [| for n in 0 .. word.Length - 1 do
                   array[wordLengthMinus1 - n, n] |]

        [| first; second |] |> Array.map wordAndRev |> Array.collect id


    let getSquares (word: string) array =
        let maxCol = (array |> Array2D.length2) - word.Length
        let maxRow = (array |> Array2D.length1) - word.Length

        [| for cidx in 0..maxCol do
               for ridx in 0..maxRow do
                   array[cidx .. cidx + word.Length - 1, ridx .. ridx + word.Length - 1] |]

    let getDiagWords (word: string) array =
        let getSquareWords = getSquareWords word
        let maxCol = (array |> Array2D.length2) - word.Length
        let maxRow = (array |> Array2D.length1) - word.Length

        [| for cidx in 0..maxCol do
               for ridx in 0..maxRow do
                   array[cidx .. cidx + word.Length - 1, ridx .. ridx + word.Length - 1] |]
        |> Array.map getSquareWords
        |> Array.collect id

module Answer1 =
    open Common
    let word = "XMAS"

    let getHorizontalWords array =
        let maxCol = (array |> Array2D.length2) - word.Length
        let maxRow = (array |> Array2D.length1) - 1

        [| for ridx in 0..maxRow do
               for cidx in 0..maxCol do
                   array[ridx, cidx .. cidx + word.Length - 1] |]
        |> Array.map wordAndRev
        |> Array.collect id

    let getVerticalWords array =
        let maxCol = (array |> Array2D.length2) - 1
        let maxRow = (array |> Array2D.length1) - word.Length

        [| for cidx in 0..maxCol do
               for ridx in 0..maxRow do
                   array[ridx .. ridx + word.Length - 1, cidx] |]
        |> Array.map wordAndRev
        |> Array.collect id

    let getAllWords array =
        let diagWords = getDiagWords word array
        let horizontalWords = getHorizontalWords array
        let verticalWords = getVerticalWords array

        seq {
            diagWords
            horizontalWords
            verticalWords
        }
        |> Array.concat

    let getAnswer () =
        inputText
        |> get2DArray
        |> getAllWords
        |> Array.filter (fun s -> s = word)
        |> Array.length


module Answer2 =
    open Common
    let word = "MAS"

    let isCrossMas array =
        let equalsWord s = s = word

        let hasTwoWord array =
            array |> Array.filter equalsWord |> Array.length = 2

        getSquareWords word array |> hasTwoWord

    let getAnswer () =
        let getSquares = getSquares word

        inputText |> get2DArray |> getSquares |> Array.filter isCrossMas |> Array.length

Answer1.getAnswer ()
Answer2.getAnswer ()
