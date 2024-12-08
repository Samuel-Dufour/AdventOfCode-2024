#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO
open System.Text.RegularExpressions
open FsToolkit.ErrorHandling

let inputText =
    // Path.Combine(__SOURCE_DIRECTORY__, "test-inputs-1.txt") |> File.ReadAllText
    Path.Combine(__SOURCE_DIRECTORY__, "test-inputs-2.txt") |> File.ReadAllText

module Common =
    let (|Regex|_|) pattern s =
        let m = Regex.Match(s, pattern)

        if m.Success then
            [ for e in m.Groups |> Seq.tail do
                  e.Value ]
            |> Some
        else
            None

    let (|MultiRegex|_|) pattern s =
        let m = Regex.Matches(s, pattern)

        if m.Count = 0 then
            None
        else
            [ for e in m do
                  e.Value ]
            |> Some

    let (|Int|_|) =
        function
        | Regex "(\d+)" [ d ] -> int d |> Some
        | _ -> None

    let (|Multiply|_|) =
        function
        | Regex "mul\((\d+),(\d+)\)" [ Int a; Int b ] -> (a, b) |> Some
        | _ -> None

    let multiplesSum = List.map (fun (a, b) -> a * b) >> List.sum

    let tryGetMultiply =
        function
        | Multiply m -> Some m
        | _ -> None

    let toMultiplies = List.map tryGetMultiply >> List.sequenceOptionM

module Answer1 =
    open Common

    let (|Multiplies|_|) =
        function
        | MultiRegex "mul\(\d+,\d+\)" e -> e |> Some
        | _ -> None

    let tryGetMultiplies =
        function
        | Multiplies m -> toMultiplies m
        | _ -> None

    let answer = inputText |> tryGetMultiplies |> Option.map multiplesSum


module Answer2 =
    open Common

    let (|Operations|_|) =
        function
        | MultiRegex "(mul\(\d+,\d+\)|do\(\)|don't\(\))" e -> e |> Some
        | _ -> None

    let (|Do|_|) =
        function
        | Regex "do()" _ -> Do |> Some
        | _ -> None

    let (|Dont|_|) =
        function
        | Regex "don't()" _ -> Dont |> Some
        | _ -> None

    let tryGetoperations =
        function
        | Operations m -> m |> Some
        | _ -> None

    let filterOperations =
        List.fold
            (fun (toKeep, acc) op ->
                match op, toKeep with
                | Do, _ -> true, acc
                | Dont, _ -> false, acc
                | _, true -> toKeep, op :: acc
                | _, false -> toKeep, acc)
            (true, List.empty)
        >> snd

    let answer =
        inputText
        |> tryGetoperations
        |> Option.bind (filterOperations >> toMultiplies)
        |> Option.map multiplesSum

Answer1.answer
Answer2.answer
