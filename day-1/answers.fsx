open System
open System.IO

let tuples =
    Path.Combine(__SOURCE_DIRECTORY__, "test-inputs.txt")
    |> File.ReadAllLines
    |> Array.map (fun l -> l.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun i -> int i[0], int i[1])

let firstList = tuples |> Array.map fst
let secondList = tuples |> Array.map snd

module Answer1 =
    let firstSortedArray = firstList |> Array.sort
    let secondSortedArray = secondList |> Array.sort

    let getAnswer () =
        Array.zip firstSortedArray secondSortedArray
        |> Array.map (fun (a, b) -> max b a - min b a)
        |> Array.sum

module Answer2 =
    let keysAndWeight = firstList |> Array.countBy id |> Map
    let valuesAndCount = secondList |> Array.countBy id |> Map

    let getAnswer () =
        keysAndWeight
        |> Seq.map (fun e ->
            match valuesAndCount |> Map.tryFind e.Key with
            | Some count -> e.Key * count * e.Value
            | _ -> 0)
        |> Seq.sum

Answer1.getAnswer ()
Answer2.getAnswer ()
