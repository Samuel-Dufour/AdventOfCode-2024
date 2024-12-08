open System
open System.IO

let arrays =
    Path.Combine(__SOURCE_DIRECTORY__, "test-inputs.txt")
    |> File.ReadAllLines
    |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

module Answer1 =
    let arrayIsOk array =
        let betweenOneAndThree n = 0 < abs n && abs n < 4
        let toDiff = Array.pairwise >> Array.map (fun (fst, snd) -> snd - fst)
        let diffArray = toDiff array
        let valuesAreOk = diffArray |> Array.forall betweenOneAndThree
        let positives, negatives = diffArray |> Array.partition (fun n -> n > 0)
        valuesAreOk && (Array.isEmpty positives || Array.isEmpty negatives)


    let getResponse () =
        arrays |> Array.filter arrayIsOk |> Array.length


module Answer2 =
    let statuses array =
        let rec loop toTest idx =
            if Answer1.arrayIsOk toTest then
                true
            elif idx >= Array.length array then
                false
            else
                let array = Array.removeAt idx array
                loop array (idx + 1)

        loop array 0


    let getAnswer () =
        arrays |> Array.filter statuses |> Array.length

Answer1.getResponse ()
