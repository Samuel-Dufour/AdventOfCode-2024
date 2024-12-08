#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO
open System.Text.RegularExpressions

let inputLines =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs.txt") |> File.ReadAllLines

type OrderingRule = (int * int)
type UpdatePages = int array

type State =
    { rules: OrderingRule array
      updates: UpdatePages array }

module Common =
    let (|Regex|_|) pattern s =
        let m = Regex.Match(s, pattern)

        if m.Success then
            [ for e in m.Groups |> Seq.tail do
                  e.Value ]
            |> Some
        else
            None

    let (|Int|_|) =
        function
        | Regex "(\d+)" [ d ] -> int d |> Some
        | _ -> None

    type Line =
        | PageOrderingRule of (int * int)
        | UpdatePages of int array

    let (|PageOrderingRuleDefinition|_|) =
        function
        | Regex "(\d+)\|(\d+)" [ Int a; Int b ] -> PageOrderingRule(a, b) |> Some
        | _ -> None

    let (|UpdatePagesDefinition|_|) (s: string) =
        match s.Split(",", StringSplitOptions.RemoveEmptyEntries) with
        | [||] -> None
        | pages -> pages |> Array.map int |> UpdatePages |> Some

    let toTypedLines lines =
        let mapper s =
            match s with
            | PageOrderingRuleDefinition a -> Some a
            | UpdatePagesDefinition a -> Some a
            | _ -> None

        lines |> Array.map mapper |> Array.choose id

    let toState lines =
        let typeLines = toTypedLines lines

        let pageRuleChooser =
            function
            | PageOrderingRule a -> Some a
            | _ -> None

        let updatePagesChooser =
            function
            | Line.UpdatePages a -> Some a
            | _ -> None

        { rules = typeLines |> Array.choose pageRuleChooser
          updates = typeLines |> Array.choose updatePagesChooser }

    let updateIsCorrect rules update =
        let tryFindIndex i = Array.tryFindIndex (fun n -> n = i)

        let rulePageIsFollowed (a, b) =
            match tryFindIndex a update, tryFindIndex b update with
            | Some fstIdx, Some sndIdx -> fstIdx < sndIdx
            | _ -> true

        rules |> Array.forall rulePageIsFollowed

    let findMiddleValue update =
        let middle = Array.length update / 2
        update[middle]

module Answer1 =
    open Common

    let findCorrectUpdates { updates = updates; rules = rules } =
        updates |> Array.filter (updateIsCorrect rules)

    let findCorrectUpdatesMiddle state =
        state |> findCorrectUpdates |> Array.map findMiddleValue

    let getAnswer () =
        let state = inputLines |> toState
        state |> findCorrectUpdatesMiddle |> Array.sum


module Answer2 =
    open Common

    let findIncorrectUpdates { updates = updates; rules = rules } =
        updates |> Array.filter (updateIsCorrect rules >> not)

    let fixUpdate rules update =
        let ruleIsInUpdate (a, b) =
            update |> Array.contains a && update |> Array.contains b

        let rulesForUpdate = rules |> Array.filter ruleIsInUpdate

        let findValueNeverAtRight =
            Seq.find (fun n -> rules |> Array.forall (fun (_, b) -> n <> b))

        let removeFromArray toRemove = Array.filter (fun n -> n <> toRemove)

        let removeFromTupleArray toRemove =
            Array.filter (fun (a, b) -> a <> toRemove && b <> toRemove)

        let rec loop update rules result =
            match update with
            | [||] -> result
            | _ ->
                let left = findValueNeverAtRight update
                let update = removeFromArray left update
                let rules = removeFromTupleArray left rules
                let result = left :: result
                loop update rules result

        loop update rulesForUpdate [] |> List.rev |> List.toArray

    let fixUpdates rules = Array.map (fixUpdate rules)

    let findMiddleValues = Array.map findMiddleValue

    let getAnswer () =
        let state = inputLines |> toState
        let fixUpdates = fixUpdates state.rules
        state |> findIncorrectUpdates |> fixUpdates |> findMiddleValues |> Array.sum

Answer1.getAnswer ()
Answer2.getAnswer ()
