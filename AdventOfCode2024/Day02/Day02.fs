module Day02

let input = System.IO.File.ReadAllText("Day02/input.txt").Trim()

let inputRows = input.Split("\n")

type Level = int
type Report = Level array

let parseRow (row: string): Report =
    row.Split(" ") |> Array.map int

let reports = inputRows |> Array.map parseRow

let getStepChanges (report: Report): int array =
    let shifted = report |> Array.skip 1
    let withoutLast = report |> Array.take (Array.length report - 1)
    let zipped = Array.zip withoutLast shifted
    let stepChanges = zipped |> Array.map (fun (a, b) -> b - a)
    stepChanges

let isStrictlyIncreasing (report: Report): bool =
    report |> getStepChanges |> Array.forall (fun x -> x > 0)

let isStrictlyDecreasing (report: Report): bool =
    report |> getStepChanges |> Array.forall (fun x -> x < 0)

let absoluteStepChangesAreWithin (report: Report) (max: int): bool =
    report |> getStepChanges |> Array.map abs |> Array.forall (fun x -> x <= max)

let isSafe (report: Report): bool =
    (isStrictlyIncreasing report || isStrictlyDecreasing report) && absoluteStepChangesAreWithin report 3

let withoutElementAtIndex (report: Report) (index: int): Report =
    let indexArray = [|0..Array.length report - 1|]
    let withIndices = Array.zip indexArray report
    let withoutIndex = withIndices |> Array.filter (fun (i, _) -> i <> index)
    withoutIndex |> Array.map snd // second element of tuple

let getCombinationsWithOneElementRemoved (report: Report): Report array =
    let indexArray = [|0..Array.length report - 1|]
    indexArray |> Array.map (withoutElementAtIndex report)

let isSafe2 (report: Report): bool =
    if isSafe report then true
    else
        let combinations = getCombinationsWithOneElementRemoved report
        combinations |> Array.exists isSafe

let partOne () =
    reports |> Array.filter isSafe |> Array.length

let partTwo () =
    reports |> Array.filter isSafe2 |> Array.length
