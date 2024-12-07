module Day01

type Row = { Left: int; Right: int }

let input = System.IO.File.ReadAllText("Day01/input.txt")

let parseRow(rowText: string): Row =
    let parts = rowText.Split("   ")
    let left = parts.[0].Trim() |> int
    let right = parts.[1].Trim() |> int
    { Left = left; Right = right }

let partOne () =
    let rows = input.Trim().Split("\n") |> Array.map parseRow
    let lefts = rows |> Array.map (_.Left) |> Array.sort
    let rights = rows |> Array.map (_.Right) |> Array.sort
    let zipped = Array.zip lefts rights
    let absoluteDifferences = zipped |> Array.map (fun (left, right) -> abs (left - right))
    let sumOfDifferences = absoluteDifferences |> Array.sum
    sumOfDifferences

let partTwo () =
    let rows = input.Trim().Split("\n") |> Array.map parseRow

    let lefts = rows |> Array.map (_.Left) |> Array.sort
    let rights = rows |> Array.map (_.Right) |> Array.sort

    let getNumberOfAppearancesInRights (left: int) =
        rights |> Array.filter (fun right -> right = left) |> Array.length

    let leftAppearancesInRights = lefts |> Array.map getNumberOfAppearancesInRights

    let zipped = Array.zip lefts leftAppearancesInRights

    let score = zipped |> Array.map (fun (left, appearances) -> left * appearances) |> Array.sum

    score
