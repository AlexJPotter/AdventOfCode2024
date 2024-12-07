module Day03

let input = System.IO.File.ReadAllText("Day03/input.txt").Trim()

let inputRows = input.Split("\n")

let getMulInstructions (text: string): string array =
    let regex = System.Text.RegularExpressions.Regex("mul\((\d+),(\d+)\)")
    let matches = regex.Matches(text)
    let stringMatches = matches |> Seq.map _.Value
    stringMatches |> Seq.toArray

let getAllInstructions (text: string): string array =
    let regex = System.Text.RegularExpressions.Regex("mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
    let matches = regex.Matches(text)
    let stringMatches = matches |> Seq.map _.Value
    stringMatches |> Seq.toArray

let isDoInstruction (instruction: string): bool =
    instruction = "do()"

let isDontInstruction (instruction: string): bool =
    instruction = "don't()"

let getResultFromMulInstruction (instruction: string): int =
    let insideParentheses = instruction[4..^1]
    let numbers = insideParentheses.Split(",")
    let x = int numbers[0]
    let y = int numbers[1]
    x * y

let partOne () =
    let mulInstructions = inputRows |> Array.map getMulInstructions |> Array.concat
    let results = mulInstructions |> Array.map getResultFromMulInstruction
    results |> Array.sum

type State = { Enabled: int; Sum: int }

let partTwo () =
    let allInstructions = inputRows |> Array.map getAllInstructions |> Array.concat

    let enabledInitial: int = 1
    let sumInitial: int = 0
    let initialState = { Enabled = enabledInitial; Sum = sumInitial }

    let getNext (state: State) (instruction: string): State =
        if isDoInstruction instruction then { state with Enabled = 1 }
        elif isDontInstruction instruction then { state with Enabled = 0 }
        else if state.Enabled = 1 then { state with Sum = state.Sum + getResultFromMulInstruction instruction }
        else state

    let finalState = allInstructions |> Array.fold getNext initialState

    finalState.Sum
