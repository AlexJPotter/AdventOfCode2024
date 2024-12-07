module Day05

let input = System.IO.File.ReadAllText("Day05/input.txt").Trim()

let inputRows = input.Split("\n") |> Array.filter (fun x -> x.Length > 0)

type PageNumber = int

type Rule = { x: PageNumber; y: PageNumber }

type Update = PageNumber[]

type State = { rules: Rule[]; updates: Update[] }

let parseRule (inputRow: string): Rule =
    let parts = inputRow.Split("|")
    { x = parts[0] |> int; y = parts[1] |> int }

let parseUpdate (inputRow: string): Update =
    inputRow.Split(",") |> Array.map (fun x -> x |> int)

let getNext (state: State) (inputRow: string) =
    if inputRow.Contains("|") then
        let rule = parseRule inputRow
        { state with rules = Array.append state.rules [| rule |] }
    else
        let update = parseUpdate inputRow
        { state with updates = Array.append state.updates [| update |] }

let state = inputRows |> Array.fold getNext { rules = [||]; updates = [||] }

let meetsRule (update: Update) (rule: Rule) =
    let updateHasX = update |> Array.exists (fun x -> x = rule.x)
    let updateHasY = update |> Array.exists (fun y -> y = rule.y)

    if (not updateHasX) || (not updateHasY) then
        true
    else
        let firstIndexOfX = update |> Array.findIndex (fun x -> x = rule.x)
        let firstIndexOfY = update |> Array.findIndex (fun y -> y = rule.y)
        firstIndexOfX < firstIndexOfY

let getMiddlePageNumber (update: Update): PageNumber =
    let middleIndex = (update.Length - 1) / 2
    update[middleIndex]

let partOne () =
    state.updates
    |> Array.filter (fun u -> state.rules |> Array.forall (meetsRule u))
    |> Array.map getMiddlePageNumber
    |> Array.sum

let getDependentRules (rule: Rule): Rule[] =
    state.rules |> Array.filter (fun r -> r.x = rule.y)

let getRestrictedSubsequentNumbers (number: int): int[] =
    state.rules
    |> Array.filter (fun r -> r.y = number)
    |> Array.map _.x
    |> Array.distinct

type ReorderState = { picked: int[]; remaining: int[] }

let reorderUpdate (update: Update) =
    let getNext (state: ReorderState): ReorderState =
        let isInRemaining (x: int) =
            state.remaining |> Array.exists (fun y -> y = x)

        let restrictedForRemaining =
            state.remaining
            |> Array.map getRestrictedSubsequentNumbers
            |> Array.map (fun x -> x |> Array.filter isInRemaining)

        let index = restrictedForRemaining |> Array.findIndex (fun x -> x.Length = 0)
        let pickedNumber = state.remaining[index]

        let nextPicked = Array.append state.picked [| pickedNumber |]
        let nextRemaining = state.remaining |> Array.filter (fun x -> x <> pickedNumber)

        { picked = nextPicked; remaining = nextRemaining }

    let initialState = { picked = [||]; remaining = update }

    let finalState = [|0..(update.Length - 1)|] |> Array.fold (fun s _ -> getNext s) initialState

    finalState.picked

let partTwo () =
    state.updates
    |> Array.filter (fun u -> not (state.rules |> Array.forall (meetsRule u)))
    |> Array.map reorderUpdate
    |> Array.map getMiddlePageNumber
    |> Array.sum
