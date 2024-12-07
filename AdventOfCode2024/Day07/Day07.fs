module Day07

let input = System.IO.File.ReadAllText("Day07/input.txt").Trim()

let inputRows = input.Split("\n") |> Array.filter (fun x -> x.Length > 0)

type Inputs = bigint[]

type Equation = { testValue: bigint; inputValues: Inputs }

type Operator = Add | Multiply | Concat

type OperatorCollection = Operator[]

type Calculation = { operator: Operator; value: bigint }

let parseBigInt (value: string): bigint =
    bigint.Parse(value)

let parseRow (row: string): Equation =
    let parts = row.Split(": ")
    let testValue = parts[0].Trim() |> parseBigInt
    let inputValues = parts[1].Trim().Split(" ") |> Array.map parseBigInt
    { testValue = testValue; inputValues = inputValues }

let equations = inputRows |> Array.map parseRow

let calculate (equation: Equation) (operators: Operator[]): bigint =
    let inputValues = equation.inputValues

    if operators.Length <> inputValues.Length - 1 then
        failwith "Invalid number of operators"

    let getNext (current: bigint) (calculation: Calculation): bigint =
        match calculation.operator with
        | Add -> current + calculation.value
        | Multiply -> current * calculation.value
        | Concat -> bigint.Parse(current.ToString() + calculation.value.ToString())

    let calculations =
        Array.zip operators inputValues[1..]
        |> Array.map (fun (op, value) -> { operator = op; value = value })

    calculations |> Array.fold getNext inputValues[0]


let rec getOperatorPermutations (length: int) (allOperators: Operator[]): OperatorCollection[] =
    if length = 1 then
        allOperators |> Array.map (fun x -> [| x |])
    else
        getOperatorPermutations (length - 1) allOperators
        |> Array.collect (fun p ->
            allOperators |> Array.map (fun o -> [| o |] |> Array.append p)
        )

let couldBeTrue (allOperators: Operator[]) (equation: Equation): bool =
    getOperatorPermutations (equation.inputValues.Length - 1) allOperators
    |> Array.exists (fun operators -> calculate equation operators = equation.testValue)

let partOne() =
    let truthFn = couldBeTrue [| Add; Multiply |]
    equations |> Array.filter truthFn |> Array.map _.testValue |> Array.sum

let partTwo() =
    let truthFn = couldBeTrue [| Add; Multiply; Concat |]
    equations |> Array.filter truthFn |> Array.map _.testValue |> Array.sum
