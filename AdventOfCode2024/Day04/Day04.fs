module Day04

let input = System.IO.File.ReadAllText("Day04/input.txt").Trim()

let inputRows = input.Split("\n")

type GridValue = char
type Grid = GridValue[][]

let grid = inputRows |> Array.map _.ToCharArray()

let yMax = grid.Length - 1
let xMax = grid[0].Length - 1

type Vector2 = { x: int; y: int }

let getGridValue (coordinate: Vector2): GridValue =
    let x = coordinate.x
    let y = coordinate.y
    if x < 0 || x > xMax || y < 0 || y > yMax then '.'
    else grid[y][x]

let getWord (coordinate: Vector2) (direction: Vector2): string =
    let getNext (word: string) (index: int): string =
        let position = { x = coordinate.x + direction.x * index; y = coordinate.y + direction.y * index }
        let valueAtPosition = getGridValue position
        let newWord = word + valueAtPosition.ToString()
        newWord

    let word = [|0..3|] |> Array.fold getNext ""
    word

let crossProduct (a: int[]) (b: int[]): Vector2[] =
    a |> Array.collect (fun x ->
        b |> Array.map (fun y -> { x = x; y = y })
    )

let allDirections =
    crossProduct [|-1; 0; 1|] [|-1; 0; 1|]
    |> Array.filter (fun x -> x <> { x = 0; y = 0 })

let getAllWords (coordinate: Vector2): string array =
    allDirections |> Array.map (getWord coordinate)

let isXmas (word: string): bool =
    word = "XMAS"

let allCoordinates =
    crossProduct [|0..xMax|] [|0..yMax|]

let partOne () =
    allCoordinates
    |> Array.filter (fun c -> getGridValue c = 'X')
    |> Array.map (fun c -> getAllWords c |> Array.filter isXmas |> Array.length)
    |> Array.sum

let getWordOfLength (coordinate: Vector2) (direction: Vector2) (length: int): string =
    let getNext (word: string) (index: int): string =
        let position = { x = coordinate.x + direction.x * index; y = coordinate.y + direction.y * index }
        let valueAtPosition = getGridValue position
        let newWord = word + valueAtPosition.ToString()
        newWord

    let word = [|0..(length - 1)|] |> Array.fold getNext ""
    word

let isMas (centre: Vector2): bool =
    let isMasForwardsOrBackwards (word: string): bool =
        word = "MAS" || word = "SAM"

    let topLeft = { x = centre.x - 1; y = centre.y - 1 }
    let bottomLeft = { x = centre.x - 1; y = centre.y + 1 }
    let downwardDiagonal = getWordOfLength topLeft { x = 1; y = 1 } 3
    let upwardDiagonal = getWordOfLength bottomLeft { x = 1; y = -1 } 3

    (isMasForwardsOrBackwards downwardDiagonal) && (isMasForwardsOrBackwards upwardDiagonal)

let partTwo () =
    allCoordinates
    |> Array.filter (fun c -> getGridValue c = 'A')
    |> Array.filter isMas
    |> Array.length
