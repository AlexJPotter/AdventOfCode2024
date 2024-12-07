module Day06

let input = System.IO.File.ReadAllText("Day06/input.txt").Trim()

let inputRows = input.Split("\n") |> Array.filter (fun x -> x.Length > 0)

type Vector2 = { x: int; y: int }

type Map = char[][]

let originalMap = inputRows |> Array.map _.ToCharArray()

type Direction = Up | Down | Left | Right

type Object = Space | Obstruction | GuardFacingUp | GuardFacingDown | GuardFacingLeft | GuardFacingRight | Void

let xMax = originalMap[0].Length - 1
let yMax = originalMap.Length - 1

let getObject (coordinate: Vector2) (map: Map): Object =
    if coordinate.x < 0 || coordinate.x > xMax || coordinate.y < 0 || coordinate.y > yMax then
        Void
    else
        match map[coordinate.y][coordinate.x] with
        | '.' -> Space
        | '#' -> Obstruction
        | '^' -> GuardFacingUp
        | 'v' -> GuardFacingDown
        | '<' -> GuardFacingLeft
        | '>' -> GuardFacingRight
        | c -> failwith $"Unknown object character: {c}"

let isGuard (obj: Object): bool =
    match obj with
    | GuardFacingUp | GuardFacingDown | GuardFacingLeft | GuardFacingRight -> true
    | _ -> false

let convertToDirection (guard: Object): Direction =
    match guard with
    | GuardFacingUp -> Up
    | GuardFacingDown -> Down
    | GuardFacingLeft -> Left
    | GuardFacingRight -> Right
    | _ -> failwith $"Not a guard: {guard}"

type Termination = WalkedOff | Looped | None

type Position = { coordinate: Vector2; direction: Direction }

type State = { guardCoordinate: Vector2; facingDirection: Direction; visited: Set<Position>; termination: Termination }

let getVector (direction: Direction): Vector2 =
    match direction with
    | Up -> { x = 0; y = -1 }
    | Down -> { x = 0; y = 1 }
    | Left -> { x = -1; y = 0 }
    | Right -> { x = 1; y = 0 }

let turnClockwise (direction: Direction): Direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let rec recurse (state: State) (map: Map): State =
    let guardCoordinate = state.guardCoordinate
    let directionVector = getVector state.facingDirection

    let coordinateInFrontOfGuard = { x = guardCoordinate.x + directionVector.x; y = guardCoordinate.y + directionVector.y }
    let objectInFrontOfGuard = getObject coordinateInFrontOfGuard map

    let nextVisited = state.visited.Add({ coordinate = state.guardCoordinate; direction = state.facingDirection })

    if state.visited |> Set.contains { coordinate = guardCoordinate; direction = state.facingDirection } then
        let coordinate = state.guardCoordinate
        printfn $"Guard looped at (%d{coordinate.x}, %d{coordinate.y}) facing %A{state.facingDirection}"
        { state with termination = Looped }

    else if objectInFrontOfGuard = Void then
        { state with visited = nextVisited; termination = WalkedOff }

    else
        let nextCoordinate = if objectInFrontOfGuard = Obstruction then guardCoordinate else coordinateInFrontOfGuard
        let nextDirection = if objectInFrontOfGuard = Obstruction then turnClockwise state.facingDirection else state.facingDirection
        let nextState = { state with guardCoordinate = nextCoordinate; facingDirection = nextDirection; visited = nextVisited; }
        recurse nextState map

let allCoordinates = [| for y in 0..yMax do for x in 0..xMax do yield { x = x; y = y } |]

let partOne () =
    let guardCoordinate = allCoordinates |> Array.find (fun c -> isGuard (getObject c originalMap))
    let guardObject = getObject guardCoordinate originalMap
    let guardDirection = convertToDirection guardObject

    let state = {
        guardCoordinate = guardCoordinate
        facingDirection = guardDirection
        visited = Set.empty
        termination = None
    }

    let finalState = recurse state originalMap
    finalState.visited |> Set.map _.coordinate |> Set.count

let canPlaceObstacle (coordinate: Vector2) (map: Map): bool =
    let obj = getObject coordinate map

    match obj with
    | Space -> true
    | Obstruction -> true
    | _ -> false

let placeObstacle (coordinate: Vector2) (map: Map): Map =
    [|0..yMax|] |> Array.map (fun y ->
        [|0..xMax|] |> Array.map (fun x ->
            if x = coordinate.x && y = coordinate.y then '#' else map[y][x]
        )
    )

let loops (map: Map): bool =
    let guardCoordinate = allCoordinates |> Array.find (fun c -> isGuard (getObject c map))
    let guardObject = getObject guardCoordinate map
    let guardDirection = convertToDirection guardObject

    let state = {
        guardCoordinate = guardCoordinate
        facingDirection = guardDirection
        visited = Set.empty
        termination = None
    }

    let finalState = recurse state map
    finalState.termination = Looped

let partTwo () =
    let validCoordinates = allCoordinates |> Array.filter (fun c -> canPlaceObstacle c originalMap)
    let alternativeMaps = validCoordinates |> Array.map (fun c -> placeObstacle c originalMap)
    alternativeMaps |> Array.filter loops |> Array.length
