module Day08

let input = System.IO.File.ReadAllText("Day08/input.txt").Trim()

let inputRows = input.Split("\n") |> Array.filter (fun x -> x.Length > 0)

type Map = char[][]

type Vector2 = { x: int; y: int }

let inputMap = inputRows |> Array.map _.ToCharArray()

let getMapValue (map: Map) (position: Vector2): char =
    let xMax = map[0].Length - 1
    let yMax = map.Length - 1

    if position.x < 0 || position.x > xMax || position.y < 0 || position.y > yMax then
        failwith $"({position.x}, {position.y}) is out of bounds"
    else
        map[position.y][position.x]

let manhattan (a: Vector2) (b: Vector2) =
    ((a.x - b.x) |> abs) + ((a.y - b.y) |> abs)

let getPairs (items: Vector2[]): seq<Vector2 * Vector2> =
    seq {
        for i in 0..items.Length - 1 do
            for j in i+1..items.Length - 1 do
                yield items.[i], items.[j]
    } |> Seq.filter (fun (a, b) -> a.x <> b.x || a.y <> b.y)

let getDistinctIdentifiers (map: Map): char[] =
    map |> Array.concat |> Array.distinct |> Array.filter (fun x -> x <> '.')

type PositionsForIdentifier = { identifier: char; antennaPositions: Vector2[] }

let getAllCoordinates (map: Map): seq<Vector2> =
    let xMax = map[0].Length - 1
    let yMax = map.Length - 1

    seq {
        for y in 0..yMax do
            for x in 0..xMax do
                yield { x = x; y = y }
    }

let getAntennaPositions (map: Map) (identifier: char): Vector2[] =
    map |> getAllCoordinates |> Seq.filter (fun x -> (x |> getMapValue map) = identifier) |> Seq.toArray

let positionsForIdentifiers (map: Map): PositionsForIdentifier[] =
    map
    |> getDistinctIdentifiers
    |> Array.map (fun x -> { identifier = x; antennaPositions = x |> getAntennaPositions map })

let gcd (a: int) (b: int): int =
  let absA = a |> abs
  let absB = b |> abs
  if a = 0 || b = 0 then
      max absA absB
  else
      let set1 = Set.ofList [ 1 .. absA ] |> Set.filter (fun x -> absA % x = 0)
      let inter = Set.ofList [ 1 .. absB ] |> Set.filter (fun x -> absB % x = 0) |> Set.intersect set1
      let result = inter.MaximumElement
      result

let getGradient (a: Vector2) (b: Vector2): Vector2 =
    let rightMost = if a.x > b.x then a else b
    let leftMost = if rightMost = a then b else a
    let dx = rightMost.x - leftMost.x
    let dy = rightMost.y - leftMost.y
    let magnitude = gcd dx dy
    { x = dx / magnitude; y = dy / magnitude }

let isInLineWith (pair: Vector2 * Vector2) (position: Vector2): bool =
    let a, b = pair
    if position = a || position = b then
        false // Overlaps do not count
    else
        let gradientWithA = getGradient a position
        let gradientWithB = getGradient b position
        gradientWithA = gradientWithB

let isAntiNode (antennaPositions: Vector2[]) (position: Vector2): bool =
    antennaPositions
    |> getPairs
    |> Seq.exists (fun (a, b) ->
        position |> isInLineWith (a, b)
        && manhattan a position <> 0
        && manhattan b position <> 0
        && (
            manhattan a position = 2 * manhattan b position ||
            manhattan b position = 2 * manhattan a position
        )
    )

let partOne() =
    let idPositions = inputMap |> positionsForIdentifiers

    inputMap
        |> getAllCoordinates
        |> Seq.filter (fun c ->
            idPositions |> Seq.exists (fun x -> c |> isAntiNode x.antennaPositions)
        )
        |> Seq.length

let isInLineWith2 (pair: Vector2 * Vector2) (position: Vector2): bool =
    let a, b = pair
    if position = a || position = b then
        true // Overlaps are now allowed
    else
        let gradientWithA = getGradient a position
        let gradientWithB = getGradient b position
        gradientWithA = gradientWithB

let isAntiNode2 (antennaPositions: Vector2[]) (position: Vector2): bool =
    antennaPositions
    |> getPairs
    |> Seq.exists (fun (a, b) -> position |> isInLineWith2 (a, b))

let partTwo() =
    let idPositions = inputMap |> positionsForIdentifiers

    inputMap
        |> getAllCoordinates
        |> Seq.filter (fun c ->
            idPositions |> Seq.exists (fun x -> c |> isAntiNode2 x.antennaPositions)
        )
        |> Seq.length
