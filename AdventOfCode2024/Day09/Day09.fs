module Day09

let parseCharacterAsNumeral (character: char): int =
    int character - int '0'

let input = System.IO.File.ReadAllText("Day09/input.txt").Trim() |> Seq.toArray

type FileBlock = { fileId: int }

type FreeSpaceBlock = None

type Block = File of FileBlock | FreeSpace of FreeSpaceBlock

type DiskMap = Block[]

type ParsingState = { diskMap: DiskMap; index: int; fileId: int }

let parseDiskMap (): DiskMap =
    let rec recurse(current: ParsingState): ParsingState =
        if current.index >= input.Length then
            current
        else
            let numberOfBlocks = input[current.index] |> parseCharacterAsNumeral
            let isFile = current.index % 2 = 0

            let blocksToAdd =
                [|0..numberOfBlocks-1|] |> Array.map(fun _ ->
                    if isFile then File { fileId = current.fileId }
                    else FreeSpace None
                )

            let nextDiskMap = Array.append current.diskMap blocksToAdd
            let nextFileId = if isFile then current.fileId + 1 else current.fileId
            let next = { diskMap = nextDiskMap; index = current.index + 1; fileId = nextFileId }
            recurse(next)

    recurse({ diskMap = [||]; index = 0; fileId = 0 }).diskMap

let originalDiskMap = parseDiskMap()

let swapBlocks (diskMap: DiskMap, indexA: int, indexB: int): DiskMap =
    diskMap
    |> Array.zip [|0..diskMap.Length-1|]
    |> Array.map (fun (index, block) ->
        match index with
        | _ when index = indexA -> diskMap[indexB]
        | _ when index = indexB -> diskMap[indexA]
        | _ -> block
    )

type CompactionState = { diskMap: DiskMap; leftIndex: int; rightIndex: int }

let compactDiskMap (diskMapToCompact: DiskMap): DiskMap =
    let rec recurse(current: CompactionState): CompactionState =
        if current.leftIndex >= current.rightIndex then
            current
        else
            let leftBlock = current.diskMap[current.leftIndex]
            let rightBlock = current.diskMap[current.rightIndex]

            let next =
                match leftBlock with
                | File _ -> { current with leftIndex = current.leftIndex + 1 }
                | FreeSpace _ ->
                    match rightBlock with
                    | FreeSpace _ -> { current with rightIndex = current.rightIndex - 1 }
                    | File _ ->
                        let nextDiskMap = swapBlocks(current.diskMap, current.leftIndex, current.rightIndex)
                        let nextLeftIndex = current.leftIndex + 1
                        let nextRightIndex = current.rightIndex - 1
                        { current with diskMap = nextDiskMap; leftIndex = nextLeftIndex; rightIndex = nextRightIndex }

            recurse(next)

    recurse({ leftIndex = 0; rightIndex = diskMapToCompact.Length - 1; diskMap = diskMapToCompact }).diskMap

let calculateCheckSum (diskMap: DiskMap): int64 =
    diskMap
    |> Array.indexed
    |> Array.map (fun (index, block) ->
        match block with
        | File file -> (index * file.fileId) |> int64
        | FreeSpace _ -> 0 |> int64
    )
    |> Array.sum

let partOne () =
    originalDiskMap |> compactDiskMap |> calculateCheckSum

let findFreeSpace (diskMap: DiskMap, maxIndex: int, size: int): int option =
    [|0..maxIndex-size|] |>
    Array.tryFind (fun index ->
        [|0..size-1|] |> Array.forall (fun i ->
            match diskMap[index + i] with
            | FreeSpace _ -> true
            | _ -> false
        )
    )

type CompactState2 = { diskMap: DiskMap; unprocessedFileIds: Set<int>; processedFileIds: Set<int> }

let compact2 (diskMapToCompact: DiskMap): DiskMap =
    let rec recurse (current: CompactState2): CompactState2 =
        if current.unprocessedFileIds.IsEmpty then
            current
        else
            let fileId = current.unprocessedFileIds |> Set.maxElement

            let startIndex = current.diskMap |> Array.findIndex (fun block ->
                match block with
                | File file -> file.fileId = fileId
                | _ -> false
            )

            let fileSize =
                current.diskMap
                |> Array.filter (fun block ->
                    match block with
                    | File file -> file.fileId = fileId
                    | _ -> false
                )
                |> Array.length

            let freeSpaceIndex = findFreeSpace(current.diskMap, startIndex, fileSize)

            let unprocessed = current.unprocessedFileIds |> Set.remove fileId
            let processed = current.processedFileIds |> Set.add fileId

            if freeSpaceIndex.IsNone then
                recurse({ current with unprocessedFileIds = unprocessed; processedFileIds = processed })
            else
                let nextDiskMap =
                    [|0..(fileSize-1)|] |> Array.fold (fun acc i ->
                        swapBlocks(acc, freeSpaceIndex.Value + i, startIndex + i)
                    ) current.diskMap

                recurse({ current with diskMap = nextDiskMap; unprocessedFileIds = unprocessed; processedFileIds = processed })

    let unprocessedFileIds =
        diskMapToCompact
        |> Array.map (fun b ->
            match b with
            | File file -> file.fileId
            | _ -> -1
        )
        |> Array.filter (fun fileId -> fileId >= 0)
        |> Set.ofArray

    recurse({ diskMap = diskMapToCompact; unprocessedFileIds = unprocessedFileIds; processedFileIds = Set.empty }).diskMap

let partTwo () =
    originalDiskMap |> compact2 |> calculateCheckSum
