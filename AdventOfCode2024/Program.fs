open Day08

[<EntryPoint>]
let main argv =
    if argv.Length > 0 && argv[0] = "1" then
        let solution1 = partOne()
        printfn $"{solution1}"
    else if argv.Length > 0 && argv[0] = "2" then
        let solution2 = partTwo()
        printfn $"{solution2}"
    else
        let solution1 = partOne()
        printfn $"Solution 1: {solution1}"

        let solution2 = partTwo()
        printfn $"Solution 2: {solution2}"

    0
