[<AutoOpen>]
module AoCHelpers.Output

let debug value =
    printfn "%A" value
    value
    
let printResult part1 part2 =
    printfn "Part 1:"
    printfn $"%f{part1}"
    printfn "Part 2:"
    printfn $"%f{part2}"
    

let printAndStop (sw: System.Diagnostics.Stopwatch) =
    sw.Stop()
    printfn $"Elapsed: %i{sw.ElapsedMilliseconds}ms"
    
let printAndRestart (sw: System.Diagnostics.Stopwatch) =
    printfn $"Elapsed: %i{sw.ElapsedMilliseconds}ms"
    sw.Restart()
