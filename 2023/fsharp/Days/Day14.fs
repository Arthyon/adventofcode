module Solutions.Days.Day14

open System
open System.Collections.Generic
open AoCHelpers
let reorder l r =
    match l,r with
    | '.', 'O' -> 1
    | 'O', '.' -> -1
    | _, _ -> 0
    
let scoreColumn numColumns col = col |> Seq.indexed |> Seq.filter (fun (_,c) -> c = 'O') |> Seq.fold (fun acc (idx,_) -> acc + (numColumns - idx)) 0
let score map =
    let rows = map |> Array2D.length1
    let cols = map |> Array2D.length2
    [0..rows - 1] |> Seq.map (fun x -> scoreColumn cols map[*, x]) |> Seq.sum
let reorderList l =
    seq {
            for part in l |> Seq.toList |> chunkWhile (fun c -> c = '#') do
                match part with
                | [] -> ()
                | ['#'] -> yield '#'
                | '#'::t ->
                    yield '#'
                    yield! t |> Seq.sortWith reorder
                | t -> yield! t |> Seq.sortWith reorder
        
    } |> Seq.toList
let tiltCol dir map =
    seq {
        for idx in 1..map |> Array2D.length2 do
            let col = map[*,idx - 1]
            if dir = 1 then yield reorderList col else yield col |> Array.rev |> reorderList |> List.rev
                
    } |> Seq.toList
    
let tiltRow dir map =
    seq {
        for idx in 1..map |> Array2D.length1 do
            let row = map[idx - 1,*]
            if dir = 1 then yield reorderList row else yield row |> Array.rev |> reorderList |> List.rev
                
    } |> array2D
    
let transpose (cols: char list list) =
    let len = cols |> List.length
    seq {
    for x in 1..len do
        yield seq {
        for y in 1..len do
            yield cols[y - 1][x - 1]
        }
    } |> array2D
    
let printMap map =
    for x in 1..(map |> Array2D.length1) do
        for y in 1..(map |> Array2D.length2) do
            printf "%c" map[x - 1, y - 1]
        printfn ""
let toKey (map:char[,]) =
    flatten map |> Seq.toArray |> String
    
let solvePart2 cycle map target =
    let c = Dictionary<_,_>()
    c.Add(toKey map, 0UL)
    
    let rec solve' map repeats target skipped =
        if repeats = target then
            map
        else
            let map = cycle map
            let key = toKey map
        
            if c.ContainsKey(key) then
                let value = c[key]
                printfn "Similar map found on cycles %A. Currently cycle %i" value repeats
                let diff = repeats - value
                let remainingAfterCycle = (target - value) % diff
                let newRepeats = target - remainingAfterCycle
                c.Clear() // so we don't enter this if anymore. dirty
                solve' map newRepeats target true
            else
                c.Add(key, repeats)
                solve' map (repeats + 1UL) target skipped
    solve' map 1UL target false    
let run input =
    let map = toArray2d input

    let tiltNorth = (tiltCol 1) >> transpose
    let tiltSouth = (tiltCol 0) >> transpose
    let tilWest = tiltRow 1
    let tiltEast = tiltRow 0
    let cycle = tiltNorth >> tilWest >> tiltSouth >> tiltEast
    
    // Part1
    let part1 = map |> tiltNorth |> score
    
    // Part 2
    let part2 = solvePart2 cycle map 1000000000UL |> score
    
    printResult part1 part2