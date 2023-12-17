module Solutions.Days.Day16

open System.Collections.Generic
open AoCHelpers
open Microsoft.FSharp.Collections


let rec traverse map pos visited dir (cache: Dictionary<_,_>) =
    let newPos = (fst pos + fst dir), (snd pos + snd dir)
    let newSet = visited |> Set.add newPos
    
    let split dir1 dir2 =
        if visited |> Set.contains newPos then
            visited
        else if cache.ContainsKey(newPos) then
            Set.union visited cache[newPos]
        else
            let visits1 = traverse map newPos newSet dir1 cache
            let visits2 = traverse map newPos newSet dir2 cache
            
            let newVisits = Set.union visits1 visits2
            let diff = Set.difference newVisits newSet
            cache.Add(newPos, diff)
            newVisits
            
    if isValidCoordinate map newPos then
        match Array2D.get map (fst newPos) (snd newPos) with
        | '.' -> traverse map newPos newSet dir cache
        | '/' when dir = east -> traverse map newPos newSet north cache
        | '/' when dir = west -> traverse map newPos newSet south cache
        | '/' when dir = south -> traverse map newPos newSet west cache
        | '/' when dir = north -> traverse map newPos newSet east cache
        | '\\' when dir = east -> traverse map newPos newSet south cache
        | '\\' when dir = west -> traverse map newPos newSet north cache
        | '\\' when dir = south -> traverse map newPos newSet east cache
        | '\\' when dir = north -> traverse map newPos newSet west cache
        | '|' when dir = east || dir = west -> split north south
        | '|' -> traverse map newPos newSet  dir cache
        | '-' when dir = north || dir = south -> split east west
        | '-' -> traverse map newPos newSet  dir cache
        | a -> failwithf $"Unhandled %A{a}"
    else
        visited
        
let getStarts map =
    let x = (map |> Array2D.length1) - 1
    let y = (map |> Array2D.length2) - 1
    seq {
        for i in [0..x] do
            yield (i,-1), east
            yield (i,y + 1), west
            
        for i in [0..y] do
            yield (-1, i), south
            yield (x + 1, i), north
           
    }
        
let run input =
    let map = toArray2d input
    
    let solve pos dir = traverse map pos Set[] dir (Dictionary<_,_>()) |> Seq.distinct |> Seq.length
    
    // Part 1
    let part1 = solve (0,-1) east
    
    
    // Part 2
    let starts = getStarts map |> Seq.toList
    let mutable counter = 0
    let mutable max = 0
    for pos, dir in getStarts map do
        counter <- counter + 1
        printfn "Processing %i/%i maps" counter starts.Length
        
        let e = solve pos dir 
        if e > max then
            max <- e

    printResult part1 max

