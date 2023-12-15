module Solutions.Days.Day11

open System
open AoCHelpers

let isEmpty s = s |> Seq.forall (fun c -> c = '.')

let emptyIndices s = s |> Seq.indexed |> mapSnd isEmpty |> Seq.filter snd |> Seq.map fst
    
let search emptyCols emptyRows padding ((startX, startY), (endX: int, endY: int)) =
    let rec s' (x,y) distance =
        if x = endX && y = endY then distance
        else
            let deltaX = Math.Sign(endX - x)
            let deltaY = Math.Sign(endY - y)
            let mutable newDist = Math.Abs(deltaX) + Math.Abs(deltaY)
            let newX = x + deltaX
            let newY = y + deltaY
            if emptyCols |> Set.contains newY then newDist <- newDist + padding
            if emptyRows |> Set.contains newX then newDist <- newDist + padding
            
            s' (newX, newY) (distance + (float newDist))
        
    s' (startX, startY) 0.0
let rec pairs l = seq {  
    match l with 
    | h::t -> for e in t do yield h, e
              yield! pairs t
    | _ -> () }

let run input =
    let map = toArray2d input
    
    let emptyCols = [0..(map |> Array2D.length2) - 1] |> Seq.map (column map) |> emptyIndices |> set
    let emptyRows = [0..(map |> Array2D.length1) - 1] |> Seq.map (row map) |> emptyIndices |> set
    
    let galaxies = map |> findIndices (fun c -> c = '#') |> Seq.toList |> pairs |> Seq.toList

    let searcher = search emptyCols emptyRows
    
    // Part 1
    let padding = 1
    let part1 = galaxies |> Seq.map (searcher padding) |> Seq.sum
    
    // Part 2
    let padding = 999999
    let part2 = galaxies |> Seq.map (searcher padding) |> Seq.sum
    
    printResult part1 part2
