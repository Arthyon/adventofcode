module Solutions.Days.Day21

open System.Collections.Generic
open AoCHelpers

let canWalk map (_,_,x,y) = (Array2D.get map x y) <> '#'

let validInfiniteCoordinates map (mapX, mapY) (x,y) =
    let xLength = map |> Array2D.length1
    let yLength = map |> Array2D.length2
    seq {
        for dir in cardinalDirections do
            let newX, newY = addMovement (x,y) dir
            let mapX, mapY, x, y =
                    if isValidCoordinate map (newX, newY) then
                        mapX, mapY, newX, newY
                      else
                        if newX < 0 then
                            mapX - 1, mapY, xLength - 1, y
                        else if newX >= xLength then
                            mapX + 1, mapY, 0, y
                        else if newY < 0 then
                            mapX, mapY - 1, x, yLength - 1
                        else if newY >= yLength then
                            mapX, mapY + 1, x, 0
                        else failwith "invalid situation"
            yield (mapX, mapY, x, y)
    }
    
let visualizeMap (arr: char[,]) extraMaps reachable =
    let min = -extraMaps
    let max = extraMaps
    let maxX = (arr |> Array2D.length1) - 1
    let maxY = (arr |> Array2D.length2) - 1
    let bounds = [(0,0);(0,maxY);(maxX,0);(maxX,maxY)]
    for mapX in [min..max] do
        for x in 0..maxX do
            for mapY in [min..max] do
                for y in 0..maxY do
                    if bounds |> List.contains (x,y) then
                        printf "#"
                    else if reachable |> Set.contains (mapX, mapY, x, y) then
                        printf "-"
                    else
                        // printf "%c" arr[x,y]
                        printf "."
            printfn ""
    printfn ""
    reachable
let validCoordinates infinite map mapCoords (x,y) =
    if infinite then
        validInfiniteCoordinates map mapCoords (x,y) 
    else
        validAdjacentCardinalCoordinates map (x,y) |> Seq.map (fun (x,y) -> 0,0,x,y)
        
let fill start map maxSteps infinite =
    let reachable = Set.empty
    let shouldCount = maxSteps % 2 <> 0
    let q = validCoordinates infinite map (0,0) start |> Seq.filter (canWalk map) |> Seq.map (fun (mapX,mapY, x,y) -> (mapX,mapY), (x,y), shouldCount, 1) |> Queue
    
    let rec fill' reachable queue =
        match nextItem queue with
        | Some(mapCoords, pos, shouldCount, steps) ->
            if steps <= maxSteps then
                if steps = maxSteps && not shouldCount then
                    failwith "shouldn't happen"
                let neighbours = validCoordinates infinite map mapCoords pos |> Seq.filter (canWalk map) |> Seq.filter (fun pos -> (Set.contains pos reachable) |> not)
                let newReachable = if not shouldCount then reachable |> Set.union (neighbours |> Set) else reachable
                    
                neighbours |> Seq.map (fun (mapX, mapY,x,y) -> ((mapX, mapY),(x,y), not shouldCount, steps + 1)) |> enqueue queue
                fill' newReachable queue
            else
                fill' reachable queue
        | None -> reachable
        
    fill' reachable q
   
    
let getReachablesInMap reachable (mapX,mapY) =
    reachable |> Set.filter (fun (x,y,_,_) -> x = mapX && y = mapY) |> Set.map (fun (_,_,x,y) -> 0,0,x,y)
    
let run input =
    let map = toArray2d input
    let start = map |> findIndices (fun c -> c = 'S') |> Seq.head
    
    // Part 1
    let reachable = fill start map 64 false
    let part1 = reachable |> Set.count
    
    // Part 2
    
    let target = 26501365
    let xLen = map |> Array2D.length1
    let yLen = map |> Array2D.length2
    let rem = target % xLen
    let tilesToTraverse = (target - rem) / xLen
    let tilesToTraverse = (float tilesToTraverse) - 1.
    
    let reachable = fill start map (rem + xLen + xLen)  true // traverse out of first square + 2 extra lengths
    let tileCount1 = getReachablesInMap reachable (1,0) |> Set.count |> float
    let tileCount2 =   getReachablesInMap reachable (0,0) |> Set.count |> float
    
    let firstSquaresToTraverse = (tilesToTraverse + 1.) ** 2.
    let secondSquaresToTraverse = tilesToTraverse ** 2.
    let firstSquareCount = firstSquaresToTraverse * tileCount1 
    let secondSquareCount = secondSquaresToTraverse * tileCount2
    
    
    let upleft = (xLen - 1, yLen - 1)
    let downleft = (xLen - 1, 0)
    let upright = (0, yLen - 1)
    let downright = (0,0)
    let up = (fst start, yLen - 1)
    let down = (fst start, 0)
    let left = (xLen - 1, snd start)
    let right = (0, snd start)
    
    let diag1 = ((target - (fst start) - (snd start) - 2) % (xLen+yLen))
    let diag2 = (target - (fst start) - (snd start) - 2 - xLen) % (xLen+yLen)
    let tip =  xLen - 1
    let upTip = fill up map tip false |> Set.count |> float
    let downTip = fill down map tip false |> Set.count |> float
    let leftTip = fill left map tip false |> Set.count |> float
    let rightTip = fill right map tip false |> Set.count |> float
    
    let upleftDiag1 = fill upleft map diag1 false |> Set.count |> float
    let downleftDiag1 = fill downleft map diag1 false |> Set.count |> float
    let uprightDiag1 = fill upright map diag1 false |> Set.count |> float
    let downrightDiag1 = fill downright map diag1 false |> Set.count |> float
    
    let upleftDiag2 = fill upleft map diag2 false |> Set.count |> float
    let downleftDiag2 = fill downleft map diag2 false |> Set.count |> float
    let uprightDiag2 = fill upright map diag2 false |> Set.count |> float 
    let downRightDiag2 = fill downright map diag2 false |> Set.count |> float
    
    let firstDiagonals = tilesToTraverse * (upleftDiag1 + downleftDiag1 + uprightDiag1 + downrightDiag1)
    let secondDiagonals = (tilesToTraverse + 1.) * (upleftDiag2 + downleftDiag2 + uprightDiag2 + downRightDiag2)
    let part2 = firstSquareCount + secondSquareCount + firstDiagonals + secondDiagonals + upTip + downTip + leftTip + rightTip
    
    printResult part1 part2
   
