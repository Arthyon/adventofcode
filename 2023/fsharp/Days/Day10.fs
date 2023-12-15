module Solutions.Days.Day10

open System.Collections.Generic
open AoCHelpers

let validDirections = function
    | '|' -> [north; south]
    | '-' -> [east; west]
    | 'L' -> [north; east]
    | 'J' -> [north; west]
    | '7' -> [south; west]
    | 'F' -> [south; east]
    | c   -> failwithf $"invalid char %c{c}"

let bfs (map: char[,]) dest start =
    let visited = HashSet [ start ]
    let q = Queue [ (start, dest, 1) ]
    let rec visit () =
        match q.TryDequeue() with
        | false, _ -> failwith "won't happen"
        | true, (coords, last, distance) ->
            visited.Add coords |> ignore

            let dirs = validDirections map[fst coords, snd coords]
                       |> Seq.map (fun c -> (fst coords + fst c, snd coords + snd c))
                       |> Seq.except [last]
            if dirs |> Seq.exists (fun d -> d = dest)
            then
                visited.Add dest |> ignore
                ((distance + 1) / 2, visited)
            else
                let newDirs = dirs |> Seq.filter (visited.Contains >> not)
                newDirs |> Seq.iter (fun next -> q.Enqueue(next, coords, distance + 1))
                visit ()

    visit ()

let m i j v = (i,j),v


let minmax set fn =
    set |> Seq.map fn |> Seq.max, set |> Seq.map fn |> Seq.min
    
let getBoundingBox points =
    let maxX, minX = minmax points fst
    let maxY, minY = minmax points snd
    (maxX, maxY), (minX, minY)

let getPotentialPoints pointsInLoop ((maxX, maxY), (minX, minY)) =
    seq {
        for x in minX..maxX do
            for y in minY..maxY do
                if pointsInLoop |> Set.contains (x,y) |> not then
                    yield (x,y)
    }

let inPolygon (map: char[,]) pointsInLoop (x,y) =
    let intersections,_ = ([0..y],(0,' ')) ||> Seq.foldBack (fun y (crossings, lastBend) ->
        if pointsInLoop |> Set.contains (x,y)
        then
            let currentPipe = if map[x,y] = 'S' then '-' else map[x,y] // THIS MUST BE CHANGED if using testinput. Didn't bother writing logic to figure out what kind of pipe S mapped to
            // if we're traversing across a pipe (---J) <-
            if lastBend <> ' ' && currentPipe = '-' then (crossings, lastBend)
            else
                match (map[x,y], lastBend) with
                | 'F', '7' -> (crossings, ' ')
                | 'L', 'J' -> (crossings, ' ')
                | 'F', 'J' -> (crossings + 1, ' ')
                | 'L', '7' -> (crossings + 1, ' ')
                | '|', b -> (crossings + 1, ' ')
                | b, ' ' -> (crossings, b)
                | (l,r) -> (crossings + 1, l)
        else (crossings, ' ')
        )
    if intersections = 0 then false
    else (intersections % 2) <> 0

let visualize map pointsInLoop potentialPoints =
    for x in 0..map |> Array2D.length1 do
        for y in 0..map |> Array2D.length2 do
            if pointsInLoop |> Set.contains (x,y) then
                printf "."
                // printf "%c" map[x,y]
            else if potentialPoints |> Set.contains (x,y) then
                if inPolygon map pointsInLoop (x,y)
                then printf "I"
                else printf "O"
            else printf " "
        printfn ""
let run input =
    let map = toArray2d input
    let start = map |> Array2D.mapi m |> flatten |> Seq.find (fun (_, v) -> v = 'S') |> fst
    let direction = validAdjacentCardinalCoordinates map start
                     |> Seq.filter (fun (x,y) -> ['|'; '-'; 'L';'J';'7';'F'] |> Seq.contains map[x,y])
                     
                     |> Seq.last // TODO Fix start dir chooser
    
    let part1, pointsInLoop = bfs map start direction 
    
    
    // Part2
    let box = getBoundingBox pointsInLoop
    let points = pointsInLoop |> Set.ofSeq
    let potentialPoints = getPotentialPoints points box |> Seq.toList
    
    visualize map points (potentialPoints |> Set.ofSeq)
    let part2 = potentialPoints |> Seq.filter (inPolygon map points) |> Seq.length
    
    printResult part1 part2
    

