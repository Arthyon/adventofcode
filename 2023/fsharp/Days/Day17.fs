module Solutions.Days.Day17

open System
open System.Collections.Generic
open AoCHelpers

let part1StepRule directions current steps =
    if steps < 3 then
        directions
    else
       directions |> List.except [current]
let part2StepRule directions current steps =
    if steps < 4 then [current]
    else if steps >= 10 then 
       directions |> List.except [current]
    else directions
let part1End location dest _ = location = dest
let part2End location dest steps = location = dest && steps >= 4
let getDirections current steps stepRule =
    let directions =
        match current with
        | '>' -> ['^';'>';'v']
        | '<' -> ['^';'<';'v']
        | '^' -> ['<';'^';'>']
        | 'v' -> ['<';'v';'>']
        | _ -> failwith ""
        
    stepRule directions current steps
   
let convertDirectionToPos (x,y) direction =
    match direction with
    | '>' -> (x + fst east), (y + snd east)
    | 'v' -> (x + fst south), (y + snd south)
    | '<' -> (x + fst west), (y + snd west)
    | '^' -> (x + fst north), (y + snd north)
    | _ -> failwith "unhandled"
let neighbours map pos directions =
    let validCoordinates = validAdjacentCardinalCoordinates map pos
    let neighbours = directions |> Seq.map (fun d -> d, convertDirectionToPos pos d)
    neighbours
    |> Seq.filter (fun (_, pos) -> validCoordinates |> Seq.contains pos)
    |> Seq.map (fun (c,(x,y)) -> (c, (x,y), map[x,y]))
    
let increaseSteps steps currentDir nextDir =
    if currentDir = nextDir then steps + 1 else 1
   
let enqueueNeighbours (queue: PriorityQueue<_,_>) (visited: Dictionary<_,_>) distance map location direction steps stepRule =
    getDirections direction steps stepRule
    |> (neighbours map location)
    |> Seq.map (fun (d, pos, weight) -> (pos, d, increaseSteps steps direction d, distance + weight))
    |> Seq.filter ( fun (pos, d, steps, _) -> visited.ContainsKey((pos,d,steps)) |> not)
    |> Seq.iter (fun (pos, d, steps, weight) -> queue.Enqueue((pos, d, steps), weight))
    
let solve map start dest stepRule endRule =
    let q = PriorityQueue<(int * int) * char * int, int>()
    q.Enqueue((start, '>', 0), 0)
    let visited = Dictionary<_,_>()
    let rec pathfind () =

        match q.TryDequeue() with
        | false, _, _ -> failwith "todo"
        | true, (location, direction, steps), distance ->
            if endRule location dest steps then
                distance
            else
                if visited.ContainsKey (location, direction,steps) then
                    if visited[(location,direction,steps)] > distance then
                        visited[(location, direction, steps)] <- distance
                        enqueueNeighbours q visited distance map location direction steps stepRule

                    pathfind ()
                        
                        
                else
                    visited.Add ((location, direction, steps), distance)
                    enqueueNeighbours q visited distance map location direction steps stepRule

                    pathfind ()

        
    
    pathfind ()
 
let run input =
    let map = toArray2d input |> Array2D.map (Char.GetNumericValue >> int)
    let dest = (map |> Array2D.length1) - 1, (map |> Array2D.length2) - 1
    
    let part1 = solve map (0,0) dest part1StepRule part1End
    let part2 = solve map (0,0) dest part2StepRule part2End
    
    printResult part1 part2