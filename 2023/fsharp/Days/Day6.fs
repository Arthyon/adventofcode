module Solutions.Days.Day6
open AoCHelpers

type Race = { Time: int ; Distance: float }

let calculateRace race =
    [1..race.Time]
    |> Seq.map (fun t -> (float (race.Time - t)) * (float t))
    |> Seq.skipWhile (fun t -> t <= race.Distance)
    |> Seq.takeWhile (fun t -> t > race.Distance)
    |> Seq.length

let run input =
    // Part 1
    let rows = input |> Seq.map (splitInTwo ':') |> Seq.map snd |> Seq.map (split ' ') |> Seq.toList
    let races = Seq.zip rows[0] rows[1] |> Seq.map (fun (time, distance) -> { Time = int time ; Distance = int distance })
    let part1 = races |> Seq.map calculateRace |> Seq.reduce (*)
    
    // Part 2
    let rows = input |> Seq.map (splitInTwo ':') |> Seq.map snd |> Seq.map (fun s -> s.Replace(" ", "")) |> Seq.map float |> Seq.toList
    let race = { Time = int rows[0]; Distance = rows[1] }
    let part2 = calculateRace race
    
    
    printResult part1 part2
    