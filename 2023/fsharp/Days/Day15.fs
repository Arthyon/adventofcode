module Solutions.Days.Day15

open System
open System.Collections.Generic
open AoCHelpers

type Lens = { FocalLength: int ; Label: String }
type Operation = | Add of int * Lens | Remove of int * string
let hash s =
    let calculate agg value = ((value + agg) * 17) % 256
    (0, s |> Seq.cast<char> |> Seq.map int) ||> Seq.fold calculate
    
let performOperation map operation =
    match operation with
    | Add(box, lens) ->
        if map |> Map.containsKey box then
            let list: List<Lens> = map[box]
            let existingIdx = list.FindIndex(fun l -> l.Label = lens.Label)
            if existingIdx > -1 then
                list[existingIdx] <- lens
            else
                list.Add lens
            map
        else
            let list = List<Lens>()
            list.Add lens
            map |> Map.add box list
    | Remove(box, label) ->
        if map |> Map.containsKey box then
            let list = map[box]
            list.RemoveAll (fun l -> l.Label = label) |> ignore
            map
        else map
    
let toOperation (s:string) =
    if s.Contains('=') then
        let label, length = splitInTwo '=' s
        Add(hash label, { FocalLength = int length ; Label = label })
    else
        let label,_ = splitInTwo '-' s
        Remove(hash label, label)
let calculateFocusingPower (kvp: KeyValuePair<int, List<Lens>>) =
    kvp.Value |> Seq.indexed |> Seq.map (fun (i, lens) -> (1 + kvp.Key) * ((i + 1) * lens.FocalLength)) |> Seq.sum
let run input =
    let sequence = input |> Seq.head |> split ','
    
    // Part 1
    let part1 = sequence |> Seq.map hash |> Seq.sum
    
    // Part 2
    let operations = sequence |> Seq.map toOperation
    let boxes = (Map[], operations) ||> Seq.fold performOperation 
    let part2 = boxes |> Seq.map calculateFocusingPower |> Seq.sum
    
    printResult part1 part2