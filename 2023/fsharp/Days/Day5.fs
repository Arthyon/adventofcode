module Solutions.Days.Day5

open System
open Jamarino.IntervalTree
open AoCHelpers

type Map = QuickIntervalTree<float, float>
let mapLookup value (map: Map) =
    match map.Query(value) |> Seq.tryHead with
    | Some(v) -> value + v
    | None -> value
    
let part2ParseSeeds seeds =
    seeds |> Seq.chunkBySize 2 |> Seq.collect (fun a -> [a[0]..(a[0]+a[1])])

let parseMapLine line =
    match split ' ' line |> Seq.toList with
    | [fst; snd; trd] ->
        let target = float fst
        let source = float snd
        let range = float trd
        Some(source, source + (range - 1.0), target - source)
    | _ -> None
    
let createMap lines =
    let map = Map()
    lines |> Seq.choose parseMapLine |> Seq.iter map.Add
    map
    
let parseMaps (input: string[]) =
    input[3..]
       |> Seq.toList
       |> (chunkWhile String.IsNullOrWhiteSpace)
       |> List.map createMap
                   
let traverseMap (maps: Map list) (minPos: float) (seed: float) =
    let foundMinimum = (seed, maps) ||> Seq.fold mapLookup
    Math.Min(minPos, foundMinimum)
let solve seeds maps = (Double.MaxValue, seeds) ||> Seq.fold (traverseMap maps)
    
let run (input: string[]) =
    let maps = parseMaps input
    let seeds = input[0] |> splitInTwo ':' |> snd |> split ' ' |> Seq.map float
    
    // Part 1
    let part1 = solve seeds maps
    
    // Part 2
    let seeds = part2ParseSeeds seeds
    let part2 = solve seeds maps
    
    printResult part1 part2