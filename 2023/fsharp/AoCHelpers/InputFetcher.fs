module AoCHelpers.InputFetcher

open System.IO
open AoCHelpers.Config

let fetchInput config =
    if config.UseTestinput then
        File.ReadAllLines "testinput.txt"
    else
        match config.Day with
        | None ->
            failwith "Day not specified or configured"
        | Some day ->
            let inputFile = $"inputs/day%i{day}_input.txt"
            if File.Exists inputFile |> not
            then failwith $"Day %i{day}'s input not found"
            else File.ReadAllLines inputFile

