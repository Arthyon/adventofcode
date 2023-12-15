open System.IO
open AoCHelpers
open Argu
open AoCHelpers.Config
open Solutions.Days

type Arguments =
    | TestInput of bool
    | Day of int
    | Cookie of string
    
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | TestInput _ -> "If testinput should be used"
            | Day _ -> "Which day to run"
            | Cookie _ -> "Set the session cookie"
let parse input = File.ReadAllText input
let parseLines input = File.ReadAllLines input

let parser =  ArgumentParser.Create<Arguments>(programName = "AoC")
let args = System.Environment.GetCommandLineArgs ()
let results = parser.Parse args[1..]

let configuration = Config.getConfig ()
let cookie = results.TryGetResult Arguments.Cookie
let day = results.TryGetResult Arguments.Day
let useTestinput = results.TryGetResult Arguments.TestInput
let newConf = {
    UseTestinput = useTestinput |> Option.defaultValue configuration.UseTestinput
    Day = day |> Option.orElse configuration.Day 
}
Config.setConfig newConf

let input = InputFetcher.fetchInput newConf

let runner = match newConf.Day with
                | None -> failwith "Day not specified or configured"
                | Some(day) ->
                                printf $"Running day %i{day}"
                                if newConf.UseTestinput then printf " using test input"
                                printfn ""
                                match day with
                                | 1 -> Day1.run
                                | 2 -> Day2.run
                                | 3 -> Day3.run
                                | 4 -> Day4.run
                                | 5 -> Day5.run
                                | 6 -> Day6.run
                                | 7 -> Day7.run
                                | 8 -> Day8.run
                                | 9 -> Day9.run
                                | 10 -> Day10.run
                                | 11 -> Day11.run
                                | 12 -> Day12.run
                                | 13 -> Day13.run
                                | 14 -> Day14.run
                                | 15 -> Day15.run
                                | _ -> failwithf $"Day %i{day} not found"
runner input