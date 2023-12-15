module AoCHelpers.Config

open System
open System.IO

type Configuration = {
    UseTestinput: bool
    Day: Option<int>
}

let private defaultConfig = { UseTestinput = false ; Day = None }
let private configPath = "config.conf"
let private TestInputKey = "testinput"
let private DayKey = "day"

let private parseLine (line: string) =
        let parts = line.Split("=", StringSplitOptions.TrimEntries)
        if parts.Length = 2 then Some(parts[0], parts[1]) else None
        
let private getConfigSetting configLines setting =
    configLines |> Seq.tryFind (fun (key, _) -> key = setting) |> Option.map snd
        
let private parseConfig () =
    let configGetter = File.ReadAllLines configPath |> Seq.choose parseLine |> getConfigSetting
    {
        Day = configGetter DayKey |> Option.map int
        UseTestinput  = configGetter TestInputKey |> Option.map bool.Parse |> Option.defaultValue false
    }

let getConfig () =
    if File.Exists configPath then
        parseConfig ()
    else
        defaultConfig
        
let setConfig config =
    let config = [
                     config.Day |> Option.map (fun v -> $"%s{DayKey}=%i{v}")
                     Some($"%s{TestInputKey}=%b{config.UseTestinput}")
                 ] |> Seq.choose id
    
    File.WriteAllLines(configPath, config)
    
        
