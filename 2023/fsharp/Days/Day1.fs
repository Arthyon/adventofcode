module Solutions.Days.Day1

open System
open AoCHelpers

let parse num1 num2 = $"%i{num1}%i{num2}" |> int

let getIndexedNumbers (line: string) =
        line |> Seq.indexed |> Seq.filter (fun (_,c) -> Char.IsNumber c) |> Seq.map (fun (i,c) -> (i, int (Char.GetNumericValue c)))
        
let transform (line: string) (substring: string, newValue: int) =
        seq {
                let idx = line.IndexOf substring
                if idx <> -1 then yield (idx, newValue)
                
                let lastidx = line.LastIndexOf substring
                if lastidx <> -1 && lastidx <> idx then yield (lastidx, newValue) 
        }
        
let replaceNumberStrings (line: string) =
        let nums = getIndexedNumbers line |> Seq.toList
        let t = [
                "one",1
                "two", 2
                "three", 3
                "four", 4
                "five", 5
                "six", 6
                "seven", 7
                "eight", 8
                "nine", 9
                 ] |> Seq.collect (transform line) |> Seq.toList
       
        t @ nums
        
        
let getNumber numbers =
        match numbers |> Seq.sortBy fst |> Seq.map snd |> Seq.toList with
        | [] -> 0
        | [v] -> parse v v
        | list -> parse list[0] list[^0]
        

let run input =
        // Part 1
        let part1 = input |> Seq.map (getIndexedNumbers >> getNumber) |> Seq.sum
        
        // Part 2
        let part2 = input |> Seq.map (replaceNumberStrings >> getNumber) |> Seq.sum
        
        printResult part1 part2