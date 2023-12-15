module Solutions.Days.Day12

open System.Collections.Generic
open System.Diagnostics
open System.Linq
open AoCHelpers

let parse line =
    let springs,criteria = splitInTwo ' ' line
    springs |> Seq.cast<char> |> Seq.toList, split ',' criteria |> Seq.map int |> Seq.toList
    
let splitRow springs =
    let mutable c = 0
    seq {
        for s in springs do
            if s = '#' then c <- c + 1 
            else if s = '.' && c <> 0 then
                yield c
                c <- 0
        if c <> 0 then yield c
    } |> Seq.toList
    
let permutations springs =
    let rec permutations' springs (s: char list) =
        seq {
            match springs with
            | [] -> if s.Length > 0 then yield (s |> Seq.rev |> Seq.toArray |> System.String)
            | '?'::t ->
                yield! permutations' t ('.'::s)
                yield! permutations' t ('#'::s)
            | h::t -> yield! permutations' t (h::s)
        }
    permutations' springs [] |> Seq.toList
    
let solve (springs, criteria: int list) =
    let mutable sum = 0.0
    for p in  permutations springs do
        let parts = splitRow p
        if parts = criteria then
            sum <- sum + 1.0
    sum

let partialSolutionIsValid cmp (springs: char list) criteria =
    let lens = springs[..criteria - 1] // Lens into the relevant springs based on criteria
    let criteriaFitsRemainingSprings = cmp criteria springs.Length
    let noWorkingCogsInTheMiddle = lens |> List.contains '.' |> not
    
    criteriaFitsRemainingSprings && noWorkingCogsInTheMiddle && (criteria = springs.Length || springs[criteria] <> '#') // If remaining springs fits criteria OR the next char after criteria is fulfilled is not broken

let fasterSolve cmp (springs, original_criteria: int list) =
    let cache = Dictionary<char list * int list, float>()
    let rec solve' restOfSprings (criteria: int list) =
        if cache.ContainsKey (restOfSprings,criteria) then
            cache[restOfSprings,criteria]
        else
            match restOfSprings, criteria with
            | [], [] -> 1. // Empty remaining springs, empty criteria
            | [], _ -> 0 // Empty springs, still unfulfilled criteria
            | s, [] -> if s |> List.contains '#' then 0 else 1 // No more criteria, success if no more damaged springs
            | '.'::t, c -> solve' t c // Skip .
            | '#'::t, c::ct -> // Main logic
                    let springs = '#'::t // cons list again, easier code below
                    
                    if partialSolutionIsValid cmp springs c then
                         let newSprings = springs[c + 1..]
                         solve' newSprings ct
                    else 0
                    
            | '?'::t, c ->
                let l = solve' ('.'::t) c // Replace with .
                let r = solve' ('#'::t) c // Replace with #
                let sum = l + r
                cache.Add(('?'::t,c), sum)
                sum
                
            | d -> failwithf "unhandled %A" d   
                
    let springs = springs |> List.skipWhile (fun c -> c = '.')
    let sum = solve' springs original_criteria
    sum

    
let unfold (springs, criteria: int list) =
    Enumerable.Repeat (springs @ ['?'], 5) |> Seq.collect id |> Seq.toList, Enumerable.Repeat (criteria, 5) |> Seq.collect id |> Seq.toList
    
let run input =
    let rows = input |> Seq.map parse
    
    let sw = Stopwatch.StartNew()
    
    // A specific comparison must use different comparison operator (<= vs <) for part1 and 2. I have NO idea why
    
    // Part 1
    let part1 = rows |> Seq.map (fasterSolve (<=)) |> Seq.sum
    
    printAndRestart sw 
    
    // Part 2
    let part2 =  rows
                 |> Seq.map (unfold >> (fasterSolve (<))) |> Seq.sum
    printAndStop sw
    
    printResult part1 part2