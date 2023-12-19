module Solutions.Days.Day19

open System.Collections.Generic
open System.Text.RegularExpressions
open AoCHelpers
open System

type Part = {
    Ratings: Map<string, int>
}

type Rule = {
    Evaluate: Part -> bool
    ComparisonData: (int * string * string) option
    Workflow: string
}

let createRule r =
    let m = Regex(@"(\w+)([<>])(\d+):(\w+)").Match r
    if m.Success then
        let rating = m.Groups[1].Value
        let comparison = if m.Groups[2].Value = "<" then (<) else (>)
        let number = int m.Groups[3].Value
        let workflow = m.Groups[4].Value
        { Evaluate = (fun p -> comparison p.Ratings[rating] number) ; Workflow = workflow; ComparisonData = Some(number, rating,  m.Groups[2].Value) }
    else
        { Evaluate = (fun _ -> true) ; Workflow = r ; ComparisonData = None }
    
let parseWorkflow line =
    let m = Regex(@"(\w+){(.+)}").Match line
    
    (m.Groups[1].Value, m.Groups[2].Value |> split ',' |> Seq.map createRule |> Seq.toList)
let parsePart (line: string) =
    { Ratings = line.Trim([|'{';'}'|]) |> split ',' |> Seq.map (splitInTwo '=') |> mapSnd int |> Map }

let solve workflows part =
    let mutable wf = "in"
    while wf <> "A" && wf <> "R" do
        let passedRule = workflows |> Map.find wf |> Seq.find (fun rule -> rule.Evaluate part)
        wf <- passedRule.Workflow
        
    wf = "A"
        
let calculateTotalRating part = part.Ratings.Values |> Seq.sum

type Combinations = {
    x: int * int
    m: int * int
    a: int * int
    s: int * int
}

let sanityCheck (min,max) (newMin,newMax) =
    if newMax > max then false
    else if newMin < min then false
    else true
let mergeCombinations combination1 combination2 =
    {
     x = Math.Max(fst combination1.x, fst combination2.x), Math.Min(snd combination1.x, snd combination2.x)
     m = Math.Max(fst combination1.m, fst combination2.m), Math.Min(snd combination1.m, snd combination2.m)
     a = Math.Max(fst combination1.a, fst combination2.a), Math.Min(snd combination1.a, snd combination2.a)
     s = Math.Max(fst combination1.s, fst combination2.s), Math.Min(snd combination1.s, snd combination2.s)
    }
let modifyCombination combination modifier comparisonField =
    let n = match comparisonField with
            | "x" ->
                let min,max = modifier combination.x
                if sanityCheck combination.x (min,max) then Some({ combination with x = (min,max) }) else None
            | "m" ->
                let min,max = modifier combination.m
                if sanityCheck combination.m (min,max) then Some( { combination with m = (min,max) }) else None
            | "a" ->
                let min,max = modifier combination.a
                if sanityCheck combination.a (min,max) then Some({ combination with a = (min,max) }) else None
            | "s" ->
                let min,max = modifier combination.s
                if sanityCheck combination.s (min,max) then Some( { combination with s = (min,max) }) else None
            | _ -> failwith "invalid"
    match n with
    | Some(n) -> Some(mergeCombinations combination n)
    | None -> None
    
let applyRule combination number operator comparisonField =
    let modifier (min,max) = if operator = "<" then min, (number - 1) else (number + 1, max)
    modifyCombination combination modifier comparisonField
    
let invertRule combination number operator comparisonField =
    let modifier (min,max) = if operator = ">" then min, number else number, max
    modifyCombination combination modifier comparisonField
    
let diff (min, max) = float (max - min + 1)

let solvePart2 workflows =
    let startWorkflow = workflows |> Map.find "in"
    
    let rec traverseWorkflow wf combo =
        let allValidCombos = List<Combinations>()
        let mutable currentCombo = combo
        for rule in wf do
            match rule.ComparisonData with
            | Some(number, comparisonField, operator) ->
                match applyRule currentCombo number operator comparisonField with
                | Some(ruleHit) ->
                    if rule.Workflow = "R" then ()
                    else if rule.Workflow = "A" then allValidCombos.Add(ruleHit)
                    else
                        let ruleHitTraversal = traverseWorkflow workflows[rule.Workflow] ruleHit
                        allValidCombos.AddRange(ruleHitTraversal)
                | None -> ()
                
                currentCombo <- match invertRule currentCombo number operator comparisonField with | Some(r) -> r | None -> failwithf $"Sanity check failed while inverting %A{currentCombo}"
            | None ->
                if rule.Workflow = "R" then ()
                else if rule.Workflow = "A" then allValidCombos.Add(currentCombo)
                else
                    allValidCombos.AddRange(traverseWorkflow workflows[rule.Workflow] currentCombo)
                
        allValidCombos    
        
    let validCombinations = traverseWorkflow startWorkflow {x = (1,4000); m = (1,4000); a = (1,4000); s = (1,4000) }
    validCombinations |> Seq.map (fun c -> (diff c.x) * (diff c.m) * (diff c.a) * (diff c.s)) |> Seq.sum
    
    
        
let run input =
    let workflows = input |> Seq.takeWhile (String.IsNullOrEmpty >> not) |> Seq.map parseWorkflow |> Map
    let parts = input |> Seq.skip (workflows.Count + 1) |> Seq.map parsePart
    
    // Part 1
    let part1 = parts |> Seq.filter (solve workflows) |> Seq.map calculateTotalRating |> Seq.sum
    
    // Part 2
    let part2 = solvePart2 workflows
    
    printResult part1 part2

