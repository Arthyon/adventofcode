[<AutoOpen>]
module AoCHelpers.Parsing

open System

let split (separator: char) (s: string) =
    s.Split (separator, StringSplitOptions.TrimEntries) |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    
let splitInTwo (separator: char) (s:string) =
    let parts = s.Split (separator, StringSplitOptions.TrimEntries)
    if parts.Length <> 2 then failwithf $"Invalid segments. Expected 2, found %i{parts.Length}"
    (parts[0], parts[1])
    
let digitsToInt (digits: Char seq) = String.Join("", digits) |> int