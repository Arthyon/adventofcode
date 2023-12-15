module Solutions.Days.Day4
open System
open System.Collections.Generic
open AoCHelpers
type Game = int * (seq<int> * seq<int>)
let parseLine line =
    let parseNumbers numbers = numbers |> split ' ' |> Seq.map int
    let _, numbers = splitInTwo ':' line
    let winningNumbers, cardNumbers = splitInTwo '|' numbers ||> (fun w c -> (parseNumbers w, parseNumbers c))
    winningNumbers, cardNumbers
    
let isWinningNumber winningNumbers number = winningNumbers |> Seq.contains number
let getWinningNumbers (winningNumbers, cardNumbers) = cardNumbers |> Seq.filter (isWinningNumber winningNumbers) |> Seq.length
let calculatePoints wins = pown 2 (wins - 1)
    
let cache = Dictionary<int,int>()
let tryGetCache idx fetcher =
    match cache.TryGetValue idx with
    | true, value -> value
    | false, _    -> fetcher idx
            
let draw (cards: Game list) (cardIdx, _) =
    let rec draw' acc cardIdx =
        let _, ticket = cards[cardIdx]
        let winningNumbers = getWinningNumbers ticket
        let from = cardIdx + 1
        if winningNumbers = 0 || from >= cards.Length then
            acc + 1
        else
            let take = Math.Min(from + winningNumbers, cards.Length) - 1
            
            (1, cards[from..take]) ||> Seq.fold (fun acc (idx,_) -> acc + tryGetCache idx (draw' 0))
                
    let sum = draw' 0 cardIdx
    cache.Add (cardIdx, sum)
    sum
    
let run input =
    let games = input |> Seq.map parseLine |> Seq.toList
    
    // Part 1
    let part1 = games |> Seq.map (getWinningNumbers >> calculatePoints) |> Seq.sum
    
    // Part 2
    let indexedCards = games |> Seq.indexed |> Seq.toList
    let part2 = indexedCards |> Seq.rev |> Seq.map (draw indexedCards) |> Seq.sum
    
    printResult part1 part2
    
   

