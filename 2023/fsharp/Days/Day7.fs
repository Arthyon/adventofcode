module Solutions.Days.Day7

open System
open AoCHelpers

let toValue joker = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' when joker -> 1
    | 'J' -> 11
    | 'T' -> 10
    | v   -> Char.GetNumericValue v |> int
    
let toComparableHand = function
    | [(5,card)] -> 7
    | [(4, card1); (1, card2)] -> 6
    | [(3, card1); (2, card2)] -> 5
    | [(3, card1); (1, card2); (1, card3)] -> 4
    | [(2, card1); (2, card2); (1, card3)] -> 3
    | [(2, card1); (1, card2); (1, card3); (1, card4)] -> 2
    | [(1, card1); (1, card2); (1, card3); (1, card4) ; (1, card5)] -> 1
    | a -> failwithf $"Invalid hand %A{a}"
    
let sortHands ((hand1:int, cards1),_) ((hand2, cards2),_) =
    let c = compare hand1 hand2
    if c <> 0 then c
    else
        let re = (cards1, cards2) ||> Seq.zip |> Seq.map (fun (c1, c2) -> compare c1 c2) |> Seq.skipWhile (fun i -> i = 0) |> Seq.tryHead
        match re with
        | Some(v) -> v
        | None -> 0
    
let modifyHand joker list =
    if not joker then list 
    else
        let jokers = list |> Seq.filter (fun (n, card) -> card = 1) |> Seq.map (fun (n, _) -> n) |> Seq.tryHead |> Option.defaultValue 0
        if jokers = 5 then
            seq { (5, 14) }
        else
            let (_, maxCard) = list |> Seq.filter (fun (n, card) -> card <> 1 ) |> Seq.maxBy fst
            seq {
                for n, card in list do
                    if card = maxCard then
                        yield (n + jokers, card)
                    else if card <> 1 then
                        yield (n, card)
                
                
                } 
let parseHand joker (cards, bid) =
    let values = cards |> Seq.map (toValue joker)
    let grouped = values |> Seq.groupBy id |> Seq.map (fun (key, cards) -> cards |> Seq.length, key) |> (modifyHand joker) |> Seq.sortDescending |> Seq.toList |> toComparableHand
    (grouped, values), int bid
    
let parse input joker = input |> Seq.map (splitInTwo ' ') |> Seq.map (parseHand joker)
let run input =
    // Part1
    let hands = parse input false
    let part1 = hands |> Seq.sortWith sortHands |> Seq.map snd |> Seq.indexed |> Seq.map (fun (i,bid) -> (i + 1) * bid) |> Seq.sum
    
    // Part2
    let hands = parse input true
    let part2 = hands |> Seq.sortWith sortHands |> Seq.map snd |> Seq.indexed |> Seq.map (fun (i,bid) -> (i + 1) * bid) |> Seq.sum
    
    printResult part1 part2
    

