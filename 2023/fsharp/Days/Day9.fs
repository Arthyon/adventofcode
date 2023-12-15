module Solutions.Days.Day9

open AoCHelpers

let parseLine line =
    split ' ' line |> Seq.map int |> Seq.toList
    
let getDifferenceNumber reverse (list: 'a list) =
    if reverse then list[0] else list[^0]
    
let solve reverse (numbers: int list) =
    let diff = getDifferenceNumber reverse
    let rec reduce numbers differenceNumbers =
        let calculatedRow = numbers |> Seq.pairwise |> Seq.map (fun (l,r) -> r - l) |> Seq.toList
        if calculatedRow |> Seq.forall (fun v -> v = 0) then
            let reducer = if reverse then (fun l r -> r - l) else (+)
            0::differenceNumbers |> Seq.reduce reducer
        else
            reduce calculatedRow ((diff calculatedRow)::differenceNumbers)
            
    reduce numbers [diff numbers]        
    
let run input =
    let numbers = input |> Seq.map parseLine 
    
    // Part 1
    let part1 = numbers |> Seq.map (solve false) |> Seq.sum
    
    // Part 2
    let part2 = numbers |> Seq.map (solve true) |> Seq.sum
    
    printResult part1 part2

