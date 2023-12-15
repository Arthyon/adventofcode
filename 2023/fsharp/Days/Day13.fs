module Solutions.Days.Day13

open AoCHelpers
open System

type Map = {
    rows: char list list
    cols: char list list
}

// let findSymmetryCount (list: char list list) idx =
//     let len = list |> List.length
//     let rec find' leftIdx rightIdx count =
//         
//         if rightIdx >= len && leftIdx >= 0 then
//             count + leftIdx + 1
//         else if leftIdx < 0 || rightIdx >= len then
//             count
//         else if list[leftIdx] = list[rightIdx] then
//             find' (leftIdx - 1) (rightIdx + 1) (count + 1)
//         else
//             0
//             
//     let leftIdx = idx - 1
//     let rightIdx = idx + 2
//     find' leftIdx rightIdx 1
//     
//     
// let tryFindSymmetries list =
//     let isEqual (list: (int * char list)[]) =
//         snd list[0] = snd list[1]
//     
//     list |> Seq.indexed |> Seq.windowed 2 |> Seq.filter isEqual |> Seq.map (fun [|idx,_;_|] -> idx)

let findDifferences l r =
    (l,r) ||> Seq.zip |> Seq.fold (fun s (l,r) -> if l <> r then s + 1 else s) 0
let isReflectionLine (left: char list list) (right: char list list) allowedDiff =
    let left = left |> List.rev
    
    let check s (l,r) =
        match s with
        | None -> None
        | Some(li,allowedDiff) -> 
            let diffs = findDifferences l r
            if diffs = 0 then
                Some((l,r), allowedDiff)
            else if allowedDiff >= diffs then
                Some((l,r), Math.Max(allowedDiff - diffs, 0))
            else
                None
        
    let li = (Some(([],[]), allowedDiff), Seq.zip left right) ||> Seq.scan check |> Seq.takeWhile Option.isSome //|> Seq.length
    let l = (li |> Seq.length) - 1
    let last = li |> Seq.tryLast |> Option.defaultValue None
    
    let res = l = left.Length || l = right.Length
    match res, last with
    | true, Some(_, 0) -> true
    | _ -> false
    
        
    
let findReflectionLines (list: char list list) allowedDiff =
    [0..list.Length - 1] |> Seq.choose (fun idx ->
        let split = list |> List.chunkBySize (idx + 1)
        if split.Length <= 1 then None
        else if isReflectionLine split[0] split[1] allowedDiff then Some(idx + 1)
        else None)
    
let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let createMap rows =
    let rows = rows |> Seq.map (fun r -> r |> Seq.cast<char> |> Seq.toList) |> Seq.filter (fun l -> l.Length > 0) |> Seq.toList
    let cols = rows |> transpose
    { rows = rows; cols = cols }

let solve allowedDiff (r,c) map =
    let newR = findReflectionLines map.rows allowedDiff |> Seq.sortDescending |> Seq.tryHead  |> Option.defaultValue 0
    let newC = findReflectionLines map.cols allowedDiff |> Seq.sortDescending |> Seq.tryHead  |> Option.defaultValue 0
    (r + newR,c + newC)
    
let calculate rows columns = columns + (rows * 100)
let run (input: string[]) =
    let maps = input |> Seq.toList |> chunkWhile String.IsNullOrEmpty |> Seq.map createMap |> Seq.toList
    
    let part1 = ((0,0), maps) ||> Seq.fold (solve 0) ||> calculate
    let part2 = ((0,0), maps) ||> Seq.fold (solve 1) ||> calculate
    
    printResult part1 part2
