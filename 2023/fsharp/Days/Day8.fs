module Solutions.Days.Day8

open System.Collections.Generic
open AoCHelpers

let parse line =
    let node, elements = splitInTwo '=' line
    let directions = elements.Trim [|'(';')'|] |> splitInTwo ','
    (node, directions)
    
let move directions = seq { while true do yield! directions }

let solve (nodes: IDictionary<string, string * string>) (movement: IEnumerator<char>) (currentNode: string)  =
    let mutable steps = 0
    let mutable currentNode = currentNode
    while (currentNode.EndsWith 'Z' |> not) do
        movement.MoveNext() |> ignore
        steps <- steps + 1
        let l,r = nodes[currentNode]
        currentNode <- match movement.Current with
                        | 'R' -> r
                        | 'L' -> l
                        | _ -> failwith "f"
    
    float steps
    
let run (input: string[]) =
    let directions = input[0]
    let movement = (move directions)
    
    let nodes = input[2..] |> Seq.map parse |> dict
    
    // Part1
    let part1 = solve nodes (movement.GetEnumerator()) "AAA"
    
    // Part2
    let startNodes = nodes |> Seq.map (_.Key) |> Seq.filter (fun n -> n.EndsWith 'A') |> Seq.toList
    let paths = startNodes |> Seq.map (solve nodes (movement.GetEnumerator())) |> Seq.toList
    let part2 = lcmOfMultiple paths
    
    printResult part1 part2
    