module Solutions.Days.Day18

open System
open AoCHelpers

let parsePart1 line =
    let parts = split ' ' line |> Seq.toList
    char parts[0], float parts[1]
    
let parsePart2 line = 
    let i = split ' ' line |> Seq.toList |> Seq.last
    let i = i.Trim([|')'; '('|]).TrimStart('#')
    let number = Convert.ToInt64(i[0..^1], 16)
    let dir = match i[^0] with
              | '0' -> 'R'
              | '1' -> 'D'
              | '2' -> 'L'
              | '3' -> 'U'
              | _ -> failwith "invalid"
    dir, float number

let getMovement = function
    | 'R' -> east
    | 'D' -> south
    | 'L' -> west
    | 'U' -> north
    | _ -> failwith "Invalid direction"
    
let dig ((x,y),trench, boundary) (direction, count) =
    let moveX, moveY = getMovement direction
    let nextCoordinate = x + (float moveX) * count, y + (float moveY) * count
    nextCoordinate, nextCoordinate::trench, boundary + count
    
// From https://rosettacode.org/wiki/Shoelace_formula_for_polygonal_area#F#
let shoelace (n::g) = abs(List.pairwise(n::g@[n])|>List.fold(fun n ((nα,gα),(nβ,gβ))->n+(nα*gβ)-(gα*nβ)) 0.0)/2.0

let solve instructions =
    let startTile = 0.,0.
    
    let _,trench, boundary = ((startTile, [], 0.), instructions) ||> Seq.fold dig
    let trench = trench |> List.rev
    
    // Shoelace for area, pick's theorem solving for interior points instead of area, sum boundary + interior points
    let area = shoelace trench
    let interior = (boundary / 2.0) - 1. - area
    
    boundary + Math.Abs(interior)
    
let run input =
    
    // Part 1
    let instructions = input |> Seq.map parsePart1
    let part1 = solve instructions
    
    // Part 2
    let instructions = input |> Seq.map parsePart2 |> Seq.toList
    let part2 = solve instructions
    
    printResult part1 part2
    