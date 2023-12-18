[<AutoOpen>]
module AoCHelpers.Arrays

open System

let north = (-1,0)
let south = (1,0)
let west = (0,-1)
let east = (0,1)
let northwest = (-1, -1)
let southwest = (1, -1)
let northeast = (-1, 1)
let southeast = (1, 1)

let addMovement (x,y) (xMovement, yMovement) = x + xMovement, y + yMovement 

let cardinalDirections = [north; south; west; east] 
let diagonalDirections = [northwest; southwest; northeast; southeast]
let allDirections = [northwest; southwest; northeast; southeast; north; south; west; east]

let isValidCoordinate array (x,y) =
    let xLength = array |> Array2D.length1
    let yLength = array |> Array2D.length2
    x >= 0 && y >= 0 && x < xLength && y < yLength
     
let private createValidCoordinates (array: 't[,]) (originalX, originalY) modifiers =
         
     modifiers
     |> Seq.map (fun (x,y) -> (originalX + x, originalY + y))
     |> Seq.filter (isValidCoordinate array)
     
     
let validAdjacentCoordinates (array: 't[,]) pos =
     List.concat [cardinalDirections; diagonalDirections ] |> (createValidCoordinates array pos)
     
let validAdjacentCardinalCoordinates (array: 't[,]) pos =
     cardinalDirections |> (createValidCoordinates array pos)

let toArray2d input = input |> Seq.map  (fun l -> l |> Seq.cast<Char>) |> array2D
let flatten (array: 't[,]) =  array |> Seq.cast<'t>

let row (array: 't[,]) i = array[i, *]
let column (array: 't[,]) i = array[*, i]

let chunkWhile predicate list =
    let rec loop chunk chunks list =
        match list with
        | [] -> List.rev((List.rev chunk)::chunks)
        | head::tail when predicate head -> loop [head] ((List.rev chunk)::chunks) tail
        | head::tail -> loop (head::chunk) chunks tail
    loop [] [] list

let print (arr: 't[,]) =
    for x in 0..(arr |> Array2D.length1) - 1 do
        for y in 0..(arr |> Array2D.length2) - 1 do
            printf "%A" arr[x,y]
        printfn ""


let findIndices f (arr: 't[,]) =
    seq {
        for x in 0..(arr |> Array2D.length1) - 1 do
            for y in 0..(arr |> Array2D.length2) - 1 do
                if f arr[x,y] then yield (x,y)
    }
