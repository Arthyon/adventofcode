module Solutions.Days.Day3

open System
open AoCHelpers

type Coordinate = {
    X: int
    Y: int
    Content: Char
}

let getNumber (array:Coordinate[,]) (x,y) =
    array[x,y..] |> Seq.map (fun c -> c.Content) |> Seq.takeWhile Char.IsNumber |> digitsToInt

let getStartOfNumber (array: Coordinate[,]) (x,y) =
    let row = array[x, ..y]
    
    let cell = row |> Seq.rev |> Seq.skipWhile (fun c -> Char.IsNumber c.Content) |> Seq.tryHead
    match cell with
    | None -> (x, 0)
    | Some(el) -> (el.X, el.Y + 1)

let getAdjacentNumbers array coordinate =
    validAdjacentCoordinates array (coordinate.X, coordinate.Y)
    |> Seq.filter (fun (x,y) -> Char.IsNumber array[x,y].Content)

let toCoordinate x y value = { X = x; Y = y; Content = value }
let isSymbol coordinate = (Char.IsNumber coordinate.Content || coordinate.Content = '.') |> not
let isGearSymbol coordinate = coordinate.Content = '*'

let calculateGearRatio array numbers =
    let numberList = numbers |> Seq.map (getStartOfNumber array) |> Seq.distinct |> Seq.toList
    if numberList.Length = 2 then
        let num1 = getNumber array numberList[0]
        let num2 = getNumber array numberList[1]
        Some(num1 * num2)
    else None
    
let run input =
    let array = input |> toArray2d |> Array2D.mapi toCoordinate
    // Part 1
    let symbols = array |> flatten |> Seq.filter isSymbol
    let part1 = symbols |> Seq.collect (getAdjacentNumbers array) |> Seq.map (getStartOfNumber array) |> Seq.distinct |> Seq.map (getNumber array) |> Seq.sum
    
    // Part 2
    let gears = array |> flatten |> Seq.filter isGearSymbol
    let part2 = gears |> Seq.map (getAdjacentNumbers array) |> Seq.choose (calculateGearRatio array) |> Seq.sum
    
    printResult part1 part2