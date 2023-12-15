module Solutions.Days.Day2

open System
open AoCHelpers

type Bag = {
    Red: int
    Blue: int
    Green: int
}

type Game = {
    Id: int
    Rounds: Bag list
}

let parseGame (game: string) =
    let parseCube (red, blue, green) cube =
        let number, color = splitInTwo ' ' cube
        match color with
        | "red" ->  (red + int number, blue, green)
        | "blue" -> (red, blue + int number, green)
        | "green" -> (red, blue, green + int number)
        | _ -> failwith "invalid cube"
        
    let red, blue, green = game |> split ',' |> Seq.fold parseCube (0,0,0)
    {
        Red = red; Blue = blue; Green = green
    }
    
let parseLine (line: string) =
    let id, rounds = splitInTwo ':' line
    let id = id.Replace ("Game ", "") |> int
    let rounds =  rounds |> split ';' |> Seq.map parseGame
    { Id = id ; Rounds = rounds |> Seq.toList }

let isRoundPossible bag round = bag.Green >= round.Green && bag.Blue >= round.Blue && bag.Red >= round.Red
let isGamePossible bag game = game.Rounds |> Seq.forall (isRoundPossible bag)

let getMinimumCubes game =
    let getMinimumCubes' current next =
        { Red = Math.Max(current.Red, next.Red) ; Blue = Math.Max(current.Blue, next.Blue) ; Green = Math.Max (current.Green, next.Green) }
        
    game.Rounds |> Seq.reduce getMinimumCubes'
    
let calculateCubePower minimumBag =
    minimumBag.Red * minimumBag.Blue * minimumBag.Green
    
let run lines =
    let games = lines |> Seq.map parseLine
    
    // Part 1
    let bag = { Blue = 14 ; Red = 12 ; Green = 13 }
    let part1 = games |> Seq.filter (isGamePossible bag) |> Seq.map _.Id |> Seq.sum
    
    // Part 2
    let cubePower = games |> Seq.map getMinimumCubes |> Seq.map calculateCubePower |> Seq.sum
    
    printResult part1 cubePower
    
    