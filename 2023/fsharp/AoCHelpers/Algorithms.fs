[<AutoOpen>]
module AoCHelpers.Algorithms

let rec gcd x y = if y = 0.0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

let lcmOfMultiple numbers = numbers |> Seq.reduce lcm

let modulo x m = (x%m + m)%m