[<AutoOpen>]
module AoCHelpers.Seqs

open System

let mapFst f s = s |> Seq.map (fun (l,r) -> f l, r)
let mapSnd f s = s |> Seq.map (fun (l,r) -> l, f r)