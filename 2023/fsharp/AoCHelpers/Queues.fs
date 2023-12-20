[<AutoOpen>]
module AoCHelpers.Queues

open System.Collections.Generic

let nextItem (q: Queue<_>) =
    match q.Count with
    | 0 -> None
    | _ -> Some (q.Dequeue())

let enqueue (q: Queue<_>) items =
    for i in items do
        q.Enqueue i