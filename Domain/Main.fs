namespace Lloyd.Domain

open System
open Lloyd.Domain.UI

module Main =
    let run() =
        let aid = ID.gen()
        let store = MemoryStore.create() |> ref
        let app = Apps.Venue.app()
        let subscription() = MemoryStore.observable store |> Observable.choose (fun (i,l) -> if i=aid then Apps.Venue.Update l |> Some else None)
        let commandHandler (lastEvent,cmd) = if List.isEmpty cmd |> not then MemoryStore.update aid cmd lastEvent store |> ignore
        UI.run (failwith "hi") app subscription commandHandler