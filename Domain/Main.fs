namespace Lloyd.Domain

open System

module Main =
    let run() =
        let aid = ID.gen()
        let store = MemoryStore.create() |> ref
        let app = Apps.Venue.app (fun () -> MemoryStore.observable store |> Observable.choose (fun (i,l) -> if i=aid then Apps.Venue.Update l |> Some else None))
        UI.UI.run (failwith "hi") app |> ignore
        ()