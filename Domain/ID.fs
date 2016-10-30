namespace Lloyd.Domain

open System

type Result<'o,'e> =
    | Ok of 'o
    | Error of 'e

[<AutoOpen>][<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
    let map f r = match r with | Ok x -> Ok (f x) | Error e -> Error e
    let bind binder r = match r with |Ok i -> binder i |Error e -> Error e
    let apply f x =
        match f,x with
        | Ok f, Ok v -> Ok (f v)
        | Error f, Ok _ -> Error f
        | Ok _, Error f -> Error [f]
        | Error f1, Error f2 -> Error (f2::f1)
    let (<*>) = apply
    let ofOption format =
        let sb = System.Text.StringBuilder()
        Printf.kbprintf (fun ()->function |Some x -> Ok x |None -> sb.ToString() |> Error) sb format
    type AttemptBuilder() =
        member __.Bind(v,f) = bind f v
        member __.Return v = Ok v
        member __.ReturnFrom o = o
    let attempt = AttemptBuilder()

type EventID = {Time:DateTime;User:int}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = {Time=time;User=user}
    let time e = e.Time
    let User e = e.User

type 'Aggregate ID = Created of EventID

type 'Aggregate LogEntry = 'Aggregate ID * (EventID * 'Aggregate list)

type 'a SetEvent =
    | Add of 'a
    | Remove of 'a

type 'Aggregate IStore =
    inherit IObservable<'Aggregate LogEntry>
    abstract member TryPersist : 'Aggregate LogEntry * lastEventID:EventID -> bool
// Need to filter by datetime, aid, field

module Test =
    let xx (a:#IStore<_>) = a :> IStore<_>

type 'Aggregate InMemoryStore() =
    static let mutable observers = []
    static let mutable events = []
    interface 'Aggregate IStore with
        member __.Subscribe(ob:IObserver<'Aggregate LogEntry>) =
            observers <- ob::observers
            List.iter ob.OnNext events
            {new IDisposable with member __.Dispose()= observers <- List.where ((=)ob) observers}
        member __.TryPersist((aid,(eid,_)) as e, lastEventID) =
            assert(eid>lastEventID)
            match Seq.tryFind (fst>>(=)aid) events with
            | Some (_,(eid,_)) when eid=lastEventID ->
                events <- e::events
                observers |> List.iter (fun ob -> ob.OnNext e)
                true
            | _ -> false

type 'Aggregate IRoot =
    inherit IObservable<EventID * 'Aggregate list>
    abstract member TryPersist : 'Aggregate list * lastEventID:EventID -> bool