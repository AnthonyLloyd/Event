namespace Lloyd.Domain

open System

type EventID = {Time:DateTime;User:int}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = {Time=time;User=user}

type 'Aggregate ID = Created of EventID

type 'Aggregate LogEntry = 'Aggregate ID * (EventID * 'Aggregate list)

type 'a SetEvent =
    | Add of 'a
    | Remove of 'a

type 'Aggregate IStore =
    inherit IObservable<'Aggregate LogEntry>
    abstract member TryPersist : 'Aggregate LogEntry * lastEventID:EventID -> bool

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