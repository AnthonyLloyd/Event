namespace Lloyd.Domain

open System

type EventID = {Time:DateTime; User:int}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = {Time=time; User=user}
    let time e = e.Time
    let User e = e.User
    let gen() = {Time=DateTime.UtcNow; User=1}

type 'Aggregate ID = Created of EventID

module ID =
    let gen() = EventID.gen() |> Created

type 'Aggregate EventUpdate = 'Aggregate ID * (EventID * 'Aggregate list) list

[<NoEquality;NoComparison>]
type 'Aggregate MemoryStore =
    {
        Updates: Map<'Aggregate ID,(EventID * 'Aggregate list) list>
        Observers: IObserver<'Aggregate EventUpdate> list
    }

module MemoryStore =
    let create() = {Updates=Map.empty; Observers=[]}
    let observable (store:'Aggregate MemoryStore ref) =
        {new IObservable<'Aggregate EventUpdate> with
            member __.Subscribe(ob:IObserver<'Aggregate EventUpdate>) =
                let msd = atomicUpdate store (fun i -> {Updates=i.Updates; Observers=ob::i.Observers})
                Map.toSeq msd.Updates |> Seq.iter ob.OnNext
                {new IDisposable with
                    member __.Dispose() =
                        atomicUpdate store (fun i -> {Updates=i.Updates; Observers=List.where ((<>)ob) i.Observers}) |> ignore
                }
        }
    let update (aid:'Aggregate ID) (updates:'Aggregate list) (lastEvent:EventID) (store:'Aggregate MemoryStore ref) =
        atomicUpdateWithQuery store (fun i ->
            match Map.tryFind aid i.Updates with
            | Some l ->
                if List.head l |> fst = lastEvent then i,None
                else
                    let eid = EventID.gen()
                    {Updates=Map.add aid ((eid,updates)::l) i.Updates; Observers=i.Observers}, Some eid
            | None ->
                let eid = EventID.gen()
                {Updates=Map.add aid [eid,updates] i.Updates; Observers=i.Observers}, Some eid
        )