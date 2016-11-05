namespace Lloyd.Core

open System

type EventID = EventID of time:DateTime*user:string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = EventID(time,user)
    let time (EventID(t,_)) = t
    let User (EventID(_,u)) = u
    let gen() = EventID(DateTime.UtcNow,"Ant")

type 'Aggregate ID = Created of EventID

module ID =
    let gen() = EventID.gen() |> Created

type 'Aggregate Events = (EventID * 'Aggregate list) list

[<NoEquality;NoComparison>]
type 'Aggregate MemoryStore = {Updates: Map<'Aggregate ID,'Aggregate Events>; Observers: IObserver<'Aggregate ID*'Aggregate Events> list}

[<NoEquality;NoComparison>]
type 'Aggregate Store =
    | MemoryStore of 'Aggregate MemoryStore ref

module Store =
    let emptyMemoryStore() = {Updates=Map.empty; Observers=[]} |> ref |> MemoryStore
    let observable store =
        match store with
        | MemoryStore storeRef ->
            {new IObservable<_> with
                member __.Subscribe(ob:IObserver<_>) =
                    let msd = atomicUpdate storeRef (fun i -> {Updates=i.Updates; Observers=ob::i.Observers})
                    Map.toSeq msd.Updates |> Seq.iter ob.OnNext
                    {new IDisposable with
                        member __.Dispose() =
                            atomicUpdate storeRef (fun i -> {Updates=i.Updates; Observers=List.where ((<>)ob) i.Observers}) |> ignore
                    }
            }
    let update (aid:'Aggregate ID) (updates:'Aggregate list) (lastEvent:EventID option) (store:'Aggregate Store) =
        assert(List.isEmpty updates |> not)
        match store with
        | MemoryStore storeRef ->
            let store,oeid =
                atomicUpdateWithQuery storeRef (fun store ->
                    match Map.tryFind aid store.Updates with
                    | Some ((eid,_)::_) when Some eid <> lastEvent -> store,None
                    | o ->
                        let eid = EventID.gen()
                        printfn "MemoryStore.update: %A %A" eid updates
                        {Updates=Map.add aid ((eid,updates)::Option.getElse [] o) store.Updates; Observers=store.Observers}, Some eid
                )
            if Option.isSome oeid then store.Observers |> Seq.iter (fun ob -> ob.OnNext(aid,Map.find aid store.Updates))
            oeid

[<NoEquality;NoComparison>]
type Property<'a,'b> = {Name:string; Getter:'a->'b option; Setter:'b->'a; Default:'b option}

module Property =
    let create name setter getter defaultValue = {Name=name; Getter=getter; Setter=setter; Default=defaultValue}
    let getUpdate (property:Property<'a,'b>) (update:'a Events) =
        List.choose (fun (e,l) -> List.tryPick property.Getter l |> Option.map (fun i -> e,i)) update
    let set (property:Property<'a,'b>) v = property.Setter v