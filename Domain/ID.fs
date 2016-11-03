namespace Lloyd.Domain

open System

type EventID = EventID of DateTime*string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = EventID(time,user)
    let time (EventID(t,_)) = t
    let User (EventID(_,u)) = u
    let gen() = EventID(DateTime.UtcNow,"Ant")

type 'Aggregate ID = Created of EventID

module ID =
    let gen() = EventID.gen() |> Created

type 'Aggregate EventUpdate = (EventID * 'Aggregate list) list

[<NoEquality;NoComparison>]
type 'Aggregate MemoryStore = {Updates: Map<'Aggregate ID,'Aggregate EventUpdate>; Observers: IObserver<'Aggregate ID*'Aggregate EventUpdate> list}

module MemoryStore =
    let create() = {Updates=Map.empty; Observers=[]}
    let observable store =
        {new IObservable<_> with
            member __.Subscribe(ob:IObserver<_>) =
                let msd = atomicUpdate store (fun i -> {Updates=i.Updates; Observers=ob::i.Observers})
                Map.toSeq msd.Updates |> Seq.iter ob.OnNext
                {new IDisposable with
                    member __.Dispose() =
                        atomicUpdate store (fun i -> {Updates=i.Updates; Observers=List.where ((<>)ob) i.Observers}) |> ignore
                }
        }
    let update (aid:'Aggregate ID) (updates:'Aggregate list) (lastEvent:EventID option) (storeRef:'Aggregate MemoryStore ref) =
        assert(List.isEmpty updates |> not)
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
type Property<'a,'b> = {Name:string; Getter:'a->'b option; Setter:'b->'a; Default:'b}

module Property =
    let create name setter getter def = {Name=name; Getter=getter; Setter=setter; Default=def}
    let getUpdate (property:Property<'a,'b>) (update:'a EventUpdate) =
        List.choose (fun (e,l) -> List.tryPick property.Getter l |> Option.map (fun i -> e,i)) update
    let set (property:Property<'a,'b>) v = property.Setter v