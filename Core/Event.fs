namespace Lloyd.Core

open System

type EventID = |EventID of time:DateTime*user:string
               static member Zero = EventID(DateTime.MinValue,String.empty)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let create time user = EventID(time,user)
    let time (EventID(t,_)) = t
    let User (EventID(_,u)) = u
    let gen() = EventID(DateTime.UtcNow,"Ant") // TODO: increment time so now dups

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
    let observable (store:'Aggregate Store) =
        match store with
        | MemoryStore storeRef ->
            {new IObservable<_> with
                member __.Subscribe(ob:IObserver<_>) =
                    let _,newStore = atomicUpdate (fun i -> {Updates=i.Updates; Observers=ob::i.Observers}) storeRef
                    Map.toSeq newStore.Updates |> Seq.iter ob.OnNext
                    {new IDisposable with
                        member __.Dispose() =
                            atomicUpdate (fun i -> {Updates=i.Updates; Observers=List.where ((<>)ob) i.Observers}) storeRef |> ignore
                    }
            }

    let update (aid:'Aggregate ID) (updates:'Aggregate list) (lastEvent:EventID) (store:'Aggregate Store) =
        assert(List.isEmpty updates |> not)
        match store with
        | MemoryStore storeRef ->
            let newStore,oeid =
                atomicUpdateQuery (fun store ->
                    match Map.tryFind aid store.Updates with
                    | Some ((eid,_)::_) when eid<>lastEvent -> store, None
                    | o ->
                        let eid = EventID.gen()
                        printfn "MemoryStore.update: %A %A" eid updates
                        {Updates=Map.add aid ((eid,updates)::Option.getElse [] o) store.Updates; Observers=store.Observers}, Some eid
                ) storeRef
            if Option.isSome oeid then newStore.Observers |> Seq.iter (fun ob -> ob.OnNext(aid,Map.find aid newStore.Updates))
            oeid

type 'a SetEvent =
    | SetAdd of 'a
    | SetRemove of 'a

module SetEvent =
    let difference (before:'a Set) (after:'a Set) =
        Seq.append (after-before |> Set.toSeq |> Seq.map SetAdd) (before-after |> Set.toSeq |> Seq.map SetRemove) |> Seq.toList

    let update events s =
        let removedAndAdded =
            List.fold (fun (removed,added) se ->
                    match se with
                    | SetAdd a -> if Set.contains a removed then removed,added else removed,Set.add a added
                    | SetRemove a -> Set.add a removed,added
               ) (Set.empty,Set.empty) events
        Set.fold (fun (removed,added) a -> if Set.contains a removed then removed,added else removed,Set.add a added) removedAndAdded s
        |> snd

    let toSet (events:'a SetEvent Events) =
        Seq.map snd events
        |> Seq.fold (List.fold (fun (removed,added) (se:'a SetEvent) ->
                match se with
                | SetAdd a -> if Set.contains a removed then removed,added else removed,Set.add a added
                | SetRemove a -> Set.add a removed,added
           )) (Set.empty,Set.empty)
        |> snd

type MapEvent<'k,'v> =
    | MapAdd of 'k * 'v
    | MapRemove of 'k

module MapEvent =
    let update events m =
        let removedAndAdded =
            List.fold (fun (removed,added) se ->
                    match se with
                    | MapAdd (k,v) -> if Set.contains k removed then removed,added else removed,Map.add k v added
                    | MapRemove k -> Set.add k removed,added
               ) (Set.empty,Map.empty) events
        Map.fold (fun (removed,added) k v -> if Set.contains k removed then removed,added else removed,Map.add k v added) removedAndAdded m
        |> snd

[<NoEquality;NoComparison>]
type Property<'a,'b> = {Name:string; Getter:'a->'b option; Setter:'b->'a; Validation:'b option->Result<'b,'a*string>}

module Property =
    let create name setter getter validation = {Name=name; Getter=getter; Setter=setter; Validation=validation}
    let set (property:Property<'a,'b>) v = property.Setter v
    let get (property:Property<'a,'b>) (updates:'a Events) =
        List.tryPick (snd >> List.tryPick property.Getter) updates
    let getAndValidate (property:Property<'a,'b>) (updates:'a Events) =
        get property updates |> property.Validation
    let getEvents (property:Property<'a,'b>) (update:'a Events) : 'b Events =
        List.choose (fun (e,l) -> match List.choose property.Getter l with |[] -> None |l -> Some (e,l)) update