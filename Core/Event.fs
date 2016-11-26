namespace Lloyd.Core

open System
open System.Diagnostics

type UserID = private User of int

module User =
    let private map = Map.ofList [1,"admin"] |> ref
    let login name =
        match Map.tryFindKey (fun _ -> (=)name) !map with
        | Some userID -> User userID
        | None ->
            let userID = Map.toSeq !map |> Seq.map fst |> Seq.max |> (+)1
            map := Map.add userID name !map
            User userID
    let name (User userID) = Map.find userID !map

type EventID = private | EventID of time:DateTime * user:UserID
               override e.ToString() = match e with | EventID(t,u) -> string t + "    " + User.name u

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EventID =
    let private timestampAndTicks0 = // Better resolution and can syncronise to NTP server on startup
        Stopwatch.GetTimestamp(), let d = DateTime.UtcNow in d.Ticks
    let private ticks() =
        let timestamp0,ticks0 = timestampAndTicks0
        ticks0+((Stopwatch.GetTimestamp()-timestamp0)*TimeSpan.TicksPerSecond)/Stopwatch.Frequency
    let lastTicks = ref 0L
    let internal gen user =
        let ticks = atomicUpdateInt64 (fun oldTicks ->
                        let newTicks = ticks()
                        if oldTicks=newTicks then oldTicks+1L
                        else newTicks) lastTicks |> snd
        EventID(DateTime ticks, user)
    let time (EventID(t,_)) = t
    let User (EventID(_,u)) = u

type 'Aggregate ID = private Created of EventID

module ID =
    let internal gen eventID = Created eventID
    let internal eventID (Created e) = e

type 'Aggregate Events = (EventID * 'Aggregate list1) list1

module Events =
    let lasteEventID (events:'Aggregate Events) =
        List1.head events |> fst
    let append (events1:'a Events) (events2:'a Events) =
        List1.append events1 events2

type StoreError =
    | Concurrency

[<NoEquality;NoComparison>]
type 'Aggregate MemoryStore =
    {
        Latest: Map<'Aggregate ID,'Aggregate Events>
        Observers: IObserver<Map<'Aggregate ID,'Aggregate Events>> list
    }

module MemoryStore =
    let empty = {Latest=Map.empty; Observers=[]}

    let observable (store:'Aggregate MemoryStore ref) =
        {new IObservable<_> with
            member __.Subscribe(ob:IObserver<_>) =
                let _,newStore = atomicUpdate (fun i -> {i with Observers=ob::i.Observers}) store
                ob.OnNext newStore.Latest
                {new IDisposable with
                    member __.Dispose() =
                        atomicUpdate (fun i -> {i with Observers=List.where ((<>)ob) i.Observers}) store |> ignore
                }
            }

    let update (user:UserID) (aggregateID:'Aggregate ID) (updates:'Aggregate list1) (lastEvent:EventID) (store:'Aggregate MemoryStore ref) =
        let newStore, result =
            atomicUpdateQuery (fun store ->
                let l = Map.find aggregateID store.Latest
                if List1.head l |> fst = lastEvent then
                    let eventID = EventID.gen user
                    {store with Latest=Map.add aggregateID (List1.init (eventID,updates) (List1.toList l)) store.Latest}, Ok eventID
                else store, Error Concurrency
            ) store
        match result with
        | Ok eventID ->
            newStore.Observers |> Seq.iter (fun ob -> Map.add aggregateID (List1.singleton (eventID,updates)) Map.empty |> ob.OnNext)
            result
        | _ -> result

    let create (user:UserID) (updates:'Aggregate list1) (store:'Aggregate MemoryStore ref) =
        let newStore,result =
            atomicUpdateQuery (fun store ->
                let eventID = EventID.gen user
                let aggregateID = ID.gen eventID
                {store with Latest=Map.add aggregateID (List1.singleton (eventID,updates)) store.Latest}, Ok aggregateID
            ) store
        match result with
        | Ok aggregateID ->
            newStore.Observers |> Seq.iter (fun ob -> Map.add aggregateID (List1.singleton (ID.eventID aggregateID,updates)) Map.empty |> ob.OnNext)
            result
        | _ -> result

[<NoEquality;NoComparison>]
type 'Aggregate Store =
    | MemoryStore of 'Aggregate MemoryStore ref
    //| RemoteStore - implement me

module Store =
    let emptyMemoryStore() = MemoryStore.empty |> ref |> MemoryStore
    /// Returns the full store state and then subsequent changes.
    let observable (store:'Aggregate Store) =
        match store with
        | MemoryStore store -> MemoryStore.observable store
    let update (user:UserID) (aggregateID:'Aggregate ID) (updates:'Aggregate list1) (lastEvent:EventID) (store:'Aggregate Store) =
        match store with
        | MemoryStore store -> MemoryStore.update user aggregateID updates lastEvent store
    let create (user:UserID) (updates:'Aggregate list1) (store:'Aggregate Store) =
        match store with
        | MemoryStore store -> MemoryStore.create user updates store
    let latest (store:'Aggregate Store) =
        match store with
        | MemoryStore store -> store.Value.Latest

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
        List1.toList events
        |> Seq.map snd
        |> Seq.fold (List1.fold (fun (removed,added) (se:'a SetEvent) ->
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
            List1.fold (fun (removed,added) se ->
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
        List1.toList updates |> List.tryPick (snd >> List1.tryPick property.Getter)
    let tryGetEvents (property:Property<'a,'b>) (update:'a Events) : 'b Events option =
        List1.tryChoose (fun (e,l) -> List1.tryChoose property.Getter l |> Option.map (addFst e)) update
    let getAndValidate (property:Property<'a,'b>) (updates:'a Events) =
        get property updates |> property.Validation
    let validate (property:Property<'a,'b>) (edit:'b option) =
        property.Validation edit
    let validateEdit (view:'a Events -> Result<'b,('a*string) list>) (current:'a Events option) (edits:'a list) =
        match List1.tryOfList edits with
        | None -> Error []
        | Some l ->
            let update = EventID.gen (User -1), l
            let proposed =
                match current with
                | None -> List1.singleton update
                | Some events -> List1.cons update events
            view proposed
    let deltaObservable property store =
        Store.observable store
        |> Observable.choose (Map.choose (fun _ -> get property) >> Option.ofMap)
    let fullObservable property store =
        deltaObservable property store |> Observable.scan (Map.fold (fun m k v -> Map.add k v m)) Map.empty
