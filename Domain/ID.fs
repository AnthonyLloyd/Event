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

type 'Aggregate EventUpdate = (EventID * 'Aggregate list) list

[<NoEquality;NoComparison>]
type 'Aggregate MemoryStore =
    {
        Updates: Map<'Aggregate ID,'Aggregate EventUpdate>
        Observers: IObserver<'Aggregate ID * 'Aggregate EventUpdate> list
    }

module MemoryStore =
    let create() = {Updates=Map.empty; Observers=[]}
    let observable (store:'Aggregate MemoryStore ref) =
        {new IObservable<'Aggregate ID * 'Aggregate EventUpdate> with
            member __.Subscribe(ob:IObserver<'Aggregate ID * 'Aggregate EventUpdate>) =
                let msd = atomicUpdate store (fun i -> {Updates=i.Updates; Observers=ob::i.Observers})
                Map.toSeq msd.Updates |> Seq.iter ob.OnNext
                {new IDisposable with
                    member __.Dispose() =
                        atomicUpdate store (fun i -> {Updates=i.Updates; Observers=List.where ((<>)ob) i.Observers}) |> ignore
                }
        }
    let update (aid:'Aggregate ID) (updates:'Aggregate list) (lastEvent:EventID option) (store:'Aggregate MemoryStore ref) =
        atomicUpdateWithQuery store (fun i ->
            match Map.tryFind aid i.Updates with
            | Some l ->
                if List.head l |> fst |> Some = lastEvent then i,None
                else
                    let eid = EventID.gen()
                    {Updates=Map.add aid ((eid,updates)::l) i.Updates; Observers=i.Observers}, Some eid
            | None ->
                let eid = EventID.gen()
                {Updates=Map.add aid [eid,updates] i.Updates; Observers=i.Observers}, Some eid
        )

[<NoEquality;NoComparison>]
type Property<'a,'b> = {Name:string; Getter:'a->'b option; Setter:'b->'a; Default:'b}

module Property =
    let create name setter getter def = {Name=name; Getter=getter; Setter=setter; Default=def}
    let getUpdate (property:Property<'a,'b>) (update:'a EventUpdate) =
        List.choose (fun (e,l) -> List.tryPick property.Getter l |> Option.map (fun i -> e,i)) update
    let set (property:Property<'a,'b>) v = property.Setter v