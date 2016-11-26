namespace Lloyd.Domain.Model

open System
open Lloyd.Core

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Toy =
    let name = Property.create "Name" Toy.Name (function | Toy.Name n -> Some n | _ -> None)
                        (fun s -> match Option.bind String.nonEmpty s with | Some s -> Ok s | None -> Error(Toy.Name "Toy name unknown" ,"Please enter a name"))
    let ageRange = Property.create "Age Range" AgeRange (function | AgeRange (l,h) -> Some (l,h) | _ -> None)
                        (function | Some(l,h) when l<=h && l>=0uy && h<=16uy -> Ok(l,h) | _ -> Error(AgeRange(0uy,0uy),"Please enter an age range (0-16)"))
    let workRequired = Property.create "Work Required" WorkRequired (function | WorkRequired c -> Some c | _ -> None)
                        (function | Some w -> Ok w | None -> Error(WorkRequired 0us,"Please enter work required"))

    type View = {Name:string; AgeRange:Age*Age; WorkRequired:Work}

    let view events =
        Ok  (fun n a w -> {Name=n; AgeRange=a; WorkRequired=w})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate ageRange events
        <*> Property.getAndValidate workRequired events

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Elf =
    let name = Property.create "Name" Elf.Name (function | Elf.Name n -> Some n | _->None)
                        (fun s -> match Option.bind String.nonEmpty s with | Some s -> Ok s | None -> Error(Elf.Name "Elf name unknown" ,"Please enter a name"))
    let workRate = Property.create "Work Rate" WorkRate (function | WorkRate r -> Some r | _ ->None)
                        (function | Some w -> Ok w | None -> Error(WorkRate 0us,"Please enter work rate"))
    let making = Property.create "Making" Making (function |Making t -> Some t | _ ->None)
                        (function | Some w -> Ok w | None -> Ok None)

    type View = {Name:string; WorkRate:Work; Making:Toy ID option}

    let view events =
        Ok  (fun n w m -> {Name=n; WorkRate=w; Making=m})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate workRate events
        <*> Property.getAndValidate making events

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Kid =
    let name = Property.create "Name" Kid.Name (function | Kid.Name n -> Some n | _ ->None)
                        (fun s -> match Option.bind String.nonEmpty s with | Some s -> Ok s | None -> Error(Kid.Name "Kid name unknown" ,"Please enter a name"))
    let age = Property.create "Age" Age (function | Age a -> Some a | _ ->None)
                        (function | Some a when a>=0uy && a<=16uy -> Ok a | _ -> Error(Age 0uy,"Please enter age (0-16)"))
    let behaviour = Property.create "Behaviour" Behaviour (function | Behaviour b -> Some b | _ ->None)
                        (function | Some b -> Ok b | None -> Error(Behaviour Good,"Please enter behaviour"))
    let wishList = Property.create "Wish List" WishList (function |WishList w -> Some w |_->None)
                        (function | Some w -> Ok w | None -> failwith "Not possible")

    type Summary = {Name:string; Age:Age; Behaviour:Behaviour; WishList:Toy ID Set}

    let view events =
        Ok  (fun n a b w -> {Name=n; Age=a; Behaviour=b; WishList=w})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate age events
        <*> Property.getAndValidate behaviour events
        <*> (Property.tryGetEvents wishList events |> Option.map SetEvent.toSet |> Option.getElse Set.empty |> Ok)



module Query =

    let toyProgress (kidStore:Kid Store) (toyStore:Toy Store) (elfStore:Elf Store) =

        let toyFinished (elfStore:Elf Store) =
            Store.deltaObservable elfStore
            |> Observable.choose (Map.choose (fun _ -> Property.tryGetEvents Elf.making) >> Option.ofMap)
            |> Observable.scan (fun ((total,making),_) (map:Map<Elf ID,Toy ID option Events>) ->
                    let newTotal =
                        Map.toSeq map
                        |> Seq.collect (snd >> List1.toList >> Seq.collect (snd >> List1.toList >> Seq.choose id))
                        |> Seq.fold Map.incr total
                    let newMaking = Map.fold (fun map elf events -> Map.addOrRemove elf (List1.head events |> snd |> List1.head) map) making map
                    let toyUpdate = Map.toSeq making |> Seq.map snd |> Seq.fold Map.decr total
                    (newTotal,newMaking),toyUpdate) ((Map.empty,Map.empty),Map.empty)
            |> Observable.map snd

        let kidWishListEvent (kidStore:Kid Store) : IObservable<Map<Kid ID,Map<Toy ID,EventID>>> =
            Store.deltaObservable kidStore
            |> Observable.choose (fun (map:Map<Kid ID,Kid Events>) ->
                Map.toList map
                |> List.choose (fun (kid,events) ->
                        List1.tryCollect (fun (eid,l)  -> List1.tryChoose Kid.wishList.Getter l |> Option.map (List1.map (function |SetAdd toy -> MapAdd (toy,eid) |SetRemove toy -> MapRemove toy))) events
                        |> Option.map (addFst kid)
                    )
                |> List1.tryOfList
               )
            |> Observable.scan (
                List1.fold (fun map (kid,mapEvents) ->
                    let newMap = Map.tryFind kid map |> Option.getElse Map.empty |> MapEvent.update mapEvents
                    Map.add kid newMap map)
               ) Map.empty

        let progress (finished:Map<Toy ID,int>) (ageRanges:Map<Toy ID,Age*Age>) (ages:Map<Kid ID,Age>) (behaviours:Map<Kid ID,Behaviour>) (wishListEvents:Map<Kid ID,Map<Toy ID,EventID>>) =

            let ageRestrict (ages:Map<Kid ID,Age>) (ageRanges:Map<Toy ID,Age*Age>) (wishListEvents:Map<Kid ID,Map<Toy ID,EventID>>) =
                Map.map (fun kid toyEvents ->
                        match Map.tryFind kid ages with
                        | None -> Map.empty
                        | Some age -> toyEvents |> Map.filter (fun toy _ -> Map.tryFind toy ageRanges |> Option.map (fun (lo,hi) -> between lo hi age) |> Option.getElse false)
                    ) wishListEvents
                |> Map.filter (fun _ v -> Map.isEmpty v |> not)

            wishListEvents |> ageRestrict ages ageRanges
            |> Map.filter (fun k _ -> Map.containsKey k behaviours)
            |> Map.map (fun k v -> Map.find k behaviours,v)
            |> Map.toSeq
            |> Seq.collect (fun (kid,(behaviour,m)) -> Map.toSeq m |> Seq.map (fun (toy,eid) -> toy,((behaviour,eid),kid)))
            |> Seq.groupByFst
            |> Seq.map (fun (toy,s) ->
                let l = Seq.sort s |> Seq.toList
                let i = Map.tryFind toy finished |> Option.map (min (List.length l)) |> Option.getElse 0
                toy, List.splitAt i l
                )
            |> Map.ofSeq

        toyFinished elfStore |> Observable.map Choice1Of5
        |> Observable.merge (Property.fullObservable Toy.ageRange toyStore |> Observable.map Choice2Of5)
        |> Observable.merge (Property.fullObservable Kid.age kidStore |> Observable.map Choice3Of5)
        |> Observable.merge (Property.fullObservable Kid.behaviour kidStore |> Observable.map Choice4Of5)
        |> Observable.merge (kidWishListEvent kidStore |> Observable.map Choice5Of5)
        |> Observable.scan (fun (finished,ageRanges,ages,behaviours,wishListEvents) choice ->
                match choice with
                |Choice1Of5 finished -> (finished,ageRanges,ages,behaviours,wishListEvents)
                |Choice2Of5 ageRanges -> (finished,ageRanges,ages,behaviours,wishListEvents)
                |Choice3Of5 ages -> (finished,ageRanges,ages,behaviours,wishListEvents)
                |Choice4Of5 behaviours -> (finished,ageRanges,ages,behaviours,wishListEvents)
                |Choice5Of5 wishListEvents -> (finished,ageRanges,ages,behaviours,wishListEvents)
            ) (Map.empty,Map.empty,Map.empty,Map.empty,Map.empty)
        |> Observable.map (fun (f,r,a,b,w) -> progress f r a b w)


    let kidRequested toyProgress : IObservable<(Kid ID * int) list> =
        toyProgress
        |> Observable.scan (fun (_,before) after -> before,after) (Map.empty,Map.empty)
        |> Observable.map (fun (before,after) ->
            let requested map = Map.toSeq map |> Seq.collect (fun (_,(f,o)) -> Seq.append f o) |> Seq.countBy snd |> Map.ofSeq
            Map.revisions (requested before) (requested after) |> Map.toList)

    let kidFinished toyProgress : IObservable<(Kid ID * int) list> =
        toyProgress
        |> Observable.scan (fun (_,before) after -> before,after) (Map.empty,Map.empty)
        |> Observable.map (fun (before,after) ->
            let finished map = Map.toSeq map |> Seq.collect (snd>>fst) |> Seq.countBy snd |> Map.ofSeq
            Map.revisions (finished before) (finished after) |> Map.toList)

    let toyRequested toyProgress : IObservable<(Toy ID * int) list> =
        toyProgress
        |> Observable.scan (fun (_,before) after -> before,after) (Map.empty,Map.empty)
        |> Observable.map (fun (before,after) ->
            let requested map = Map.map (fun _ (f,o) -> List.length f + List.length o) map
            Map.revisions (requested before) (requested after) |> Map.toList)

    let toyFinished toyProgress : IObservable<(Toy ID * int) list> =
        toyProgress
        |> Observable.scan (fun (_,before) after -> before,after) (Map.empty,Map.empty)
        |> Observable.map (fun (before,after) ->
            let finished map = Map.map (fun _ (f,_) -> List.length f) map
            Map.revisions (finished before) (finished after) |> Map.toList)

    let toysOutstanding toyProgress : IObservable<((Behaviour * EventID) * Toy ID) list> =
        toyProgress
        |> Observable.map (fun m ->
            Map.toSeq m |> Seq.collect (snd>>snd) |> Seq.sort |> Seq.toList)