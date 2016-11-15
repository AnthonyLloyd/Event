namespace Lloyd.Domain.Model

open System
open Lloyd.Core

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Toy =
    let name = Property.create "Name" Toy.Name (function |Toy.Name n -> Some n |_->None)
                        (fun s -> match Option.bind String.nonEmpty s with |Some s -> Ok s |None -> Error(Toy.Name "Toy name unknown" ,"Toy name missing"))
    let ageRange = Property.create "Age Range" AgeRange (function |AgeRange (l,h) -> Some (l,h) |_->None)
                        (function |Some(l,h) when l>=h && l>=0uy && h<=16uy -> Ok(l,h) |_ -> Error(AgeRange(0uy,0uy),"Please enter an age range (0-16)"))
    let workRequired = Property.create "Work Required" WorkRequired (function |WorkRequired c -> Some c |_->None)
                        (function |Some w -> Ok w |None -> Error(WorkRequired 0us,"Please enter work required"))

    type View = {Name:string; AgeRange:Age*Age; WorkRequired:Work}

    let view events =
        Ok  (fun n a w -> {Name=n; AgeRange=a; WorkRequired=w})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate ageRange events
        <*> Property.getAndValidate workRequired events

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Elf =
    let name = Property.create "Name" Elf.Name (function |Elf.Name n -> Some n |_->None)
                        (fun s -> match Option.bind String.nonEmpty s with |Some s -> Ok s |None -> Error(Elf.Name "Elf name unknown" ,"Elf name missing"))
    let workRate = Property.create "Work Rate" WorkRate (function |WorkRate r -> Some r |_->None)
                        (function |Some w -> Ok w |None -> Error(WorkRate 0us,"Please enter work rate"))
    let making = Property.create "Making" Making (function |Making t -> Some t |_->None)
                        (function |Some w -> Ok w |None -> Ok None)

    type View = {Name:string; WorkRate:Work; Making:Toy ID option}

    let view events =
        Ok  (fun n w m -> {Name=n; WorkRate=w; Making=m})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate workRate events
        <*> Property.getAndValidate making events

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Kid =
    let name = Property.create "Name" Kid.Name (function |Kid.Name n -> Some n |_->None)
                        (fun s -> match Option.bind String.nonEmpty s with |Some s -> Ok s |None -> Error(Kid.Name "Kid name unknown" ,"Kid name missing"))
    let age = Property.create "Age" Age (function |Age a -> Some a |_->None)
                        (function |Some a when a>=0uy && a<=18uy -> Ok a |_ -> Error(Age 0uy,"Please enter age (0-18)"))
    let behaviour = Property.create "Behaviour" Behaviour (function |Behaviour b -> Some b |_->None)
                        (function |Some b -> Ok b |None -> Error(Behaviour Good,"Please enter behaviour"))
    let wishList = Property.create "Wish List" WishList (function |WishList w -> Some w |_->None)
                        (function |Some w -> Ok w |None -> failwith "Not possible")

    type Summary = {Name:string; Age:Age; Behaviour:Behaviour; WishList:Toy ID Set}

    let summary events =
        Ok  (fun n a b w -> {Name=n; Age=a; Behaviour=b; WishList=w})
        <*> Property.getAndValidate name events
        <*> Property.getAndValidate age events
        <*> Property.getAndValidate behaviour events
        <*> (Property.getEvents wishList events |> SetEvent.toSet |> Ok)



module Summary =
    
    type ToysMade = {Total:Map<Toy ID,int>; Making:Map<Elf ID,Toy ID>}

    let toysMade (deltas:IObservable<Elf ID * Elf Events>) =
        deltas
        |> Observable.choose (fun (eid,events) ->
                match List.collect (snd >> List.choose Elf.making.Getter) events with
                |[] -> None
                |l -> Some (eid,l)
            )
        |> Observable.scan (fun summary (eid,events) ->
            {
                Total = List.choose id events |> List.fold Map.incr summary.Total
                Making = Map.addOrRemove eid (List.head events) summary.Making
            }) {Total=Map.empty; Making=Map.empty}

    let toyMade2 (t:ToysMade) = Map.toSeq t.Making |> Seq.map snd |> Seq.fold Map.decr t.Total

    type ToysRequest = {Age:Age; Behaviour:Behaviour; WishList:Map<Toy ID,EventID>}

    let toysRequested (deltas:IObservable<Kid ID * Kid Events>) =
        deltas
        |> Observable.filter (snd >> List.exists (snd >> List.forall (function |Name _ -> true |_ -> false) >> not))
        |> Observable.scan (fun map (kid,events) ->
                let s = Map.tryFind kid map
                let ns = {
                    Age = Property.get Kid.age events |> Option.getElseFun (fun () -> Option.map (fun i -> i.Age) s |> Option.getElse 0uy)
                    Behaviour = Property.get Kid.behaviour events |> Option.getElseFun (fun () -> Option.map (fun i -> i.Behaviour) s |> Option.getElse Good)
                    WishList =
                        let remove,add =
                            Property.getEvents Kid.wishList events
                            |> Seq.fold (fun state (eid,l) ->
                                    List.fold (fun (removed,added) ->
                                        function
                                        | Add a -> if Set.contains a removed then removed,added else removed,Map.add a eid added
                                        | Remove a -> Set.add a removed,added) state l
                                ) (Set.empty,Map.empty)
                        let afterRemove = Set.fold (fun l r -> Map.remove r l) (Option.map (fun i -> i.WishList) s |> Option.getElse Map.empty) remove
                        Map.fold (fun m k v -> Map.add k v m) afterRemove add
                }
                Map.add kid ns map
            ) Map.empty

    let toyName (deltas:IObservable<Toy ID * Toy Events>) =
        deltas
        |> Observable.choose (fun (eid,events) -> Property.get Toy.name events |> Option.map (fun l -> eid,l))
        |> Observable.scan (fun m (eid,a) -> Map.add eid a m) Map.empty

    let toyAgeRange (deltas:IObservable<Toy ID * Toy Events>) =
        deltas
        |> Observable.choose (fun (eid,events) -> Property.get Toy.ageRange events |> Option.map (fun l -> eid,l))
        |> Observable.scan (fun m (eid,a) -> Map.add eid a m) Map.empty

    
    //type Row = {ID:Toy ID; Name:string; Requested:int}
    //type Row = {ID:Kid ID; Name:string; WishList:int; Made:int}

    //Kid ID,Age,(EventID*Toy ID) list
    //Toy ID,AgeRange

