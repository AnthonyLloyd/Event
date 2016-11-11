namespace Lloyd.Domain.Model

open Lloyd.Core

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Toy =
    let name = Property.create "Name" Toy.Name (function |Toy.Name n -> Some n |_->None)
                        (fun s -> match Option.bind String.nonEmpty s with |Some s -> Ok s |None -> Error(Toy.Name "Toy name unknown" ,"Toy name missing"))
    let ageRange = Property.create "Age Range" AgeRange (function |AgeRange (l,h) -> Some (l,h) |_->None)
                        (function |Some(l,h) when l>=h && l>=0uy && h<=18uy -> Ok(l,h) |_ -> Error(AgeRange(0uy,0uy),"Please enter an age range (0-18)"))
    let workRequired = Property.create "Work Required" WorkRequired (function |WorkRequired c -> Some c |_->None)
                        (function |Some w -> Ok w |None -> Error(WorkRequired 0us,"Please enter work required"))

    type View = {Name:string; AgeRange:Age*Age; WorkRequired:Work}

    let view events =
        Ok  (fun n a w -> {Name=n; AgeRange=a; WorkRequired=w})
        <*> Property.get name events
        <*> Property.get ageRange events
        <*> Property.get workRequired events

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
        <*> Property.get name events
        <*> Property.get workRate events
        <*> Property.get making events

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

    type View = {Name:string; Age:Age; Behaviour:Behaviour; WishList:Toy ID Set}

    let view events =
        Ok  (fun n a b w -> {Name=n; Age=a; Behaviour=b; WishList=w})
        <*> Property.get name events
        <*> Property.get age events
        <*> Property.get behaviour events
        <*> (Property.getEvents wishList events |> SetEvent.toSet |> Ok)