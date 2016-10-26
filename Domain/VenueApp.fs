namespace Lloyd.Domain.UI

open System
open Lloyd.Domain
open Lloyd.Domain.Model


type 'a Edit = {Original:'a option;Updated:'a option;Edited:'a option}

module Edit =
    let init a = {Original=a;Updated=None;Edited=None}
    let updateEdited a e = {Original=e.Original;Updated=e.Updated;Edited=a}
    let puree (f:'a->'b) = {Original=Some f;Updated=Some f;Edited=Some f}
    let apply f a =
        let optionApply f a = match f,a with | Some f,Some a -> f a |> Some | _ -> None
        {Original=optionApply f.Original a.Original; Updated=optionApply f.Updated a.Updated; Edited=optionApply f.Edited a.Edited}
    let map f a = {Original=Option.map f a.Original; Updated=Option.map f a.Updated; Edited=Option.map f a.Edited}

type VenueState = {Name:string; Capacity:uint16}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Venue =
    let parse (s:Venue list) =
        Ok (fun n s -> {Name=n;Capacity=s})
        <*> (List.tryPick (function |Venue.Name n -> Some n |_ -> None) s |> ofOption "Name")
        <*> (List.tryPick (function |Capacity n -> Some n |_ -> None) s |> ofOption "Capacity")

    let test = Edit.puree (fun s c -> {Name=s;Capacity=c})
module VenueApp =
    
    type Model =
        {
            Name: string Edit
            Capacity: uint16 Edit
        }

    let init (store:Venue IRoot) =
        store :> IObservable<_> |> Observable.subscribe ignore |> ignore
        {Name=Edit.init None;Capacity=Edit.init None}

    type Msg =
        | Name of string option
        | Capacity of uint16 option
        | Save

    let update msg (model:Model) =
        match msg with
        | Name s -> {model with Name=Edit.updateEdited s model.Name}
        | Capacity e -> {model with Capacity=Edit.updateEdited e model.Capacity}
        | Save -> model

    let view model =
        UI.div Vertical [
            UI.input model.Name.Edited |> UI.map Name
            UI.inputUInt16 model.Capacity.Edited |> UI.map Capacity
            UI.button "Save" Save
        ]

    let app s =
        UI.app (init s) update view