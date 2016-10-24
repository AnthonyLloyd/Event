namespace Lloyd.Domain.UI

open System
open Lloyd.Domain
open Lloyd.Domain.Model


type VenueState = {Name:string option; Capacity:uint16 option}


module VenueApp =
    
    type Model =
        {
            Original: VenueState option
            Current: VenueState
        }

    let init (store:Venue IRoot) =
        store :> IObservable<_> |> Observable.subscribe ignore |> ignore
        {Original=None;Current={Name=None;Capacity=None}}

    type Msg =
        | Name of string option
        | Capacity of uint16 option
        | Save

    let update msg model =
        match msg with
        | Name s -> {model with Current={model.Current with Name=s}}
        | Capacity e -> {model with Current={model.Current with Capacity=e}}
        | Save -> model

    let view model =
        UI.div Vertical [
            UI.input model.Current.Name |> UI.map Name
            UI.inputUInt16 model.Current.Capacity |> UI.map Capacity
            UI.button "Save" Save
        ]

    let app s =
        UI.app (init s) update view