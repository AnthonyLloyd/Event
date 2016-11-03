namespace Lloyd.Domain.Apps

open Lloyd.Domain
open Lloyd.Domain.UI

module Editor =
    type 'a Model = {Label:string; Previous:(EventID * 'a) option; Latest:(EventID * 'a) option; Edit:'a option}

    let init property =
        {Label=property.Name+":"; Previous=None; Latest=None; Edit=None}

    type 'a Msg =
        | Reset
        | Update of (EventID * 'a) list
        | Edit of 'a option
        
    let update msg model =
        match msg with
        | Edit e -> {model with Edit=e}
        | Reset -> {model with Edit=None}
        | Update (latest::tail) ->
            {model with
                Previous = List.tryHead tail |> Option.orTry model.Latest
                Latest = Some latest
                Edit = if model.Edit=Some(snd latest) then None else model.Edit
            }
        | Update [] -> model

    let updateProperty property msg model =
        update (Property.getUpdate property msg |> Update) model
        
    let view inputUI model =
        let currentValue = model.Edit |> Option.orTry (Option.map snd model.Latest)
        UI.div Vertical [
            UI.text model.Label
            inputUI currentValue |> UI.map Edit
        ]

    let app inputUI property = UI.appSimple (fun () -> init property) update (view inputUI) // TODO: tooltip, coloured border, lots of input editors for types, rightclick reset

open Lloyd.Domain.Model

module Venue =
    type Model = {Name: string Editor.Model; Capacity: uint16 Editor.Model; LastEvent: EventID option}

    let init() =
        {Name=Editor.init Query.Venue.name; Capacity=Editor.init Query.Venue.capacity; LastEvent=None},None

    type Msg =
        | Update of (EventID * Venue list) list
        | NameMsg of string Editor.Msg
        | CapacityMsg of uint16 Editor.Msg
        | Save

    let update msg model =
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Query.Venue.name l model.Name
                        Capacity = Editor.updateProperty Query.Venue.capacity l model.Capacity
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      },None
        | NameMsg n -> {model with Name=Editor.update n model.Name},None
        | CapacityMsg c -> {model with Capacity=Editor.update c model.Capacity},None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Query.Venue.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Query.Venue.capacity) model.Capacity.Edit)
            model,Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.input model.Name |> UI.map NameMsg
            Editor.view UI.inputUInt16 model.Capacity |> UI.map CapacityMsg
            UI.button "Save" Save
        ]

    let app() = UI.appFull init update view subscription