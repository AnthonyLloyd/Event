namespace Lloyd.Domain.Apps

open Lloyd.Domain
open Lloyd.Core.UI

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
        | Update l ->
            let latest,previous = match l with |[] -> None,None |[l] -> Some l,None |l::p::_ -> Some l,Some p
            {model with
                Previous = previous
                Latest = latest
                Edit = if model.Edit=Option.map snd latest then None else model.Edit
            }

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

module Toy =
    type Model = {Name: string Editor.Model; Effort: uint16 Editor.Model; LastEvent: EventID option}

    let init() =
        {Name=Editor.init Toy.name; Effort=Editor.init Toy.effort; LastEvent=None}, None

    type Msg =
        | Update of (EventID * Toy list) list
        | NameMsg of string Editor.Msg
        | EffortMsg of uint16 Editor.Msg
        | Save

    let update msg model =
        printfn "Toy.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Toy.name l model.Name
                        Effort = Editor.updateProperty Toy.effort l model.Effort
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      }, None
        | NameMsg n -> {model with Name=Editor.update n model.Name}, None
        | EffortMsg c -> {model with Effort=Editor.update c model.Effort}, None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Toy.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Toy.effort) model.Effort.Edit)
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.input model.Name |> UI.map NameMsg
            Editor.view UI.inputUInt16 model.Effort |> UI.map EffortMsg
            UI.button "Save" Save
        ]

    let app() = UI.appFull init update view subscription