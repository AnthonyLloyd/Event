namespace Lloyd.Domain.Apps

open Lloyd.Domain
open Lloyd.Domain.UI

module Editor =
    type 'a Model = {Label:string; Previous:(EventID * 'a) option; Latest:(EventID * 'a) option; Edit:'a option}

    let init label =
        {Label=label+":"; Previous=None; Latest=None; Edit=None}

    type 'a Msg =
        | Reset
        | Update of (EventID * 'a) list
        | Edit of 'a option
        
    let update msg model =
        match msg with
        | Edit e -> {model with Edit=e}
        | Reset -> {model with Edit=None}
        | Update (latest::tail) -> {model with Previous= match tail with |previous::_ -> Some previous | [] -> model.Latest
                                               Latest=Some latest
                                               Edit=if model.Edit=Some(snd latest) then None else model.Edit
                                   }
        | Update [] -> model

    let updateChoose chooser msg model =
        let msg = List.choose (fun (e,l) -> List.tryPick chooser l |> Option.map (fun i -> e,i)) msg |> Update
        update msg model

    let view inputUI model =
        let currentValue = match model.Edit with | Some v -> Some v | None -> Option.map snd model.Latest
        UI.div Vertical [
            UI.text model.Label
            inputUI currentValue |> UI.map Edit
        ]

    let app inputUI label =
        UI.app (fun () -> init label) update (view inputUI) // TODO: tooltip, coloured border, lots of input editors for types, rightclick reset

open Lloyd.Domain.Model

module Venue =
    type Model = {Name: string Editor.Model; Capacity: uint16 Editor.Model}

    let init() =
        {Name=Editor.init "Name";Capacity=Editor.init "Capacity"},ignore

    type Msg =
        | Update of (EventID * Venue list) list
        | NameMsg of string Editor.Msg
        | CapacityMsg of uint16 Editor.Msg
        | Save

    let update msg model =
        match msg with
        | Update l -> {model with Name=Editor.updateChoose (function |Venue.Name n -> Some n | _ -> None) l model.Name
                                  Capacity=Editor.updateChoose (function |Venue.Capacity c -> Some c | _ -> None) l model.Capacity},ignore
        | NameMsg n -> {model with Name=Editor.update n model.Name},ignore
        | CapacityMsg c -> {model with Capacity=Editor.update c model.Capacity},ignore
        | Save -> model,ignore

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.input model.Name |> UI.map NameMsg
            Editor.view UI.inputUInt16 model.Capacity |> UI.map CapacityMsg
            UI.button "Save" Save
        ]

    let app observable =
        UI.appWithCmdSub init update view subscription observable