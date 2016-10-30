namespace Lloyd.Domain.UI


module EditorApp =
    type 'a Model = {Label:string; Original:'a option; Update:'a option; Edit:'a option}

    let init label original =
        {Label=label+":"; Original=original; Update=None; Edit=None}

    type 'a Msg =
        | Reset
        | Update of 'a option
        | Edit of 'a option
        
    let update msg model =
        match msg with
        | Reset -> {model with Edit=None}
        | Update u -> {model with Update=u; Edit=if u=model.Edit then None else model.Edit}
        | Edit e -> {model with Edit=e}

    let view inputUI model =
        let currentValue = List.tryPick id [model.Edit; model.Update; model.Original] // TODO: Option method
        UI.div Vertical [
            UI.text model.Label
            inputUI currentValue |> UI.map Edit
        ]

    let app inputUI label original =
        UI.app (fun () -> init label original) update (view inputUI) // TODO: tooltip, coloured border, lots of input editors for types, rightclick reset

module VenueApp =
    type Model = {Name: string EditorApp.Model; Capacity: uint16 EditorApp.Model}

    let init name capacity =
        {Name=EditorApp.init "Name" name;Capacity=EditorApp.init "Capacity" capacity},ignore

    type Msg =
        | NameMsg of string EditorApp.Msg
        | CapacityMsg of uint16 EditorApp.Msg
        | Save

    let update msg model =
        match msg with
        | NameMsg n -> {model with Name=EditorApp.update n model.Name},ignore
        | CapacityMsg c -> {model with Capacity=EditorApp.update c model.Capacity},ignore
        | Save -> model,ignore

    let subscription model =
        Set.empty

    let view model =
        UI.div Vertical [
            UI.div Horizontal [
                EditorApp.view UI.input model.Name |> UI.map NameMsg
                EditorApp.view UI.inputUInt16 model.Capacity |> UI.map CapacityMsg
            ]
            UI.button "Save" Save
        ]

    let app name capacity =
        UI.appWithCmdSub (fun () -> init name capacity) update view subscription (failwith "no imp")