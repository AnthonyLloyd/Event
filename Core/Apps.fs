namespace Lloyd.Core.Apps

open Lloyd.Core
open Lloyd.Core.UI

module Editor =
    type 'a Model = {Label:string; Previous:(EventID * 'a) option; Latest:(EventID * 'a) option; Edit:'a option}

    let init property =
        {Label=property.Name+":"; Previous=None; Latest=None; Edit=None}

    type 'a Msg =
        | Edit of 'a option
        | Reset
        | Update of 'a Events

    let update msg model =
        match msg with
        | Edit e -> {model with Edit=e}
        | Reset -> {model with Edit=None}
        | Update l ->
            let latest = List1.head l |> mapSnd List1.head |> Some
            {model with
                Previous = List1.tail l |> List.tryHead |> Option.map (mapSnd List1.head)
                Latest = latest
                Edit = if model.Edit=Option.map snd latest then None else model.Edit
            }

    let updateProperty property msg model =
        match Property.tryGetEvents property msg with
        | None -> model
        | Some events -> update (Update events) model
        
    let view inputUI model =
        let current = model.Edit |> Option.orTry (Option.map snd model.Latest)
        UI.div Vertical [
            UI.text model.Label
            inputUI current |> UI.map Edit
        ]

    let app inputUI property = UI.appSimple (fun () -> init property) update (view inputUI) // TODO: tooltip, coloured border, rightclick reset


module EditorSet =
    type Model<'a when 'a : comparison> = {Label:string; Previous:(EventID * 'a Set) option; Latest:(EventID * 'a Set) option; Edit:'a option list option}

    let init property =
        {Label=property.Name+":"; Previous=None; Latest=None; Edit=None}

    type 'a Msg =
        | Insert
        | Remove of int
        | Modify of int * 'a option
        | Update of 'a SetEvent Events

    let current model = model.Edit |> Option.orTry (Option.map (snd >> Set.toList >> List.map Some) model.Latest) |> Option.getElse []

    let update msg model =
        match msg with
        | Insert -> {model with Edit= None::current model |> Some}
        | Remove i -> {model with Edit= current model |> List.removei i |> Some}
        | Modify (i,v) -> {model with Edit= current model |> List.replacei i v |> Some}
        | Update l ->
            let latest = SetEvent.toSet l
            {model with
                Previous = List1.tail l |> List1.tryOfList |> Option.map (fun l -> List1.head l |> fst, SetEvent.toSet l)
                Latest = List1.head l |> fst |> addSnd latest |> Some
                Edit =  match model.Edit with
                        | Some l when List.forall Option.isSome l && List.map Option.get l |> Set.ofList = latest -> None
                        | _ -> model.Edit
            }

    let updateProperty property msg model = //TODO Can this method go?
        match Property.tryGetEvents property msg with
        | None -> model
        | Some events -> update (Update events) model

    let view inputUI model =
        let header = UI.div Horizontal [UI.text model.Label ; UI.button "+" Insert]
        let item i a = UI.div Horizontal [inputUI a |> UI.map (fun v -> Modify(i,v)); UI.button "-" (Remove i)]
        let items = current model |> List.mapi item
        header::items |> UI.div Vertical