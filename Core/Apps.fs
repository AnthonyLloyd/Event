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
            let latest,previous = match l with |[e,[a]] -> Some(e,a),None |(le,[la])::(pe,[pa])::_ -> Some(le,la),Some(pe,pa) |_ -> None,None
            {model with
                Previous = previous
                Latest = latest
                Edit = if model.Edit=Option.map snd latest then None else model.Edit
            }

    let updateProperty property msg model =
        update (Property.getEvents property msg |> Update) model
        
    let view inputUI model =
        let current = model.Edit |> Option.orTry (Option.map snd model.Latest)
        UI.div Vertical [
            UI.text model.Label
            inputUI current |> UI.map Edit
        ]

    let app inputUI property = UI.appSimple (fun () -> init property) update (view inputUI) // TODO: tooltip, coloured border, rightclick reset

module EditorSet =
    type 'a Model = {Label:string; Previous:(EventID * 'a list) option; Latest:(EventID * 'a list) option; Edit:'a option list option}

    let init property =
        {Label=property.Name+":"; Previous=None; Latest=None; Edit=None}

    type 'a Msg =
        | Insert
        | Remove of int
        | Modify of int * 'a option
        | Update of 'a SetEvent Events

    let current model = model.Edit |> Option.orTry (Option.map (snd>>List.map Some) model.Latest) |> Option.getElse []

    let update msg model =
        match msg with
        | Insert -> {model with Edit= None::current model |> Some}
        | Remove i -> {model with Edit= current model |> List.removei i |> Some}
        | Modify (i,v) -> {model with Edit= current model |> List.replacei i v |> Some}
        | Update l ->
            let latest,previous = match l with |[] -> None,None |[_] -> Some l,None |_::t -> Some l,Some t
            let latestSet = Option.map (fun l -> List.head l |> fst, SetEvent.toSet l) latest
            {model with
                Previous = Option.map (fun l -> List.head l |> fst, SetEvent.toSet l |> Set.toList) previous
                Latest = Option.map (fun (eid,s) -> eid, Set.toList s) latestSet
                Edit =  match model.Edit with
                        | Some l when List.forall Option.isSome l
                            && List.map Option.get l |> Set.ofList = (Option.map snd latestSet |> Option.getElse Set.empty) -> None
                        | _ -> model.Edit
            }

    let updateProperty property msg model =
        update (Property.getEvents property msg |> Update) model

    let view inputUI model =
        let header = UI.div Horizontal [UI.text model.Label ; UI.button "+" Insert]
        let item i a = UI.div Horizontal [inputUI a |> UI.map (fun v -> Modify(i,v)); UI.button "-" (Remove i)]
        let items = current model |> List.mapi item
        header::items |> UI.div Vertical