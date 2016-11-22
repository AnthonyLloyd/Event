namespace Lloyd.Core.Apps

open Lloyd.Core
open Lloyd.Core.UI

module Editor =
    type 'a Model = {Label:string; Previous:(EventID * 'a) option; Latest:(EventID * 'a) option; Edit:'a option; Invalid:string option}
                    member m.Current = m.Edit |> Option.orTry (Option.map snd m.Latest)

    let init property =
        {Label=property.Name+":"; Previous=None; Latest=None; Edit=None; Invalid=None}

    type 'a Msg =
        | Edit of 'a option
        | Reset
        | Update of 'a Events
        | Invalid of string option

    let update msg model =
        match msg with
        | Edit e -> {model with Edit=if e=Option.map snd model.Latest then None else e}
        | Reset -> {model with Edit=None}
        | Update l ->
            let latest = List1.head l |> mapSnd List1.head |> Some
            {model with
                Previous = List1.tail l |> List.tryHead |> Option.map (mapSnd List1.head)
                Latest = latest
                Edit = if model.Edit=Option.map snd latest then None else model.Edit
            }
        | Invalid i -> {model with Invalid=i}

    let eventUpdate property msg model =
        match Property.tryGetEvents property msg with
        | None -> model
        | Some events -> update (Update events) model

    let validate property model =
        {model with
            Invalid =
                if model.Edit=None then None
                else match property.Validation model.Edit with
                     | Ok _ -> None
                     | Error (_,s) -> Some s}

    let updateAndValidate property msg model =
        update msg model |> validate property

    let tooltip (model:'a Model) =
        let versioning =
            match model.Latest with
            | None -> None
            | Some (eid,v) ->
                let t = sprintf "Current :  %-25s%A" (v :> obj |> string) eid // TODO: How do we do enum like Behaviour
                match model.Previous with
                | None -> Some t
                | Some (eid,v) -> sprintf "%s\nPrevious:  %-25s%A" t (v :> obj |> string) eid |> Some
        match model.Invalid, versioning with
        | Some i, Some v -> i+"\n\n"+v |> Some
        | Some i, None -> Some i
        | None, Some v -> Some v
        | None, None -> None
        

    let view inputUI model =
        let current = model.Edit |> Option.orTry (Option.map snd model.Latest)
        let colour = match model.Invalid with | None -> Black | Some _ -> Red
        UI.div [Vertical] [
            UI.text [Bold;Tooltip (tooltip model); TextColour colour] model.Label
            inputUI current |> UI.map Edit
        ]

    let app inputUI property = UI.appSimple (fun () -> init property) update (view inputUI)


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
        let header = UI.div [Horizontal] [UI.text [Bold; Width 150] model.Label ; UI.button [Width 20] "+" Insert]
        let item i a = UI.div [Horizontal] [inputUI [Width 150] a |> UI.map (fun v -> Modify(i,v)); UI.button [Width 20] "-" (Remove i)]
        let items = current model |> List.mapi item
        header::items |> UI.div [Vertical]