namespace Lloyd.Domain.Apps

open Lloyd.Core.UI
open Lloyd.Domain.Model

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
            let latest,previous = match l with |[] -> None,None |[e,[a]] -> Some(e,a),None |(le,[la])::(pe,[pa])::_ -> Some(le,la),Some(pe,pa) |_ -> failwith "Not possible"
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

    let app inputUI property = UI.appSimple (fun () -> init property) update (view inputUI) // TODO: tooltip, coloured border, lots of input editors for types, rightclick reset

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

module Toy =

    type Model = {Name:string Editor.Model; AgeRange:(Age*Age) Editor.Model; WorkRequired:uint16 Editor.Model; LastEvent:EventID option}

    let init() =
        {Name=Editor.init Toy.name; AgeRange=Editor.init Toy.ageRange; WorkRequired=Editor.init Toy.workRequired; LastEvent=None}, None

    type Msg =
        | Update of Toy Events
        | NameMsg of string Editor.Msg
        | AgeRangeMsg of (Age*Age) Editor.Msg
        | EffortMsg of uint16 Editor.Msg
        | Save

    let update msg model =
        printfn "Toy.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Toy.name l model.Name
                        AgeRange = Editor.updateProperty Toy.ageRange l model.AgeRange
                        WorkRequired = Editor.updateProperty Toy.workRequired l model.WorkRequired
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      }, None
        | NameMsg n -> {model with Name=Editor.update n model.Name}, None
        | AgeRangeMsg r -> {model with AgeRange=Editor.update r model.AgeRange}, None
        | EffortMsg c -> {model with WorkRequired=Editor.update c model.WorkRequired}, None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Toy.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Toy.ageRange) model.AgeRange.Edit)
                |> List.tryCons (Option.map (Property.set Toy.workRequired) model.WorkRequired.Edit)
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputRange model.AgeRange |> UI.map AgeRangeMsg
            Editor.view UI.inputDigits model.WorkRequired |> UI.map EffortMsg
            UI.button "Save" Save
        ]

    let app() = UI.app init update view subscription


module Elf =

    type Model = {Name:string Editor.Model; WorkRate:Work Editor.Model; Making:Toy ID option; LastEvent:EventID option}

    let init() =
        {Name=Editor.init Elf.name; WorkRate=Editor.init Elf.workRate; Making=None; LastEvent=None}, None

    type Msg =
        | Update of Elf Events
        | NameMsg of string Editor.Msg
        | WorkRateMsg of Work Editor.Msg
        | Save

    let update msg model =
        printfn "Elf.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Elf.name l model.Name
                        WorkRate = Editor.updateProperty Elf.workRate l model.WorkRate
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      }, None
        | NameMsg n -> {model with Name=Editor.update n model.Name}, None
        | WorkRateMsg r -> {model with WorkRate=Editor.update r model.WorkRate}, None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Elf.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Elf.workRate) model.WorkRate.Edit)
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.WorkRate |> UI.map WorkRateMsg
            UI.button "Save" Save
            UI.text (Elf.making.Name+": "+match model.Making with | None -> "Nothing" | Some _ -> "Need name!!!!!")
        ]

    let app() = UI.app init update view subscription


module Kid =

    type Model = {Name:string Editor.Model; Age:Age Editor.Model; Behaviour:Behaviour Editor.Model; WishList:Toy ID EditorSet.Model; LastEvent:EventID option}

    let init() =
        {Name=Editor.init Kid.name; Age=Editor.init Kid.age; Behaviour=Editor.init Kid.behaviour; WishList=EditorSet.init Kid.wishList; LastEvent=None}, None

    type Msg =
        | Update of Kid Events
        | NameMsg of string Editor.Msg
        | AgeMsg of Age Editor.Msg
        | BehaviourMsg of Behaviour Editor.Msg
        | WishListMsg of Toy ID EditorSet.Msg
        | Save

    let update msg model =
        printfn "Kid.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Kid.name l model.Name
                        Age = Editor.updateProperty Kid.age l model.Age
                        Behaviour = Editor.updateProperty Kid.behaviour l model.Behaviour
                        WishList = EditorSet.updateProperty Kid.wishList l model.WishList
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      }, None
        | NameMsg n -> {model with Name=Editor.update n model.Name}, None
        | AgeMsg r -> {model with Age=Editor.update r model.Age}, None
        | BehaviourMsg c -> {model with Behaviour=Editor.update c model.Behaviour}, None
        | WishListMsg w -> {model with WishList=EditorSet.update w model.WishList}, None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Kid.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Kid.age) model.Age.Edit)
                |> List.tryCons (Option.map (Property.set Kid.behaviour) model.Behaviour.Edit)
            let after = model.WishList.Edit |> Option.map (List.choose id>>Set.ofList)
            let before = model.WishList.Latest |> Option.map (snd>>Set.ofList) |> Option.getElse Set.empty
            let cmd = List.tryAppend (Option.map (SetEvent.difference before>>List.map Kid.WishList) after) cmd
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.Age |> UI.map AgeMsg
            Editor.view (UI.select [Bad,"Bad";Ok,"Ok";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            EditorSet.view (UI.select []) model.WishList |> UI.map WishListMsg
            UI.button "Save" Save
        ]

    let app() = UI.app init update view subscription