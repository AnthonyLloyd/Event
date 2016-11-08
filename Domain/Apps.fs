namespace Lloyd.Domain.Apps

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

    type Model = {Name:string Editor.Model; Age:Age Editor.Model; Behaviour:Behaviour Editor.Model; LastEvent:EventID option}

    let init() =
        {Name=Editor.init Kid.name; Age=Editor.init Kid.age; Behaviour=Editor.init Kid.behaviour; LastEvent=None}, None

    type Msg =
        | Update of Kid Events
        | NameMsg of string Editor.Msg
        | AgeMsg of Age Editor.Msg
        | BehaviourMsg of Behaviour Editor.Msg
        | Save

    let update msg model =
        printfn "Kid.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Kid.name l model.Name
                        Age = Editor.updateProperty Kid.age l model.Age
                        LastEvent = List.tryHead l |> Option.map fst |> Option.orTry model.LastEvent
                      }, None
        | NameMsg n -> {model with Name=Editor.update n model.Name}, None
        | AgeMsg r -> {model with Age=Editor.update r model.Age}, None
        | BehaviourMsg c -> {model with Behaviour=Editor.update c model.Behaviour}, None
        | Save ->
            let cmd =
                List.tryCons (Option.map (Property.set Kid.name) model.Name.Edit) []
                |> List.tryCons (Option.map (Property.set Kid.age) model.Age.Edit)
                |> List.tryCons (Option.map (Property.set Kid.behaviour) model.Behaviour.Edit)
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.Age |> UI.map AgeMsg
            Editor.view (UI.select [Bad,"Bad";Ok,"Ok";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            (*Toy set*)
            UI.button "Save" Save
        ]

    let app() = UI.app init update view subscription