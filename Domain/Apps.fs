namespace Lloyd.Domain.Apps

open Lloyd.Core.UI
open Lloyd.Core.Apps
open Lloyd.Domain.Model

module ToyEdit =
    type Model = {Name:string Editor.Model; AgeRange:(Age*Age) Editor.Model; WorkRequired:uint16 Editor.Model; LastEvent:EventID}

    let init() =
        {Name=Editor.init Toy.name; AgeRange=Editor.init Toy.ageRange; WorkRequired=Editor.init Toy.workRequired; LastEvent=EventID.Zero}, None

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
                        LastEvent = List.head l |> fst
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


module ElfEdit =
    type Model = {Name:string Editor.Model; WorkRate:Work Editor.Model; Making:Toy ID option; LastEvent:EventID; ToyNames:Map<Toy ID,string>}

    let init() =
        {Name=Editor.init Elf.name; WorkRate=Editor.init Elf.workRate; Making=None; LastEvent=EventID.Zero; ToyNames=Map.empty}, None

    type Msg =
        | Update of Elf Events
        | ToyNames of Map<Toy ID,string>
        | NameMsg of string Editor.Msg
        | WorkRateMsg of Work Editor.Msg
        | Save

    let update msg model =
        printfn "Elf.update: %A" msg
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Elf.name l model.Name
                        WorkRate = Editor.updateProperty Elf.workRate l model.WorkRate
                        LastEvent = List.head l |> fst
                      }, None
        | ToyNames m -> {model with ToyNames=m}, None
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
        let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) model.Making |> Option.getElse "Nothing"
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.WorkRate |> UI.map WorkRateMsg
            UI.button "Save" Save
            UI.text (Elf.making.Name+": "+making)
        ]

    let app() = UI.app init update view subscription


module KidEdit =
    type Model = {Name:string Editor.Model; Age:Age Editor.Model; Behaviour:Behaviour Editor.Model; WishList:Toy ID EditorSet.Model; LastEvent:EventID}

    let init() =
        {Name=Editor.init Kid.name; Age=Editor.init Kid.age; Behaviour=Editor.init Kid.behaviour; WishList=EditorSet.init Kid.wishList; LastEvent=EventID.Zero}, None

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
                        LastEvent = List.head l |> fst
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
            let cmd =
                let after = model.WishList.Edit |> Option.map (List.choose id>>Set.ofList)
                let before = model.WishList.Latest |> Option.map (snd>>Set.ofList) |> Option.getElse Set.empty
                List.tryAppend (Option.map (SetEvent.difference before>>List.map Kid.WishList) after) cmd
            model, Some(model.LastEvent,cmd)

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.Age |> UI.map AgeMsg
            Editor.view (UI.select [Bad,"Bad";Mixed,"Mixed";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            EditorSet.view (UI.select []) model.WishList |> UI.map WishListMsg
            UI.button "Save" Save
        ]

    let app() = UI.app init update view subscription

type Cmd =
    | OpenKidEdit of Kid ID option
    | OpenToyEdit of Toy ID option
    | OpenElfEdit of Elf ID option

module ElfList =
    type Row = {ID:Elf ID; Name:string; Making:Toy ID option}
    type Model = {Rows:Row list; ToyNames:Map<Toy ID,string>}

    let init() = {Rows=[]; ToyNames=Map.empty}, None

    type Msg =
        | ElfUpdate of Elf ID * Elf Events
        | ToyUpdate of Toy ID * Toy Events
        | OpenEdit of Elf ID option

    let update msg model =
        match msg with
        | ElfUpdate (eid,events) ->
            let createRow() = {
                ID = eid
                Name = Property.get Elf.name events |> Option.getElse String.empty
                Making = Property.get Elf.making events |> Option.getElse None
            }
            match List.tryFindIndex (fun r -> r.ID=eid) model.Rows with
            | None -> {model with Rows = createRow()::model.Rows |> List.sortBy (fun r -> r.Name)}, None
            | Some i -> {model with Rows = List.replacei i (createRow()) model.Rows |> List.sortBy (fun r -> r.Name)}, None
        | ToyUpdate (tid,events) ->
            match Map.tryFind tid model.ToyNames, Property.get Toy.name events with
            | _, None -> model, None
            | None, Some n -> {model with ToyNames = Map.add tid n model.ToyNames}, None
            | Some o, Some n -> if o=n then model, None else {model with ToyNames = Map.add tid n model.ToyNames}, None
        | OpenEdit eid -> model, OpenElfEdit eid |> Some

    let subscription _ =
        Set.singleton ()

    let view model =
        let header = UI.div Horizontal [UI.text "Elf"; UI.text "Making"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) row.Making |> Option.getElse String.empty
            UI.div Horizontal [UI.text row.Name; UI.text making; UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model.Rows |> UI.div Vertical

    let app() = UI.app init update view subscription


module KidList =
    type Row = {ID:Kid ID; Name:string; WishList:int; Made:int}
    type Model = {Rows:Row list; ToyTotal:Map<Toy ID,int>; ElfLastEvent:EventID}

    let init() = {Rows=[]; ToyTotal=Map.empty; ElfLastEvent=EventID.Zero}, None

    type Msg =
        | KidUpdate of Kid ID * Kid Events
        | ElfUpdate of Elf ID * Elf Events
        | OpenEdit of Kid ID option

    let update msg model : Model * Cmd option =
        match msg with
        | KidUpdate (kid,events) ->
            failwith "hi"
        | ElfUpdate (eid,events) ->
            {model with
                ElfLastEvent = List.head events |> fst
            }, None
        | OpenEdit kid -> model, OpenKidEdit kid |> Some

    let subscription _ =
        Set.singleton ()

    let view model =
        let header = UI.div Horizontal [UI.text "Kid"; UI.text "Wish List"; UI.text "Made"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            UI.div Horizontal [UI.text row.Name; UI.text (string row.WishList); UI.text (string row.Made); UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model.Rows |> UI.div Vertical

    let app() = UI.app init update view subscription


module ToyList =
    type Row = {ID:Toy ID; Name:string; Requested:int; Finished:int}
    type Model = Row list

    let init() = [], None

    type Msg =
        | KidUpdate of Kid ID * Kid Events
        | ToyUpdate of Toy ID * Toy Events
        | ElfUpdate of Elf ID * Elf Events
        | OpenEdit of Toy ID option

    let update msg model : Model * Cmd option =
        failwith "hi"

    let subscription _ =
        Set.singleton ()

    let view model =
        let header = UI.div Horizontal [UI.text "Toy"; UI.text "Requested"; UI.text "Finished"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            UI.div Horizontal [UI.text row.Name; UI.text (string row.Requested); UI.text (string row.Finished); UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model |> UI.div Vertical

    let app() = UI.app init update view subscription


module Main =
    type Model = {Kid:KidList.Model; Toy:ToyList.Model; Elf:ElfList.Model}

    let init() = {Kid=KidList.init() |> fst; Toy=ToyList.init() |> fst; Elf=ElfList.init() |> fst}, []

    type Msg =
        | KidUpdate of Kid ID * Kid Events
        | ToyUpdate of Toy ID * Toy Events
        | ElfUpdate of Elf ID * Elf Events
        | KidMsg of KidList.Msg
        | ToyMsg of ToyList.Msg
        | ElfMsg of ElfList.Msg

    let update msg model =
        match msg with
        | KidUpdate (kid,events) ->
            let kidModel,kidCmd = KidList.update (KidList.Msg.KidUpdate(kid,events)) model.Kid
            let toyModel,toyCmd = ToyList.update (ToyList.Msg.KidUpdate(kid,events)) model.Toy
            {model with Kid = kidModel; Toy = toyModel}, List.choose id [kidCmd;toyCmd]
        | ToyUpdate (tid,events) ->
            let toyModel,toyCmd = ToyList.update (ToyList.Msg.ToyUpdate(tid,events)) model.Toy
            let elfModel,elfCmd = ElfList.update (ElfList.Msg.ToyUpdate(tid,events)) model.Elf
            {model with Toy = toyModel; Elf = elfModel}, List.choose id [toyCmd;elfCmd]
        | ElfUpdate (eid,events) ->
            let kidModel,kidCmd = KidList.update (KidList.Msg.ElfUpdate(eid,events)) model.Kid
            let toyModel,toyCmd = ToyList.update (ToyList.Msg.ElfUpdate(eid,events)) model.Toy
            let elfModel,elfCmd = ElfList.update (ElfList.Msg.ElfUpdate(eid,events)) model.Elf
            {model with Kid = kidModel; Toy = toyModel; Elf = elfModel}, List.choose id [kidCmd;toyCmd;elfCmd]
        | KidMsg m -> let kid,cmd = KidList.update m model.Kid in {model with Kid = kid}, Option.toList cmd
        | ToyMsg m -> let toy,cmd = ToyList.update m model.Toy in {model with Toy = toy}, Option.toList cmd
        | ElfMsg m -> let elf,cmd = ElfList.update m model.Elf in {model with Elf = elf}, Option.toList cmd

    let subscription _ =
        Set.singleton ()

    let view model =
        UI.div Horizontal [
            KidList.view model.Kid |> UI.map KidMsg
            ToyList.view model.Toy |> UI.map ToyMsg
            ElfList.view model.Elf |> UI.map ElfMsg
        ]

    let app() = UI.app init update view subscription