namespace Lloyd.Domain.Apps

open Lloyd.Core
open Lloyd.Core.UI
open Lloyd.Core.Apps
open Lloyd.Domain.Model

type Cmd =
    | OpenKidEdit of Kid ID option
    | OpenToyEdit of Toy ID option
    | OpenElfEdit of Elf ID option

module ToyEdit =
    type Model = {ID:Toy ID option; Name:string Editor.Model; AgeRange:(Age*Age) Editor.Model; WorkRequired:uint16 Editor.Model; LastEvent:EventID}

    let init() =
        {ID=None; Name=Editor.init Toy.name; AgeRange=Editor.init Toy.ageRange; WorkRequired=Editor.init Toy.workRequired; LastEvent=EventID.Zero}, None

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

    type Sub =
        | Toy of Toy ID

    let subscription toyEvents model =
        Option.map (fun tid ->
            Toy tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) toyEvents |> Observable.map Update
            ) model.ID
        |> Option.toList |> Map.ofList

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputRange model.AgeRange |> UI.map AgeRangeMsg
            Editor.view UI.inputDigits model.WorkRequired |> UI.map EffortMsg
            UI.button "Save" Save
        ]

    let app toyEvents =
        UI.app init update view (subscription toyEvents)


module ElfEdit =
    type Model = {ID:Elf ID option; Name:string Editor.Model; WorkRate:Work Editor.Model; Making:Toy ID option; LastEvent:EventID; ToyNames:Map<Toy ID,string>}

    let init() =
        {ID=None; Name=Editor.init Elf.name; WorkRate=Editor.init Elf.workRate; Making=None; LastEvent=EventID.Zero; ToyNames=Map.empty}, None

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

    type Sub =
        | Elf of Elf ID
        | ToyName

    let subscription elfEvents toyEvents model =
        (ToyName, Query.toyNames toyEvents |> Observable.map ToyNames)
        :: (Option.map (fun tid ->
                Elf tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) elfEvents |> Observable.map Update
                ) model.ID
           |> Option.toList)
        |> Map.ofList

    let view model =
        let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) model.Making |> Option.getElse "Nothing"
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.WorkRate |> UI.map WorkRateMsg
            UI.button "Save" Save
            UI.text (Elf.making.Name+": "+making)
        ]

    let app elfEvents toyEvents  = UI.app init update view (subscription elfEvents toyEvents)


module KidEdit =
    type Model = {ID:Kid ID option; Name:string Editor.Model; Age:Age Editor.Model; Behaviour:Behaviour Editor.Model; WishList:Toy ID EditorSet.Model; LastEvent:EventID; ToyNames:Map<Toy ID,string>}

    let init() =
        {ID=None; Name=Editor.init Kid.name; Age=Editor.init Kid.age; Behaviour=Editor.init Kid.behaviour; WishList=EditorSet.init Kid.wishList; LastEvent=EventID.Zero; ToyNames=Map.empty}, None

    type Msg =
        | Update of Kid Events
        | ToyNames of Map<Toy ID,string>
        | NameMsg of string Editor.Msg
        | AgeMsg of Age Editor.Msg
        | BehaviourMsg of Behaviour Editor.Msg
        | WishListMsg of Toy ID EditorSet.Msg
        | Save

    let update msg model =
        match msg with
        | Update l -> {model with
                        Name = Editor.updateProperty Kid.name l model.Name
                        Age = Editor.updateProperty Kid.age l model.Age
                        Behaviour = Editor.updateProperty Kid.behaviour l model.Behaviour
                        WishList = EditorSet.updateProperty Kid.wishList l model.WishList
                        LastEvent = List.head l |> fst
                      }, None
        | ToyNames m -> {model with ToyNames=m}, None
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

    type Sub =
        | Kid of Kid ID
        | ToyName

    let subscription kidEvents toyEvents model =
        (ToyName, Query.toyNames toyEvents |> Observable.map ToyNames)
        :: (Option.map (fun tid ->
                Kid tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) kidEvents
                         |> Observable.map Update
                ) model.ID
           |> Option.toList)
        |> Map.ofList

    let view model =
        UI.div Vertical [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.Age |> UI.map AgeMsg
            Editor.view (UI.select [Bad,"Bad";Mixed,"Mixed";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            EditorSet.view (UI.select (Map.toList model.ToyNames)) model.WishList |> UI.map WishListMsg
            UI.button "Save" Save
        ]

    let app kidEvents toyEvents = UI.app init update view (subscription kidEvents toyEvents)


module ElfList =
    type Row = {ID:Elf ID; Name:string; Making:Toy ID option}
    type Model = {Rows:Row list; ToyNames:Map<Toy ID,string>}

    let init() = {Rows=[]; ToyNames=Map.empty}, None

    type Msg =
        | Update of Elf ID * Elf Events
        | ToyNames of Map<Toy ID,string>
        | OpenEdit of Elf ID option

    let update msg model =
        match msg with
        | Update (eid,events) ->
            let createRow() = {
                ID = eid
                Name = Property.get Elf.name events |> Option.getElse String.empty
                Making = Property.get Elf.making events |> Option.getElse None
            }
            match List.tryFindIndex (fun r -> r.ID=eid) model.Rows with
            | None -> {model with Rows = createRow()::model.Rows |> List.sortBy (fun r -> r.Name)}, None
            | Some i -> {model with Rows = List.replacei i (createRow()) model.Rows |> List.sortBy (fun r -> r.Name)}, None
        | ToyNames m -> {model with ToyNames=m}, None
        | OpenEdit eid -> model, OpenElfEdit eid |> Some

    type Sub =
        | ElfUpdate
        | ToyName

    let subscription elfEvents toyEvents =
        let subs =
            Map.ofList [
                ElfUpdate, Observable.map Update elfEvents
                ToyName, Query.toyNames toyEvents |> Observable.map ToyNames
            ]
        fun (_:Model) -> subs

    let view model =
        let header = UI.div Horizontal [UI.text "Elf"; UI.text "Making"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) row.Making |> Option.getElse String.empty
            UI.div Horizontal [UI.text row.Name; UI.text making; UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model.Rows |> UI.div Vertical

    let app elfEvents toyEvents = UI.app init update view (subscription elfEvents toyEvents)


module KidList =
    type Row = {ID:Kid ID; Name:string; WishList:int; Made:int}
    type Model = {Rows:Row list; ToyTotal:Map<Toy ID,int>; ElfLastEvent:EventID}

    let init() = {Rows=[]; ToyTotal=Map.empty; ElfLastEvent=EventID.Zero}, None

    type Msg =
        | KidName of Kid ID * string
        | ToysMade of Map<Toy ID, int>
        | ToyAgeRanges of Map<Toy ID, Age*Age>
        | OpenEdit of Kid ID option

    let update msg model : Model * Cmd option =
        match msg with
        | KidName (_kid,_name) -> failwith "hi"
        | ToysMade _m -> failwith "hi"
        | ToyAgeRanges _m -> failwith "hi"
        | OpenEdit kid -> model, OpenKidEdit kid |> Some

    type Sub =
        | KidName
        | ToysMade
        | ToysAgeRanges
    
    let subscription kidEvents toyEvents elfEvents =
        let subs =
            Map.ofList [
                KidName, Query.kidName kidEvents |> Observable.map Msg.KidName
                ToysMade, Query.toysMade elfEvents |> Observable.map Msg.ToysMade
                ToysAgeRanges, Query.toyAgeRanges toyEvents |> Observable.map Msg.ToyAgeRanges
            ]
        fun (_:Model) -> subs

    let view model =
        let header = UI.div Horizontal [UI.text "Kid"; UI.text "Wish List"; UI.text "Made"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            UI.div Horizontal [UI.text row.Name; UI.text (string row.WishList); UI.text (string row.Made); UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model.Rows |> UI.div Vertical

    let app kidEvents toyEvents elfEvents = UI.app init update view (subscription kidEvents toyEvents elfEvents)


module ToyList =
    type Row = {ID:Toy ID; Name:string; Requested:int; Finished:int}
    type Model = Row list

    let init() = [], None

    type Msg =
        | ToyName of Toy ID * string
        | ToyFinished of Toy ID * int
        | ToyRequested of Toy ID * int
        | OpenEdit of Toy ID option

    let update msg model : Model * Cmd option =
        match msg with
        | ToyName (toy,name) ->
            match List.tryFindIndex (fun r -> r.ID=toy) model with
            | None -> {ID=toy; Name=name; Requested=0; Finished=0}::model |> List.sortBy (fun r -> r.Name), None
            | Some i -> List.replacei i {List.item i model with Name=name} model |> List.sortBy (fun r -> r.Name), None
        | ToyFinished (toy,i) ->
            match List.tryFindIndex (fun r -> r.ID=toy) model with
            | None -> {ID=toy; Name=String.empty; Requested=0; Finished=i}::model, None
            | Some i -> List.replacei i {List.item i model with Finished=i} model, None
        | ToyRequested (toy,i) ->
            match List.tryFindIndex (fun r -> r.ID=toy) model with
            | None -> {ID=toy; Name=String.empty; Requested=i; Finished=0}::model, None
            | Some i -> List.replacei i {List.item i model with Requested=i} model, None
        | OpenEdit toy -> model, OpenToyEdit toy |> Some

    type Sub =
        | ToyName
        | ToyFinished
        | ToyRequested

    let subscription kidEvents toyEvents elfEvents =
        let subs =
            Map.ofList [
                ToyName, Query.toyName toyEvents |> Observable.map Msg.ToyName
                ToyFinished, Query.toyFinished elfEvents |> Observable.map Msg.ToyFinished
                ToyRequested, Query.toyRequested kidEvents toyEvents elfEvents |> Observable.map Msg.ToyRequested
            ]
        fun (_:Model) -> subs


    let view model =
        let header = UI.div Horizontal [UI.text "Toy"; UI.text "Requested"; UI.text "Finished"; UI.button "new" (OpenEdit None)]
        let rowUI row =
            UI.div Horizontal [UI.text row.Name; UI.text (string row.Requested); UI.text (string row.Finished); UI.button "edit" (OpenEdit (Some row.ID))]
        header::List.map rowUI model |> UI.div Vertical

    let app kidEvents toyEvents elfEvents = UI.app init update view (subscription kidEvents toyEvents elfEvents)


module Main =
    type Model = {Kid:KidList.Model; Toy:ToyList.Model; Elf:ElfList.Model}

    let init() =
        let kidModel,kidCmd = KidList.init()
        let toyModel,toyCmd = ToyList.init()
        let elfModel,elfCmd = ElfList.init()
        {Kid=kidModel; Toy=toyModel; Elf=elfModel}, List.choose id [kidCmd;toyCmd;elfCmd]

    type Msg =
        | KidMsg of KidList.Msg
        | ToyMsg of ToyList.Msg
        | ElfMsg of ElfList.Msg

    let update msg model =
        match msg with
        | KidMsg m -> let kid,cmd = KidList.update m model.Kid in {model with Kid = kid}, Option.toList cmd
        | ToyMsg m -> let toy,cmd = ToyList.update m model.Toy in {model with Toy = toy}, Option.toList cmd
        | ElfMsg m -> let elf,cmd = ElfList.update m model.Elf in {model with Elf = elf}, Option.toList cmd

    type Sub =
        | KidList of KidList.Sub
        | ToyList of ToyList.Sub
        | ElfList of ElfList.Sub
    
    let subscription kidEvents toyEvents elfEvents model =
        KidList.subscription kidEvents toyEvents elfEvents model.Kid |> Map.toSeq |> Seq.map (fun (k,v) -> KidList k,Observable.map KidMsg v)
        |> Seq.append (ToyList.subscription kidEvents toyEvents elfEvents model.Toy |> Map.toSeq |> Seq.map (fun (k,v) -> ToyList k,Observable.map ToyMsg v))
        |> Seq.append (ElfList.subscription elfEvents toyEvents model.Elf |> Map.toSeq |> Seq.map (fun (k,v) -> ElfList k,Observable.map ElfMsg v))
        |> Map.ofSeq

    let view model =
        UI.div Horizontal [
            KidList.view model.Kid |> UI.map KidMsg
            ToyList.view model.Toy |> UI.map ToyMsg
            ElfList.view model.Elf |> UI.map ElfMsg
        ]

    let app kidEvents toyEvents elfEvents = UI.app init update view (subscription kidEvents toyEvents elfEvents)