namespace Lloyd.Domain.Apps

open Lloyd.Core
open Lloyd.Core.UI
open Lloyd.Core.Apps
open Lloyd.Domain.Model


type Cmd =
    | OpenKidEdit of Kid ID option
    | OpenToyEdit of Toy ID option
    | OpenElfEdit of Elf ID option


module KidEdit =

    type Model = {
        ID: Kid ID option
        Current: Kid Events option
        Name: string Editor.Model
        Age: Age Editor.Model
        Behaviour: Behaviour Editor.Model
        WishList: Toy ID EditorSet.Model
        ToyNames: Map<Toy ID,string>
        ToyAgeRange: Map<Toy ID,Age*Age>
        SaveValidation: Result<unit,string list>
        SaveResponse: string
    }

    let init kid =
        {
            ID = kid
            Current = None
            Name = Editor.init Kid.name
            Age = Editor.init Kid.age
            Behaviour = Editor.init Kid.behaviour
            WishList = EditorSet.init Kid.wishList
            ToyNames = Map.empty
            ToyAgeRange = Map.empty
            SaveValidation = Error []
            SaveResponse = String.empty}, []

    type Msg =
        | Update of Kid Events
        | ToyNames of Map<Toy ID,string>
        | ToyAgeRanges of Map<Toy ID,Age*Age>
        | NameMsg of string Editor.Msg
        | AgeMsg of Age Editor.Msg
        | BehaviourMsg of Behaviour Editor.Msg
        | WishListMsg of Toy ID EditorSet.Msg
        | Save
        | CreateResult of Result<Kid ID,Store.Error>
        | UpdateResult of Result<unit,Store.Error>

    let update msg model =
        let edits model = // TODO: deleteing a field doesn't raise the validation
            model.WishList.Edit
            |> Option.map (fun e ->
                let before = model.WishList.Latest |> Option.map snd |> Option.getElse Set.empty
                List.choose id e |> Set.ofList |> SetEvent.difference before |> List.map Kid.WishList
                )
            |> Option.getElse []
            |> Option.cons (Option.bind (Option.map Kid.Name) model.Name.Edit)
            |> Option.cons (Option.bind (Option.map Kid.Age) model.Age.Edit)
            |> Option.cons (Option.bind (Option.map Kid.Behaviour) model.Behaviour.Edit)
        let model,cmd =
            match msg with
            | Update l -> {model with
                            Current = Some l
                            Name = Editor.eventUpdate Kid.name l model.Name
                            Age = Editor.eventUpdate Kid.age l model.Age
                            Behaviour = Editor.eventUpdate Kid.behaviour l model.Behaviour
                            WishList = EditorSet.updateProperty Kid.wishList l model.WishList
                          }, []
            | ToyNames m -> {model with ToyNames=m; WishList=EditorSet.update (EditorSet.Order m) model.WishList}, []
            | ToyAgeRanges m -> {model with ToyAgeRange=m}, []
            | NameMsg n -> {model with Name=Editor.updateAndValidate Kid.name n model.Name; SaveResponse=String.empty}, []
            | AgeMsg r -> {model with Age=Editor.updateAndValidate Kid.age r model.Age; SaveResponse=String.empty}, []
            | BehaviourMsg c -> {model with Behaviour=Editor.updateAndValidate Kid.behaviour c model.Behaviour; SaveResponse=String.empty}, []
            | WishListMsg w -> {model with WishList=EditorSet.update w model.WishList; SaveResponse=String.empty}, []
            | Save ->
                let cmd =
                    List1.tryOfList (edits model)
                    |> Option.map (addSnd (model.ID, model.Current))
                    |> Option.toList
                (if List.isEmpty cmd then model else {model with SaveResponse="Saving..."}), cmd
            | CreateResult (Ok kid) -> {model with ID=Some kid; SaveResponse="Saved"}, []
            | UpdateResult (Ok ()) -> {model with SaveResponse="Saved"}, []
            | CreateResult (Error Store.Error.Concurrency)
            | UpdateResult (Error Store.Error.Concurrency) ->
                {model with SaveResponse="Update happened during save, please retry"}, []
        {model with SaveValidation=Property.validateEdit Kid.view model.Current (edits model)}, cmd

    type Sub =
        | Kid of Kid ID
        | ToyName
        | ToyAgeRange

    let subscription kidEvents toyEvents model =
        [
            ToyName, Query.toyNames toyEvents |> Observable.map ToyNames
            ToyAgeRange, Query.toyAgeRanges toyEvents |> Observable.map ToyAgeRanges
        ]
        |> Option.cons (Option.map (fun tid -> Kid tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) kidEvents |> Observable.map Update) model.ID)
        |> Map.ofList

    let view model =
        let toyNames = Map.toList model.ToyAgeRange |> List.choose (fun (toy,(lo,hi)) ->
                            Option.bind (fun a -> if between lo hi a then
                                                    Map.tryFind toy model.ToyNames |> Option.map (addFst toy)
                                                  else None ) model.Age.Current)
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.Age |> UI.map AgeMsg
            Editor.view (UI.select [] [Bad,"Bad";Mixed,"Mixed";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            EditorSet.view (fun style current -> UI.select style toyNames current) model.WishList |> UI.map WishListMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
        ]

    let app kid kidEvents toyEvents handler = UI.app (fun () -> init kid) update view (subscription kidEvents toyEvents) handler


module ToyEdit =

    type Model = {
        ID: Toy ID option
        Current: Toy Events option
        Name: string Editor.Model
        AgeRange: (Age*Age) Editor.Model
        WorkRequired: uint16 Editor.Model
        SaveValidation: Result<unit,string list>
        SaveResponse: string
    }

    let init toy =
        {
            ID = toy
            Current = None
            Name = Editor.init Toy.name
            AgeRange = Editor.init Toy.ageRange
            WorkRequired = Editor.init Toy.workRequired
            SaveValidation = Error []
            SaveResponse = String.empty
        }, []

    type Msg =
        | Update of Toy Events
        | NameMsg of string Editor.Msg
        | AgeRangeMsg of (Age*Age) Editor.Msg
        | EffortMsg of uint16 Editor.Msg
        | Save
        | CreateResult of Result<Toy ID,Store.Error>
        | UpdateResult of Result<unit,Store.Error>

    let update msg model =
        let edits model =
            Option.cons (Option.bind (Option.map Toy.Name) model.Name.Edit) []
            |> Option.cons (Option.bind (Option.map Toy.AgeRange) model.AgeRange.Edit)
            |> Option.cons (Option.bind (Option.map Toy.WorkRequired) model.WorkRequired.Edit)
        let model, cmd =
            match msg with
            | Update l -> {model with
                            Current = Some l
                            Name = Editor.eventUpdate Toy.name l model.Name
                            AgeRange = Editor.eventUpdate Toy.ageRange l model.AgeRange
                            WorkRequired = Editor.eventUpdate Toy.workRequired l model.WorkRequired
                          }, []
            | NameMsg n -> {model with Name=Editor.updateAndValidate Toy.name n model.Name; SaveResponse=String.empty}, []
            | AgeRangeMsg r -> {model with AgeRange=Editor.updateAndValidate Toy.ageRange r model.AgeRange; SaveResponse=String.empty}, []
            | EffortMsg c -> {model with WorkRequired=Editor.updateAndValidate Toy.workRequired c model.WorkRequired; SaveResponse=String.empty}, []
            | Save ->
                let cmd =
                    List1.tryOfList (edits model)
                    |> Option.map (addSnd (model.ID, model.Current))
                    |> Option.toList
                model, cmd
            | CreateResult (Ok toy) -> {model with ID=Some toy; SaveResponse="Saved"}, []
            | UpdateResult (Ok ()) -> {model with SaveResponse="Saved"}, []
            | CreateResult (Error Store.Error.Concurrency)
            | UpdateResult (Error Store.Error.Concurrency) ->
                {model with SaveResponse="Update happened during save, please retry"}, []
        {model with SaveValidation=Property.validateEdit Toy.view model.Current (edits model)}, cmd

    type Sub =
        | Toy of Toy ID

    let subscription toyEvents model =
        Option.map (fun tid ->
            Toy tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) toyEvents |> Observable.map Update
            ) model.ID
        |> Option.toList |> Map.ofList

    let view model =
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputRange model.AgeRange |> UI.map AgeRangeMsg
            Editor.view UI.inputDigits model.WorkRequired |> UI.map EffortMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
        ]

    let app toy toyEvents handler =
        UI.app (fun () -> init toy) update view (subscription toyEvents) handler


module ElfEdit =

    type Model = {
        ID: Elf ID option
        Current: Elf Events option
        Name: string Editor.Model
        WorkRate: Work Editor.Model
        Making: Toy ID option
        ToyNames: Map<Toy ID,string>
        SaveValidation: Result<unit,string list>
        SaveResponse: string
    }

    let init elf =
        {
            ID = elf
            Current = None
            Name = Editor.init Elf.name
            WorkRate = Editor.init Elf.workRate
            Making = None
            ToyNames = Map.empty
            SaveValidation = Error []
            SaveResponse = String.empty
        }, []

    type Msg =
        | Update of Elf Events
        | ToyNames of Map<Toy ID,string>
        | NameMsg of string Editor.Msg
        | WorkRateMsg of Work Editor.Msg
        | Save
        | CreateResult of Result<Elf ID,Store.Error>
        | UpdateResult of Result<unit,Store.Error>

    let update msg model =
        let edits model =
            Option.cons (Option.bind (Option.map Elf.Name) model.Name.Edit) []
            |> Option.cons (Option.bind (Option.map Elf.WorkRate) model.WorkRate.Edit)
        let model, cmd =
            match msg with
            | Update l -> {model with
                            Current = Some l
                            Name = Editor.eventUpdate Elf.name l model.Name
                            WorkRate = Editor.eventUpdate Elf.workRate l model.WorkRate
                            Making = Property.get Elf.making l |> Option.bind id
                          }, []
            | ToyNames m -> {model with ToyNames=m}, []
            | NameMsg n -> {model with Name=Editor.updateAndValidate Elf.name n model.Name; SaveResponse=String.empty}, []
            | WorkRateMsg r -> {model with WorkRate=Editor.updateAndValidate Elf.workRate r model.WorkRate; SaveResponse=String.empty}, []
            | Save ->
                let cmd =
                    List1.tryOfList (edits model)
                    |> Option.map (addSnd (model.ID, model.Current))
                    |> Option.toList
                model, cmd
            | CreateResult (Ok toy) -> {model with ID=Some toy; SaveResponse="Saved"}, []
            | UpdateResult (Ok ()) -> {model with SaveResponse="Saved"}, []
            | CreateResult (Error Store.Error.Concurrency)
            | UpdateResult (Error Store.Error.Concurrency) ->
                {model with SaveResponse="Update happened during save, please retry"}, []
        {model with SaveValidation=Property.validateEdit Elf.view model.Current (edits model)}, cmd

    type Sub =
        | Elf of Elf ID
        | ToyName

    let subscription elfEvents toyEvents model =
        [(ToyName, Query.toyNames toyEvents |> Observable.map ToyNames)]
        |> Option.cons (Option.map (fun tid -> Elf tid, Observable.choose (fun (i,e) -> if i=tid then Some e else None) elfEvents |> Observable.map Update) model.ID)
        |> Map.ofList

    let view model =
        let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) model.Making |> Option.getElse "Nothing"
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputDigits model.WorkRate |> UI.map WorkRateMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
            UI.div [Horizontal] [UI.text [Bold] (Elf.making.Name+":"); UI.text [] making]
        ]

    let app elf elfEvents toyEvents handler = UI.app (fun () -> init elf) update view (subscription elfEvents toyEvents) handler


module KidList =
    type Row = {ID:Kid ID; Name:string; Requested:int; Finished:int}
    type Model = Row list

    let init() = [], []

    type Msg =
        | KidName of Kid ID * string
        | KidRequested of (Kid ID * int) list
        | KidFinished of (Kid ID * int) list
        | OpenEdit of Kid ID option

    let update msg model =
        match msg with
        | KidName (kid,name) ->
            match List.tryFindIndex (fun r -> r.ID=kid) model with
            | None -> {ID=kid; Name=name; Requested=0; Finished=0}::model |> List.sortBy (fun r -> r.Name), []
            | Some i -> List.replacei i {List.item i model with Name=name} model |> List.sortBy (fun r -> r.Name), []
        | KidRequested l ->
            List.fold (fun model (kid,requested) ->
                    match List.tryFindIndex (fun r -> r.ID=kid) model with
                    | None -> {ID=kid; Name=String.empty; Requested=requested; Finished=0}::model
                    | Some i -> List.replacei i {List.item i model with Requested=requested} model
                ) model l, []
        | KidFinished l ->
            List.fold (fun model (kid,finished) ->
                    match List.tryFindIndex (fun r -> r.ID=kid) model with
                    | None -> {ID=kid; Name=String.empty; Requested=0; Finished=finished}::model
                    | Some i -> List.replacei i {List.item i model with Finished=finished} model
                ) model l, []
        | OpenEdit kid -> model, [OpenKidEdit kid]

    type Sub =
        | KidName
        | KidRequested
        | KidFinished
    
    let subscription kidEvents toyProgress =
        let subs =
            Map.ofList [
                KidName, Query.kidName kidEvents |> Observable.map Msg.KidName
                KidRequested, Query.kidRequested toyProgress |> Observable.map Msg.KidRequested 
                KidFinished, Query.kidFinished toyProgress |> Observable.map Msg.KidFinished
            ]
        fun (_:Model) -> subs

    let view model =
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; Width 150] "Kid"
                UI.text [Bold; Width 70] "Requested"
                UI.text [Bold; Width 70] "Finished"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI row =
            UI.div [Horizontal] [
                UI.text [Width 150] row.Name
                UI.text [Width 70] (string row.Requested)
                UI.text [Width 70] (string row.Finished)
                UI.button [Width 50] "edit" (OpenEdit (Some row.ID))
            ]
        header::List.map rowUI model |> UI.div [Vertical;Width 400]

    let app kidEvents toyProgress handler = UI.app init update view (subscription kidEvents toyProgress) handler


module ToyList =
    type Row = {ID:Toy ID; Name:string; Requested:int; Finished:int}
    type Model = Row list

    let init() = [], []

    type Msg =
        | ToyName of Toy ID * string
        | ToyRequested of (Toy ID * int) list
        | ToyFinished of (Toy ID * int) list
        | OpenEdit of Toy ID option

    let update msg model =
        match msg with
        | ToyName (toy,name) ->
            match List.tryFindIndex (fun r -> r.ID=toy) model with
            | None -> {ID=toy; Name=name; Requested=0; Finished=0}::model |> List.sortBy (fun r -> r.Name), []
            | Some i -> List.replacei i {List.item i model with Name=name} model |> List.sortBy (fun r -> r.Name), []
        | ToyRequested l ->
            List.fold (fun model (toy,requested) ->
                    match List.tryFindIndex (fun r -> r.ID=toy) model with
                    | None -> {ID=toy; Name=String.empty; Requested=requested; Finished=0}::model
                    | Some i -> List.replacei i {List.item i model with Requested=requested} model
                ) model l, []
        | ToyFinished l ->
            List.fold (fun model (toy,finished) ->
                    match List.tryFindIndex (fun r -> r.ID=toy) model with
                    | None -> {ID=toy; Name=String.empty; Requested=0; Finished=finished}::model
                    | Some i -> List.replacei i {List.item i model with Finished=finished} model
                ) model l, []
        | OpenEdit toy -> model, [OpenToyEdit toy]

    type Sub =
        | ToyName
        | ToyFinished
        | ToyRequested

    let subscription toyEvents toyProgress =
        let subs =
            Map.ofList [
                ToyName, Query.toyName toyEvents |> Observable.map Msg.ToyName
                ToyFinished, Query.toyFinished toyProgress |> Observable.map Msg.ToyFinished
                ToyRequested, Query.toyRequested toyProgress |> Observable.map Msg.ToyRequested
            ]
        fun (_:Model) -> subs

    let view model =
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; Width 150] "Toy"
                UI.text [Bold; Width 70] "Requested"
                UI.text [Bold; Width 70] "Finished"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI row =
            UI.div [Horizontal] [
                UI.text [Width 150] row.Name
                UI.text [Width 70] (string row.Requested)
                UI.text [Width 70] (string row.Finished)
                UI.button [Width 50] "edit" (OpenEdit (Some row.ID))
            ]
        header::List.map rowUI model |> UI.div [Vertical;Width 400]

    let app toyEvents toyProgress handler = UI.app init update view (subscription toyEvents toyProgress) handler


module ElfList =
    type Row = {ID:Elf ID; Name:string; Making:Toy ID option}
    type Model = {Rows:Row list; ToyNames:Map<Toy ID,string>}

    let init() = {Rows=[]; ToyNames=Map.empty}, []

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
            | None -> {model with Rows = createRow()::model.Rows |> List.sortBy (fun r -> r.Name)}, []
            | Some i -> {model with Rows = List.replacei i (createRow()) model.Rows |> List.sortBy (fun r -> r.Name)}, []
        | ToyNames m -> {model with ToyNames=m}, []
        | OpenEdit eid -> model, [OpenElfEdit eid]

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
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; Width 180] "Elf"
                UI.text [Bold; Width 140] "Making"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI row =
            let making = Option.bind (fun tid -> Map.tryFind tid model.ToyNames) row.Making |> Option.getElse String.empty
            UI.div [Horizontal] [
                UI.text [Width 180] row.Name
                UI.text [Width 140] making
                UI.button [Width 50] "edit" (OpenEdit (Some row.ID))
            ]
        header::List.map rowUI model.Rows |> UI.div [Vertical;Width 400]

    let app elfEvents toyEvents handler = UI.app init update view (subscription elfEvents toyEvents) handler


module Main =
    type Model = {Kid:KidList.Model; Toy:ToyList.Model; Elf:ElfList.Model}

    let init() =
        let kidModel,kidCmd = KidList.init()
        let toyModel,toyCmd = ToyList.init()
        let elfModel,elfCmd = ElfList.init()
        {Kid=kidModel; Toy=toyModel; Elf=elfModel}, List.concat [kidCmd;toyCmd;elfCmd]

    type Msg =
        | KidMsg of KidList.Msg
        | ToyMsg of ToyList.Msg
        | ElfMsg of ElfList.Msg

    let update msg model =
        match msg with
        | KidMsg m -> let kid,cmd = KidList.update m model.Kid in {model with Kid = kid}, cmd
        | ToyMsg m -> let toy,cmd = ToyList.update m model.Toy in {model with Toy = toy}, cmd
        | ElfMsg m -> let elf,cmd = ElfList.update m model.Elf in {model with Elf = elf}, cmd

    type Sub =
        | KidList of KidList.Sub
        | ToyList of ToyList.Sub
        | ElfList of ElfList.Sub
    
    let subscription kidEvents toyEvents elfEvents toyProgress model =
        KidList.subscription kidEvents toyProgress model.Kid |> Map.toSeq |> Seq.map (fun (k,v) -> KidList k,Observable.map KidMsg v)
        |> Seq.append (ToyList.subscription toyEvents toyProgress model.Toy |> Map.toSeq |> Seq.map (fun (k,v) -> ToyList k,Observable.map ToyMsg v))
        |> Seq.append (ElfList.subscription elfEvents toyEvents model.Elf |> Map.toSeq |> Seq.map (fun (k,v) -> ElfList k,Observable.map ElfMsg v))
        |> Map.ofSeq

    let view model =
        UI.div [Horizontal] [
            KidList.view model.Kid |> UI.map KidMsg
            ToyList.view model.Toy |> UI.map ToyMsg
            ElfList.view model.Elf |> UI.map ElfMsg
        ]

    let app kidEvents toyEvents elfEvents toyProgress handler =
        UI.app init update view (subscription kidEvents toyEvents elfEvents toyProgress) handler