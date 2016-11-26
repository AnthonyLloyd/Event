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

    type Model =
        {
            ID: Kid ID option
            Latest: Kid Events option
            Name: string Editor.Model
            Age: Age Editor.Model
            Behaviour: Behaviour Editor.Model
            WishList: Toy ID EditorSet.Model
            ToyNames: Map<Toy ID,string>
            ToyAgeRange: Map<Toy ID,Age*Age>
            SaveValidation: Result<unit,(Kid*string) list>
            SaveResponse: string
        }
        member m.Edits =
            EditorSet.edit Kid.wishList m.WishList
            |> Option.cons (Editor.edit Kid.name m.Name)
            |> Option.cons (Editor.edit Kid.age m.Age)
            |> Option.cons (Editor.edit Kid.behaviour m.Behaviour)

    let init kid =
        {
            ID = kid
            Latest = None
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
        | CreateResult of Result<Kid ID,StoreError>
        | UpdateResult of Result<EventID,StoreError>

    let update msg model =
        match msg with
        | Update l ->
            let l = match model.Latest with | None -> l | Some l2 -> Events.append l l2
            let model = {model with
                            Latest = Some l
                            Name = Editor.eventUpdate Kid.name l model.Name
                            Age = Editor.eventUpdate Kid.age l model.Age
                            Behaviour = Editor.eventUpdate Kid.behaviour l model.Behaviour
                            WishList = EditorSet.updateProperty Kid.wishList l model.WishList
                        }
            {model with SaveValidation=Property.validateEdit (Kid.view>>Result.map ignore) model.Latest model.Edits}, []
        | ToyNames m -> {model with ToyNames=m; WishList=EditorSet.update (EditorSet.Order m) model.WishList}, []
        | ToyAgeRanges m -> {model with ToyAgeRange=m}, []
        | NameMsg n ->
            let name, valid = Editor.updateAndValidate Kid.name (Kid.view>>Result.map ignore) model.Name model.Latest model.Edits n
            {model with Name=name; SaveValidation=valid; SaveResponse=String.empty}, []
        | AgeMsg a ->
            let age, valid = Editor.updateAndValidate Kid.age (Kid.view>>Result.map ignore) model.Age model.Latest model.Edits a
            {model with Age=age; SaveValidation=valid; SaveResponse=String.empty}, []
        | BehaviourMsg b ->
            let beh, valid = Editor.updateAndValidate Kid.behaviour (Kid.view>>Result.map ignore) model.Behaviour model.Latest model.Edits b
            {model with Behaviour=beh; SaveValidation=valid; SaveResponse=String.empty}, []
        | WishListMsg w -> {model with WishList=EditorSet.update w model.WishList; SaveResponse=String.empty}, []
        | Save ->
            let cmd =
                List1.tryOfList model.Edits
                |> Option.map (addSnd (model.ID, model.Latest))
                |> Option.toList
            (if List.isEmpty cmd then model else {model with SaveResponse="Saving..."}), cmd
        | CreateResult (Ok kid) -> {model with ID=Some kid; SaveResponse="Saved"}, []
        | UpdateResult (Ok _) -> {model with SaveResponse="Saved"}, []
        | CreateResult (Error StoreError.Concurrency)
        | UpdateResult (Error StoreError.Concurrency) ->
            {model with SaveResponse="Update happened during save, please retry"}, []

    type Sub =
        | Kid of Kid ID
        | ToyName
        | ToyAgeRange

    let subscription kidStore toyStore model =
        [
            ToyName, Property.fullObservable Toy.name toyStore |> Observable.map ToyNames
            ToyAgeRange, Property.fullObservable Toy.ageRange toyStore |> Observable.map ToyAgeRanges
        ]
        |> Option.cons (Option.map (fun kid -> Kid kid, Store.observable kidStore |> Observable.choose (Map.tryFind kid >> Option.map Update)) model.ID)
        |> Map.ofList

    let view model =
        let toyNames = Map.toList model.ToyAgeRange |> List.choose (fun (toy,(lo,hi)) ->
                            Option.bind (fun a -> if between lo hi a then
                                                    Map.tryFind toy model.ToyNames |> Option.map (addFst toy)
                                                  else None ) model.Age.Current)
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> Seq.map snd |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view (UI.inputDigits []) model.Age |> UI.map AgeMsg
            Editor.view (UI.select [] [Bad,"Bad";Mixed,"Mixed";Good,"Good"]) model.Behaviour |> UI.map BehaviourMsg
            EditorSet.view (fun style current -> UI.select style toyNames current) model.WishList |> UI.map WishListMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
        ]

    let app kid kidStore toyStore handler = UI.app (fun () -> init kid) update view (subscription kidStore toyStore) handler


module ToyEdit =

    type Model =
        {
            ID: Toy ID option
            Latest: Toy Events option
            Name: string Editor.Model
            AgeRange: (Age*Age) Editor.Model
            WorkRequired: uint16 Editor.Model
            SaveValidation: Result<unit,(Toy*string) list>
            SaveResponse: string
        }
        member m.Edits =
            Option.toList (Editor.edit Toy.name m.Name)
            |> Option.cons (Editor.edit Toy.ageRange m.AgeRange)
            |> Option.cons (Editor.edit Toy.workRequired m.WorkRequired)

    let init toy =
        {
            ID = toy
            Latest = None
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
        | WorkMsg of uint16 Editor.Msg
        | Save
        | CreateResult of Result<Toy ID,StoreError>
        | UpdateResult of Result<EventID,StoreError>

    let update msg model =
        match msg with
        | Update l ->
            let l = match model.Latest with | None -> l | Some l2 -> Events.append l l2
            let model = {model with
                            Latest = Some l
                            Name = Editor.eventUpdate Toy.name l model.Name
                            AgeRange = Editor.eventUpdate Toy.ageRange l model.AgeRange
                            WorkRequired = Editor.eventUpdate Toy.workRequired l model.WorkRequired
                        }
            {model with SaveValidation=Property.validateEdit (Toy.view>>Result.map ignore) model.Latest model.Edits}, []
        | NameMsg n ->
            let name, valid = Editor.updateAndValidate Toy.name (Toy.view>>Result.map ignore) model.Name model.Latest model.Edits n
            {model with Name=name; SaveValidation=valid; SaveResponse=String.empty}, []
        | AgeRangeMsg a ->
            let age, valid = Editor.updateAndValidate Toy.ageRange (Toy.view>>Result.map ignore) model.AgeRange model.Latest model.Edits a
            {model with AgeRange=age; SaveValidation=valid; SaveResponse=String.empty}, []
        | WorkMsg w ->
            let work, valid = Editor.updateAndValidate Toy.workRequired (Toy.view>>Result.map ignore) model.WorkRequired model.Latest model.Edits w
            {model with WorkRequired=work; SaveValidation=valid; SaveResponse=String.empty}, []
        | Save -> model, List1.tryOfList model.Edits
                            |> Option.map (addSnd (model.ID, model.Latest))
                            |> Option.toList
        | CreateResult (Ok toy) -> {model with ID=Some toy; SaveResponse="Saved"}, []
        | UpdateResult (Ok _) -> {model with SaveResponse="Saved"}, []
        | CreateResult (Error StoreError.Concurrency)
        | UpdateResult (Error StoreError.Concurrency) ->
            {model with SaveResponse="Update happened during save, please retry"}, []

    type Sub =
        | Toy of Toy ID

    let subscription toyStore model =
        Option.map (fun toy ->
            Toy toy, Store.observable toyStore |> Observable.choose (Map.tryFind toy >> Option.map Update)
            ) model.ID
        |> Option.toList |> Map.ofList

    let view model =
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> Seq.map snd |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view UI.inputRange model.AgeRange |> UI.map AgeRangeMsg
            Editor.view (UI.inputDigits []) model.WorkRequired |> UI.map WorkMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
        ]

    let app toy toyEvents handler =
        UI.app (fun () -> init toy) update view (subscription toyEvents) handler


module ElfEdit =

    type Model =
        {
            ID: Elf ID option
            Latest: Elf Events option
            Name: string Editor.Model
            WorkRate: Work Editor.Model
            Making: Toy ID option
            ToyNames: Map<Toy ID,string>
            SaveValidation: Result<unit,(Elf*string) list>
            SaveResponse: string
        }
        member m.Edits =
            Option.toList (Editor.edit Elf.name m.Name)
            |> Option.cons (Editor.edit Elf.workRate m.WorkRate)

    let init elf =
        {
            ID = elf
            Latest = None
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
        | CreateResult of Result<Elf ID,StoreError>
        | UpdateResult of Result<EventID,StoreError>

    let update msg model =
        match msg with
        | Update l ->
            let l = match model.Latest with | None -> l | Some l2 -> Events.append l l2
            let model = {model with
                            Latest = Some l
                            Name = Editor.eventUpdate Elf.name l model.Name
                            WorkRate = Editor.eventUpdate Elf.workRate l model.WorkRate
                            Making = Property.get Elf.making l |> Option.bind id
                        }
            {model with SaveValidation=Property.validateEdit (Elf.view>>Result.map ignore) model.Latest model.Edits}, []
        | ToyNames m -> {model with ToyNames=m}, []
        | NameMsg n ->
            let name, valid = Editor.updateAndValidate Elf.name (Elf.view>>Result.map ignore) model.Name model.Latest model.Edits n
            {model with Name=name; SaveValidation=valid; SaveResponse=String.empty}, []
        | WorkRateMsg w ->
            let work, valid = Editor.updateAndValidate Elf.workRate (Elf.view>>Result.map ignore) model.WorkRate model.Latest model.Edits w
            {model with WorkRate=work; SaveValidation=valid; SaveResponse=String.empty}, []
        | Save -> model, List1.tryOfList model.Edits
                            |> Option.map (addSnd (model.ID, model.Latest))
                            |> Option.toList
        | CreateResult (Ok toy) -> {model with ID=Some toy; SaveResponse="Saved"}, []
        | UpdateResult (Ok _) -> {model with SaveResponse="Saved"}, []
        | CreateResult (Error StoreError.Concurrency)
        | UpdateResult (Error StoreError.Concurrency) ->
            {model with SaveResponse="Update happened during save, please retry"}, []

    type Sub =
        | Elf of Elf ID
        | ToyName

    let subscription elfStore toyStore model =
        [(ToyName, Property.fullObservable Toy.name toyStore |> Observable.map ToyNames)]
        |> Option.cons (Option.map (fun elf -> Elf elf, Store.observable elfStore |> Observable.choose (Map.tryFind elf >> Option.map Update)) model.ID)
        |> Map.ofList

    let view model =
        let making = Option.bind (fun toy -> Map.tryFind toy model.ToyNames) model.Making |> Option.getElse "Nothing"
        let isEnabled = Result.isOk model.SaveValidation |> IsEnabled
        let tooltip = match model.SaveValidation with | Ok () | Error [] -> None | Error l -> List.rev l |> Seq.map snd |> String.join "\n" |> Some
        UI.div [Vertical] [
            Editor.view UI.inputText model.Name |> UI.map NameMsg
            Editor.view (UI.inputDigits []) model.WorkRate |> UI.map WorkRateMsg
            UI.div [Horizontal] [UI.button [isEnabled; Tooltip tooltip] "Save" Save; UI.text [] model.SaveResponse]
            UI.div [Horizontal] [UI.text [Bold] (Elf.making.Name+":  "); UI.text [] making]
        ]

    let app elf elfStore toyStore handler = UI.app (fun () -> init elf) update view (subscription elfStore toyStore) handler


module KidList =

    type Model = {Name:Map<Kid ID,string>; Requested:Map<Kid ID,int>; Finished:Map<Kid ID,int>}

    let init() = {Name=Map.empty; Requested=Map.empty; Finished=Map.empty}, []

    type Msg =
        | KidName of Map<Kid ID,string>
        | KidRequested of Map<Kid ID,int>
        | KidFinished of Map<Kid ID,int>
        | OpenEdit of Kid ID option

    let update msg (model:Model) =
        match msg with
        | KidName n -> {model with Name=n}, []
        | KidRequested r -> {model with Requested=r}, []
        | KidFinished f -> {model with Finished=f}, []
        | OpenEdit kid -> model, [OpenKidEdit kid]

    type Sub =
        | KidName
        | KidRequested
        | KidFinished
    
    let subscription kidStore toyProgress =
        let subs =
            Map.ofList [
                KidName, Property.fullObservable Kid.name kidStore |> Observable.map Msg.KidName
                KidRequested, Query.kidRequested toyProgress |> Observable.map Msg.KidRequested 
                KidFinished, Query.kidFinished toyProgress |> Observable.map Msg.KidFinished
            ]
        fun (_:Model) -> subs

    let view (model:Model) =
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; TextColour Red; Width 150] "Kid"
                UI.text [Bold; TextColour Red; Width 70] "Requested"
                UI.text [Bold; TextColour Red; Width 70] "Finished"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI (kid,name) =
            UI.div [Horizontal] [
                UI.text [Width 150] name
                UI.text [Width 70] (Map.tryFind kid model.Requested |> Option.getElse 0 |> string)
                UI.text [Width 70] (Map.tryFind kid model.Finished |> Option.getElse 0 |> string)
                UI.button [Width 50] "edit" (OpenEdit (Some kid))
            ]
        let kids = Map.toList model.Name |> List.sortBy snd
        header::List.map rowUI kids |> UI.div [Vertical;Width 400]

    let app kidStore toyProgress handler = UI.app init update view (subscription kidStore toyProgress) handler


module ToyList =

    type Model = {Name:Map<Toy ID,string>; Requested:Map<Toy ID,int>; Finished:Map<Toy ID,int>}

    let init() = {Name=Map.empty; Requested=Map.empty; Finished=Map.empty}, []

    type Msg =
        | ToyName of Map<Toy ID,string>
        | ToyRequested of Map<Toy ID,int>
        | ToyFinished of Map<Toy ID,int>
        | OpenEdit of Toy ID option

    let update msg (model:Model) =
        match msg with
        | ToyName n -> {model with Name=n}, []
        | ToyRequested r -> {model with Requested=r}, []
        | ToyFinished f -> {model with Finished=f}, []
        | OpenEdit toy -> model, [OpenToyEdit toy]

    type Sub =
        | ToyName
        | ToyFinished
        | ToyRequested

    let subscription toyStore toyProgress =
        let subs =
            Map.ofList [
                ToyName, Property.fullObservable Toy.name toyStore |> Observable.map Msg.ToyName
                ToyFinished, Query.toyFinished toyProgress |> Observable.map Msg.ToyFinished
                ToyRequested, Query.toyRequested toyProgress |> Observable.map Msg.ToyRequested
            ]
        fun (_:Model) -> subs

    let view (model:Model) =
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; TextColour Red; Width 150] "Toy"
                UI.text [Bold; TextColour Red; Width 70] "Requested"
                UI.text [Bold; TextColour Red; Width 70] "Finished"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI (toy,name) =
            UI.div [Horizontal] [
                UI.text [Width 150] name
                UI.text [Width 70] (Map.tryFind toy model.Requested |> Option.getElse 0 |> string)
                UI.text [Width 70] (Map.tryFind toy model.Finished |> Option.getElse 0 |> string)
                UI.button [Width 50] "edit" (OpenEdit (Some toy))
            ]
        let toys = Map.toList model.Name |> List.sortBy snd
        header::List.map rowUI toys |> UI.div [Vertical;Width 400]

    let app toyStore toyProgress handler = UI.app init update view (subscription toyStore toyProgress) handler


module ElfList =
    
    type Model = {Name:Map<Elf ID,string>; Making:Map<Elf ID,Toy ID option>; ToyNames:Map<Toy ID,string>}

    let init() = {Name=Map.empty; Making=Map.empty; ToyNames=Map.empty}, []

    type Msg =
        | ElfName of Map<Elf ID,string>
        | ElfMaking of Map<Elf ID,Toy ID option>
        | ToyNames of Map<Toy ID,string>
        | OpenEdit of Elf ID option

    let update msg (model:Model) =
        match msg with
        | ElfName n -> {model with Name=n}, []
        | ElfMaking m -> {model with Making=m}, []
        | ToyNames m -> {model with ToyNames=m}, []
        | OpenEdit elf -> model, [OpenElfEdit elf]

    type Sub =
        | ElfNames
        | ElfMaking
        | ToyName

    let subscription elfStore toyStore =
        let subs =
            Map.ofList [
                ElfNames, Property.fullObservable Elf.name elfStore |> Observable.map ElfName
                ElfMaking, Property.fullObservable Elf.making elfStore |> Observable.map Msg.ElfMaking
                ToyName, Property.fullObservable Toy.name toyStore |> Observable.map ToyNames
            ]
        fun (_:Model) -> subs

    let view model =
        let header =
            UI.div [Horizontal] [
                UI.text [Bold; TextColour Red; Width 180] "Elf"
                UI.text [Bold; TextColour Red; Width 140] "Making"
                UI.button [Width 50] "new" (OpenEdit None)
            ]
        let rowUI (elf,name) =
            let making = Map.tryFind elf model.Making
                         |> Option.bind (Option.bind (fun toy -> Map.tryFind toy model.ToyNames))
                         |> Option.getElse String.empty
            UI.div [Horizontal] [
                UI.text [Width 180] name
                UI.text [Width 140] making
                UI.button [Width 50] "edit" (OpenEdit (Some elf))
            ]
        let elfs = Map.toList model.Name |> List.sortBy snd
        header::List.map rowUI elfs |> UI.div [Vertical;Width 400]

    let app elfStore toyStore handler = UI.app init update view (subscription elfStore toyStore) handler


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
    
    let subscription kidStore toyStore elfStore toyProgress model =
        KidList.subscription kidStore toyProgress model.Kid |> Map.toSeq |> Seq.map (fun (k,v) -> KidList k,Observable.map KidMsg v)
        |> Seq.append (ToyList.subscription toyStore toyProgress model.Toy |> Map.toSeq |> Seq.map (fun (k,v) -> ToyList k,Observable.map ToyMsg v))
        |> Seq.append (ElfList.subscription elfStore toyStore model.Elf |> Map.toSeq |> Seq.map (fun (k,v) -> ElfList k,Observable.map ElfMsg v))
        |> Map.ofSeq

    let view model =
        UI.div [Horizontal] [
            KidList.view model.Kid |> UI.map KidMsg
            ToyList.view model.Toy |> UI.map ToyMsg
            ElfList.view model.Elf |> UI.map ElfMsg
        ]

    let app kidStore toyStore elfStore toyProgress handler =
        UI.app init update view (subscription kidStore toyStore elfStore toyProgress) handler