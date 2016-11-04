namespace Lloyd.Core.UI

open Lloyd.Core

/// Message event used on the primative UI components.
type 'msg Event = ('msg->unit) ref ref

/// Layout for a section of UI components.
type Layout = Horizontal | Vertical

/// Primative UI components.
[<NoEquality;NoComparison>]
type UI =
    | Text of string
    | Input of string * string Event
    | Button of string * unit Event
    | Div of Layout * UI list

/// UI component update and event redirection.
[<NoEquality;NoComparison>]
type UIUpdate =
    | InsertUI of int list * UI
    | UpdateUI of int list * UI
    | ReplaceUI of int list * UI
    | RemoveUI of int list
    | EventUI of (unit->unit)

/// UI component including a message event.
[<NoEquality;NoComparison>]
type 'msg UI = {UI:UI;mutable Event:'msg->unit}

/// UI application.
[<NoEquality;NoComparison>]
type App<'msg,'model,'sub,'cmd when 'sub : comparison> =
    {
        Init: unit -> 'model*'cmd
        Update: 'msg -> 'model->'model*'cmd
        View: 'model -> 'msg UI
        Subscription: 'model -> Set<'sub>
    }

/// Native UI interface.
type INativeUI =
    abstract member Send : UIUpdate list -> unit

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UI =
    /// Memoize view generation from model object references.
    let memoize<'model ,'msg  when 'model : not struct and 'msg : not struct> =
        let d = System.Runtime.CompilerServices.ConditionalWeakTable<'model,'msg UI>()
        fun view model ->
            match d.TryGetValue model with
            |true,ui -> ui
            |false,_ ->
                let ui = view model
                d.Add(model,ui)
                ui

    /// Returns a Text display UI component.
    let text text = {UI=Text text;Event=ignore}
    
    /// Returns a text Input UI component.
    let input text =
        let ev = ref ignore |> ref
        let ui = {UI=Input(Option.toObj text,ev);Event=ignore}
        let raise a = ui.Event <| if System.String.IsNullOrWhiteSpace a then None else Some a
        (!ev):=raise
        ui

    let inputUInt16 (i:uint16 option) =
        let ev = ref ignore |> ref
        let ui = {UI=Input(Option.map string i |> Option.toObj,ev);Event=ignore}
        let raise a = ui.Event <| match System.UInt16.TryParse a with | true,v -> Some v | _ -> None
        (!ev):=raise
        ui

    /// Returns a Button UI component.
    let button text msg =
        let ev = ref ignore |> ref
        let ui = {UI=Button(text,ev);Event=ignore}
        (!ev):=fun () -> ui.Event msg
        ui

    /// Returns a section of UI components given a layout.
    /// The name div comes from HTML and represents a division (or section) of UI components.
    let div layout list =
        let ui = {UI=Div(layout,List.map (fun ui -> ui.UI) list);Event=ignore}
        let raise a = ui.Event a
        List.iter (fun i -> i.Event<-raise) list
        ui
    
    /// Returns a new UI component mapping the message event using the given function.
    let rec map f ui =
        let ui2 = {UI=ui.UI;Event=ignore}
        let raise e = f e |> ui2.Event
        ui.Event<-raise
        ui2

    /// Returns a list of UI updates from two UI components.
    let diff ui1 ui2 =
        let inline update e1 e2 = fun () -> let ev = !e1 in ev:=!(!e2); e2:=ev
        let rec diff ui1 ui2 path index diffs =
            match ui1,ui2 with
            | _,_ when LanguagePrimitives.PhysicalEquality ui1 ui2 -> diffs
            |Text t1,Text t2 -> if t1=t2 then diffs else UpdateUI(path,ui2)::diffs
            |Button (t1,e1),Button (t2,e2) -> if t1=t2 then EventUI(update e1 e2)::diffs else EventUI(update e1 e2)::UpdateUI(path,ui2)::diffs
            |Input (t1,e1),Input (t2,e2) -> if t1=t2 then EventUI(update e1 e2)::diffs else EventUI(update e1 e2)::UpdateUI(path,ui2)::diffs
            |Button _,Button _ |Input _,Input _ -> UpdateUI(path,ui2)::diffs
            |Div (l1,_),Div (l2,_) when l1<>l2 -> ReplaceUI(index::path,ui2)::diffs
            |Div (_,[]),Div (_,[]) -> diffs
            |Div (_,[]),Div (_,l) -> List.fold (fun (i,diffs) ui -> i+1,InsertUI(i::path,ui)::diffs) (index,diffs) l |> snd
            |Div (_,l),Div (_,[]) -> List.fold (fun (i,diffs) _ -> i+1,RemoveUI(i::path)::diffs) (index,diffs) l |> snd
            |Div (l,(h1::t1)),Div (_,(h2::t2)) when LanguagePrimitives.PhysicalEquality h1 h2 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) diffs
            |Div (l,(h1::t1)),Div (_,(h2::h3::t2)) when LanguagePrimitives.PhysicalEquality h1 h3 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) (InsertUI(index::path,h2)::diffs)
            |Div (l,(_::h2::t1)),Div (_,(h3::t2)) when LanguagePrimitives.PhysicalEquality h2 h3 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) (RemoveUI(index::path)::diffs)
            |Div (l,(h1::t1)),Div (_,(h2::t2)) -> diff h1 h2 (index::path) 0 diffs |> diff (Div(l,t1)) (Div(l,t2)) path (index+1)
            |_,_ -> ReplaceUI(index::path,ui2)::diffs
        diff ui1.UI ui2.UI [] 0 []

    /// Returns a UI application from a UI init, update and view.
    let appSimple init update view = {Init=(fun () ->init(),());Update=(fun msg model -> update msg model,());View=view;Subscription=(fun _ -> Set.empty)}

    let appFull init update view subscription = {Init=init;Update=update;View=view;Subscription=subscription}

    let private remapEvents l = List.iter (function |EventUI f -> f() |_-> ()) l

    /// Runs a UI application given a native UI.
    let run (nativeUI:INativeUI) app subscriptionHandler commandHandler =
        MailboxProcessor.Start(fun mb ->
            let rec loop model ui subs =
                async {
                    let! msg = mb.Receive()
                    let model,cmd = app.Update msg model
                    let subs = Map.updateFromKeys (subscriptionHandler >> Observable.subscribe mb.Post) (fun d -> d.Dispose()) (app.Subscription model) subs
                    Option.iter commandHandler cmd
                    let newUI = app.View model
                    newUI.Event<-mb.Post
                    let diff = diff ui newUI
                    remapEvents diff
                    nativeUI.Send diff
                    return! loop model newUI subs
                }
            let model,cmd = app.Init()
            let subs = app.Subscription model |> Seq.map  (fun s -> s,subscriptionHandler s |> Observable.subscribe mb.Post) |> Map.ofSeq
            Option.iter commandHandler cmd
            let ui = app.View model
            ui.Event<-mb.Post
            nativeUI.Send [InsertUI([],ui.UI)]
            loop model ui subs
        ) |> ignore