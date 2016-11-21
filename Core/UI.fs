namespace Lloyd.Core.UI

open System
open Lloyd.Core

/// Message event used on the primative UI components.
type 'msg Event = ('msg->unit) ref ref

/// Style for a section of UI components.
type TextStyle = Bold | Width of int // TODO: Global control style
type InputStyle = AnyText | Digits | Width of int
type SelectStyle = Width of int
type ButtonStyle = Disabled | Width of int
type DivStyle = Horizontal | Vertical | Width of int

/// Primative UI components.
[<NoEquality;NoComparison>]
type UI =
    | Text of TextStyle list * string
    | Input of InputStyle list * string * string Event
    | Select of SelectStyle list * string list * int option * int option Event
    | Button of ButtonStyle list * string * unit Event
    | Div of DivStyle list * UI list

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
        Init: unit -> 'model * 'cmd list
        Update: 'msg -> 'model -> 'model * 'cmd list
        View: 'model -> 'msg UI
        Subscription: 'model -> Map<'sub,IObservable<'msg>>
        Handler: 'cmd -> 'msg option
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
            | true,ui -> ui
            | false,_ ->
                let ui = view model
                d.Add(model,ui)
                ui

    /// Returns a Text display UI component.
    let text style text = {UI=Text (style,text);Event=ignore}
    
    /// Returns a text Input UI component.
    let inputText text =
        let ev = ref ignore |> ref
        let ui = {UI=Input([AnyText],Option.toObj text,ev);Event=ignore}
        let raise a = String.nonEmpty a |> ui.Event
        (!ev):=raise
        ui

    let inline inputDigits (digits:'a option) : 'a option UI =
        let ev = ref ignore |> ref
        let ui = {UI=Input([Digits],Option.map string digits |> Option.toObj,ev);Event=ignore}
        let raise a = String.tryParse a |> ui.Event
        (!ev):=raise
        ui

    /// Returns a generic Select UI component.
    let select style options current =
        let options = List.sortBy snd options
        let ev = ref ignore |> ref
        let ui =
            let strings = List.map snd options
            let selected = Option.bind (fun c -> List.tryFindIndex (fst>>(=)c) options) current
            {UI=Select(style,strings,selected,ev);Event=ignore}
        let raise a = Option.map (fun i -> List.item i options |> fst) a |> ui.Event
        (!ev):=raise
        ui

    /// Returns a Button UI component.
    let button style text msg =
        let ev = ref ignore |> ref
        let ui = {UI=Button(style,text,ev);Event=ignore}
        (!ev):=fun () -> ui.Event msg
        ui

    /// Returns a section of UI components given a layout.
    /// The name div comes from HTML and represents a division (or section) of UI components.
    let div style list =
        let ui = {UI=Div(style,List.map (fun ui -> ui.UI) list);Event=ignore}
        let raise a = ui.Event a
        List.iter (fun i -> i.Event<-raise) list
        ui
    
    /// Returns a new UI component mapping the message event using the given function.
    let rec map f ui =
        let ui2 = {UI=ui.UI;Event=ignore}
        let raise e = f e |> ui2.Event
        ui.Event<-raise
        ui2

    let inline inputRange (range:('a*'a) option) =
        let mutable lo = Option.map fst range
        let mutable hi = Option.map snd range
        let range() = match lo,hi with | Some l,Some h -> Some (l,h) |_ -> None
        let ui =
            div [Horizontal] [
                inputDigits lo |> map Choice1Of2
                text [] " - "
                inputDigits hi |> map Choice2Of2
            ]
        let ui2 = {UI=ui.UI;Event=ignore}
        let raise e =
            let before = range()
            match e with
            | Choice1Of2 l -> lo<-l
            | Choice2Of2 h -> hi<-h
            let after = range()
            if after<>before then ui2.Event after
        ui.Event<-raise
        ui2

    /// Returns a list of UI updates from two UI components.
    let diff ui1 ui2 =
        let inline update e1 e2 = fun () -> let ev = !e1 in ev:=!(!e2); e2:=ev
        let rec diff ui1 ui2 path index diffs =
            match ui1,ui2 with
            | _,_ when LanguagePrimitives.PhysicalEquality ui1 ui2 -> diffs
            | Text (y1,t1),Text (y2,t2) -> if t1=t2 && y1=y2 then diffs else UpdateUI(path,ui2)::diffs
            | Button (s1,t1,e1),Button (s2,t2,e2) -> if s1=s2 && t1=t2 then EventUI(update e1 e2)::diffs else EventUI(update e1 e2)::UpdateUI(path,ui2)::diffs
            | Input (c1,t1,e1),Input (c2,t2,e2) -> if c1=c2 && t1=t2 then EventUI(update e1 e2)::diffs else EventUI(update e1 e2)::UpdateUI(path,ui2)::diffs
            | Select (y1,o1,s1,e1),Select (y2,o2,s2,e2) -> if y1=y2 && o1=o2 && s1=s2 then EventUI(update e1 e2)::diffs else EventUI(update e1 e2)::UpdateUI(path,ui2)::diffs
            | Div (l1,_),Div (l2,_) when l1<>l2 -> ReplaceUI(path,ui2)::diffs
            | Div (_,[]),Div (_,[]) -> diffs
            | Div (_,[]),Div (_,l) -> List.fold (fun (i,diffs) ui -> i+1,InsertUI(i::path,ui)::diffs) (index,diffs) l |> snd |> List.rev
            | Div (_,l),Div (_,[]) -> List.fold (fun (i,diffs) _ -> i+1,RemoveUI(i::path)::diffs) (index,diffs) l |> snd
            | Div (l,(h1::t1)),Div (_,(h2::t2)) when LanguagePrimitives.PhysicalEquality h1 h2 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) diffs
            | Div (l,(h1::t1)),Div (_,(h2::h3::t2)) when LanguagePrimitives.PhysicalEquality h1 h3 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) (InsertUI(index::path,h2)::diffs)
            | Div (l,(_::h2::t1)),Div (_,(h3::t2)) when LanguagePrimitives.PhysicalEquality h2 h3 -> diff (Div(l,t1)) (Div(l,t2)) path (index+1) (RemoveUI(index::path)::diffs)
            | Div (l,(h1::t1)),Div (_,(h2::t2)) -> diff h1 h2 (index::path) 0 diffs |> diff (Div(l,t1)) (Div(l,t2)) path (index+1)
            | _,_ -> ReplaceUI(path,ui2)::diffs
        diff ui1.UI ui2.UI [] 0 []

    /// Returns a UI application from a UI init, update and view.
    let appSimple init update view = {Init=(fun () ->init(),[]);Update=(fun msg model -> update msg model,[]);View=view;Subscription=(fun _ -> Map.empty);Handler=fun _ -> None}

    let app init update view subscription handler = {Init=init;Update=update;View=view;Subscription=subscription;Handler=handler}

    let private remapEvents l = List.iter (function | EventUI f -> f() | _-> ()) l

    /// Runs a UI application given a native UI.
    let run (app:App<'msg,'model,'sub,'cmd>) (nativeUI:INativeUI) =
        MailboxProcessor.Start(fun mb ->
            let rec loop model ui subs =
                async {
                    let! msg = mb.Receive()
                    let model,cmd = app.Update msg model
                    let newSubs = app.Subscription model
                    subs |> Map.iter (fun k d -> if Map.containsKey k newSubs |> not then (d:IDisposable).Dispose())
                    let subs = Map.map (fun k sub -> match Map.tryFind k subs with |Some d -> d |None-> Observable.subscribe mb.Post sub) newSubs
                    List.iter (app.Handler >> Option.iter mb.Post) cmd
                    let newUI = app.View model
                    newUI.Event<-mb.Post
                    let diff = diff ui newUI
                    remapEvents diff
                    nativeUI.Send diff
                    return! loop model newUI subs
                }
            let model,cmd = app.Init() //TODO Should this be async, can it be done in the async loop
            let subs = app.Subscription model |> Map.map (fun _ -> Observable.subscribe mb.Post)
            List.iter (app.Handler >> Option.iter mb.Post) cmd
            let ui = app.View model
            ui.Event<-mb.Post
            nativeUI.Send [InsertUI([],ui.UI)]
            loop model ui subs
        ) |> ignore //TODO Should be IDisposable?