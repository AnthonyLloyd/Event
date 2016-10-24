namespace Lloyd.Domain.UI

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
type App<'msg,'model> = {Model:'model;Update:'msg->'model->'model;View:'model->'msg UI}

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
        let ui = {UI=Input(text,ev);Event=ignore}
        let raise a = ui.Event a
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

    /// Returns a UI application from a UI model, update and view.
    let app model update view = {Model=model;Update=update;View=view}

    let private remapEvents l = List.iter (function |EventUI f -> f() |_-> ()) l
    

    /// Runs a UI application given a native UI.
    let run (nativeUI:INativeUI) app =
        let rec handle model ui msg =
            let newModel = app.Update msg model
            let newUI = app.View newModel
            newUI.Event<-handle newModel newUI
            let diff = diff ui newUI
            remapEvents diff
            nativeUI.Send diff
        let ui = app.View app.Model
        ui.Event<-handle app.Model ui
        nativeUI.Send [InsertUI([],ui.UI)]