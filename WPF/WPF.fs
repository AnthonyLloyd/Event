module Lloyd.WPF.NativeUI.WPF

open System
open System.Windows
open System.Windows.Controls
open Lloyd.Core.UI

let CreateNaiveUI (root:ContentControl) =

    let rec createUI ui : UIElement =
        match ui with
        | Text text ->
            let c = Label(Content=text)
            upcast c
        | Input (text,event) ->
            let c = TextBox(Text=text)
            let event = !event
            //c.PreviewKeyDown.Add(fun x -> x.)
            c.TextChanged.Add(fun _ -> !event c.Text)
            upcast c
        | Select (options,current,event) ->
            let c = ComboBox(ItemsSource=options)
            Option.iter (fun i -> c.SelectedIndex <- i) current
            let event = !event
            c.SelectionChanged.Add(fun _ -> !event <| match c.SelectedIndex with | -1 -> None |i -> Some i)
            upcast c
        | Button (text,event) ->
            let c = Controls.Button(Content=text)
            let event = !event
            c.Click.Add(fun _ -> !event ())
            upcast c
        | Div (layout,list) ->
            let children = List.map createUI list
            let c = StackPanel(Orientation=match layout with |Vertical->Orientation.Vertical |Horizontal->Orientation.Horizontal)
            List.iter (c.Children.Add>>ignore) children
            upcast c

    let updateTextBox =
        memoize (fun (tb:TextBox) ->
            deferred (TimeSpan.FromMilliseconds 200.0) (fun t -> if tb.Text<>t then tb.Text<-t)
        )

    let updateUI ui (element:UIElement) =
        match ui with
        | Text text -> (element :?> Label).Content <- text
        | Input (text,_) -> updateTextBox (element :?> TextBox) text
        | Select (options,current,_) ->
            let c = element :?> ComboBox
            if List.toSeq options <> Seq.cast c.ItemsSource then c.ItemsSource <- options
            c.SelectedIndex <- match current with |None -> -1 | Some i -> i
        | Button (text,_) -> (element :?> Button).Content <- text
        | Div _ -> failwith "updateUI not possible for div"

    let rec locatePanel loc : Panel =
        match loc with
        |[] -> root.Content :?> _
        |i::xs -> (locatePanel xs).Children.Item i :?> _

    let uiUpdate u =
        match u with
        | InsertUI (loc,ui) ->
            match loc with
            |[] -> root.Content <- createUI ui
            |i::xs -> (locatePanel xs).Children.Insert(i,createUI ui)
        | UpdateUI (loc,ui) ->
            let uiElement = match loc with |[] -> root.Content :?> _ |i::xs -> (locatePanel xs).Children.Item i
            updateUI ui uiElement
        | ReplaceUI (loc,ui) ->
            match loc with
            |[] -> root.Content <- createUI ui
            |i::xs ->
                let c = (locatePanel xs).Children
                c.RemoveAt i
                c.Insert(i,createUI ui)
        | RemoveUI loc ->
            match loc with
            |[] -> failwith "RemoveUI not possible for the root"
            |i::xs -> (locatePanel xs).Children.RemoveAt i
        | EventUI _ -> ()

    { new INativeUI with
        member __.Send list =
            root.Dispatcher.Invoke (fun () -> List.iter uiUpdate list)
    }


