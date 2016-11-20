module Lloyd.WPF.NativeUI.WPF

open System
open System.Windows
open System.Windows.Controls
open Lloyd.Core
open Lloyd.Core.UI

let CreateNaiveUI (root:ContentControl) =

    let rec createUI ui : UIElement =
        match ui with
        | Text (style,text) ->
            let c = Label(Content=text)
            List.iter (function
                | TextStyle.Bold -> c.FontWeight <- FontWeights.Bold
                | TextStyle.Width w -> c.Width <- float w
                ) style
            upcast c
        | Input (style,text,event) ->
            let c = TextBox(Text=text)
            List.iter (function
                | InputStyle.AnyText -> ()
                | InputStyle.Digits -> c.PreviewTextInput.Add(fun e -> e.Handled<-Seq.forall Char.IsDigit e.Text |> not)
                | InputStyle.Width w -> c.Width <- float w
                ) style
            let event = !event
            c.TextChanged.Add(fun _ -> !event c.Text)
            upcast c
        | Select (style,options,current,event) ->
            let c = ComboBox(ItemsSource=options)
            List.iter (function
                | SelectStyle.Width w -> c.Width <- float w
                ) style
            Option.iter (fun i -> c.SelectedIndex <- i) current
            let event = !event
            c.SelectionChanged.Add(fun _ -> !event <| match c.SelectedIndex with | -1 -> None |i -> Some i)
            upcast c
        | Button (style,text,event) ->
            let c = Controls.Button(Content=text)
            List.iter (function
                | ButtonStyle.Disabled -> c.IsEnabled <- false
                | ButtonStyle.Width w -> c.Width <- float w
                ) style
            let event = !event
            c.Click.Add(fun _ -> !event ())
            upcast c
        | Div (style,list) ->
            let children = List.map createUI list
            let c = StackPanel()
            List.iter (function
                | DivStyle.Vertical -> c.Orientation <- Orientation.Vertical
                | DivStyle.Horizontal -> c.Orientation <- Orientation.Horizontal
                | DivStyle.Width w -> c.Width <- float w
                ) style
            List.iter (c.Children.Add>>ignore) children
            upcast c

    let updateTextBox =
        let update (tb:TextBox) t = root.Dispatcher.Invoke (fun () -> if tb.Text<>t then tb.Text<-t)
        fun tb -> deferred (update tb) (TimeSpan.FromMilliseconds 200.0)
        |> memoize |> flip

    let updateUI ui (element:UIElement) =
        match ui with
        | Text (_,text) -> (element :?> Label).Content <- text
        | Input (_,text,_) -> element :?> TextBox |> updateTextBox text
        | Select (_,options,current,_) ->
            let c = element :?> ComboBox
            if List.toSeq options <> Seq.cast c.ItemsSource then c.ItemsSource <- options
            c.SelectedIndex <- match current with |None -> -1 | Some i -> i
        | Button (_,text,_) -> (element :?> Button).Content <- text
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