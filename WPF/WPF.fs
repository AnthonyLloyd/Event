module Lloyd.WPF.NativeUI.WPF

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open Lloyd.Core
open Lloyd.Core.UI

let Initialise() =
    ToolTipService.ShowOnDisabledProperty.OverrideMetadata(typeof<Controls.Button>, FrameworkPropertyMetadata true)

let CreateNaiveUI (root:ContentControl) =

    let setStyle style (e:FrameworkElement) =
        List.iter (function
            | Width w -> e.Width <- float w
            | Height h -> e.Height <- float h
            | IsEnabled b -> e.IsEnabled <- b
            | Tooltip s -> e.ToolTip <- Option.toObj s
            | _ -> ()
        ) style

    let setStyleText style (e:TextBlock) =
        List.iter (function
            | Bold -> e.FontWeight <- FontWeights.Bold
            | TextColour f -> e.Foreground <- match f with | Red -> Brushes.Red | Blue -> Brushes.Blue | Green -> Brushes.Green | Black | Default -> Brushes.Black
            | _ -> ()
        ) style
        setStyle style e

    let setStyleInput style (e:TextBox) =
        List.iter (function
            | Digits -> e.PreviewTextInput.Add(fun e -> e.Handled<-Seq.forall Char.IsDigit e.Text |> not)
            | _ -> ()
        ) style
        setStyle style e

    let setStyleDiv style (e:StackPanel) =
        List.iter (function
            | Vertical -> e.Orientation <- Orientation.Vertical
            | Horizontal -> e.Orientation <- Orientation.Horizontal
            | _ -> ()
        ) style
        setStyle style e

    let rec createUI ui : UIElement =
        match ui with
        | Text (style,text) ->
            let c = TextBlock(Text=text)
            setStyleText style c
            upcast c
        | Input (style,text,event) ->
            let c = TextBox(Text=text)
            setStyleInput style c
            let event = !event
            c.TextChanged.Add(fun _ -> !event c.Text)
            upcast c
        | Select (style,options,current,event) ->
            let c = ComboBox(ItemsSource=options)
            setStyle style c
            Option.iter (fun i -> c.SelectedIndex <- i) current
            let event = !event
            c.SelectionChanged.Add(fun _ -> !event <| match c.SelectedIndex with | -1 -> None |i -> Some i)
            upcast c
        | Button (style,text,event) ->
            let c = Controls.Button(Content=text)
            setStyle style c
            let event = !event
            c.Click.Add(fun _ -> !event ())
            upcast c
        | Div (style,list) ->
            let children = List.map createUI list
            let c = StackPanel()
            setStyleDiv style c
            List.iter (c.Children.Add>>ignore) children
            upcast c

    let updateTextBlock =
        let deferreds = System.Collections.Generic.Dictionary<_,_>()
        let reset (tb:TextBlock) c w =
            root.Dispatcher.Invoke (fun () ->
                tb.Foreground <- c
                tb.FontWeight <- w
            )
            deferreds.Remove(tb) |> ignore
        fun (tb:TextBlock) (t:string) ->
            if String.IsNullOrEmpty tb.Text |> not
            && String.IsNullOrEmpty t |> not
            && tb.Text<>t then
                match deferreds.TryGetValue tb with
                | false,_ ->
                    let c = tb.Foreground
                    let w = tb.FontWeight
                    let d = deferred (fun () -> reset tb c w) (TimeSpan.FromMilliseconds 2000.0)
                    deferreds.[tb] <- d
                    tb.Foreground <- Brushes.Green
                    tb.FontWeight <- FontWeights.Bold
                    d()
                | true,d -> d()
            tb.Text <- t

    let updateTextBox =
        let update (tb:TextBox) t = root.Dispatcher.Invoke (fun () -> if tb.Text<>t then tb.Text<-t)
        memoizeWeak (fun tb -> deferred (update tb) (TimeSpan.FromMilliseconds 200.0)) |> flip

    let updateUI ui (element:UIElement) =
        match ui with
        | Text (style,text) ->
            let c = element :?> TextBlock
            updateTextBlock c text
            setStyleText style c
        | Input (style,text,_) ->
            let c = element :?> TextBox
            updateTextBox text c
            setStyleInput style c
        | Select (style,options,current,_) ->
            let c = element :?> ComboBox
            if List.toSeq options <> Seq.cast c.ItemsSource then c.ItemsSource <- options
            c.SelectedIndex <- match current with |None -> -1 | Some i -> i
            setStyle style c
        | Button (style,text,_) ->
            let c = element :?> Button
            c.Content <- text
            setStyle style c
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