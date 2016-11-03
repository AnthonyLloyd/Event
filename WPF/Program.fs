module Lloyd.WPF.Program

open System
open System.Windows
open Lloyd.Domain
open Lloyd.Domain.UI
open Lloyd.WPF.NativeUI

[<EntryPoint;STAThread>]
let main _ =
    let aid = ID.gen()
    let store = MemoryStore.create() |> ref
    let app = Apps.Venue.app()
    let subscription() = MemoryStore.observable store |> Observable.choose (fun (i,l) -> if i=aid then Apps.Venue.Update l |> Some else None)
    let commandHandler (lastEvent,cmd) = if List.isEmpty cmd |> not then MemoryStore.update aid cmd lastEvent store |> ignore
   
    let window = new Window()
    let nativeUI = WPF.CreateNaiveUI window
    UI.run nativeUI app subscription commandHandler
    Application().Run(window)

