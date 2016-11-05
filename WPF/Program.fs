module Lloyd.WPF.Program

open System
open System.Windows
open Lloyd.Domain
open Lloyd.Core.UI
open Lloyd.WPF.NativeUI

[<EntryPoint;STAThread>]
let main _ =
    let aid = ID.gen()
    let store = Store.emptyMemoryStore()
    let app = Apps.Toy.app()
    let subscription() = Store.observable store |> Observable.choose (fun (i,l) -> if i=aid then Apps.Toy.Update l |> Some else None)
    let commandHandler (lastEvent,cmd) = if List.isEmpty cmd |> not then Store.update aid cmd lastEvent store |> ignore
   
    let window = new Window()
    let nativeUI = WPF.CreateNaiveUI window
    UI.run nativeUI app subscription commandHandler
    Application().Run(window)