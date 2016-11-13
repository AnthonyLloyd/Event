module Lloyd.WPF.Program

open System
open System.Windows
open Lloyd.Domain
open Lloyd.Domain.Model
open Lloyd.Core.UI
open Lloyd.WPF.NativeUI

[<EntryPoint;STAThread>]
let main _ =

    let rand = Random()

    let kidStore =
        let store = Store.emptyMemoryStore()
        let randomAge() = rand.Next 17 |> byte |> Kid.Age
        let randomBehaviour() = Kid.Behaviour <| match rand.Next 3 with |0->Bad|1->Mixed|_->Good
        ["Porter Profit";"Simon Swatzell";"Harold Hamada";"Eldon Edman";"Silas Shotts";"Trent Torrez";"Kraig Knowlton";"Aaron Allender";"Evan Espino";"Heriberto Holliman";"Hugh Haro";"Newton Nagle";"Lowell Level";"Mohamed Mutter";"Douglas Delapena";"Russ River";"Boris Bertin";"Rod Ruyle";"Anthony Aguiar";"Louis Lavelle";"Francisca Fung";"Irena Ines";"Geralyn Groseclose";"Sadye Selby";"Kati Kingsley";"Nelia Nimmons";"Annita Ashbrook";"Vilma Villalobos";"Stephania Symons";"Shirely Sweitzer";"Delphia Devilbiss";"Dodie Danko";"Arvilla Alcazar";"Sherlyn Shawgo";"Terresa Tygart";"Ines Izzo";"Mirian Markert";"Sheena Slover";"Ethelene Ebinger";"Cammie Croslin"]
        |> List.iter (fun n ->
            Store.update (ID.gen()) [Kid.Name n;randomAge();randomBehaviour()] None store |> ignore
        )
        store

    let toyStore =
        let store = Store.emptyMemoryStore()
        let randomAgeRange() = Toy.AgeRange <| match rand.Next 5 with |0->0uy,4uy|1->3uy,7uy|2->6uy,10uy|3->9uy,13uy|_->12uy,16uy
        let randomWorkRequired() = rand.Next(50,150) |> uint16 |> Toy.WorkRequired
        ["Playmobil";"Smurfs";"Toy Soldier";"Transformers";"My Little Pony";"Corgi Car";"Lego";"Meccano";"Stickle Bricks";"Play-Doh";"Rainbow Loom";"Spirograph";"Lego Mindstorms";"Speak & Spell";"Ant Farm";"Dominoes";"Risk";"Mouse Trap";"Xbox";"Trivial Pursuit";"Scrabble";"Monopoly";"Mr Potato Head";"Rubik's Cube";"Jigsaw Puzzle";"Chemistry Set";"Kaleidoscope";"Magna Doodle";"Etch A Sketch";"Toy Piano";"Gyroscope";"Hula Hoop";"Yo-Yo";"Frisbee";"Whistle";"Water Gun";"Slinky";"Roller Skates";"Marbles";"Tea Set"]
        |> List.iter (fun n ->
            Store.update (ID.gen()) [Toy.Name n;randomAgeRange();randomWorkRequired()] None store |> ignore
        )
        store

    let elfStore =
        let store = Store.emptyMemoryStore()
        let randomWorkRate() = rand.Next(5,15) |> uint16 |> Elf.WorkRate
        ["Brandybutter Cuddlebubbles";"Brandysnap Frostpie";"Tiramisu Stripycane";"Sugarmouse Brandypears";"Pompom Glittertrifle";"Eggnog Ivysocks";"Sherry Twinkletrifle";"Sugarplum Gingerberry";"Clementine Starstockings";"Cinnamon Sugartree";"Bluebell Fruitsnaps";"Clove Starfig";"Figgy Icicleleaves";"Florentine Snoozybaubles";"Garland Mullingsleigh";"Merry Goldenspice";"Hazelnut Sparklefir";"Nutmeg Jinglecrackers";"Noel Chocolatetoes";"Robin Glittercrystals"]
        |> List.iter (fun n ->
            Store.update (ID.gen()) [Elf.Name n;randomWorkRate()] None store |> ignore
        )
        store
    
    let openElfEdit eid =
        let app = Apps.ElfEdit.app()
        let subscription() = Store.observable elfStore |> Observable.choose (fun (i,l) -> if i=eid then Apps.ElfEdit.Update l |> Some else None)
        let commandHandler = Option.iter (fun (lastEvent,cmd) -> if List.isEmpty cmd |> not then Store.update eid cmd lastEvent elfStore |> ignore)
   
        let window = new Window()
        let nativeUI = WPF.CreateNaiveUI window
        UI.run nativeUI app subscription commandHandler
        Application().Run(window) |> ignore

    let app = Apps.Main.app()

    let subscription() =
        Store.observable kidStore |> Observable.map Apps.Main.KidUpdate
        |> Observable.merge (Store.observable toyStore |> Observable.map Apps.Main.ToyUpdate)
        |> Observable.merge (Store.observable elfStore |> Observable.map Apps.Main.ElfUpdate)

    let commandHandler cmds = ()
   
    let window = new Window()
    let nativeUI = WPF.CreateNaiveUI window
    UI.run nativeUI app subscription commandHandler
    Application().Run(window)