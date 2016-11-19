namespace Lloyd.Domain.Model

open System
open Lloyd.Core

module Procs =

    let santaRun _kidObservable _toyObservable _elfObservable _toyProgressObservable =

//        let elfsAvailable (toyStore:Toy Store) (elfStore:Elf Store) =
//            let toys =
//                Store.getAll toyStore
//                |> Map.choose (fun _ -> Property.get Toy.workRequired)
//
//            let elfWorkRate =
//                Store.getAll elfStore
//                |> Map.choose (fun _ -> Property.get Elf.workRate)
//
//            let elfMaking =
//                Store.getAll elfStore
//                |> Map.map (fun _ events ->
//                    Property.getEvents Elf.making events |> List.tryHead
//                    |> Option.bind (fun (e,l) -> List.head l |> Option.map (fun t -> e,t))
//                )
//            
//            failwith "later"

        let user = User.login "santa"


        user


    let kidsRun _kidObservable _toyObservable _elfObservable _toyProgressObservable =

        let user = User.login "kids"


        user