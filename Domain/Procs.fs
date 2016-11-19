namespace Lloyd.Domain.Model

open System
open Lloyd.Core

module Procs =
    let unitWorkTime = TimeSpan.FromSeconds 10.0
    let santaCheckTime = TimeSpan.FromSeconds 3.0
    let kidActionTime = TimeSpan.FromSeconds 2.0

    let kidsRun kidStore toyStore =
        let run() =
            let rand = Random()
            let randPick l =
                if List.isEmpty l then None
                else
                    let i = List.length l |> rand.Next
                    List.item i l |> Some
            
            maybe {
                let! kid,kidEvents = Store.getAll kidStore |> Map.toList |> randPick
                let! age = Property.get Kid.age kidEvents

                let toyAgeRange =
                    Store.getAll toyStore |> Map.choose (fun _ -> Property.get Toy.ageRange)
                    |> Map.filter (fun _ (lo,hi) -> between lo hi age)

                let wishList =
                    Property.getEvents Kid.wishList kidEvents |> SetEvent.toSet
                    |> Set.filter (fun toy ->
                            match Map.tryFind toy toyAgeRange with
                            | None -> false
                            | Some (lo,hi) -> between lo hi age
                        )

                let addOne() =
                    let availableToys = Map.toSeq toyAgeRange |> Seq.map fst |> Set.ofSeq
                    availableToys - wishList
                    |> Set.toList |> randPick |> Option.map (SetAdd >> Kid.WishList) |> Option.toList

                let removeOne() =
                    Set.toList wishList |> randPick |> Option.map (SetRemove >> Kid.WishList) |> Option.toList

                let randDo probability makeList =
                    if rand.NextDouble()>probability then []
                    else makeList()

                let changes =
                    match Set.count wishList with
                    | 0 -> addOne()
                    | 1 -> randDo 0.9 addOne @ randDo 0.1 removeOne
                    | 2 -> randDo 0.8 addOne @ randDo 0.2 removeOne
                    | 3 -> randDo 0.7 addOne @ randDo 0.3 removeOne
                    | 4 -> randDo 0.6 addOne @ randDo 0.4 removeOne
                    | 5 -> randDo 0.5 addOne @ randDo 0.5 removeOne
                    | 6 -> randDo 0.2 addOne @ randDo 0.6 removeOne
                    | _ -> removeOne()

                List1.tryOfList changes
                |> Option.iter (fun l ->
                    let user = User.login "kid"
                    let lastEvent = List.head kidEvents |> fst
                    Store.update user kid l lastEvent kidStore |> ignore
                )
            } |> ignore

        repeat run kidActionTime

    let santaRun toyStore elfStore toyProgress =
        let run() =
            let elfToyFinishTime =

                let toyWorkRequired =
                    Store.getAll toyStore
                    |> Map.choose (fun _ -> Property.get Toy.workRequired)

                let elfWorkRate =
                    Store.getAll elfStore
                    |> Map.choose (fun _ -> Property.get Elf.workRate)

                let elfMaking =
                    Store.getAll elfStore
                    |> Map.map (fun _ events ->
                        Property.getEvents Elf.making events |> List.tryHead
                        |> Option.bind (fun (e,l) -> List1.head l |> Option.map (addFst e))
                        )

                elfMaking
                |> Map.map (fun elf making ->
                    maybe {
                        let! eventID,toy = making
                        let! workRequired = Map.tryFind toy toyWorkRequired
                        let! workRate = Map.tryFind elf elfWorkRate
                        let completeTime (startTime:DateTime) (unitWorkTime:TimeSpan) workRequired workRate =
                            startTime.Ticks + unitWorkTime.Ticks * int64 workRequired / int64 workRate |> DateTime
                        return completeTime (EventID.time eventID) unitWorkTime workRequired workRate, toy
                    })

            let assignNewToy =
                let now = DateTime.UtcNow
                if Map.exists (fun _ -> function | None -> true | Some (t,_) -> t<=now) elfToyFinishTime |> not then Map.empty
                else
                    let toyNeeded =
                        let toyOutstanding = Observable.headAsync toyProgress |> Async.RunSynchronously |> Map.map (fun _ -> snd)
                        // Need to remove toys working on and recently finished
                        let toyOutstanding =
                            Map.toSeq elfToyFinishTime |> Seq.choose snd
                            |> Seq.fold (fun m (_,toy) -> match Map.tryFind toy m with | None | Some [] -> m | Some (_::t) -> Map.add toy t m) toyOutstanding

                        Map.toSeq toyOutstanding
                        |> Seq.collect (fun (toy,l) -> Seq.map (fst>>addSnd toy) l)
                        |> Seq.sort |> Seq.toList

                    let now = DateTime.UtcNow
                    let elfFinished = Map.filter (fun _ -> function | None -> true | Some (t,_) -> t<now) elfToyFinishTime

                    Map.fold (fun (assign,needed) elf lastToy ->
                        match needed with 
                        | [] -> Map.add elf (Option.map snd lastToy,None) assign, []
                        | (_,h)::t -> Map.add elf (Option.map snd lastToy,Some h) assign, t
                    ) (Map.empty,toyNeeded) elfFinished
                    |> fst
        
            if Map.isEmpty assignNewToy |> not then
                let santa = User.login "santa"
                Map.iter (fun elf (_,newToy) ->
                        let l = List1.singleton (Elf.Making newToy)
                        Store.update santa elf l EventID.Zero elfStore |> ignore // TODO: Need to make making not concurrent
                    ) assignNewToy

        repeat run santaCheckTime
