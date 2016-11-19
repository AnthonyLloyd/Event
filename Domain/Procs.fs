namespace Lloyd.Domain.Model

open System
open Lloyd.Core

module Procs =
    
    let unitWorkDuration = let t = TimeSpan.FromSeconds(10.0) in t.Ticks

    let santaRun toyStore elfStore toyProgress =

        let elfToyFinishTime() =

            let toyCompleteTime (startTime:DateTime) workRequired workRate =
                startTime.Ticks + unitWorkDuration * int64 workRequired / int64 workRate |> DateTime

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
                    |> Option.bind (fun (e,l) -> List.head l |> Option.map (addFst e))
                    )

            elfMaking
            |> Map.map (fun elf making ->
                maybe {
                    let! eventID,toy = making
                    let! workRequired = Map.tryFind toy toyWorkRequired
                    let! workRate = Map.tryFind elf elfWorkRate
                    return toyCompleteTime (EventID.time eventID) workRequired workRate, toy
                })

        let checkAssignNewToy() =
            let elfToyFinishTime = elfToyFinishTime()
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

        let assignNewToy = checkAssignNewToy()
        
        if Map.isEmpty assignNewToy |> not then
            let santa = User.login "santa"
            Map.iter (fun elf (_,newToy) ->
                    Store.update santa elf [Elf.Making newToy] EventID.Zero elfStore |> ignore
                ) assignNewToy


    let kidsRun _kidObservable _toyObservable _elfObservable _toyProgressObservable =

        let user = User.login "kids"


        user