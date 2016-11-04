namespace Lloyd.Core

open System.Threading

type Result<'o,'e> =
    | Ok of 'o
    | Error of 'e

[<AutoOpen>][<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
    let map f r = match r with | Ok x -> Ok (f x) | Error e -> Error e
    let bind binder r = match r with |Ok i -> binder i |Error e -> Error e
    let apply f x =
        match f,x with
        | Ok f, Ok v -> Ok (f v)
        | Error f, Ok _ -> Error f
        | Ok _, Error f -> Error [f]
        | Error f1, Error f2 -> Error (f2::f1)
    let (<*>) = apply
    let ofOption format =
        let sb = System.Text.StringBuilder()
        Printf.kbprintf (fun ()->function |Some x -> Ok x |None -> sb.ToString() |> Error) sb format
    type AttemptBuilder() =
        member __.Bind(v,f) = bind f v
        member __.Return v = Ok v
        member __.ReturnFrom o = o
    let attempt = AttemptBuilder()

[<AutoOpen>]
module Threading =
    let atomicUpdate state f =
        let rec update() =
            let o = !state
            let o' = f o
            if Interlocked.CompareExchange<_>(state, o', o) |> LanguagePrimitives.PhysicalEquality o then o'
            else update()
        update()
    let atomicUpdateWithQuery state f =
        let rec update() =
            let o = !state
            let o',result = f o
            if Interlocked.CompareExchange<_>(state, o', o) |> LanguagePrimitives.PhysicalEquality o then o',result
            else update()
        update()

module Option =
    let getElse v o = match o with | Some i -> i | None -> v
    let orTry a o = match o with | None -> a | some -> some


module List =
    let tryCons o xs = match o with |None -> xs | Some x -> x::xs

module Map =
    let updateFromKeys create onRemove keys existing =
        let eSet = (Set.toSeq keys).GetEnumerator()
        let eMap = (Map.toSeq existing).GetEnumerator()
        let rec addRest map =
            let k = eSet.Current
            let map = Map.add k (create k) map
            if eSet.MoveNext() |> not then map
            else addRest map
        let rec removeRest map =
            let k,v = eMap.Current
            let map = Map.remove k map
            onRemove v
            if eMap.MoveNext() |> not then map
            else removeRest map
        let rec loop map =
            match eSet.MoveNext(),eMap.MoveNext() with
            | false,false -> map
            | false,true -> removeRest map
            | true,false -> addRest map
            | true,true ->
                let rec moveEqual map =
                    let kM,vM = eMap.Current
                    let kS = eSet.Current
                    if kS=kM then false,map
                    elif kS<kM then
                        let map = Map.add kS (create kS) map
                        if eSet.MoveNext() |> not then true,removeRest map
                        else moveEqual map
                    else
                        let map = Map.remove kM map
                        onRemove vM
                        if eMap.MoveNext() |> not then true,addRest map
                        else moveEqual map
                let finished,map = moveEqual map
                if finished then map else loop map
        loop existing
