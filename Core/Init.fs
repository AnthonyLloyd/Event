namespace Lloyd.Core

open System
open System.Threading
open System.Diagnostics.CodeAnalysis

type Result<'o,'e> =
    | Ok of 'o
    | Error of 'e

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
    let map f r = match r with | Ok x -> Ok (f x) | Error e -> Error e
    let bind binder r = match r with | Ok i -> binder i | Error e -> Error e
    let isOk r = match r with | Ok _ -> true | Error _ -> false
    let apply f x =
        match f,x with
        | Ok f, Ok v -> Ok (f v)
        | Error f, Ok _ -> Error f
        | Ok _, Error f -> Error [f]
        | Error f1, Error f2 -> Error (f2::f1)
    let ofOption f o = match o with | None -> f() |> Error | Some v -> Ok v
    let toOption r = match r with | Ok o -> Some o | Error _ -> None
    let getElse v r = match r with | Ok o -> o | Error _ -> v
    type AttemptBuilder() =
        member __.Bind(v,f) = bind f v
        member __.Return v = Ok v
        member __.ReturnFrom o = o

module String =
    let empty = String.Empty
    let nonEmpty s = if String.IsNullOrWhiteSpace s then None else s.Trim() |> Some
    let inline tryParse (s:string) =
        let mutable r = Unchecked.defaultof<_>
        if (^a : (static member TryParse: string * ^a byref -> bool) (s, &r)) then Some r else None

module Option =
    let getElse v o = match o with | Some i -> i | None -> v
    let getElseFun f o = match o with | Some i -> i | None -> f()
    let orTry a o = match o with | None -> a | _ -> o
    let cons x xs = match x with | None -> xs | Some x -> x::xs
    type OptionBuilder() =
        member __.Bind(v,f) = Option.bind f v
        member __.Return v = Some v
        member __.ReturnFrom o = o
        member __.Zero() = None

module Seq =
    let groupByFst s = Seq.groupBy fst s |> Seq.map (fun (k,l) -> k, Seq.map snd l)

module List =
    let removei i l =
        let s1,s2 =List.splitAt i l
        s1 @ List.tail s2
    let replacei i a l =
        let s1,s2 =List.splitAt i l
        s1 @ a::List.tail s2

module Map =
    let incr m k = Map.add k (Map.tryFind k m |> Option.getElse 0 |> (+)1) m
    let decr m k = Map.add k (Map.tryFind k m |> Option.getElse 0 |> (+) -1) m
    let addOrRemove k o m = match o with | Some v -> Map.add k v m | None -> Map.remove k m
    let revisions before after =
        let after = Map.fold (fun after k _ -> if Map.containsKey k after then after else Map.add k 0 after) after before
        Map.filter (fun k v -> Map.tryFind k before |> Option.getElse (LanguagePrimitives.GenericZero) <> v) after
    let choose chooser map =
        Map.filter (fun k v -> chooser k v |> Option.isSome) map
        |> Map.map (fun k v -> chooser k v |> Option.get)


[<SuppressMessage("NameConventions","TypeNamesMustBePascalCase")>]
type 'a list1 = private NonEmptyList of 'a list

module List1 =
    let head (NonEmptyList l) = List.head l
    let tail (NonEmptyList l) = List.tail l
    let init x xs = NonEmptyList(x::xs)
    let tryOfList l = match l with | [] -> None | x::xs -> init x xs |> Some
    let toList (NonEmptyList l) = l
    let singleton s = NonEmptyList [s]
    let map mapper (NonEmptyList l) = List.map mapper l |> NonEmptyList
    let sort (NonEmptyList l) = List.sort l |> NonEmptyList
    let fold folder state (NonEmptyList list) = List.fold folder state list
    let tryPick chooser (NonEmptyList list) = List.tryPick chooser list
    let tryChoose chooser (NonEmptyList list) =
        match List.choose chooser list with | [] -> None | l -> NonEmptyList l |> Some
    let tryCollect mapping (NonEmptyList list) =
        List.choose mapping list |> List.collect toList |> tryOfList

module Observable =

    let headAsync (o:IObservable<_>) =
        async {
            let wait = new System.Threading.ManualResetEventSlim()
            let mutable v = Unchecked.defaultof<_>
            let d = Observable.subscribe (fun i -> v<-i; wait.Set()) o
            let! _ = Async.AwaitWaitHandle wait.WaitHandle
            d.Dispose()
            return v
        }

    [<NoComparison>]
    type private 'a CacheLastMsg =
        | Add of IObserver<'a>
        | Remove of IObserver<'a>
        | OnNext of 'a
        | OnError of exn
        | OnCompleted
        | Raise
 
    let cacheLast (o:IObservable<_>) =
        let mbox =
            MailboxProcessor.Start(fun inbox ->
                let rec loop observers disposable a raiseCount = async {
                    let! msg = inbox.Receive()
                    let observers,disposable,a,raiseCount =
                        match msg with
                        | Add ob ->
                            
                            let disposable =
                                match disposable with
                                |None ->
                                    { new IObserver<_> with
                                        member __.OnCompleted() = inbox.Post OnCompleted
                                        member __.OnError(e) = OnError e |> inbox.Post
                                        member __.OnNext(v) = OnNext v |> inbox.Post
                                    } |> o.Subscribe |> Some
                                |some -> some
                            Option.iter ob.OnNext a
                            ob::observers, disposable, a, raiseCount
                        | Remove ob ->
                            let observers = List.filter ((<>)ob) observers
                            let disposable = if List.isEmpty observers then disposable |> Option.iter (fun d -> d.Dispose()); None else disposable
                            observers, disposable, a, raiseCount
                        | OnNext a ->
                            inbox.Post Raise
                            observers, disposable, Some a, raiseCount+1
                        | OnError e ->
                            observers |> List.iter (fun ob -> ob.OnError e)
                            observers, disposable, a, raiseCount
                        | OnCompleted ->
                            observers |> List.iter (fun ob -> ob.OnCompleted())
                            observers, disposable, a, raiseCount
                        | Raise ->
                            let a =
                                if raiseCount=1 then
                                    let a = Option.get a
                                    observers |> List.iter (fun ob -> ob.OnNext a)
                                    None
                                else a
                            observers, disposable, a, raiseCount-1
                    return! loop observers disposable a raiseCount
                }
                loop List.empty None None 0
                )
        { new IObservable<_> with
            member __.Subscribe(ob) =
                Add ob |> mbox.Post
                {new IDisposable with member __.Dispose() = Remove ob |> mbox.Post }
        }

[<AutoOpen>]
module Common =
    let (<*>) = Result.apply
    let attempt = Result.AttemptBuilder()
    let maybe = Option.OptionBuilder()
    let rec atomicUpdate update state =
        let oldState = !state
        let newState = update oldState
        if Interlocked.CompareExchange<_>(state, newState, oldState) |> LanguagePrimitives.PhysicalEquality oldState then oldState,newState
        else atomicUpdate update state
    let rec atomicUpdateQuery update state =
        let oldState = !state
        let newState,result = update oldState
        if Interlocked.CompareExchange<_>(state, newState, oldState) |> LanguagePrimitives.PhysicalEquality oldState then newState,result
        else atomicUpdateQuery update state
    let deferred f =
        let nextTime = ref None
        let rec check() = async {
            atomicUpdate (Option.bind (fun (next,a) ->
                let now = DateTime.UtcNow
                if next>now then
                    async {
                        do! Async.Sleep(let ts=next-now in int ts.Milliseconds)
                        do! check()
                    } |> Async.Start
                    Some(next,a)
                else
                    async { f a } |> Async.Start
                    None
            )) nextTime |> ignore
        }
        fun t a ->
            let oldNextTime,_ = atomicUpdate (fun _ -> let utc = DateTime.UtcNow in Some(utc.Add(t),a)) nextTime
            if Option.isNone oldNextTime then check() |> Async.Start
    let memoize f =
        let d = Collections.Generic.Dictionary(HashIdentity.Structural)
        fun a ->
            let t,b = d.TryGetValue a
            if t then b
            else let b = f a in d.Add(a,b); b
    let repeat action (timeSpan:TimeSpan) =
        let cancel = new CancellationTokenSource()
        let rec runLoop() = async {
            do action()
            do! Async.Sleep (int timeSpan.TotalMilliseconds)
            do! runLoop()
        }
        Async.Start (runLoop(), cancel.Token)
        {new IDisposable with member __.Dispose() = cancel.Cancel() }
    let inline flip f b a = f a b
    let inline between a b x = (a<=x&&x<=b) || (a>=x&&x>=b)
    let inline mapFst f (a,b) = f a,b
    let inline mapSnd f (a,b) = a,f b
    let inline addFst a b = a,b
    let inline addSnd a b = b,a
    
