namespace Lloyd.Domain.Model

open System
open Lloyd.Core

module Procs =

    let santaRun _kidObservable _toyObservable _elfObservable _toyProgressObservable =

        let user = User.login "santa"


        user


    let kidsRun _kidObservable _toyObservable _elfObservable _toyProgressObservable =

        let user = User.login "kids"


        user