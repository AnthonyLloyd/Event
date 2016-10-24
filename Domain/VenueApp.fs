namespace Lloyd.Domain.UI

open Lloyd.Domain
open Lloyd.Domain.Model

module VenueApp =
    
    type Model =
        {
            ID: Venue ID option
            Name: string option
            Capacity: uint16 option
        }

    let init (id:Venue ID option) (store:Venue IStore) =
        store,{ID=id;Name=None;Capacity=None}