module Lloyd.Domain.Query

open Lloyd.Domain.Model

module Venue =
    let name = Property.create "Name" Venue.Name (function |Venue.Name n -> Some n |_->None) "Missing Venue Name"
    let capacity = Property.create "Capacity" Venue.Capacity (function |Venue.Capacity c -> Some c |_->None) 0us
