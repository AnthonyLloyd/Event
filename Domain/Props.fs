namespace Lloyd.Domain.Model

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Toy =
    let name = Property.create "Name" Toy.Name (function |Toy.Name n -> Some n |_->None) None
    let ageRange = Property.create "Age Range" Toy.AgeRange (function |Toy.AgeRange (l,h) -> Some (l,h) |_->None) None
    let effort = Property.create "Effort" Toy.Effort (function |Toy.Effort c -> Some c |_->None) None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Elf =
    let name = Property.create "Name" Elf.Name (function |Elf.Name n -> Some n |_->None) None
    let rate = Property.create "Work Rate" Elf.Rate (function |Elf.Rate r -> Some r |_->None) None
    let making = Property.create "Making" Elf.Making (function |Elf.Making t -> Some t |_->None) None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Kid =
    let name = Property.create "Name" Kid.Name (function |Kid.Name n -> Some n |_->None) None
    let age = Property.create "Age" Kid.Age (function |Kid.Age a -> Some a |_->None) None
    let been = Property.create "Been" Kid.Been (function |Kid.Been b -> Some b |_->None) None
    let wishList = Property.create "Wish List" Kid.WishList (function |Kid.WishList w -> Some w |_->None) None