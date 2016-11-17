namespace Lloyd.Domain.Model

open Lloyd.Core

type Work = uint16
type Age = byte
type Behaviour = Good|Mixed|Bad

type Toy =
    | Name of string
    | AgeRange of lo:Age * hi:Age
    | WorkRequired of Work

type Elf =
    | Name of string
    | WorkRate of Work
    | Making of Toy ID option

type Kid =
    | Name of string
    | Age of Age
    | Behaviour of Behaviour
    | WishList of Toy ID SetEvent