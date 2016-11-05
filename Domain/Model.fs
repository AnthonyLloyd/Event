namespace Lloyd.Domain.Model

type 'a SetEvent =
    | Add of 'a
    | Remove of 'a

type Work = uint16
type Age = byte
type Behaviour = Bad|Ok|Good

type Toy =
    | Name of string
    | AgeRange of lo:Age * hi:Age
    | Effort of Work

type Elf =
    | Name of string
    | Rate of Work
    | Making of Toy ID option

type Kid =
    | Name of string
    | Age of Age
    | Been of Behaviour
    | WishList of Toy SetEvent