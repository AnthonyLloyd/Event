namespace Lloyd.Domain.Model

open System
open Lloyd.Domain

type Person =
    | Name of string

type Venue =
    | Name of string
    | Capacity of uint16

type Talk =
    | Title of string
    | Speaker of Person ID

type Meeting =
    | Name of string
    | Venue of Venue ID
    | Time of DateTime
    | Talks of Talk ID SetEvent
    | Attendees of Person ID SetEvent