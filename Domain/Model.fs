namespace Lloyd.Domain.Model

open System

type 'a SetEvent =
    | Add of 'a
    | Remove of 'a

type EventID = int

type 'Aggregate ID = Created of EventID

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

type 'Aggregate LogEntry = EventID * 'Aggregate ID * 'Aggregate list 


