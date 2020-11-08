module Analytics exposing (..)

import Json.Decode as D
import Time exposing(Posix)
import List.Extra


type alias Event = {
      id : Int 
    , username : String
    , session: String
    , eventname : String
    , eventtime : Posix
   }


type alias Username = String

sessionIds: List Event -> List String
sessionIds events =
    events
      |> List.map .session
      |> List.Extra.unique
      |> List.sort

usernames: List Event -> List String
usernames events =
    events
      |> List.map .username
      |> List.Extra.unique
      |> List.sort





eventListDecoder : D.Decoder (List Event)
eventListDecoder = D.list eventDecoder

eventDecoder : D.Decoder Event
eventDecoder =
  D.map5 Event
    (D.field "id" D.int)
    (D.field "username" D.string)
    (D.field "session" D.string)
    (D.field "eventname" D.string)
    (D.field "eventtime" D.float |> D.map toPosix)




toPosix : Float -> Posix
toPosix t = 
  Time.millisToPosix (round (1000.0*t))