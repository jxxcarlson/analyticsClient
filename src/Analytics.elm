module Analytics exposing (..)

import Json.Decode as D
import Time exposing(Posix)


type alias Event = {
      id : Int 
    , username : String
    , eventname : String
    , eventtime : Posix
   }


eventListDecoder : D.Decoder (List Event)
eventListDecoder = D.list eventDecoder

eventDecoder : D.Decoder Event
eventDecoder =
  D.map4 Event
    (D.field "id" D.int)
    (D.field "username" D.string)
    (D.field "eventname" D.string)
    (D.field "eventtime" D.float |> D.map toPosix)




toPosix : Float -> Posix
toPosix t = 
  Time.millisToPosix (round (1000.0*t))