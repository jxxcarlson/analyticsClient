module Utility exposing (..)

import Html
import Json.Decode as D
import Html.Events exposing (keyCode, on)

onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (keyCode |> D.andThen isEnter)

