module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http
import Analytics exposing(Event)
import Time exposing(Posix, Month(..))
import Task
import Utility
import Query

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { queryString : String
    , output : String
    , eventList : List Event
    , zone : Time.Zone
    }


type Msg
    = NoOp
    | InputText String
    | GetData
    | GotData (Result Http.Error (List Event))
    | GotZone Time.Zone
    | DoSearch



type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { queryString = ""
      , output = ""
      , eventList = []
      , zone = Time.utc
      }
    , getZone
    )


subscriptions model =
    Sub.none



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | queryString = str, output = str }, Cmd.none )

        GetData ->
            ( model, getData)

        GotData result ->
            case result of
                Err _ -> (model, Cmd.none)
                Ok value -> ({model | eventList = value}, Cmd.none)

        GotZone zone ->
            ({ model | zone = zone}, Cmd.none )

        DoSearch ->
            ( model, Cmd.none)





getData : Cmd Msg
getData =
  Http.get
    { url = "https://shoobox.io/a/analytics"
    , expect = Http.expectJson GotData Analytics.eventListDecoder
    }

getZone : Cmd Msg
getZone =
  Task.perform GotZone Time.here

--
-- VIEW
--

appWidth = 700
appHeight = 800

fontGray g = Font.color (Element.rgb g g g )
bgGray g =  Background.color (Element.rgb g g g)

view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] }
      [bgGray 0.2] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px appWidth), height (px appHeight) ]
            [ title "Analytics"
            , inputText model
            , outputDisplay model
            , getDataButton
            
            ]
        ]

noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }

title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]



outputDisplay : Model -> Element msg
outputDisplay model =
    let
       filteredEventList =  Query.runQueriesWithString model.queryString  (List.reverse model.eventList)
    in
    column [ spacing 8 ]
        [ el [fontGray 0.9] (text <| "Data: " ++ String.fromInt (List.length filteredEventList))
        , outputDisplay_ model filteredEventList    ]

outputDisplay_ : Model -> List Event -> Element msg
outputDisplay_ model events =
    column [ spacing 8
             , Background.color (Element.rgb 1.0 1.0 1.0)
             , paddingXY 8 12
            , width (px appWidth)]
        [ viewEventList model.zone events ]

viewEventList: Time.Zone -> (List Event) -> Element msg
viewEventList zone eventList =
  Element.table [width (px appWidth), height (px 500), Font.size 14, spacing 8, scrollbarY, clipX]
    { data =  eventList
    , columns =
        [ { header = el [Font.bold] (Element.text "id")
          , width = (px 40)
          , view =
                \event ->
                    Element.text (String.fromInt event.id)
          }
        , { header = el [Font.bold] (Element.text "User")
          , width = (px 140)
          , view =
                \event ->
                    Element.text event.username
          }
        , { header = el [Font.bold] (Element.text "Event")
                  , width = (px 300)
                  , view =
                        \event ->
                            Element.text event.eventname
                  }
        , { header = el [Font.bold] (Element.text "Time")
                          , width = (px 200)
                          , view =
                                \event ->
                                    el [Font.size 12] (Element.text (posixToString zone event.eventtime))
                          }
        ]
    }

posixToString : Time.Zone -> Posix -> String
posixToString zone time =
    if (Time.posixToMillis time < 123456789) then "--" else posixToString_ zone time

posixToString_ : Time.Zone -> Posix -> String
posixToString_ zone time =
  monthToString (Time.toMonth zone time)
  ++ " " ++
  String.fromInt (Time.toDay zone time)
  ++ ", " ++
  String.fromInt (Time.toHour zone time)
  ++ ":" ++
  String.fromInt (Time.toMinute zone time)
  ++ ":" ++
  String.fromInt (Time.toSecond zone time)


monthToString : Month -> String
monthToString month =
    case month of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"



inputText : Model -> Element Msg
inputText model =
    Input.text [width (px appWidth), Utility.onEnter DoSearch |> Element.htmlAttribute ]
        { onChange = InputText
        , text = model.queryString
        , placeholder = Nothing
        , label = Input.labelAbove [fontGray 0.9] <| el [] (text "Filter")
        }


getDataButton : Element Msg
getDataButton =
    row [  ]
        [ Input.button buttonStyle
            { onPress = Just GetData
            , label = el [ centerX, centerY ] (text "Get data")
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (Element.rgb 0.5 0.5 1.0)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]



--