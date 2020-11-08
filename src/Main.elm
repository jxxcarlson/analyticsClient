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
import Element.Lazy
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
    , eventListLength : Int
    , filteredEventList : List Event
    , zone : Time.Zone
    }


type Msg
    = NoOp
    | InputText String
    | GetData
    | GotData (Result Http.Error (List Event))
    | GotZone Time.Zone
    | DoSearch
    | Filter



type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { queryString = ""
      , output = ""
      , eventList = []
      , eventListLength = 0
      , filteredEventList = []
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
                Ok value -> ({model | eventList = value
                                , eventListLength = List.length value
                                , filteredEventList = Query.runQueriesWithString model.queryString  (List.reverse  value)
                              }
                               , Cmd.none)

        Filter ->
           ( { model | filteredEventList =
               Query.runQueriesWithString model.queryString  (List.reverse model.eventList) }, Cmd.none)

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

appWidth = 1170
appHeight = 800
lhsWidth = 700

white = (Element.rgb 1.0 1.0 1.0)

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
            , column [spacing 12] [
                getDataButton
              , row [spacing 8] [filterDataButton, inputText model]
              ]
            , outputDisplay model
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
    column [ spacing 8 ]
        [
         row [spacing 24] [
            outputDisplay_ model model.filteredEventList
          , row [spacing 24] [
                 usernameDisplay model
               , sessionDisplay model
            ]
           ]
         ]

sessionDisplay : Model -> Element msg
sessionDisplay model =
   let
     sessions = Analytics.sessionIds model.filteredEventList
   in
    column [scrollbarY
      , width (px 200)
      , height (px 520)
      , paddingXY 8 8
      , Background.color white
      , Font.size 16
      , spacing 8
      ]
      (List.map (\s -> el [] (text s)) sessions)


usernameDisplay : Model -> Element msg
usernameDisplay model =
   let
     sessions = Analytics.usernames model.filteredEventList
   in
     column [spacing 12, Background.color white, Font.size 16, paddingXY 8 8] [
        usernameHeading sessions
        , usernameDisplay_ sessions
       ]


usernameDisplay_ : (List String) -> Element msg
usernameDisplay_ sessions =
    (column [scrollbarY
      , width (px 200)
      , height (px 510)
      , paddingXY 8 8
      , Background.color white
      , Font.size 16
      , spacing 8
      ]
      (List.map (\s -> el [] (text s)) sessions))

usernameHeading sessions =
  el [fontGray 0.4] (text <| "Sessions: " ++ String.fromInt (List.length sessions))

outputDisplay_ : Model -> List Event -> Element msg
outputDisplay_ model events =
    column [ spacing 8
             , Background.color white
             , paddingXY 8 12
             , spacing 12
            , width (px lhsWidth)]
        [ el [fontGray 0.4] (text <| "Data (filtered/all): "
                         ++ String.fromInt (List.length model.filteredEventList)
                         ++ "/"
                         ++ String.fromInt (model.eventListLength))
         ,Element.Lazy.lazy2 viewEventList model.zone events ]

viewEventList: Time.Zone -> (List Event) -> Element msg
viewEventList zone eventList =
  Element.table [width (px lhsWidth), height (px 500), Font.size 14, spacing 8, scrollbarY, clipX]
    { data =  eventList
    , columns =
        [ { header = el [Font.bold] (Element.text "id")
          , width = (px 40)
          , view =
                \event ->
                    Element.text (String.fromInt event.id)
          }
        , { header = el [Font.bold] (Element.text "User")
          , width = (px 100)
          , view =
                \event ->
                    Element.text event.username
          }
        , { header = el [Font.bold] (Element.text "Session")
                  , width = (px 100)
                  , view =
                        \event ->
                            Element.text event.session
                  }
        , { header = el [Font.bold] (Element.text "Event")
                  , width = (px 200)
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
  (String.padLeft 2 '0' <| String.fromInt (Time.toDay zone time))
  ++ ", " ++
  (String.padLeft 2 '0' <| String.fromInt (Time.toHour zone time))
  ++ ":" ++
  (String.padLeft 2 '0' <|String.fromInt (Time.toMinute zone time))
  ++ ":" ++
  (String.padLeft 2 '0' <|String.fromInt (Time.toSecond zone time))


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
    Input.text [width (px (lhsWidth - 110)), Utility.onEnter DoSearch |> Element.htmlAttribute ]
        { onChange = InputText
        , text = model.queryString
        , placeholder = Nothing
        , label = Input.labelLeft[fontGray 0.9] <| el [] (text "")
        }


getDataButton : Element Msg
getDataButton =
    row [  ]
        [ Input.button buttonStyle
            { onPress = Just GetData
            , label = el [ centerX, centerY ] (text "Get data")
            }
        ]

filterDataButton : Element Msg
filterDataButton =
    row [  ]
        [ Input.button buttonStyle
            { onPress = Just Filter
            , label = el [ centerX, centerY ] (text "Filter")
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
    , width (px 100)
    , pointer
    , mouseDown [ buttonFontSize, Background.color mouseDownColor ]
    ]

buttonFontSize =
    Font.size 16

mouseDownColor =
    Element.rgb 0.7 0.1 0.1

--