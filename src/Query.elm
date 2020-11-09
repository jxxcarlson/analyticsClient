module Query exposing (..)


-- (runQueries,runQueriesWithString,  queries)

import Parser exposing(..)
import Analytics exposing(Event)
import Time exposing(Posix)
import Iso8601

type QueryTerm =
  Username String
  | NotUsername String
  | Session String
  | NotSession String
  | Eventname String
  | NotEventname String
  | EventDate DateRelation Posix
  | EventDateWithTime DateRelation Posix

type alias PrimitiveDate = {month : Int, day: Int, year : Int}
type alias PrimitiveDateWithTime = {month : Int, day: Int, year : Int, hour: Int, minute: Int}

type DateRelation = DRBefore | DRAfter | DRUnknown

runQueriesWithString : String -> List Event -> List Event
runQueriesWithString queryString eventList =
    case run queries queryString of
        Err _ -> eventList
        Ok queryList -> runQueries queryList eventList

runQueries : List QueryTerm -> List Event -> List Event
runQueries queries_ eventList =
    List.foldl (\q evList -> filterBy q evList) eventList queries_

filterBy : QueryTerm -> List Event -> List Event
filterBy qt evlist =
    case qt of
        Username u -> List.filter (\ev -> String.contains u ev.username) evlist
        NotUsername u -> List.filter (\ev -> not <| String.contains u ev.username) evlist
        Session s -> List.filter (\ev -> String.contains s ev.session) evlist
        NotSession s -> List.filter (\ev -> not <| String.contains s ev.session) evlist
        Eventname e -> List.filter (\ev -> String.contains e ev.eventname) evlist
        NotEventname e -> List.filter (\ev -> not <| String.contains e ev.eventname) evlist
        EventDate DRBefore t -> List.filter (\ev -> posixLessThan ev t) evlist
        EventDate DRAfter t -> List.filter (\ev -> posixGreaterThan ev t) evlist
        EventDate DRUnknown t -> evlist
        EventDateWithTime DRBefore t -> List.filter (\ev -> posixLessThan ev t) evlist
        EventDateWithTime DRAfter t -> List.filter (\ev -> posixGreaterThan ev t) evlist
        EventDateWithTime DRUnknown t -> evlist


posixGreaterThan : Event -> Posix -> Bool
posixGreaterThan e p = Time.posixToMillis e.eventtime > Time.posixToMillis p

posixLessThan : Event -> Posix -> Bool
posixLessThan e p = Time.posixToMillis e.eventtime < Time.posixToMillis p


-- QUERY PARSER

queries : Parser (List QueryTerm)
queries = many query

query : Parser QueryTerm
query = oneOf [notUsername, notEventname, username, session, notsession, eventname, eventDateWithTime, eventDate]

username : Parser QueryTerm
username = succeed Username
     |. symbol "u."
     |= identifier

session : Parser QueryTerm
session = succeed Session
     |. symbol "s."
     |= identifier

notsession : Parser QueryTerm
notsession = succeed NotSession
     |. symbol "-s."
     |= identifier

notUsername : Parser QueryTerm
notUsername = succeed NotUsername
     |. symbol "-u."
     |= identifier

eventname : Parser QueryTerm
eventname = succeed Eventname
     |. symbol "e."
     |= identifier

notEventname : Parser QueryTerm
notEventname = succeed NotEventname
     |. symbol "-e."
     |= identifier

eventDate : Parser QueryTerm
eventDate = succeed EventDate
  |. symbol "d."
  |= dateRelation
  |. symbol "."
  |= (primitiveDate |> map primitiveDateToPosix)

primitiveDate : Parser PrimitiveDate
primitiveDate = succeed PrimitiveDate
   |= int
   |. symbol "/"
   |= int
   |. symbol "/"
   |= int

primitiveDateToISO8601 : PrimitiveDate -> String
primitiveDateToISO8601 pd =
    [pd.year, pd.month, pd.day]
      |> List.map (String.fromInt >> String.padLeft 2 '0' )
      |> String.join "-"
      |> (\x -> x ++ "T00:00:00.000-05:00")

primitiveDateToPosix : PrimitiveDate -> Posix
primitiveDateToPosix pd =
    case pd |> primitiveDateToISO8601 |> Iso8601.toTime of
        Ok time -> time
        Err _ -> Time.millisToPosix 0



-- WITH TIME

eventDateWithTime : Parser QueryTerm
eventDateWithTime= succeed EventDateWithTime
  |. symbol "t."
  |= dateRelation
  |. symbol "."
  |= (primitiveDateWithTime |> map primitiveDateWithTimeToPosix)

primitiveDateWithTime : Parser PrimitiveDateWithTime
primitiveDateWithTime = succeed PrimitiveDateWithTime
   |= int
   |. symbol "/"
   |= int
   |. symbol "/"
   |= int
   |. symbol ":"
   |= (timeSegment |> map segmentToInt)
   |. symbol ":"
   |= (timeSegment |> map segmentToInt)

segmentToInt : String -> Int
segmentToInt str =
   let
      normalForm = if String.left 1 str == "0"
                   then String.dropLeft 1 str
                   else str
   in
   String.toInt normalForm |> Maybe.withDefault 0

primitiveDateWithTimeToISO8601 : PrimitiveDateWithTime -> String
primitiveDateWithTimeToISO8601 pd =
   let
      f x = x |> String.fromInt |> String.padLeft 2 '0'
    in
      (f pd.year) ++ "-" ++ (f pd.month) ++ "-" ++ (f pd.day) ++ "T" ++ (f pd.hour) ++ ":" ++ (f pd.minute) ++ ":00.000-05:00"


primitiveDateWithTimeToPosix : PrimitiveDateWithTime -> Posix
primitiveDateWithTimeToPosix pd =
    case pd |> primitiveDateWithTimeToISO8601 |> Iso8601.toTime of
        Ok time -> time
        Err _ -> Time.millisToPosix 0


stringToDateRelation : String -> DateRelation
stringToDateRelation str =
    case str of
        "after" -> DRAfter
        "before" -> DRBefore
        _ -> DRUnknown

dateRelation : Parser DateRelation
dateRelation =
    segmentWithEndChar '.' |> map stringToDateRelation

segmentWithEndChar: Char -> Parser String
segmentWithEndChar e=
  getChompedString <|
    succeed ()
      |. chompWhile (\c -> c /= e)



identifier : Parser String
identifier =
  getChompedString <|
    succeed ()
      |. chompIf (\c -> c /= '-')
      |. chompWhile (\c -> c /= ' ')

timeSegment : Parser String
timeSegment =
  getChompedString <|
    succeed ()
      |. chompWhile (\c -> c /= ':')



many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
            |. spaces

        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]
