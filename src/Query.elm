module Query exposing (..)


-- (runQueries,runQueriesWithString,  queries)

import Parser exposing(..)
import Analytics exposing(Event)
import Time exposing(Posix)
import Iso8601

type QueryTerm =
  Username String
  | NotUsername String
  | Eventname String
  | NotEventname String
  | EventDate DateRelation Posix

type alias PrimitiveDate = {month : Int, day: Int, year : Int}

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
        Eventname e -> List.filter (\ev -> String.contains e ev.eventname) evlist
        NotEventname e -> List.filter (\ev -> not <| String.contains e ev.eventname) evlist
        EventDate DRBefore t -> List.filter (\ev -> posixLessThan ev t) evlist
        EventDate DRAfter t -> List.filter (\ev -> posixGreaterThan ev t) evlist
        EventDate DRUnknown t -> evlist

posixGreaterThan : Event -> Posix -> Bool
posixGreaterThan e p = Time.posixToMillis e.eventtime > Time.posixToMillis p

posixLessThan : Event -> Posix -> Bool
posixLessThan e p = Time.posixToMillis e.eventtime < Time.posixToMillis p


-- QUERY PARSER

queries : Parser (List QueryTerm)
queries = many query

query : Parser QueryTerm
query = oneOf [notUsername, notEventname, username, eventname, eventDate]

username : Parser QueryTerm
username = succeed Username
     |. symbol "u."
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

date = 1604683832000
    |> Time.millisToPosix
    |> succeed

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
