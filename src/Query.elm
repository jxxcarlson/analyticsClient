module Query exposing (runQueries,runQueriesWithString,  queries)

import Parser exposing(..)
import Analytics exposing(Event)

type QueryTerm =
  Username String
  | NotUsername String
  | Eventname String
  | NotEventname String


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

-- QUERY PARSER

queries : Parser (List QueryTerm)
queries = many query

query : Parser QueryTerm
query = oneOf [notUsername, notEventname, username, eventname]

username : Parser QueryTerm
username = succeed Username
     |. symbol "u:"
     |= identifier

notUsername : Parser QueryTerm
notUsername = succeed NotUsername
     |. symbol "-u:"
     |= identifier

eventname : Parser QueryTerm
eventname = succeed Eventname
     |. symbol "e:"
     |= identifier

notEventname : Parser QueryTerm
notEventname = succeed NotEventname
     |. symbol "-e:"
     |= identifier

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
