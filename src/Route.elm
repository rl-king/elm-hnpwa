module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Top
    | New
    | Ask
    | Show
    | Jobs
    | Item Int


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Top top
        , Url.map New (s "new")
        , Url.map Ask (s "ask")
        , Url.map Show (s "show")
        , Url.map Jobs (s "jobs")
        , Url.map Item (s "item" </> int)
        ]


toString : Route -> String
toString route =
    case route of
        Top ->
            "Top"

        New ->
            "New"

        Ask ->
            "Ask"

        Show ->
            "Show"

        Jobs ->
            "Jobs"

        Item x ->
            "Item"


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault Top
