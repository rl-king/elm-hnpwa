module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Home
    | Work
    | Contact
    | Page String


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Work (s "work")
        , Url.map Contact (s "contact")
        , Url.map Page (s "work" </> string)
        ]


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            ""

        Work ->
            "work"

        Contact ->
            "contact"

        Page x ->
            x


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault Home
