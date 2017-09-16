module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Top
    | New
    | Ask
    | Show
    | Jobs
    | ItemRoute Int


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Top top
        , Url.map New (s "new")
        , Url.map Ask (s "ask")
        , Url.map Show (s "show")
        , Url.map Jobs (s "jobs")
        , Url.map ItemRoute (s "item" </> int)
        ]


toUrlString : Route -> ( String, String )
toUrlString route =
    case route of
        Top ->
            ( "Top", "/" )

        New ->
            ( "New", "/new" )

        Ask ->
            ( "Ask", "/ask" )

        Show ->
            ( "Show", "/show" )

        Jobs ->
            ( "Jobs", "/jobs" )

        ItemRoute x ->
            ( "Item", "/item" )


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault Top
