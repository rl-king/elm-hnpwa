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
    | User String
    | NotFound


type alias RouteData =
    { title : String
    , url : String
    , api : String
    }


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Top top
        , Url.map New (s "new")
        , Url.map Ask (s "ask")
        , Url.map Show (s "show")
        , Url.map Jobs (s "jobs")
        , Url.map ItemRoute (s "item" </> int)
        , Url.map User (s "user" </> string)
        ]


toTitle : Route -> String
toTitle =
    toRouteData >> .title


toUrl : Route -> String
toUrl =
    toRouteData >> .url


toApi : Route -> String
toApi =
    toRouteData >> .api


toRouteData : Route -> RouteData
toRouteData route =
    case route of
        Top ->
            RouteData "Top" "/" "news"

        New ->
            RouteData "New" "/new" "newest"

        Ask ->
            RouteData "Ask" "/ask" "ask"

        Show ->
            RouteData "Show" "/show" "show"

        Jobs ->
            RouteData "Jobs" "/jobs" "jobs"

        ItemRoute x ->
            RouteData "Item" ("/item/" ++ toString x) "item"

        User x ->
            RouteData "User" ("/user/" ++ x) "user"

        NotFound ->
            RouteData "404" "/404" "404"


parseLocation : Location -> Route
parseLocation =
    Url.parsePath route >> Maybe.withDefault NotFound
