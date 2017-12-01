module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Feeds Feed
    | Item Int
    | User String
    | NotFound


type Feed
    = Top
    | New
    | Ask
    | Show
    | Jobs


type alias RouteData =
    { title : String
    , url : String
    , api : String
    }


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map (Feeds Top) top
        , Url.map (Feeds New) (s "new")
        , Url.map (Feeds Ask) (s "ask")
        , Url.map (Feeds Show) (s "show")
        , Url.map (Feeds Jobs) (s "jobs")
        , Url.map Item (s "item" </> int)
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
        Feeds feed ->
            toFeedData feed

        Item x ->
            RouteData "Item" ("/item/" ++ toString x) "item"

        User x ->
            RouteData "User" ("/user/" ++ x) "user"

        NotFound ->
            RouteData "404" "/404" "404"


toFeedData feed =
    case feed of
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


parse : Location -> Route
parse =
    Url.parsePath route >> Maybe.withDefault NotFound
