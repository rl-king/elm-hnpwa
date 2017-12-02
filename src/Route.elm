module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Feeds Feed (Maybe String)
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
    , pagination : Maybe Int
    }


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map (Feeds Top) (top <?> stringParam "page")
        , Url.map (Feeds New) (s "new" <?> stringParam "page")
        , Url.map (Feeds Ask) (s "ask" <?> stringParam "page")
        , Url.map (Feeds Show) (s "show" <?> stringParam "page")
        , Url.map (Feeds Jobs) (s "jobs" <?> stringParam "page")
        , Url.map Item (s "item" </> int)
        , Url.map User (s "user" </> string)
        ]


toTitle : Route -> String
toTitle =
    toRouteData >> .title


toUrl : Route -> String
toUrl =
    toRouteData >> .url


toMsg : Route -> (String -> msg) -> msg
toMsg route msg =
    msg (toUrl route)


toApi : Route -> String
toApi =
    toRouteData >> .api


toFeedPage : Route -> String
toFeedPage route =
    case route of
        Feeds _ (Just page) ->
            page

        _ ->
            ""


toPagination : Route -> Maybe Int
toPagination =
    toRouteData >> .pagination


mapFeeds : (String -> String) -> Route -> Route
mapFeeds fn route =
    case route of
        Feeds feed page ->
            Feeds feed (Maybe.map fn page)

        _ ->
            route


toRouteData : Route -> RouteData
toRouteData route =
    case route of
        Feeds feed param ->
            Maybe.withDefault (toFeedData feed "1") (Maybe.map (toFeedData feed) param)

        Item x ->
            RouteData "Item" ("/item/" ++ toString x) "item" Nothing

        User x ->
            RouteData "User" ("/user/" ++ x) "user" Nothing

        NotFound ->
            RouteData "404" "/404" "404" Nothing


toFeedData : Feed -> String -> RouteData
toFeedData feed page =
    case feed of
        Top ->
            RouteData "Top" ("/?page=" ++ page) ("news.json?page=" ++ page) (Just 10)

        New ->
            RouteData "New" ("/new?page=" ++ page) ("newest.json?page=" ++ page) (Just 12)

        Ask ->
            RouteData "Ask" ("/ask?page=" ++ page) ("ask.json?page=" ++ page) (Just 3)

        Show ->
            RouteData "Show" ("/show?page=" ++ page) ("show.json?page=" ++ page) (Just 2)

        Jobs ->
            RouteData "Jobs" ("/jobs?page=" ++ page) ("jobs.json?page=" ++ page) Nothing


parse : Location -> Route
parse =
    Url.parsePath route >> Maybe.withDefault NotFound
