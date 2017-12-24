module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (..)


type Route
    = Root
    | Feeds Feed (Maybe Int)
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


parse : Location -> Route
parse =
    Url.parsePath route >> Maybe.withDefault NotFound


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map (Feeds Top) (top <?> intParam "page")
        , Url.map (Feeds Top) (s "top" <?> intParam "page")
        , Url.map (Feeds New) (s "new" <?> intParam "page")
        , Url.map (Feeds Ask) (s "ask" <?> intParam "page")
        , Url.map (Feeds Show) (s "show" <?> intParam "page")
        , Url.map (Feeds Jobs) (s "jobs" <?> intParam "page")
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


toPagination : Route -> Maybe Int
toPagination =
    toRouteData >> .pagination


toFeedPage : Route -> Int
toFeedPage route =
    case route of
        Feeds _ (Just page) ->
            page

        Feeds _ Nothing ->
            1

        _ ->
            0


mapFeedPage : (Int -> Int) -> Route -> Route
mapFeedPage fn route =
    case route of
        Feeds feed page ->
            Feeds feed (Maybe.map fn page)

        _ ->
            route


toNext : Route -> Maybe Route
toNext route =
    case ( toPagination route, route ) of
        ( Just max, Feeds feed (Just page) ) ->
            if page < max then
                Just (mapFeedPage ((+) 1) route)
            else
                Nothing

        ( _, Feeds feed Nothing ) ->
            Just (Feeds feed (Just 2))

        _ ->
            Nothing


toPrevious : Route -> Maybe Route
toPrevious route =
    case route of
        Feeds feed (Just page) ->
            if page > 1 then
                Just (mapFeedPage (flip (-) 1) route)
            else
                Nothing

        _ ->
            Nothing


toRouteData : Route -> RouteData
toRouteData route =
    case route of
        Root ->
            RouteData "Top" "/" "news.json" Nothing

        Feeds feed param ->
            Maybe.withDefault (toFeedData feed 1) (Maybe.map (toFeedData feed) param)

        Item x ->
            RouteData "Item" ("/item/" ++ toString x) "item" Nothing

        User x ->
            RouteData "User" ("/user/" ++ x) "user" Nothing

        NotFound ->
            RouteData "404" "/404" "404" Nothing


toFeedData : Feed -> Int -> RouteData
toFeedData feed page =
    case feed of
        Top ->
            RouteData "Top" ("/top?page=" ++ toString page) ("news.json?page=" ++ toString page) (Just 10)

        New ->
            RouteData "New" ("/new?page=" ++ toString page) ("newest.json?page=" ++ toString page) (Just 12)

        Ask ->
            RouteData "Ask" ("/ask?page=" ++ toString page) ("ask.json?page=" ++ toString page) (Just 3)

        Show ->
            RouteData "Show" ("/show?page=" ++ toString page) ("show.json?page=" ++ toString page) (Just 2)

        Jobs ->
            RouteData "Jobs" ("/jobs?page=" ++ toString page) ("jobs.json?page=" ++ toString page) Nothing
