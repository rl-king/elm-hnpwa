module Route exposing
    ( Feed(..)
    , Route(..)
    , mapFeedPage
    , parse
    , toApi
    , toFeedData
    , toFeedPage
    , toNext
    , toPagination
    , toPrevious
    , toTitle
    , toUrl
    )

import Url
import Url.Builder as Builder
import Url.Parser as Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        )
import Url.Parser.Query as Query


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


parse : Url.Url -> Route
parse =
    let
        parser =
            Parser.oneOf
                [ Parser.map (Feeds Top (Just 1)) Parser.top
                , Parser.map (Feeds Top) (Parser.s "top" <?> Query.int "page")
                , Parser.map (Feeds New) (Parser.s "new" <?> Query.int "page")
                , Parser.map (Feeds Ask) (Parser.s "ask" <?> Query.int "page")
                , Parser.map (Feeds Show) (Parser.s "show" <?> Query.int "page")
                , Parser.map (Feeds Jobs) (Parser.s "jobs" <?> Query.int "page")
                , Parser.map Item (Parser.s "item" </> Parser.int)
                , Parser.map User (Parser.s "user" </> Parser.string)
                ]
    in
    Parser.parse parser
        >> Maybe.withDefault NotFound


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

        _ ->
            Nothing


toPrevious : Route -> Maybe Route
toPrevious route =
    case route of
        Feeds feed (Just page) ->
            if page > 1 then
                Just (mapFeedPage (\x -> x - 1) route)

            else
                Nothing

        _ ->
            Nothing


toRouteData : Route -> RouteData
toRouteData route =
    case route of
        Root ->
            RouteData "Top" (Builder.absolute [] []) "news.json" Nothing

        Feeds feed param ->
            Maybe.withDefault (toFeedData feed 1) (Maybe.map (toFeedData feed) param)

        Item x ->
            RouteData "Item" (Builder.absolute [ "item", String.fromInt x ] []) "item" Nothing

        User x ->
            RouteData "User" (Builder.absolute [ "user", x ] []) "user" Nothing

        NotFound ->
            RouteData "404" (Builder.absolute [ "404" ] []) "404" Nothing


toFeedData : Feed -> Int -> RouteData
toFeedData feed page =
    let
        url path pageNumber =
            Builder.absolute [ path ] [ Builder.int "page" pageNumber ]

        api path pageNumber =
            Builder.relative [ "v0", path, String.fromInt pageNumber ++ ".json" ] []
    in
    case feed of
        Top ->
            RouteData "Top" (url "top" page) (api "news" page) (Just 10)

        New ->
            RouteData "New" (url "new" page) (api "newest" page) (Just 12)

        Ask ->
            RouteData "Ask" (url "ask" page) (api "ask" page) (Just 3)

        Show ->
            RouteData "Show" (url "show" page) (api "show" page) (Just 2)

        Jobs ->
            RouteData "Jobs" (url "jobs" page) (api "jobs" page) Nothing
