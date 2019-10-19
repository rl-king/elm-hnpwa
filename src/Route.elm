module Route exposing
    ( Feed(..)
    , Route(..)
    , fromUrl
    , mapFeedPage
    , toFeedPage
    , toMaxPages
    , toNext
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



-- DEFINITIONS


type Route
    = Root
    | Feeds Feed Int
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
    , maxPages : Maybe Int
    }



-- PARSING


fromUrl : Url.Url -> Route
fromUrl =
    let
        parser =
            Parser.oneOf
                [ Parser.map (Feeds Top 1) Parser.top
                , feedParser Top "top"
                , feedParser New "new"
                , feedParser Ask "ask"
                , feedParser Show "show"
                , feedParser Jobs "jobs"
                , Parser.map Item (Parser.s "item" </> Parser.int)
                , Parser.map User (Parser.s "user" </> Parser.string)
                ]

        feedParser route path =
            Parser.map (Feeds route << fallbackPage) <|
                (Parser.s path <?> Query.int "page")

        fallbackPage =
            Maybe.withDefault 1
    in
    Parser.parse parser
        >> Maybe.withDefault NotFound


toTitle : Route -> String
toTitle =
    toRouteData >> .title


toUrl : Route -> String
toUrl =
    toRouteData >> .url


toMaxPages : Route -> Maybe Int
toMaxPages =
    toRouteData >> .maxPages


toFeedPage : Route -> Int
toFeedPage route =
    case route of
        Feeds _ page ->
            page

        _ ->
            0


mapFeedPage : (Int -> Int) -> Route -> Route
mapFeedPage fn route =
    case route of
        Feeds feed page ->
            Feeds feed (fn page)

        _ ->
            route


toNext : Route -> Maybe Route
toNext route =
    case ( toMaxPages route, route ) of
        ( Just max, Feeds feed page ) ->
            if page < max then
                Just (mapFeedPage ((+) 1) route)

            else
                Nothing

        _ ->
            Nothing


toPrevious : Route -> Maybe Route
toPrevious route =
    case route of
        Feeds feed page ->
            if page > 1 then
                Just (mapFeedPage (\x -> x - 1) route)

            else
                Nothing

        _ ->
            Nothing


toRouteData : Route -> RouteData
toRouteData route =
    let
        url path page =
            Builder.absolute [ path ]
                [ Builder.int "page" page ]
    in
    case route of
        Root ->
            RouteData "Top" (Builder.absolute [] []) Nothing

        Feeds Top page ->
            RouteData "Top" (url "top" page) (Just 10)

        Feeds New page ->
            RouteData "New" (url "new" page) (Just 12)

        Feeds Ask page ->
            RouteData "Ask" (url "ask" page) (Just 3)

        Feeds Show page ->
            RouteData "Show" (url "show" page) (Just 2)

        Feeds Jobs page ->
            RouteData "Jobs" (url "jobs" page) Nothing

        Item x ->
            RouteData "Item" (Builder.absolute [ "item", String.fromInt x ] []) Nothing

        User x ->
            RouteData "User" (Builder.absolute [ "user", x ] []) Nothing

        NotFound ->
            RouteData "404" (Builder.absolute [ "404" ] []) Nothing
