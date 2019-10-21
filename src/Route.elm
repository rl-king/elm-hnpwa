module Route exposing
    ( Feed(..)
    , Route(..)
    , fromUrl
    , maxPages
    , toTitle
    , toUrl
    )

import Url
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>))
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
            Parser.map (Feeds route << Maybe.withDefault 1) <|
                (Parser.s path <?> Query.int "page")
    in
    Parser.parse parser
        >> Maybe.withDefault NotFound


toTitle : Route -> String
toTitle =
    toRouteData >> .title


toUrl : Route -> String
toUrl =
    toRouteData >> .url


toRouteData : Route -> RouteData
toRouteData route =
    let
        url path page =
            Builder.absolute [ path ] [ Builder.int "page" page ]
    in
    case route of
        Root ->
            RouteData "Top" (Builder.absolute [] [])

        Feeds Top page ->
            RouteData "Top" (url "top" page)

        Feeds New page ->
            RouteData "New" (url "new" page)

        Feeds Ask page ->
            RouteData "Ask" (url "ask" page)

        Feeds Show page ->
            RouteData "Show" (url "show" page)

        Feeds Jobs page ->
            RouteData "Jobs" (url "jobs" page)

        Item x ->
            RouteData "Item" (Builder.absolute [ "item", String.fromInt x ] [])

        User x ->
            RouteData "User" (Builder.absolute [ "user", x ] [])

        NotFound ->
            RouteData "404" (Builder.absolute [ "404" ] [])


maxPages : Feed -> Int
maxPages feed =
    case feed of
        Top ->
            10

        New ->
            12

        Ask ->
            3

        Show ->
            2

        Jobs ->
            1
