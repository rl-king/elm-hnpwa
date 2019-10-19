module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Markdown exposing (defaultOptions)
import Route exposing (Route)
import Svg
import Svg.Attributes as SA
import Url
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Navigation.Key
    , route : Route
    , cache : Cache
    , page : Page
    }


type Page
    = Feed Route.Feed Int (List Item)
    | Article Item
    | Profile User
    | Loading
    | Error Http.Error
    | NotFound


type alias Cache =
    { feeds : Dict String (Result Http.Error (List Item))
    , items : Dict Int (Result Http.Error Item)
    , users : Dict String (Result Http.Error User)
    }



--INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    check
        { key = key
        , route = Route.fromUrl url
        , page = Loading
        , cache = Cache Dict.empty Dict.empty Dict.empty
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotItem Int (Result Http.Error Item)
    | GotUser String (Result Http.Error User)
    | GotFeed String (Result Http.Error (List Item))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ cache } as model) =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Navigation.load href )

        UrlChanged url ->
            check { model | route = Route.fromUrl url }

        GotItem id item ->
            check { model | cache = { cache | items = Dict.insert id item cache.items } }

        GotUser id user ->
            check { model | cache = { cache | users = Dict.insert id user cache.users } }

        GotFeed id feed ->
            check { model | cache = { cache | feeds = Dict.insert id feed cache.feeds } }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        ( page, title ) =
            viewPage model
    in
    { title = title ++ " | Elm HNPWA"
    , body =
        [ main_ []
            [ viewHeader model.route
            , section [ id "content" ] [ page ]
            ]
        ]
    }


viewPage : Model -> ( Html Msg, String )
viewPage model =
    case model.page of
        Feed feed page items ->
            ( viewList feed page items, Route.toTitle model.route )

        Article item ->
            ( viewItem item, item.title )

        Profile user ->
            ( viewUser user, user.id )

        Loading ->
            ( viewLoading, Route.toTitle model.route )

        Error error ->
            ( viewError error, Route.toTitle model.route )

        NotFound ->
            ( viewNotFound, Route.toTitle model.route )



-- HEADER VIEW


viewHeader : Route -> Html Msg
viewHeader route =
    header []
        [ link Route.Root [ i [ attribute "aria-label" "Homepage", class "logo" ] [ logo ] ]
        , nav [] <|
            List.map (headerLink route)
                [ Route.Top, Route.New, Route.Ask, Route.Show, Route.Jobs ]
        , a
            [ class "githublink"
            , href "https://github.com/rl-king/elm-hnpwa"
            , target "_blank"
            , rel "noopener"
            ]
            [ text "About" ]
        ]


headerLink : Route -> Route.Feed -> Html Msg
headerLink route feed =
    let
        highlightCurrent =
            case route of
                Route.Feeds currentFeed _ ->
                    feed == currentFeed

                _ ->
                    False

        feedRoute =
            Route.Feeds feed 1
    in
    if highlightCurrent then
        span [ attribute "aria-current" "page" ] [ text (Route.toTitle feedRoute) ]

    else
        link feedRoute [ text (Route.toTitle feedRoute) ]



-- LIST VIEW


viewList : Route.Feed -> Int -> List Item -> Html Msg
viewList feed page items =
    section [ class "list-view" ]
        [ ul [] (List.indexedMap viewListItem items)
        , viewPagination feed page
        ]


viewListItem : Int -> Item -> Html Msg
viewListItem index item =
    li []
        [ aside [] [ text (String.fromInt (index + 1)) ]
        , div []
            [ listItemUrl item.id item.url item.title
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        ]


listItemUrl : Int -> String -> String -> Html Msg
listItemUrl id url title =
    if String.contains "item?id=" url then
        link (Route.Item id) [ text title ]

    else
        a [ href url, target "_blank", rel "noopener" ] [ text title ]



-- PAGINATION


viewPagination : Route.Feed -> Int -> Html Msg
viewPagination feed page =
    let
        maxPages =
            Route.maxPages feed
    in
    section [ class "pagination" ]
        [ previousPageLink feed page
        , nav [] (List.map (paginationDesktop feed page) (List.range 1 maxPages))
        , div [ class "mobile" ]
            [ span [] [ text (String.fromInt page) ]
            , span [] [ text "/" ]
            , span [] [ text (String.fromInt maxPages) ]
            ]
        , nextPageLink feed page
        ]


nextPageLink : Route.Feed -> Int -> Html Msg
nextPageLink feed page =
    if page == Route.maxPages feed then
        span [ class "inactive" ] [ text "Next" ]

    else
        link (Route.Feeds feed (page + 1)) [ text "Next" ]


previousPageLink : Route.Feed -> Int -> Html Msg
previousPageLink feed page =
    if page == 1 then
        span [ class "inactive" ] [ text "Previous" ]

    else
        link (Route.Feeds feed (page - 1)) [ text "Previous" ]


paginationDesktop : Route.Feed -> Int -> Int -> Html Msg
paginationDesktop feed currentPage page =
    if page == currentPage then
        span [ attribute "aria-current" "page" ] [ text (String.fromInt page) ]

    else
        link (Route.Feeds feed page) [ text (String.fromInt page) ]



-- ITEM VIEW


viewItem : Item -> Html Msg
viewItem item =
    article []
        [ section []
            [ itemUrl item.id item.url item.title
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        , rawHtml item.content
        , section [ class "comments-view" ]
            [ viewComments (getComments item.comments)
            ]
        ]


itemUrl : Int -> String -> String -> Html Msg
itemUrl id url title =
    if String.contains "item?id=" url then
        h2 [] [ text title ]

    else
        a [ href url, target "_blank", rel "noopener" ] [ h2 [] [ text title ] ]


itemFooter : Item -> Html Msg
itemFooter item =
    if item.type_ == "job" then
        footer [] [ text item.timeAgo ]

    else
        footer []
            [ text (String.fromInt item.points ++ " points by ")
            , link (Route.User item.user) [ text item.user ]
            , text (" " ++ item.timeAgo ++ " | ")
            , link (Route.Item item.id) [ text (String.fromInt item.commentsCount ++ " comments") ]
            ]



-- COMMENTS VIEW


viewComments : List Item -> Html Msg
viewComments comments =
    ul [] (List.map commentView comments)


commentView : Item -> Html Msg
commentView item =
    li []
        [ div [ class "comment-meta" ]
            [ link (Route.User item.user) [ text item.user ]
            , text (" " ++ item.timeAgo)
            ]
        , rawHtml item.content
        , viewComments (getComments item.comments)
        ]



-- COMMENTS HELPER


getComments : Comments -> List Item
getComments comments =
    case comments of
        Comments items ->
            items

        Empty ->
            []



-- USER VIEW


viewUser : User -> Html Msg
viewUser user =
    section [ class "user-view" ]
        [ table []
            [ viewRow "user:" user.id
            , viewRow "created:" user.created
            , viewRow "karma:" (String.fromInt user.karma)
            , viewRow "about:" user.about
            ]
        ]


viewRow : String -> String -> Html Msg
viewRow x y =
    tr []
        [ td [] [ text x ]
        , td [] [ text y ]
        ]



-- ERROR AND NOTIFICATION VIEWS


viewNotification : Html msg -> Html msg
viewNotification content =
    div [ class "notification" ] [ content ]


viewLoading : Html Msg
viewLoading =
    viewNotification <|
        div [ class "spinner" ] []


viewNotFound : Html Msg
viewNotFound =
    viewNotification <|
        text "404"


viewError : Http.Error -> Html Msg
viewError error =
    (viewNotification << text) <|
        case error of
            Http.Timeout ->
                "Timeout"

            Http.NetworkError ->
                "NetworkError | You seem to be offline"

            Http.BadStatus status ->
                "BadStatus | The server gave me a " ++ String.fromInt status ++ " error"

            Http.BadBody _ ->
                "BadPayload | The server gave me back something I did not expect"

            Http.BadUrl _ ->
                "The Hackernews API seems to have changed"



--VIEW HELPERS


rawHtml : String -> Html Msg
rawHtml =
    Markdown.toHtmlWith { defaultOptions | sanitize = False } []



-- LINK HELPERS


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route) ] kids



-- ROUTE TO REQUEST


type Load result cmd
    = View result
    | Request cmd


check : Model -> ( Model, Cmd Msg )
check ({ route, cache } as model) =
    case checkHelper route cache of
        View (Ok page) ->
            ( { model | page = page }, Cmd.none )

        View (Err err) ->
            ( { model | page = Error err }, Cmd.none )

        Request cmd ->
            ( { model | page = Loading }, cmd )


checkHelper : Route -> Cache -> Load (Result Http.Error Page) (Cmd Msg)
checkHelper route cache =
    case route of
        Route.Feeds feed page ->
            Dict.get (endpoint (feedPathSegments feed page)) cache.feeds
                |> Maybe.map (View << Result.map (Feed feed page))
                |> Maybe.withDefault (Request (requestFeed feed page))

        Route.Item id ->
            Maybe.map (View << Result.map Article) (Dict.get id cache.items)
                |> Maybe.withDefault (Request (requestItem id))

        Route.User id ->
            Maybe.map (View << Result.map Profile) (Dict.get id cache.users)
                |> Maybe.withDefault (Request (requestUser id))

        _ ->
            View (Ok NotFound)



-- HTTP


endpoint : List String -> String
endpoint segments =
    Url.crossOrigin "https://api.hnpwa.com" segments []


requestItem : Int -> Cmd Msg
requestItem id =
    Http.get
        { url = endpoint [ "v0", "item", String.fromInt id ++ ".json" ]
        , expect = Http.expectJson (GotItem id) decodeItem
        }


requestUser : String -> Cmd Msg
requestUser id =
    Http.get
        { url = endpoint [ "v0", "user", id ++ ".json" ]
        , expect = Http.expectJson (GotUser id) decodeUser
        }


requestFeed : Route.Feed -> Int -> Cmd Msg
requestFeed feed page =
    let
        url =
            endpoint (feedPathSegments feed page)
    in
    Http.get
        { url = url
        , expect = Http.expectJson (GotFeed url) decodeFeed
        }


feedPathSegments : Route.Feed -> Int -> List String
feedPathSegments feed page =
    let
        toSegements path =
            [ "v0", path, String.fromInt page ++ ".json" ]
    in
    case feed of
        Route.Top ->
            toSegements "news"

        Route.New ->
            toSegements "newest"

        Route.Ask ->
            toSegements "ask"

        Route.Show ->
            toSegements "show"

        Route.Jobs ->
            toSegements "jobs"



-- DATATYPES AND DECODERS


type alias Item =
    { id : Int
    , title : String
    , points : Int
    , user : String
    , timeAgo : String
    , url : String
    , domain : String
    , commentsCount : Int
    , comments : Comments
    , content : String
    , type_ : String
    }


type alias User =
    { about : String
    , created : String
    , id : String
    , karma : Int
    }


type Comments
    = Comments (List Item)
    | Empty


decodeFeed : Decode.Decoder (List Item)
decodeFeed =
    Decode.list decodeItem


decodeItem : Decode.Decoder Item
decodeItem =
    Decode.succeed Item
        |> Pipeline.required "id" Decode.int
        |> Pipeline.optional "title" Decode.string "No title"
        |> Pipeline.optional "points" Decode.int 0
        |> Pipeline.optional "user" Decode.string ""
        |> Pipeline.required "time_ago" Decode.string
        |> Pipeline.optional "url" Decode.string ""
        |> Pipeline.optional "domain" Decode.string ""
        |> Pipeline.required "comments_count" Decode.int
        |> Pipeline.optional "comments" (Decode.lazy (\_ -> decodeComments)) Empty
        |> Pipeline.optional "content" Decode.string ""
        |> Pipeline.required "type" Decode.string


decodeUser : Decode.Decoder User
decodeUser =
    Decode.succeed User
        |> Pipeline.optional "title" Decode.string ""
        |> Pipeline.required "created" Decode.string
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "karma" Decode.int


decodeComments : Decode.Decoder Comments
decodeComments =
    Decode.map Comments (Decode.list (Decode.lazy (\_ -> decodeItem)))



-- LOGO


logo : Svg.Svg Msg
logo =
    Svg.svg [ width 25, height 26, SA.viewBox "0 0 25 26" ]
        [ Svg.g [ SA.fill "none" ]
            [ Svg.path [ SA.fill "#FFFFFF", SA.d "M12.4 6l5.3.2L12.3.8m0 12.5v5.3l5.4-5.3" ] []
            , Svg.path [ SA.fill "#FFFFFF", SA.d "M12.3 25v-5.3l6-6v5.5m-6-12.4h6v5.8h-6z" ] []
            , Svg.path [ SA.fill "#FFFFFF", SA.d "M19 18.4l5.3-5.4L19 7.5" ] []
            , Svg.path [ SA.fill "#FFFFFF", SA.d "M11.7.8H0l11.7 11.7" ] []
            , Svg.path [ SA.fill "#FFFFFF", SA.d "M11.7 25.2V13.5L0 25.2" ] []
            ]
        ]
