module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (get, send)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Route exposing (Route)
import Svg
import Svg.Attributes as SA
import Url



-- MAIN


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
    = Feed (List Item)
    | Article Item
    | Profile User
    | Loading
    | Error Http.Error
    | NotFound


type alias Cache =
    { feeds : Dict.Dict String (Result Http.Error (List Item))
    , items : Dict.Dict Int (Result Http.Error Item)
    , users : Dict.Dict String (Result Http.Error User)
    }



--INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    check
        { key = key
        , route = Route.parse url
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
            check { model | route = Route.parse url }

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
        viewPage =
            case model.page of
                Feed items ->
                    viewList model.route items

                Article item ->
                    viewItem item

                Profile user ->
                    viewUser user

                Loading ->
                    viewLoading

                Error error ->
                    viewError error

                NotFound ->
                    viewNotFound
    in
    { title = Route.toTitle model.route
    , body =
        [ main_ []
            [ viewHeader model.route
            , section [ id "content" ] [ viewPage ]
            ]
        ]
    }



-- HEADER VIEW


viewHeader : Route -> Html Msg
viewHeader route =
    header []
        [ link Route.Root [ i [ attribute "aria-label" "Homepage", class "logo" ] [ logo ] ]
        , nav []
            (List.map (headerLink route)
                [ Route.Feeds Route.Top Nothing
                , Route.Feeds Route.New Nothing
                , Route.Feeds Route.Ask Nothing
                , Route.Feeds Route.Show Nothing
                , Route.Feeds Route.Jobs Nothing
                ]
            )
        , a
            [ class "githublink"
            , href "https://github.com/rl-king/elm-hnpwa"
            , target "_blank"
            , rel "noopener"
            ]
            [ text "About" ]
        ]


headerLink : Route -> Route -> Html Msg
headerLink currentRoute route =
    if Route.toTitle currentRoute == Route.toTitle route then
        span [ attribute "aria-current" "page" ] [ text (Route.toTitle route) ]

    else
        link route [ text (Route.toTitle route) ]



-- LIST VIEW


viewList : Route -> List Item -> Html Msg
viewList route feed =
    section [ class "list-view" ]
        [ ul [] (List.indexedMap viewListItem feed)
        , viewPagination route
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


viewPagination : Route -> Html Msg
viewPagination route =
    case Route.toPagination route of
        Just total ->
            section [ class "pagination" ]
                [ previousPageLink route
                , nav [] (List.map (paginationDesktop route) (List.range 1 total))
                , div [ class "mobile" ]
                    [ span [] [ text (String.fromInt (Route.toFeedPage route)) ]
                    , span [] [ text "/" ]
                    , span [] [ text (String.fromInt total) ]
                    ]
                , nextPageLink route
                ]

        Nothing ->
            text ""


nextPageLink : Route -> Html Msg
nextPageLink route =
    Maybe.map (\x -> link x [ text "Next" ]) (Route.toNext route)
        |> Maybe.withDefault (span [ class "inactive" ] [ text "Next" ])


previousPageLink : Route -> Html Msg
previousPageLink route =
    Maybe.map (\x -> link x [ text "Previous" ]) (Route.toPrevious route)
        |> Maybe.withDefault (span [ class "inactive" ] [ text "Previous" ])


paginationDesktop : Route -> Int -> Html Msg
paginationDesktop route page =
    if page == Route.toFeedPage route then
        span [ attribute "aria-current" "page" ] [ text (String.fromInt page) ]

    else
        link (Route.mapFeedPage (\_ -> page) route) [ text (String.fromInt page) ]



-- ITEM VIEW


viewItem : Item -> Html Msg
viewItem item =
    article []
        [ section []
            [ itemUrl item.id item.url item.title
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        , rawHtml div item.content
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
        , rawHtml div item.content
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

            Http.BadStatus { status } ->
                "BadStatus | The server gave me a " ++ String.fromInt status.code ++ " error"

            Http.BadPayload _ _ ->
                "BadPayload | The server gave me back something I did not expect"

            Http.BadUrl _ ->
                "The Hackernews API seems to have changed"



--VIEW HELPERS


rawHtml : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> Html Msg
rawHtml node htmlString =
    node [ property "innerHTML" (Encode.string htmlString) ] []



-- LINK HELPERS


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route) ] kids



-- ROUTE TO REQUEST


type Load result cmd
    = Show result
    | Get cmd


check : Model -> ( Model, Cmd Msg )
check ({ route, cache } as model) =
    case checkHelper route cache of
        Show (Ok page) ->
            ( { model | page = page }, Cmd.none )

        Show (Err err) ->
            ( { model | page = Error err }, Cmd.none )

        Get cmd ->
            ( { model | page = Loading }, cmd )


checkHelper : Route -> Cache -> Load (Result Http.Error Page) (Cmd Msg)
checkHelper route cache =
    case route of
        Route.Feeds _ _ ->
            Maybe.map (Show << Result.map Feed) (Dict.get (Route.toApi route) cache.feeds)
                |> Maybe.withDefault (Get (requestFeed route))

        Route.Item id ->
            Maybe.map (Show << Result.map Article) (Dict.get id cache.items)
                |> Maybe.withDefault (Get (requestItem id))

        Route.User id ->
            Maybe.map (Show << Result.map Profile) (Dict.get id cache.users)
                |> Maybe.withDefault (Get (requestUser id))

        _ ->
            Show (Ok NotFound)



-- HTTP


endpoint : String
endpoint =
    "https://api.hnpwa.com/v0"


requestItem : Int -> Cmd Msg
requestItem id =
    Http.get (endpoint ++ "item/" ++ String.fromInt id ++ ".json") decodeItem
        |> Http.send (GotItem id)


requestUser : String -> Cmd Msg
requestUser id =
    Http.get (endpoint ++ "user/" ++ id ++ ".json") decodeUser
        |> Http.send (GotUser id)


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (endpoint ++ Route.toApi route) decodeFeed
        |> Http.send (GotFeed (Route.toApi route))



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
