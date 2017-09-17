module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http exposing (get, send)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, hardcoded, optional, required)
import Markdown exposing (..)
import Navigation exposing (Location)
import Result exposing (..)
import Route exposing (..)
import Task exposing (..)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { route : Route
    , feed : RemoteData (List Item)
    , item : RemoteData Item
    , user : RemoteData User
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    parseRoute (parseLocation location)


parseRoute : Route -> ( Model, Cmd Msg )
parseRoute route =
    case route of
        User x ->
            ( Model route NotAsked NotAsked Loading, requestUser x )

        ItemRoute x ->
            ( Model route NotAsked Loading NotAsked, requestItem x )

        _ ->
            ( Model route Loading NotAsked NotAsked, requestFeed route )


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed (Result Http.Error (List Item))
    | GotItem (Result Http.Error Item)
    | GotUser (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            parseRoute (parseLocation location)

        GotFeed (Ok xs) ->
            { model | feed = Success xs } ! []

        GotFeed (Err x) ->
            { model | feed = Failure x } ! []

        GotItem (Ok x) ->
            { model | item = Success x } ! []

        GotItem (Err x) ->
            { model | feed = Failure x } ! []

        GotUser (Ok x) ->
            { model | user = Success x } ! []

        GotUser (Err x) ->
            { model | feed = Failure x } ! []


view : Model -> Html Msg
view { route, feed, item, user } =
    let
        activeView =
            case route of
                ItemRoute _ ->
                    handleViewState itemView item

                User _ ->
                    handleViewState userView user

                _ ->
                    handleViewState listView feed
    in
    main_ []
        [ headerView route
        , activeView
        ]


handleViewState : (a -> Html Msg) -> RemoteData a -> Html Msg
handleViewState successView remoteData =
    case remoteData of
        NotAsked ->
            viewLoading

        Loading ->
            viewLoading

        Failure _ ->
            viewLoading

        Success x ->
            successView x


headerView : Route -> Html Msg
headerView currentRoute =
    header []
        [ strong [] [ text "logo" ]
        , nav [] <|
            List.map (headerLink currentRoute) [ Top, New, Ask, Show, Jobs ]
        ]


headerLink : Route -> Route -> Html Msg
headerLink currentRoute route =
    if currentRoute == route then
        span [] [ text (Route.toRouteData route |> .title) ]
    else
        link route [] [ text (Route.toRouteData route |> .title) ]


listView : List Item -> Html Msg
listView xs =
    ul [ class "list-view" ] (List.indexedMap listViewItem xs)


listViewItem : Int -> Item -> Html Msg
listViewItem index item =
    li []
        [ span [ class "index" ] [ text <| toString <| index + 1 ]
        , div [ class "list-item-content" ]
            [ link (Route.ItemRoute item.id) [] [ text item.title ]
            , span [ class "domain" ] [ maybeText "" item.domain ]
            , footer []
                [ span [] [ text (toString item.points) ]
                , text " points by "
                , link (Route.User (Maybe.withDefault "" item.user)) [] [ maybeText "No user found" item.user ]
                , text (" " ++ item.timeAgo ++ " | ")
                , link (Route.ItemRoute item.id) [] [ text <| toString item.commentsCount ++ " comments" ]
                ]
            ]
        ]


itemView : Item -> Html Msg
itemView item =
    article []
        [ section [ class "item-article" ]
            [ h2 [] [ text item.title ]
            , span [ class "domain" ] [ maybeText "" item.domain ]
            , footer []
                [ span [] [ text (toString item.points) ]
                , text " points by "
                , link (Route.User (Maybe.withDefault "" item.user)) [] [ maybeText "No user found" item.user ]
                , text (" " ++ item.timeAgo)
                ]
            ]
        , Markdown.toHtml [] item.content
        , section [ class "comments-view" ]
            [ commentsView (getComments item.comments)
            ]
        ]


commentsView : List Item -> Html Msg
commentsView xs =
    ul [] (List.map commentView xs)


commentView : Item -> Html Msg
commentView item =
    li []
        [ div [ class "comment-meta" ]
            [ link (Route.User <| Maybe.withDefault "" item.user)
                []
                [ text <| Maybe.withDefault "" item.user ]
            , text (" " ++ item.timeAgo)
            ]
        , Markdown.toHtml [] item.content
        , commentsView (getComments item.comments)
        ]


userView : User -> Html Msg
userView { id, created, karma, about } =
    section [ class "user-view" ]
        [ table []
            [ row "user:" id
            , row "created:" created
            , row "karma:" (toString karma)
            , row "about:" about
            ]
        ]


row : String -> String -> Html Msg
row x y =
    tr []
        [ td [] [ text x ]
        , td [] [ text y ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "loader" ] [ div [ class "spinner" ] [] ]


getComments : Comments -> List Item
getComments x =
    case x of
        Comments xs ->
            xs



-- HTTP


endpoint : String
endpoint =
    "https://hnpwa.com/api/v0/"


requestItem x =
    request GotItem decodeItem (endpoint ++ "item/" ++ toString x ++ ".json")


requestUser x =
    request GotUser decodeUser (endpoint ++ "user/" ++ x ++ ".json")


requestFeed route =
    request GotFeed decodeFeed (endpoint ++ .api (Route.toRouteData route) ++ ".json")


request : (Result Http.Error a -> Msg) -> D.Decoder a -> String -> Cmd Msg
request msg decoder url =
    Http.send msg (Http.get url decoder)


decodeFeed : D.Decoder (List Item)
decodeFeed =
    D.list decodeItem


decodeItem : D.Decoder Item
decodeItem =
    P.decode Item
        |> P.required "id" D.int
        |> P.optional "title" D.string "No title"
        |> P.optional "points" D.int 0
        |> P.optional "user" (D.nullable D.string) Nothing
        |> P.required "time_ago" D.string
        |> P.optional "url" (D.nullable D.string) Nothing
        |> P.optional "domain" (D.nullable D.string) Nothing
        |> P.required "comments_count" D.int
        |> P.optional "comments" decodeComments (Comments [])
        |> P.optional "content" D.string ""


decodeUser : D.Decoder User
decodeUser =
    P.decode User
        |> P.optional "title" D.string ""
        |> P.required "created" D.string
        |> P.required "id" D.string
        |> P.required "karma" D.int


decodeComments : Decoder Comments
decodeComments =
    D.map Comments (D.list (D.lazy (\_ -> decodeItem)))



-- TODO Fix ctrl click


link : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
link route attrs kids =
    a (attrs ++ [ href (Route.toRouteData route |> .url), onClickPreventDefault (Route.toRouteData route |> .url) ]) kids


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault urlPath =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (D.succeed <| NewUrl urlPath)


maybeText : String -> Maybe String -> Html Msg
maybeText default maybeValue =
    Html.text <| Maybe.withDefault default maybeValue



-- TYPES


type RemoteData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success a


type alias Item =
    { id : Int
    , title : String
    , points : Int
    , user : Maybe String
    , timeAgo : String
    , url : Maybe String
    , domain : Maybe String
    , commentsCount : Int
    , comments : Comments
    , content : String
    }


type alias User =
    { about : String
    , created : String
    , id : String
    , karma : Int
    }


type Comments
    = Comments (List Item)
