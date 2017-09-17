module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http exposing (get, send)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, optional, required)
import Markdown exposing (toHtml)
import Navigation exposing (Location)
import Result exposing (..)
import Route exposing (..)


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
    | GotFeed (RemoteData (List Item))
    | GotItem (RemoteData Item)
    | GotUser (RemoteData User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            parseRoute (parseLocation location)

        GotFeed x ->
            { model | feed = x } ! []

        GotItem x ->
            { model | item = x } ! []

        GotUser x ->
            { model | user = x } ! []


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


headerView : Route -> Html Msg
headerView route =
    header []
        [ strong [] [ text "logo" ]
        , nav [] (List.map (headerLink route) [ Top, New, Ask, Show, Jobs ])
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
listViewItem index { id, title, domain, points, user, commentsCount, timeAgo } =
    li []
        [ span [ class "index" ] [ text (toString (index + 1)) ]
        , div []
            [ link (Route.ItemRoute id) [] [ text title ]
            , span [ class "domain" ] [ text domain ]
            , footer []
                [ span [] [ text (toString points ++ " points") ]
                , maybeElement user (\x -> span [] [ text " by ", link (Route.User x) [] [ text x ] ])
                , text (" " ++ timeAgo ++ " | ")
                , link (Route.ItemRoute id) [] [ text (toString commentsCount ++ " comments") ]
                ]
            ]
        ]


itemView : Item -> Html Msg
itemView item =
    article []
        [ section [ class "item-article" ]
            [ h2 [] [ text item.title ]
            , span [ class "domain" ] [ text item.domain ]
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
commentView { user, timeAgo, comments, content } =
    li []
        [ div [ class "comment-meta" ]
            [ maybeElement user (\x -> link (Route.User x) [] [ text x ])
            , text (" " ++ timeAgo)
            ]
        , Markdown.toHtml [] content
        , commentsView (getComments comments)
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


getComments : Comments -> List Item
getComments x =
    case x of
        Comments xs ->
            xs


maybeElement : Maybe a -> (a -> Html Msg) -> Html Msg
maybeElement m element =
    case m of
        Just x ->
            element x

        Nothing ->
            text ""



-- HTTP


endpoint : String
endpoint =
    "https://hnpwa.com/api/v0/"


requestItem : Int -> Cmd Msg
requestItem x =
    Http.get (endpoint ++ "item/" ++ toString x ++ ".json") decodeItem
        |> Http.send (toRemoteDate >> GotItem)


requestUser : String -> Cmd Msg
requestUser x =
    Http.get (endpoint ++ "user/" ++ x ++ ".json") decodeUser
        |> Http.send (toRemoteDate >> GotUser)


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (endpoint ++ .api (Route.toRouteData route) ++ ".json") decodeFeed
        |> Http.send (toRemoteDate >> GotFeed)


toRemoteDate : Result Http.Error a -> RemoteData a
toRemoteDate result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x


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
        |> P.optional "domain" D.string ""
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
    , domain : String
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
