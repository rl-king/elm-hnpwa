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
    , feed : Dict String (List Int)
    , items : Dict Int Item
    , users : Dict String User
    }


type alias Item =
    { id : Int
    , title : String
    , points : Int
    , user : Maybe String
    , time : Float
    , timeAgo : String
    , type_ : String
    , url : Maybe String
    , domain : Maybe String
    , commentsCount : Int
    , comments : Comments
    , content : String
    , deleted : Bool
    , dead : Bool
    , level : Int
    }


type alias User =
    { about : String
    , created : String
    , id : String
    , karma : Int
    }


type Comments
    = Comments (List Item)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location
    , feed = Dict.empty
    , items = Dict.empty
    , users = Dict.empty
    }
        ! [ getCmd (parseLocation location) ]



-- ! [ getCmd (parseLocation location), preLoadFeeds (parseLocation location) ]


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed (Result Http.Error (List Item))
    | GotPreLoadedFeed (Result Http.Error (List (List Item)))
    | GotItem (Result Http.Error Item)
    | GotUser (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            let
                route =
                    parseLocation location
            in
            { model | route = route } ! [ getCmd route ]

        GotFeed (Ok xs) ->
            let
                routeTitle =
                    Route.toRouteData model.route |> .title

                feed =
                    Dict.insert routeTitle (List.map .id xs) model.feed

                insert x d =
                    Dict.insert x.id x d

                items =
                    List.foldl insert model.items xs
            in
            { model | feed = feed, items = items } ! []

        GotFeed (Err x) ->
            Debug.log (toString x) model ! []

        GotPreLoadedFeed (Ok x) ->
            model ! []

        GotPreLoadedFeed (Err x) ->
            model ! []

        GotItem (Ok x) ->
            { model | items = Dict.insert x.id x model.items } ! []

        GotItem (Err x) ->
            Debug.log (toString x) model ! []

        GotUser (Ok x) ->
            { model | users = Dict.insert x.id x model.users } ! []

        GotUser (Err x) ->
            Debug.log (toString x) model ! []


view : Model -> Html Msg
view model =
    let
        currentView =
            case model.route of
                ItemRoute x ->
                    case Dict.get x model.items of
                        Just x ->
                            itemView x

                        Nothing ->
                            viewLoading

                User x ->
                    case Dict.get x model.users of
                        Just x ->
                            userView x

                        Nothing ->
                            viewLoading

                _ ->
                    case Dict.get (toRouteData model.route |> .title) model.feed of
                        Just xs ->
                            listView (getFeedFromItems xs model.items)

                        Nothing ->
                            viewLoading
    in
    main_ []
        [ headerView model.route
        , currentView
        ]


getFeedFromItems : List Int -> Dict Int Item -> List Item
getFeedFromItems feed items =
    List.filterMap (flip Dict.get items) feed


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


endpoint =
    "https://hnpwa.com/api/v0/"


getCmd : Route -> Cmd Msg
getCmd route =
    case route of
        ItemRoute id ->
            request GotItem decodeItem (endpoint ++ "item/" ++ toString id ++ ".json")

        User id ->
            request GotUser decodeUser (endpoint ++ "user/" ++ id ++ ".json")

        _ ->
            request GotFeed decodeFeed (endpoint ++ .api (Route.toRouteData route) ++ ".json")


request : (Result Http.Error a -> Msg) -> D.Decoder a -> String -> Cmd Msg
request msg decoder url =
    Http.send msg (Http.get url decoder)


preLoadFeeds route =
    let
        makeTask x =
            Http.get ("https://hnpwa.com/api/v0/" ++ (.api <| Route.toRouteData x) ++ ".json") decodeFeed
                |> Http.toTask

        otherRoutes =
            List.filter ((/=) route) [ Top, New, Ask, Show, Jobs ]
    in
    Task.sequence (List.map makeTask otherRoutes)
        |> Task.attempt GotPreLoadedFeed


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
        |> P.required "time" D.float
        |> P.required "time_ago" D.string
        |> P.required "type" D.string
        |> P.optional "url" (D.nullable D.string) Nothing
        |> P.optional "domain" (D.nullable D.string) Nothing
        |> P.required "comments_count" D.int
        |> P.optional "comments" decodeComments (Comments [])
        |> P.optional "content" D.string ""
        |> P.optional "deleted" D.bool False
        |> P.optional "dead" D.bool False
        |> P.optional "level" D.int 0


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
