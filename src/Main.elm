module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http exposing (get, send)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, hardcoded, optional, required)
import Navigation exposing (Location)
import Result exposing (..)
import Route exposing (..)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        }


type alias Model =
    { route : Route
    , feed : Dict String (List Int)
    , items : Dict Int Item
    }


type alias Item =
    { id : Int
    , title : String
    , points : Maybe Int
    , user : Maybe String
    , time : Float
    , timeAgo : String
    , type_ : String
    , url : Maybe String
    , domain : Maybe String
    , commentsCount : Int
    , content : String
    , deleted : Bool
    , dead : Bool
    , level : Int
    }


type RemoteData a
    = Loading
    | Error Http.Error
    | Updating a
    | Complete a


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location
    , feed = Dict.empty
    , items = Dict.empty
    }
        ! [ initCmd (parseLocation location) ]


initCmd : Route -> Cmd Msg
initCmd route =
    case route of
        ItemRoute x ->
            requestItem route

        _ ->
            requestFeed route


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed (Result Http.Error (List Item))
    | GotItem (Result Http.Error Item)


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
            { model | route = route } ! [ requestFeed route ]

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

        GotItem (Ok x) ->
            { model | items = Dict.insert x.id x model.items } ! []

        GotItem (Err x) ->
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
                            div [] []

                _ ->
                    case Dict.get (toRouteData model.route |> .title) model.feed of
                        Just xs ->
                            listView (getFeedFromItems xs model.items)

                        Nothing ->
                            div [] []
    in
    main_ []
        [ headerView
        , currentView
        ]


getFeedFromItems : List Int -> Dict Int Item -> List Item
getFeedFromItems feed items =
    List.filterMap (flip Dict.get items) feed


headerView : Html Msg
headerView =
    header []
        [ i [] [ text "logo" ]
        , nav [] <|
            List.map headerLink [ Top, New, Ask, Show, Jobs ]
        ]


headerLink : Route -> Html Msg
headerLink route =
    link route [] [ text (Route.toRouteData route |> .title) ]


listView : List Item -> Html Msg
listView xs =
    ul [] (List.indexedMap listViewItem xs)


listViewItem : Int -> Item -> Html Msg
listViewItem index item =
    li []
        [ span [] [ text <| toString <| index + 1 ]
        , div []
            [ link (Route.ItemRoute item.id) [] [ text item.title ]
            , span [] [ maybeText "" item.domain ]
            , footer []
                [ span [] [ maybeText "0" (Maybe.map toString item.points) ]
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
        [ div []
            [ a [] [ text item.title ]
            , span [] [ maybeText "" item.domain ]
            , footer []
                [ span [] [ maybeText "0" (Maybe.map toString item.points) ]
                , text " points by "
                , link (Route.User (Maybe.withDefault "" item.user)) [] [ maybeText "No user found" item.user ]
                , text (" " ++ item.timeAgo)
                , text " | "
                , a [ href ("/item/" ++ toString item.id) ] [ text <| toString item.commentsCount ++ " comments" ]
                ]
            ]
        , p [] [ text item.content ]
        ]



-- HTTP


getUrl : Route -> String
getUrl route =
    case route of
        ItemRoute id ->
            "https://hnpwa.com/api/v0/item/" ++ toString id ++ ".json"

        _ ->
            "https://hnpwa.com/api/v0/" ++ (.api <| Route.toRouteData route) ++ ".json"


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (getUrl route) feedDecoder
        |> Http.send GotFeed


requestItem : Route -> Cmd Msg
requestItem route =
    Http.get (getUrl route) itemDecoder
        |> Http.send GotItem


feedDecoder : D.Decoder (List Item)
feedDecoder =
    D.list itemDecoder


itemDecoder : D.Decoder Item
itemDecoder =
    P.decode Item
        |> P.required "id" D.int
        |> P.required "title" D.string
        |> P.optional "points" (D.nullable D.int) Nothing
        |> P.optional "user" (D.nullable D.string) Nothing
        |> P.required "time" D.float
        |> P.required "time_ago" D.string
        |> P.required "type" D.string
        |> P.optional "url" (D.nullable D.string) Nothing
        |> P.optional "domain" (D.nullable D.string) Nothing
        |> P.required "comments_count" D.int
        |> P.optional "content" D.string ""
        |> P.optional "deleted" D.bool False
        |> P.optional "dead" D.bool False
        |> P.optional "level" D.int 0



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
