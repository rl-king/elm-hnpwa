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
    , feed : List Int
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
    }



-- type alias Item =
--     , content : String
--     , deleted : Maybe Bool
--     , dead : Maybe Bool
--     , level : Int
--     {
--     }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location
    , feed = []
    , items = Dict.empty
    }
        ! [ initCmd (parseLocation location) ]


initCmd : Route -> Cmd Msg
initCmd route =
    case route of
        ItemRoute x ->
            Cmd.none

        _ ->
            requestFeed


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed (Result Http.Error (List Item))


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
            { model | route = route } ! []

        GotFeed (Ok xs) ->
            let
                feed =
                    List.map .id xs

                insert x d =
                    Dict.insert x.id x d

                items =
                    List.foldl insert model.items xs
            in
            { model | feed = feed, items = items } ! []

        GotFeed (Err x) ->
            model ! []


view : Model -> Html Msg
view model =
    let
        currentView =
            case model.route of
                ItemRoute x ->
                    itemView model

                _ ->
                    listView (getFeedFromItems model.feed model.items)
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
            List.map (headerLink << Route.toUrlString)
                [ Top, New, Ask, Show, Jobs ]
        ]


headerLink : ( String, String ) -> Html Msg
headerLink ( title, url ) =
    a [ href url, onClickPreventDefault url ] [ text title ]


listView : List Item -> Html Msg
listView xs =
    ul [] (List.indexedMap listViewItem xs)


listViewItem : Int -> Item -> Html Msg
listViewItem index item =
    li []
        [ span [] [ text <| toString <| index + 1 ]
        , div []
            [ a [] [ text item.title ]
            , span [] [ maybeText "" item.domain ]
            , footer []
                [ span [] [ maybeText "0" (Maybe.map toString item.points) ]
                , text " points by "
                , a [ href ("/user/" ++ Maybe.withDefault "" item.user) ] [ maybeText "No user found" item.user ]
                , text (" " ++ item.timeAgo)
                , text " | "
                , a [ href ("/item/" ++ toString item.id) ] [ text <| toString item.commentsCount ++ " comments" ]
                ]
            ]
        ]


itemView : Model -> Html Msg
itemView model =
    article [] []



-- HTTP


requestFeed : Cmd Msg
requestFeed =
    let
        url =
            "https://hnpwa.com/api/v0/news.json"
    in
    Http.get url feedDecoder
        |> Http.send GotFeed


feedDecoder : D.Decoder (List Item)
feedDecoder =
    D.list itemDecoder


itemDecoder : D.Decoder Item
itemDecoder =
    P.decode Item
        |> P.required "id" D.int
        |> P.required "title" D.string
        |> P.required "points" (D.nullable D.int)
        |> P.required "user" (D.nullable D.string)
        |> P.required "time" D.float
        |> P.required "time_ago" D.string
        |> P.required "type" D.string
        |> P.required "url" (D.nullable D.string)
        |> P.required "domain" (D.nullable D.string)
        |> P.required "comments_count" D.int



-- TODO Fix ctrl click


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
