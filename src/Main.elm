module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http exposing (get, send)
import Json.Decode as D exposing (..)
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
    }



-- type alias Item =
--     { id : Int
--     , title : String
--     , points : Maybe Int
--     , user : Maybe String
--     , time : Float
--     , time_ago : String
--     , content : String
--     , deleted : Maybe Bool
--     , dead : Maybe Bool
--     , type_ : String
--     , url : Maybe String
--     , domain : Maybe String
--     , level : Int
--     , comments_count : Int
--     }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location
    , feed = []
    , items = Dict.empty
    }
        ! []


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
    main_ []
        [ headerView
        , listView model
        , itemView model
        ]


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


listView : Model -> Html Msg
listView model =
    ul [] []


itemView : Model -> Html Msg
itemView model =
    article [] []



-- HTTP


requestPage : Int -> Cmd Msg
requestPage id =
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
    D.map2
        Item
        (D.field "id" D.int)
        (D.field "title" D.string)



-- TODO Fix ctrl click


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault urlPath =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (D.succeed <| NewUrl urlPath)
