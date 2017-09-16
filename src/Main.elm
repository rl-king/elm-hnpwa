module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
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
    { route : Route }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { route = parseLocation location } ! []


type Msg
    = NewUrl String
    | UrlChange Location


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


view : Model -> Html Msg
view model =
    main_ []
        [ headerView
        ]


headerView =
    header []
        [ i [] [ text "logo" ]
        , nav [] (List.map headerLink [ "Top", "New", "Ask", "Show", "Jobs" ])
        ]


headerLink x =
    a [] [ text x ]
