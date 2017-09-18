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



--INIT


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    toRequest
        { route = parseLocation location
        , feed = NotAsked
        , item = NotAsked
        , user = NotAsked
        }



-- UPDATE


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
            toRequest { model | route = parseLocation location }

        GotFeed x ->
            { model | feed = x } ! []

        GotItem x ->
            { model | item = x } ! []

        GotUser x ->
            { model | user = x } ! []



-- VIEW


view : Model -> Html Msg
view { route, feed, item, user } =
    let
        routeView =
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
        , routeView
        ]



-- HEADER VIEW


headerView : Route -> Html Msg
headerView route =
    header []
        [ strong [] [ text "logo" ]
        , nav [] (List.map (headerLink route) [ Top, New, Ask, Show, Jobs ])
        ]


headerLink : Route -> Route -> Html Msg
headerLink currentRoute route =
    if currentRoute == route then
        span [] [ text (Route.toTitle route) ]
    else
        link route [ text (Route.toTitle route) ]



-- LIST VIEW


listView : List Item -> Html Msg
listView feed =
    ul [ class "list-view" ] (List.indexedMap listViewItem feed)


listViewItem : Int -> Item -> Html Msg
listViewItem index item =
    li []
        [ aside [] [ text (toString (index + 1)) ]
        , div []
            [ link (Route.ItemRoute item.id) [ text item.title ]
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        ]



-- ITEM VIEW


itemView : Item -> Html Msg
itemView item =
    article []
        [ section []
            [ h2 [] [ text item.title ]
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        , Markdown.toHtml [] item.content
        , section [ class "comments-view" ]
            [ commentsView (getComments item.comments)
            ]
        ]


itemFooter : Item -> Html Msg
itemFooter item =
    if item.type_ == "job" then
        footer [] [ text item.timeAgo ]
    else
        footer []
            [ text (toString item.points ++ " points by ")
            , link (Route.User item.user) [ text item.user ]
            , text (" " ++ item.timeAgo ++ " | ")
            , link (Route.ItemRoute item.id) [ text (toString item.commentsCount ++ " comments") ]
            ]



-- COMMENTS VIEW


commentsView : List Item -> Html Msg
commentsView comments =
    ul [] (List.map commentView comments)


commentView : Item -> Html Msg
commentView item =
    li []
        [ div [ class "comment-meta" ]
            [ link (Route.User item.user) [ text item.user ]
            , text (" " ++ item.timeAgo)
            ]
        , Markdown.toHtml [] item.content
        , commentsView (getComments item.comments)
        ]



-- USER VIEW


userView : User -> Html Msg
userView user =
    section [ class "user-view" ]
        [ table []
            [ row "user:" user.id
            , row "created:" user.created
            , row "karma:" (toString user.karma)
            , row "about:" user.about
            ]
        ]


row : String -> String -> Html Msg
row x y =
    tr []
        [ td [] [ text x ]
        , td [] [ text y ]
        ]



-- VIEW HELPERS


handleViewState : (a -> Html Msg) -> RemoteData a -> Html Msg
handleViewState successView remoteData =
    case remoteData of
        NotAsked ->
            loadingView

        Loading ->
            loadingView

        Failure e ->
            errorView e

        Success x ->
            successView x


loadingView : Html Msg
loadingView =
    div [ class "loader" ] [ div [ class "spinner" ] [] ]


errorView : Http.Error -> Html Msg
errorView error =
    div [ class "loader" ] [ text (toString error) ]


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route), onClickPreventDefault (Route.toUrl route) ] kids


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault urlPath =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (D.succeed <| NewUrl urlPath)


getComments : Comments -> List Item
getComments x =
    case x of
        Comments xs ->
            xs



-- ROUTE TO REQUEST


toRequest : Model -> ( Model, Cmd Msg )
toRequest model =
    case model.route of
        User x ->
            { model | user = Loading } ! [ requestUser x ]

        ItemRoute x ->
            { model | item = Loading } ! [ requestItem x ]

        _ ->
            { model | feed = Loading } ! [ requestFeed model.route ]



-- HTTP


endpoint : String
endpoint =
    "https://hnpwa.com/api/v0/"


requestItem : Int -> Cmd Msg
requestItem x =
    Http.get (endpoint ++ "item/" ++ toString x ++ ".json") decodeItem
        |> Http.send (toRemoteData >> GotItem)


requestUser : String -> Cmd Msg
requestUser x =
    Http.get (endpoint ++ "user/" ++ x ++ ".json") decodeUser
        |> Http.send (toRemoteData >> GotUser)


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (endpoint ++ Route.toApi route ++ ".json") decodeFeed
        |> Http.send (toRemoteData >> GotFeed)


toRemoteData : Result Http.Error a -> RemoteData a
toRemoteData result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x



--DECODERS


decodeFeed : D.Decoder (List Item)
decodeFeed =
    D.list decodeItem


decodeItem : D.Decoder Item
decodeItem =
    P.decode Item
        |> P.required "id" D.int
        |> P.optional "title" D.string "No title"
        |> P.optional "points" D.int 0
        |> P.optional "user" D.string "No user found"
        |> P.required "time_ago" D.string
        |> P.optional "url" (D.nullable D.string) Nothing
        |> P.optional "domain" D.string ""
        |> P.required "comments_count" D.int
        |> P.optional "comments" decodeComments (Comments [])
        |> P.optional "content" D.string ""
        |> P.required "type" D.string


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
    , user : String
    , timeAgo : String
    , url : Maybe String
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
