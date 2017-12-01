module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http exposing (Error(..), get, send)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, optional, required)
import Markdown exposing (toHtml)
import Navigation exposing (Location)
import Result exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Svg
import Svg.Attributes as SA
import Task
import Types exposing (..)


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
    , session : Session
    , page : Page
    }


type Page
    = FeedPage (Session.Status (List Item))
    | ItemPage
    | UserPage
    | LoadingPage
    | ErrorPage



--INIT


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    routeToRequest
        { route = Route.parseLocation location
        , page = LoadingPage
        , session = Session Dict.empty Dict.empty
        }



-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Location
    | GotFeed String (Session.Status (List Item))
    | GotItem Int (Session.Status Item)



-- | GotUser (Session.Status User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        UrlChange location ->
            routeToRequest { model | route = Route.parseLocation location }

        GotItem id item ->
            ( { model | session = Session.insertItem model.session id item }, Cmd.none )

        GotFeed id feed ->
            check { model | session = Session.insertFeed model.session id feed }



-- VIEW


view : Model -> Html Msg
view { page, route } =
    let
        pageView =
            case page of
                -- NotFound ->
                --     notFoundView
                -- Route.Item _ ->
                --     handleViewState itemView item
                -- User _ ->
                FeedPage items ->
                    handleViewState listView items

                _ ->
                    text "bla"
    in
    main_ []
        [ headerView route
        , pageView
        ]



-- HEADER VIEW


headerView : Route -> Html Msg
headerView route =
    header []
        [ logo
        , nav []
            (List.map (headerLink route)
                [ Route.Feeds Route.Top
                , Route.Feeds Route.New
                , Route.Feeds Route.Ask
                , Route.Feeds Route.Show
                , Route.Feeds Route.Jobs
                ]
            )
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
            [ itemUrl item.id item.url item.title
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        ]


itemUrl : Int -> String -> String -> Html Msg
itemUrl id url title =
    if String.contains "item?id=" url then
        link (Route.Item id) [ text title ]
    else
        a [ href url, target "_blank" ] [ text title ]



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
            , link (Route.Item item.id) [ text (toString item.commentsCount ++ " comments") ]
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


handleViewState : (a -> Html Msg) -> Session.Status a -> Html Msg
handleViewState successView status =
    case status of
        Session.NotAsked ->
            loadingView

        Session.Loading ->
            loadingView

        Session.Failure e ->
            errorView e

        Session.Partial x ->
            div [] [ loadingView, successView x ]

        Session.Success x ->
            successView x


loadingView : Html Msg
loadingView =
    div [ class "notification" ] [ div [ class "spinner" ] [] ]


notFoundView : Html Msg
notFoundView =
    div [ class "notification" ] [ text "404" ]


errorView : Http.Error -> Html Msg
errorView error =
    let
        message =
            case error of
                Timeout ->
                    "Timeout"

                NetworkError ->
                    "You seem to be offline"

                BadStatus x ->
                    "The server gave me a " ++ toString x.status.code ++ " error"

                BadPayload _ _ ->
                    "The server gave me back something I did not expect"

                _ ->
                    "Woops"
    in
    div [ class "notification" ] [ text message ]


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route), onClickPreventDefault (Route.toUrl route) ] kids


onClickPreventDefault : String -> Attribute Msg
onClickPreventDefault url =
    onWithOptions "click"
        { preventDefault = True
        , stopPropagation = False
        }
        (D.succeed <| NewUrl url)


getComments : Comments -> List Item
getComments x =
    case x of
        Comments xs ->
            xs



-- ROUTE TO REQUEST


check : Model -> ( Model, Cmd Msg )
check ({ route, page, session } as model) =
    let
        goto page =
            ( { model | page = page }, Cmd.none )
    in
    case route of
        Route.Feeds _ ->
            Session.getFeed session route
                |> Maybe.map FeedPage
                |> Maybe.withDefault LoadingPage
                |> goto

        _ ->
            goto ErrorPage


routeToRequest : Model -> ( Model, Cmd Msg )
routeToRequest ({ route } as model) =
    case route of
        Route.Item id ->
            ( model, requestItem id )

        _ ->
            ( model, requestFeed route )



-- HTTP


endpoint : String
endpoint =
    "https://hnpwa.com/api/v0/"


requestItem : Int -> Cmd Msg
requestItem id =
    Http.get (endpoint ++ "item/" ++ toString id ++ ".json") decodeItem
        |> Http.send (Session.toSessionStatus >> GotItem id)



-- requestUser : String -> Cmd Msg
-- requestUser x =
--     Http.get (endpoint ++ "user/" ++ x ++ ".json") decodeUser
--         |> Http.send (toSession.Status >> GotUser)


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (endpoint ++ Route.toApi route ++ ".json") decodeFeed
        |> Http.send (Session.toSessionStatus >> GotFeed (Route.toApi route))



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
        |> P.optional "url" D.string ""
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



-- LOGO


logo : Svg.Svg Msg
logo =
    Svg.svg [ width 25, height 26, SA.viewBox "0 0 25 26" ]
        [ Svg.g [ SA.fill "none" ]
            [ Svg.path [ SA.fill "#F0AD00", SA.d "M12.4 6l5.3.2L12.3.8m0 12.5v5.3l5.4-5.3" ] []
            , Svg.path [ SA.fill "#7FD13B", SA.d "M12.3 25v-5.3l6-6v5.5m-6-12.4h6v5.8h-6z" ] []
            , Svg.path [ SA.fill "#60B5CC", SA.d "M19 18.4l5.3-5.4L19 7.5" ] []
            , Svg.path [ SA.fill "#5A6378", SA.d "M11.7.8H0l11.7 11.7" ] []
            , Svg.path [ SA.fill "#60B5CC", SA.d "M11.7 25.2V13.5L0 25.2" ] []
            ]
        ]
