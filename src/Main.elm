module Main exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (defaultOptions, onClick, onWithOptions)
import Html.Lazy as Lazy
import Http exposing (Error(..), get, send)
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as P exposing (decode, optional, required)
import Markdown exposing (toHtml)
import Navigation exposing (Location)
import Route exposing (Route)
import Svg
import Svg.Attributes as SA
import Types exposing (..)


main : Program Never Model Msg
main =
    Navigation.program OnNavigation
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
    = FeedPage (List Item)
    | ItemPage Item
    | UserPage User
    | LoadingPage
    | ErrorPage Http.Error
    | NotFound


type alias Session =
    { feeds : Dict.Dict String (Result Http.Error (List Item))
    , items : Dict.Dict Int (Result Http.Error Item)
    , users : Dict.Dict String (Result Http.Error User)
    }



--INIT


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    check
        { route = Route.parse location
        , page = LoadingPage
        , session = Session Dict.empty Dict.empty Dict.empty
        }



-- UPDATE


type Msg
    = NewUrl String
    | OnNavigation Location
    | GotItem Int (Result Http.Error Item)
    | GotUser String (Result Http.Error User)
    | GotFeed String (Result Http.Error (List Item))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        OnNavigation location ->
            check { model | route = Route.parse location }

        GotItem id item ->
            check { model | session = { session | items = Dict.insert id item session.items } }

        GotUser id user ->
            check { model | session = { session | users = Dict.insert id user session.users } }

        GotFeed id feed ->
            check { model | session = { session | feeds = Dict.insert id feed session.feeds } }



-- VIEW


view : Model -> Html Msg
view { page, route } =
    let
        viewPage =
            case page of
                FeedPage items ->
                    Lazy.lazy2 viewList route items

                ItemPage item ->
                    viewItem item

                UserPage user ->
                    viewUser user

                LoadingPage ->
                    viewLoading

                ErrorPage error ->
                    viewError error

                NotFound ->
                    viewNotFound
    in
    main_ []
        [ Lazy.lazy viewHeader route
        , viewPage
        ]



-- HEADER VIEW


viewHeader : Route -> Html Msg
viewHeader route =
    header []
        [ logo
        , nav []
            (List.map (headerLink route)
                [ Route.Feeds Route.Top Nothing
                , Route.Feeds Route.New Nothing
                , Route.Feeds Route.Ask Nothing
                , Route.Feeds Route.Show Nothing
                , Route.Feeds Route.Jobs Nothing
                ]
            )
        ]


headerLink : Route -> Route -> Html Msg
headerLink currentRoute route =
    if Route.toTitle currentRoute == Route.toTitle route then
        span [] [ text (Route.toTitle route) ]
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
        a [ href url, target "_blank", rel "noopener" ] [ text title ]


viewPagination : Route -> Html Msg
viewPagination route =
    case Route.toPagination route of
        Just total ->
            section [ class "pagination" ]
                [ previousPageLink route
                , nav []
                    (List.map (paginationLink route) (List.range 1 total))
                , nextPageLink route
                ]

        Nothing ->
            text ""


nextPageLink : Route -> Html Msg
nextPageLink route =
    Maybe.map (flip link [ text "Next" ]) (Route.toNext route)
        |> Maybe.withDefault (text "")


previousPageLink : Route -> Html Msg
previousPageLink route =
    Maybe.map (flip link [ text "Previous" ]) (Route.toPrevious route)
        |> Maybe.withDefault (text "")


paginationLink : Route -> Int -> Html Msg
paginationLink route page =
    if page == Route.toFeedPage route then
        span [] [ text (toString page) ]
    else
        link (Route.mapFeedPage (\_ -> page) route) [ text (toString page) ]



-- ITEM VIEW


viewItem : Item -> Html Msg
viewItem item =
    article []
        [ section []
            [ h2 [] [ text item.title ]
            , span [ class "domain" ] [ text item.domain ]
            , itemFooter item
            ]
        , Markdown.toHtml [] item.content
        , section [ class "comments-view" ]
            [ viewComments (getComments item.comments)
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
        , Markdown.toHtml [] item.content
        , viewComments (getComments item.comments)
        ]



-- USER VIEW


viewUser : User -> Html Msg
viewUser user =
    section [ class "user-view" ]
        [ table []
            [ viewRow "user:" user.id
            , viewRow "created:" user.created
            , viewRow "karma:" (toString user.karma)
            , viewRow "about:" user.about
            ]
        ]


viewRow : String -> String -> Html Msg
viewRow x y =
    tr []
        [ td [] [ text x ]
        , td [] [ text y ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "notification" ] [ div [ class "spinner" ] [] ]


viewNotFound : Html Msg
viewNotFound =
    div [ class "notification" ] [ text "404" ]


viewError : Http.Error -> Html Msg
viewError error =
    div [ class "notification" ] [ text (htmlErrorToString error) ]


htmlErrorToString : Error -> String
htmlErrorToString error =
    case error of
        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError | You seem to be offline"

        BadStatus { status } ->
            "BadStatus | The server gave me a " ++ toString status.code ++ " error"

        BadPayload _ _ ->
            "BadPayload | The server gave me back something I did not expect"

        BadUrl _ ->
            "The Hackernews API seems to have changed"



-- LINK HELPERS


link : Route -> List (Html Msg) -> Html Msg
link route kids =
    a [ href (Route.toUrl route), onPreventDefaultClick (Route.toMsg route NewUrl) ] kids


onPreventDefaultClick : Msg -> Attribute Msg
onPreventDefaultClick msg =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (D.andThen (eventDecoder msg) eventKeyDecoder)


eventKeyDecoder : Decoder Bool
eventKeyDecoder =
    D.map2
        (not >> xor)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)


eventDecoder : msg -> Bool -> Decoder msg
eventDecoder msg preventDefault =
    if preventDefault then
        D.succeed msg
    else
        D.fail ""



-- COMMENT HELPER


getComments : Comments -> List Item
getComments comments =
    case comments of
        Comments items ->
            items

        Empty ->
            []



-- ROUTE TO REQUEST


check : Model -> ( Model, Cmd Msg )
check ({ route, page, session } as model) =
    case checkHelper route session of
        Go result ->
            case result of
                Ok x ->
                    ( { model | page = x }, Cmd.none )

                Err err ->
                    ( { model | page = ErrorPage err }, Cmd.none )

        Get cmd ->
            ( { model | page = LoadingPage }, cmd )


checkHelper : Route -> Session -> PageHelper (Result Http.Error Page) (Cmd Msg)
checkHelper route session =
    case route of
        Route.Feeds _ _ ->
            Maybe.map (Go << Result.map FeedPage) (Dict.get (Route.toApi route) session.feeds)
                |> Maybe.withDefault (Get (requestFeed route))

        Route.Item id ->
            Maybe.map (Go << Result.map ItemPage) (Dict.get id session.items)
                |> Maybe.withDefault (Get (requestItem id))

        Route.User id ->
            Maybe.map (Go << Result.map UserPage) (Dict.get id session.users)
                |> Maybe.withDefault (Get (requestUser id))

        Route.NotFound ->
            Go (Ok NotFound)



-- HTTP


endpoint : String
endpoint =
    "https://hnpwa.com/api/v0/"


requestItem : Int -> Cmd Msg
requestItem id =
    Http.get (endpoint ++ "item/" ++ toString id ++ ".json") decodeItem
        |> Http.send (GotItem id)


requestUser : String -> Cmd Msg
requestUser id =
    Http.get (endpoint ++ "user/" ++ id ++ ".json") decodeUser
        |> Http.send (GotUser id)


requestFeed : Route -> Cmd Msg
requestFeed route =
    Http.get (endpoint ++ Route.toApi route) decodeFeed
        |> Http.send (GotFeed (Route.toApi route))



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
        |> P.optional "comments" (D.lazy (\_ -> decodeComments)) Empty
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
