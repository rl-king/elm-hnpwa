module Session exposing (..)

import Dict
import Http
import Route
import Types exposing (..)


type alias Session =
    { feeds : Dict.Dict String (Status (List Item))
    , items : Dict.Dict Int (Status Item)
    }


insertItem : Session -> Int -> Status Item -> Session
insertItem session key val =
    { session | items = Dict.insert key val session.items }


insertFeed : Session -> String -> Status (List Item) -> Session
insertFeed session key val =
    { session | feeds = Dict.insert key val session.feeds }


getFeed : Session -> Route.Route -> Maybe (Status (List Item))
getFeed session route =
    Dict.get (Route.toApi route) session.feeds


type Status a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Partial a
    | Success a


statusMap fn status =
    case status of
        Success x ->
            fn x

        Partial x ->
            fn x

        _ ->
            status


statusWithDefault default status =
    case status of
        Success x ->
            x

        Partial x ->
            x

        _ ->
            default


toSessionStatus : Result Http.Error a -> Status a
toSessionStatus result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x
