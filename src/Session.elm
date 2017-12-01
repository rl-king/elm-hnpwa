module Session exposing (..)

import Dict
import Http
import Route
import Types exposing (..)


type alias Session =
    { feeds : Dict.Dict String (Result Http.Error (List Item))
    , items : Dict.Dict Int (Result Http.Error Item)
    , users : Dict.Dict String (Result Http.Error User)
    }


getFeed : Session -> Route.Route -> Maybe (Result Http.Error (List Item))
getFeed session route =
    Dict.get (Route.toApi route) session.feeds
