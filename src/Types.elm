module Types exposing (..)


type PageHelper page result cmd
    = Go result
    | Get cmd


type alias Item =
    { id : Int
    , title : String
    , points : Int
    , user : String
    , timeAgo : String
    , url : String
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
    | Empty
