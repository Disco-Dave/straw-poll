module Route exposing (Route(..), fromUrl)

import Poll exposing (PollKey(..))
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser, custom, map, oneOf, top)


type Route
    = CreatePoll
    | Poll PollKey


pollKey : Parser (PollKey -> a) a
pollKey =
    let
        fromString =
            Maybe.map PollKey << String.toInt
    in
    custom "STRAW_POLL_KEY" fromString


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map CreatePoll top
        , map Poll (top </> pollKey)
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Url.parse parser
