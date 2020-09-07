module Route exposing (Route(..), fromUrl)

import StrawPoll exposing (StrawPollKey(..))
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser, custom, map, oneOf, top)


type Route
    = CreateStrawPoll
    | StrawPoll StrawPollKey


strawPollKey : Parser (StrawPollKey -> a) a
strawPollKey =
    let
        fromString =
            Maybe.map StrawPollKey << String.toInt
    in
    custom "STRAW_POLL_KEY" fromString


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map CreateStrawPoll top
        , map StrawPoll (top </> strawPollKey)
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Url.parse parser
