module Pages.Poll exposing
    ( Message(..)
    , Model
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (h1, text)
import Poll exposing (PollKey(..))


type Message
    = NoOp


type alias Model =
    PollKey


init : PollKey -> Model
init pollKey =
    pollKey


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Document Message
view strawPollKey =
    let
        displayKey (PollKey key) =
            String.fromInt key
    in
    { title = "Straw Poll"
    , body =
        [ h1 [] [ text <| "Straw poll: " ++ displayKey strawPollKey ]
        ]
    }
