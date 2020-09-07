module Pages.StrawPoll exposing
    ( Message(..)
    , Model
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (h1, text)
import StrawPoll exposing (StrawPollKey(..))


type Message
    = NoOp


type alias Model =
    StrawPollKey


init : StrawPollKey -> Model
init strawPollKey =
    strawPollKey


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Document Message
view strawPollKey =
    let
        displayKey (StrawPollKey key) =
            String.fromInt key
    in
    { title = "Straw Poll"
    , body =
        [ h1 [] [ text <| "Straw poll: " ++ displayKey strawPollKey ]
        ]
    }
