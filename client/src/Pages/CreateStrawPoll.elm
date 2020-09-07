module Pages.CreateStrawPoll exposing
    ( Message(..)
    , Model
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (h1, text)


type Message
    = NoOp


type alias Model =
    ()


init : Model
init =
    ()


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Document Message
view _ =
    { title = "Create Straw Poll"
    , body =
        [ h1 [] [ text "Create a straw poll" ]
        ]
    }
