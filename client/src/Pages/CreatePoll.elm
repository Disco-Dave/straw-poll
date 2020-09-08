module Pages.CreatePoll exposing
    ( Message(..)
    , Model
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html exposing (h1, text)
import TwoOrMore exposing (TwoOrMore)


type alias Field value =
    { value : value
    , error : String
    }


type alias Model =
    { question : Field String
    , answers : TwoOrMore (Field String)
    }


type Message
    = NoOp


init : Model
init =
    { question = { value = "", error = "" }
    , answers =
        TwoOrMore.make
            { value = "", error = "" }
            { value = "", error = "" }
            []
    }


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
