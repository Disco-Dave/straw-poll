module Main exposing (main)

import Browser exposing (Document, document)


type alias Model =
    {}


type alias Message =
    ()


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Message )
init _ =
    ( {}, Cmd.none )


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Document Message
view _ =
    { title = "Create Straw Poll"
    , body = []
    }


main : Program Flags Model Message
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
