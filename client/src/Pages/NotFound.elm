module Pages.NotFound exposing (view)

import Browser exposing (Document)
import Html exposing (h1, text)


view : Document message
view =
    { title = "Not found"
    , body =
        [ h1 [] [ text "not found..." ]
        ]
    }
