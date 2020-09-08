module NonEmptyString exposing
    ( NonEmptyString
    , make
    , toString
    )


type NonEmptyString
    = NonEmptyString String


toString : NonEmptyString -> String
toString (NonEmptyString string) =
    string


make : String -> Maybe NonEmptyString
make string =
    case string of
        "" ->
            Nothing

        _ ->
            Just <| NonEmptyString string
