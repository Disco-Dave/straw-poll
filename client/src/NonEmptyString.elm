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
    if String.isEmpty string then
        Nothing

    else
        Just <| NonEmptyString string
