module TwoOrMore exposing
    ( TwoOrMore
    , make
    , map
    , toList
    )


type TwoOrMore a
    = TwoOrMore (List a)


make : a -> a -> List a -> TwoOrMore a
make first second tail =
    TwoOrMore <| first :: second :: tail


toList : TwoOrMore a -> List a
toList (TwoOrMore list) =
    list


map : (a -> b) -> TwoOrMore a -> TwoOrMore b
map f (TwoOrMore list) =
    TwoOrMore <| List.map f list
