module TwoOrMore exposing
    ( TwoOrMore
    , make
    , map
    , toList
    )


type TwoOrMore a
    = TwoOrMore (List a)


make : a -> List a -> TwoOrMore a
make head tail =
    TwoOrMore <| head :: tail


toList : TwoOrMore a -> List a
toList (TwoOrMore list) =
    list


map : (a -> b) -> TwoOrMore a -> TwoOrMore b
map f (TwoOrMore list) =
    TwoOrMore <| List.map f list
