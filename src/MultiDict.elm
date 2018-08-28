module MultiDict exposing (MultiDict, insert, remove, get)

{-| MultiDict implements a one-to-many relationship, between `comparable` keys
and `Set`s of values. Each key may be associated with zero or many values, compared
with a `Dict` which associated each key with just one value. Its implementation is
a `Dict` of `Set`s, but the overhead of creating new sets or removing empty sets as
values are added to or removed from the data structyre is managed for you.

@docs MultiDict, insert, remove, get

-}

import Dict exposing (Dict)
import DictSet exposing (DictSet)


{-| The type of multi dicts from comparable keys to sets of values.
-}
type alias MultiDict comparable value comparable1 =
    { vfun : value -> comparable1
    , dict : Dict comparable (DictSet comparable1 value)
    }


empty : (value -> comparable1) -> MultiDict comparable value comparable1
empty vfun =
    { vfun = vfun, dict = Dict.empty }


{-| Adds a key-value pair into the dictionary. It is added to the set of values
already associated with that key.
-}
insert :
    comparable
    -> value
    -> MultiDict comparable value comparable1
    -> MultiDict comparable value comparable1
insert k v { vfun, dict } =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault (DictSet.empty vfun)
                |> DictSet.insert v
    in
    { vfun = vfun, dict = Dict.insert k set dict }


{-| Removes a key-value pair from the dictionary. It is removed from the set of values
associated with the key.
-}
remove :
    comparable
    -> value
    -> MultiDict comparable value comparable1
    -> MultiDict comparable value comparable1
remove k v { vfun, dict } =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault (DictSet.empty vfun)
                |> DictSet.remove v
    in
    if DictSet.isEmpty set then
        { vfun = vfun, dict = Dict.remove k dict }

    else
        { vfun = vfun, dict = Dict.insert k set dict }


{-| Gets the set of values associated with a key, or `Nothing` if there is no
set of values.
-}
get :
    comparable
    -> MultiDict comparable value comparable1
    -> Maybe (DictSet comparable1 value)
get k { vfun, dict } =
    let
        maybeSet =
            Dict.get k dict
    in
    case maybeSet of
        Nothing ->
            Nothing

        Just set ->
            if DictSet.isEmpty set then
                Nothing

            else
                Just set
