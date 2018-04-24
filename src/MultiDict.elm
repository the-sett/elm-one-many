module MultiDict exposing (MultiDict, insert, remove, get)

import Dict exposing (Dict)
import EverySet exposing (EverySet)


type alias MultiDict comparable value =
    Dict comparable (EverySet value)


insert :
    comparable
    -> value
    -> MultiDict comparable value
    -> MultiDict comparable value
insert k v dict =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault EverySet.empty
                |> EverySet.insert v
    in
        Dict.insert k set dict


remove :
    comparable
    -> value
    -> MultiDict comparable value
    -> MultiDict comparable value
remove k v dict =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault EverySet.empty
                |> EverySet.remove v
    in
        if EverySet.isEmpty set then
            Dict.remove k dict
        else
            Dict.insert k set dict


get :
    comparable
    -> MultiDict comparable value
    -> Maybe (EverySet value)
get k dict =
    let
        maybeSet =
            Dict.get k dict
    in
        case maybeSet of
            Nothing ->
                Nothing

            Just set ->
                if EverySet.isEmpty set then
                    Nothing
                else
                    Just set
