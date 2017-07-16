module Docs.Module
    exposing
        ( Alias
        , Block(..)
        , Module
        , Section(..)
        , Tipe
        , Value
        , decode
        )

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)


type alias Module =
    { name : String
    , content : List Section
    }


type Section
    = Text String
    | Docs (List Block)


type Block
    = AnAlias Alias
    | AType Tipe
    | AValue Value


type alias Alias =
    { name : String
    , comment : String
    , args : List String
    , tipe : String
    }


alias_ : Decoder Alias
alias_ =
    JD.map4 Alias
        (JD.field "name" JD.string)
        (JD.field "comment" JD.string)
        (JD.field "args" (JD.list JD.string))
        (JD.field "type" JD.string)


type alias Tipe =
    { name : String
    , comment : String
    , args : List String
    , cases : List String
    }


tipe : Decoder Tipe
tipe =
    JD.map4 Tipe
        (JD.field "name" JD.string)
        (JD.field "comment" JD.string)
        (JD.field "args" (JD.list JD.string))
        (JD.field "cases" (JD.list JD.string))


type alias Value =
    { name : String
    , comment : String
    , tipe : String
    }


value : Decoder Value
value =
    JD.map3 Value
        (JD.field "name" JD.string)
        (JD.field "comment" JD.string)
        (JD.field "type" JD.string)


type alias InitialRepresentation =
    { name : String
    , comment : String
    , types : List Tipe
    , aliases : List Alias
    , values : List Value
    }


decode : Decoder Module
decode =
    JD.map5 InitialRepresentation
        (JD.field "name" JD.string)
        (JD.field "comment" JD.string)
        (JD.field "types" (JD.list tipe))
        (JD.field "aliases" (JD.list alias_))
        (JD.field "values" (JD.list value))
        |> JD.map finalize


finalize : InitialRepresentation -> Module
finalize { name, comment, types, aliases, values } =
    let
        exports : Dict String Block
        exports =
            [ List.map (\x -> ( x.name, AnAlias x )) aliases
            , List.map (\x -> ( x.name, AType x )) types
            , List.map (\x -> ( x.name, AValue x )) values
            ]
                |> List.concat
                |> Dict.fromList

        content : List Section
        content =
            String.lines comment
                |> List.foldr (\line lines -> add (handleLine line) lines) []

        handleLine : String -> Section
        handleLine input =
            if String.startsWith "@docs" input then
                String.dropLeft 5 input
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filterMap (flip Dict.get exports)
                    |> Docs
            else
                Text input

        add : Section -> List Section -> List Section
        add section sections =
            case sections of
                [] ->
                    [ section ]

                x :: xs ->
                    case ( section, x ) of
                        ( Text s, Text o ) ->
                            Text (s ++ "\n" ++ o) :: xs

                        _ ->
                            section :: sections
    in
    { name = name, content = content }
