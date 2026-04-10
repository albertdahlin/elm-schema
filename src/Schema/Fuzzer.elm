module Schema.Fuzzer exposing (fromSchema, fromType)

{-| This module provides functionality to generate random data (fuzzing) based on a given schema.
It uses the elm-explorations/elm-test library to create fuzzers for various data types defined in the schema.

@docs fromSchema, fromType

-}

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Schema exposing (Schema)
import Schema.Type as Type exposing (Type)


{-| TODO: document
-}
fromSchema : Schema m a -> Fuzzer a
fromSchema schema =
    Schema.toType schema
        |> fromType
        |> Fuzz.andThen
            (\val ->
                case JD.decodeValue (Schema.decoder schema) val of
                    Ok v ->
                        Fuzz.constant v

                    Err e ->
                        "Could not decode generated value from type Schema.Fuzzer.fromType: \n"
                            ++ JD.errorToString e
                            |> Fuzz.invalid
            )


{-| TODO: document
-}
fromType : Type.Node m -> Fuzzer Value
fromType meta =
    fromTypeHelp 10 (Type.gatherNamed meta) meta


fromTypeHelp : Int -> Dict String (Type.Node m) -> Type.Node m -> Fuzzer Value
fromTypeHelp limit namedTypes meta =
    case meta.type_ of
        Type.Unit ->
            Fuzz.constant JE.null

        Type.String opt ->
            Fuzz.string
                |> Fuzz.map JE.string

        Type.Uuid ->
            Fuzz.string
                |> Fuzz.map JE.string

        Type.Bool ->
            Fuzz.bool
                |> Fuzz.map JE.bool

        Type.Int ->
            Fuzz.int
                |> Fuzz.map JE.int

        Type.Float ->
            Fuzz.niceFloat
                |> Fuzz.map JE.float

        Type.List itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.listOfLengthBetween 0 limit
                    |> Fuzz.map (JE.list identity)

        Type.Array itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Maybe justType ->
            if limit <= 0 then
                Fuzz.constant JE.null

            else
                Fuzz.oneOf
                    [ Fuzz.constant JE.null
                    , fromTypeHelp limit namedTypes justType
                    ]

        Type.Dict key value ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                Fuzz.map2
                    (\k v ->
                        [ ( "k", k )
                        , ( "v", v )
                        ]
                            |> JE.object
                    )
                    (fromTypeHelp limit namedTypes key)
                    (fromTypeHelp limit namedTypes value)
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Set itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Tuple types ->
            types
                |> List.map (fromTypeHelp limit namedTypes)
                |> Fuzz.sequence
                |> Fuzz.map (JE.list identity)

        Type.Record fields ->
            fields
                |> List.map
                    (\( n, t ) ->
                        fromTypeHelp limit namedTypes t
                            |> Fuzz.map (\v -> ( n, v ))
                    )
                |> Fuzz.sequence
                |> Fuzz.map JE.object

        Type.CustomType args ->
            List.map (variantFuzzers (limit - 1) namedTypes) args.variants
                |> Fuzz.oneOf

        Type.Recursive name ->
            case Dict.get name namedTypes of
                Just t ->
                    fromTypeHelp (limit - 1) namedTypes t

                Nothing ->
                    Fuzz.invalid ("Could not find recursive type: " ++ name)


variantFuzzers : Int -> Dict String (Type.Node m) -> { name : String, args : List (Type.Node m) } -> Fuzzer Value
variantFuzzers limit namedTypes variant =
    List.map (fromTypeHelp limit namedTypes) variant.args
        |> Fuzz.sequence
        |> Fuzz.map (JE.list identity)
        |> Fuzz.map
            (\args ->
                JE.object
                    [ ( "tag", JE.string variant.name )
                    , ( "args", args )
                    ]
            )
