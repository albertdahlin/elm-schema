module Schema.Type.Value exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Schema.Type as Type


type Value
    = Unit
    | String String
    | Uuid String
    | Int Int
    | Bool Bool
    | Float Float
    | Maybe (Maybe Value)
    | Tuple (Array Value)
    | List (List Value)
    | Array (List Value)
    | Set (List Value)
    | Dict (List ( Value, Value ))
    | Record (Dict String Value)
    | Variant String (Array Value)


encode : Value -> JD.Value
encode state =
    case state of
        Unit ->
            JE.null

        String s ->
            JE.string s

        Uuid u ->
            JE.string u

        Bool b ->
            JE.bool b

        Int i ->
            JE.int i

        Float f ->
            JE.float f

        Tuple items ->
            JE.array encode items

        List items ->
            items
                |> JE.list encode

        Array items ->
            items
                |> JE.list encode

        Set items ->
            items
                |> JE.list encode

        Dict pairs ->
            JE.list
                (\( k, v ) ->
                    JE.object
                        [ ( "k", encode k )
                        , ( "v", encode v )
                        ]
                )
                pairs

        Maybe maybeItem ->
            case maybeItem of
                Just item ->
                    encode item

                Nothing ->
                    JE.null

        Record dict ->
            dict
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, encode v ))
                |> JE.object

        Variant current args ->
            [ ( "tag", JE.string current )
            , ( "args"
              , args
                    |> JE.array encode
              )
            ]
                |> JE.object


decoder : Type.Node m -> JD.Decoder Value
decoder meta =
    decoderHelp (Type.gatherNamed meta) meta


decoderHelp : Dict String (Type.Node m) -> Type.Node m -> JD.Decoder Value
decoderHelp namedTypes meta =
    case meta.type_ of
        Type.Unit ->
            JD.succeed Unit

        Type.String _ ->
            JD.string
                |> JD.map String

        Type.Uuid ->
            JD.string
                |> JD.map Uuid

        Type.Int ->
            JD.int
                |> JD.map Int

        Type.Bool ->
            JD.bool
                |> JD.map Bool

        Type.Float ->
            JD.float
                |> JD.map Float

        Type.Tuple types ->
            types
                |> List.indexedMap
                    (\i typeMeta ->
                        JD.index i (decoderHelp namedTypes typeMeta)
                    )
                |> List.foldl
                    (\itemDecoder accDecoder ->
                        JD.map2
                            Array.push
                            itemDecoder
                            accDecoder
                    )
                    (JD.succeed Array.empty)
                |> JD.map Tuple

        Type.Maybe justType ->
            JD.nullable (decoderHelp namedTypes justType)
                |> JD.map Maybe

        Type.List itemType ->
            JD.list (decoderHelp namedTypes itemType)
                |> JD.map List

        Type.Array itemType ->
            JD.list (decoderHelp namedTypes itemType)
                |> JD.map List

        Type.Set itemType ->
            JD.list (decoderHelp namedTypes itemType)
                |> JD.map List

        Type.Dict keyType valueType ->
            JD.map2
                (\k v -> ( k, v ))
                (JD.field "k" (decoderHelp namedTypes keyType))
                (JD.field "v" (decoderHelp namedTypes valueType))
                |> JD.list
                |> JD.map Dict

        Type.Record fields ->
            fields
                |> List.map
                    (\( fieldName, fieldMeta ) ->
                        JD.field
                            fieldName
                            (decoderHelp namedTypes fieldMeta)
                            |> JD.map (\s -> ( fieldName, s ))
                    )
                |> List.foldl
                    (\fieldDecoder accDecoder ->
                        JD.map2
                            (\( name, state ) dict -> Dict.insert name state dict)
                            fieldDecoder
                            accDecoder
                    )
                    (JD.succeed Dict.empty)
                |> JD.map Record

        Type.CustomType args ->
            JD.field "tag" JD.string
                |> JD.andThen
                    (\tag ->
                        case List.filter (\v -> v.name == tag) args.variants of
                            [ variant ] ->
                                let
                                    argsDecoder =
                                        variant.args
                                            |> List.indexedMap
                                                (\i argMeta ->
                                                    JD.index i (decoderHelp namedTypes argMeta)
                                                )
                                            |> List.foldl
                                                (\argDecoder accDecoder ->
                                                    JD.map2
                                                        Array.push
                                                        argDecoder
                                                        accDecoder
                                                )
                                                (JD.succeed Array.empty)
                                in
                                JD.field "args" argsDecoder
                                    |> JD.map (Variant tag)

                            _ ->
                                JD.fail ("Unknown variant tag: " ++ tag)
                    )

        Type.Recursive name ->
            case Dict.get name namedTypes of
                Just typeMeta ->
                    decoderHelp namedTypes typeMeta

                Nothing ->
                    JD.fail ("Unknown recursive type: " ++ name)
