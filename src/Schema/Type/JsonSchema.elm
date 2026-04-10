module Schema.Type.JsonSchema exposing (fromType)

{-| Generate a JSON Schema (Draft 7) from a `Schema.Type.Node`

@docs fromType

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Schema.Type as Type exposing (Type)


{-| TODO: document
-}
fromType : String -> Type.Node m -> Value
fromType name meta =
    let
        defs =
            Type.gatherNamed meta
    in
    JE.object
        [ ( "$schema", JE.string "http://json-schema.org/draft-07/schema#" )
        , ( "type", JE.string "json_schema" )
        , ( "name", JE.string name )
        , ( "strict", JE.bool True )
        , ( "schema"
          , ( "$defs"
            , JE.object
                (Dict.toList defs
                    |> List.map
                        (\( typeName, def ) ->
                            ( String.toLower typeName, toJsonSchema_Named def )
                        )
                )
            )
                :: toJsonSchemaHelp meta
                |> JE.object
          )
        ]


toSchemaObject : Type.Node m -> String -> List ( String, Value )
toSchemaObject meta type_ =
    [ ( "type", JE.string type_ )
    ]


toJsonSchemaHelp : Type.Node m -> List ( String, Value )
toJsonSchemaHelp meta =
    case meta.type_ of
        Type.String opts ->
            toSchemaObject meta "string"

        Type.Int ->
            toSchemaObject meta "integer"

        Type.Float ->
            toSchemaObject meta "number"

        Type.Bool ->
            toSchemaObject meta "boolean"

        Type.List itemType ->
            toSchemaObject meta
                "array"
                ++ [ ( "items", toJsonSchemaHelp itemType |> JE.object )
                   ]

        Type.Array itemType ->
            toSchemaObject
                meta
                "array"
                ++ [ ( "items", toJsonSchemaHelp itemType |> JE.object )
                   ]

        Type.Dict keyType valueType ->
            toSchemaObject
                meta
                "array"
                ++ [ ( "items"
                     , JE.object
                        [ ( "k", toJsonSchemaHelp keyType |> JE.object )
                        , ( "v", toJsonSchemaHelp valueType |> JE.object )
                        ]
                     )
                   ]

        Type.Record fields ->
            toSchemaObject
                meta
                "object"
                ++ [ ( "properties"
                     , JE.object
                        (List.map
                            (\( name, fieldType ) -> ( name, toJsonSchemaHelp fieldType |> JE.object ))
                            fields
                        )
                     )
                   , ( "required"
                     , JE.list JE.string (List.map Tuple.first fields)
                     )
                   , ( "additionalProperties", JE.bool False )
                   ]

        Type.Maybe itemType ->
            toJsonSchemaHelp itemType
                |> List.map
                    (\( k, v ) ->
                        if k == "type" then
                            ( k, [ v, JE.string "null" ] |> JE.list identity )

                        else
                            ( k, v )
                    )

        Type.Set itemType ->
            toSchemaObject
                meta
                "array"
                ++ [ ( "items", toJsonSchemaHelp itemType |> JE.object )

                   --, ( "uniqueItems", JE.bool True )
                   ]

        Type.Unit ->
            toSchemaObject meta "null"

        Type.Uuid ->
            toSchemaObject meta
                "string"
                ++ [ ( "format", JE.string "uuid" )
                   ]

        Type.Tuple types ->
            toJsonSchema_Tuple types

        Type.CustomType args ->
            [ ( "$ref", JE.string ("#/$defs/" ++ String.toLower args.name) )
            ]

        Type.Recursive name ->
            [ ( "$ref", JE.string ("#/$defs/" ++ String.toLower name) )
            ]


toJsonSchema_Named : Type.Node m -> Value
toJsonSchema_Named meta =
    case meta.type_ of
        Type.CustomType args ->
            toJsonSchema_Custom args.variants

        _ ->
            JE.object (toJsonSchemaHelp meta)


toJsonSchema_Custom : List { name : String, args : List (Type.Node m) } -> Value
toJsonSchema_Custom variants =
    JE.object
        [ ( "anyOf"
          , JE.list
                (\variant -> toJsonSchema_Variant variant.name variant.args)
                variants
          )
        ]


toJsonSchema_Tag : String -> ( String, Value )
toJsonSchema_Tag name =
    ( "tag"
    , JE.object
        [ ( "type", JE.string "string" )
        , ( "const", JE.string name )
        ]
    )


toJsonSchema_Variant : String -> List (Type.Node m) -> Value
toJsonSchema_Variant name metas =
    JE.object
        [ ( "type", JE.string "object" )
        , ( "properties"
          , toJsonSchema_Tag name
                :: (if List.isEmpty metas then
                        []

                    else
                        [ ( "args", toJsonSchema_Tuple metas |> JE.object )
                        ]
                   )
                |> JE.object
          )
        , ( "required"
          , "tag"
                :: (if List.isEmpty metas then
                        []

                    else
                        [ "args" ]
                   )
                |> JE.list JE.string
          )
        , ( "additionalProperties", JE.bool False )
        ]


toJsonSchema_Tuple : List (Type.Node m) -> List ( String, Value )
toJsonSchema_Tuple metas =
    let
        argsCount =
            List.length metas
    in
    [ ( "type", JE.string "array" )
    , ( "prefixItems", JE.list (toJsonSchemaHelp >> JE.object) metas )
    , ( "items", JE.object [ ( "type", JE.string "null" ) ] )
    , ( "minItems", JE.int argsCount )
    , ( "maxItems", JE.int argsCount )
    ]


gatherDefs : Type.Node m -> Dict String (List { name : String, args : List (Type.Node m) })
gatherDefs meta =
    case meta.type_ of
        Type.Unit ->
            Dict.empty

        Type.String _ ->
            Dict.empty

        Type.Uuid ->
            Dict.empty

        Type.Bool ->
            Dict.empty

        Type.Int ->
            Dict.empty

        Type.Float ->
            Dict.empty

        Type.List itemType ->
            gatherDefs itemType

        Type.Array itemType ->
            gatherDefs itemType

        Type.Maybe itemType ->
            gatherDefs itemType

        Type.Dict keyType valueType ->
            gatherDefs valueType

        Type.Set itemType ->
            gatherDefs itemType

        Type.Tuple types ->
            List.foldl
                (\t acc -> Dict.union acc (gatherDefs t))
                Dict.empty
                types

        Type.Record fields ->
            List.foldl
                (\( _, fieldType ) acc -> Dict.union acc (gatherDefs fieldType))
                Dict.empty
                fields

        Type.CustomType args ->
            List.foldl
                (\variant acc ->
                    Dict.union acc
                        (List.foldl
                            (\arg acc2 -> Dict.union acc2 (gatherDefs arg))
                            Dict.empty
                            variant.args
                        )
                )
                (Dict.singleton args.name args.variants)
                args.variants

        Type.Recursive name ->
            Dict.empty
