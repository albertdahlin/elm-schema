module Schema.Type.JsonSchema exposing (fromType, Meta)

{-| Generate a JSON Schema for OpenAI Structured Output from a `Schema.Type.Node`

@docs fromType, Meta

-}

import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Schema.Type as Type exposing (Type)


type alias Node m =
    Type.Node (Meta m)


type alias Meta m =
    { m
        | name : String
        , description : String
    }


{-| TODO: document
-}
fromType : String -> Node m -> Value
fromType name meta =
    let
        defs =
            Type.gatherNamed meta
    in
    JE.object
        [ ( "type", JE.string "json_schema" )
        , ( "json_schema"
          , JE.object
                [ ( "name", JE.string name )
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
          )
        ]


toSchemaObject : Node m -> String -> List ( String, Value )
toSchemaObject node type_ =
    ( "type", JE.string type_ )
        :: metaFields node.meta


metaFields : Meta m -> List ( String, Value )
metaFields meta =
    (if String.isEmpty meta.name then
        []

     else
        [ ( "title", JE.string meta.name ) ]
    )
        ++ (if String.isEmpty meta.description then
                []

            else
                [ ( "description", JE.string meta.description ) ]
           )


toJsonSchemaHelp : Node m -> List ( String, Value )
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
                        [ ( "type", JE.string "object" )
                        , ( "properties"
                          , JE.object
                                [ ( "k", toJsonSchemaHelp keyType |> JE.object )
                                , ( "v", toJsonSchemaHelp valueType |> JE.object )
                                ]
                          )
                        , ( "required", JE.list JE.string [ "k", "v" ] )
                        , ( "additionalProperties", JE.bool False )
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
            [ ( "anyOf"
              , JE.list identity
                    [ JE.object (toJsonSchemaHelp itemType)
                    , JE.object [ ( "type", JE.string "null" ) ]
                    ]
              )
            ]

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


toJsonSchema_Named : Node m -> Value
toJsonSchema_Named meta =
    case meta.type_ of
        Type.CustomType args ->
            toJsonSchema_Custom args.variants

        _ ->
            JE.object (toJsonSchemaHelp meta)


toJsonSchema_Custom : List (Type.CustomType_Variant (Meta m)) -> Value
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


toJsonSchema_Variant : String -> List (Node m) -> Value
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


toJsonSchema_Tuple : List (Node m) -> List ( String, Value )
toJsonSchema_Tuple nodes =
    [ ( "type", JE.string "object" )
    , ( "properties"
      , JE.object
            (List.indexedMap
                (\i node -> ( String.fromInt i, toJsonSchemaHelp node |> JE.object ))
                nodes
            )
      )
    , ( "required"
      , JE.list JE.string
            (List.indexedMap (\i _ -> String.fromInt i) nodes)
      )
    , ( "additionalProperties", JE.bool False )
    ]
