module Schema.Type.JsonSchema exposing (fromType, Meta)

{-| Generate a JSON Schema document from a [`Schema.Type.Node`](Schema-Type#Node),
targeting [OpenAI Structured Outputs][openai-so].

The output is a constrained subset of JSON Schema, not plain Draft 7:

  - every object sets `additionalProperties: false` and lists every
    property in `required`
  - every named type (record or custom type) is emitted once under
    `$defs` and referenced via `$ref`, so recursive types are
    representable
  - custom types are encoded as `anyOf` over their variants, each
    variant being an object with a `tag` string `const` and an `args`
    object indexed `"0"`, `"1"`, …
  - `Maybe a` is encoded as `anyOf [a, {"type": "null"}]`
  - `Dict k v` and tuples are encoded as objects (not arrays), since
    OpenAI Structured Outputs does not support JSON Schema's
    positional-array tuples or arbitrary-keyed objects

Typical usage:

    import Schema
    import Schema.Type.JsonSchema

    Schema.toType schema_User
        |> Schema.Type.JsonSchema.fromType "User"
        -- : Json.Encode.Value

The schema you pass in must carry a meta record with at least a `name`
and a `description` field — see [`Meta`](#Meta).

[openai-so]: https://platform.openai.com/docs/guides/structured-outputs

@docs fromType, Meta

-}

import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Schema.Type as Type exposing (Type)


type alias Node m =
    Type.Node (Meta m)


{-| A constraint on the schema's meta record: it must carry at least a
`name` and a `description` string. When non-empty these become the
`title` and `description` of the corresponding JSON Schema object.

You can thread any extra fields through the `m` extension — they are
ignored by this module.

A minimal meta type looks like:

    type alias Meta =
        { name : String
        , description : String
        }

-}
type alias Meta m =
    { m
        | name : String
        , description : String
    }


{-| Generate a JSON Schema document from a type node.

The first argument is reserved for the root type's name; it is not
currently written to the output. The top-level `title` and
`description`, when present, are taken from the node's meta record.

All named types reachable from the root (records and custom types) are
gathered into a `$defs` block at the top level and referenced via
`$ref` everywhere else — including the root if it is itself a named
type — so recursive types work out of the box.

-}
fromType : String -> Node m -> Value
fromType name node =
    let
        defs =
            Type.gatherNamed node
    in
    ( "$defs"
    , JE.object
        (Dict.toList defs
            |> List.map
                (\( typeName, def ) ->
                    ( String.toLower typeName, toJsonSchema_Named def )
                )
        )
    )
        :: toJsonSchemaHelp node
        |> JE.object


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
          , JE.list toJsonSchema_Variant variants
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


toJsonSchema_Variant : Type.CustomType_Variant (Meta m) -> Value
toJsonSchema_Variant variant =
    JE.object
        ([ ( "type", JE.string "object" )
         , ( "properties"
           , JE.object
                [ toJsonSchema_Tag variant.name
                , ( "args", toJsonSchema_Tuple variant.args |> JE.object )
                ]
           )
         , ( "required"
           , JE.list JE.string [ "tag", "args" ]
           )
         , ( "additionalProperties", JE.bool False )
         ]
            ++ metaFields variant.meta
        )


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
