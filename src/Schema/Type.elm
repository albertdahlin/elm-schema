module Schema.Type exposing
    ( Node, Type(..), CustomType_Variant
    , StringOptions, Sanitize(..), defaultStringOpts, sanitize, sanitizeAll
    , gatherNamed, isRecursive
    )

{-| A plain data structure describing the shape of a type defined with
[`Schema`](Schema). Use [`Schema.toType`](Schema#toType) to obtain a `Node`
from a `Schema`, then walk the tree to introspect, render, or transform it.

This module is consumed by:

  - [`Schema.Type.JsonSchema`](Schema-Type-JsonSchema) — generates JSON
    Schema (Draft 7) from a `Node`.
  - [`Schema.Type.Value`](Schema-Type-Value) — a generic `Value` union
    with its own encoder/decoder, useful for type-agnostic data
    manipulation such as form editing.
  - [`Schema.Fuzzer`](Schema-Fuzzer) — generates JSON fuzzers from a
    `Node`.

The `m` type parameter is the _meta_ value attached to every node. It is
whatever type you passed when building the schema (typically a record
containing a label, description, etc.) and is propagated unchanged into
the `Node` tree.


## Nodes and Types

@docs Node, Type, CustomType_Variant


## Strings

@docs StringOptions, Sanitize, defaultStringOpts, sanitize, sanitizeAll


## Traversal helpers

@docs gatherNamed, isRecursive

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| The shape of a type, one level deep. Nested types are represented as
child [`Node`](#Node)s so that each level can carry its own meta.

  - `Unit` — the `()` type (also used for `Schema.const` and `Schema.null`).
  - `String` — a `String`, optionally annotated with [`StringOptions`](#StringOptions).
  - `Uuid` — a `String` that has been marked as a UUID.
  - `Bool`, `Int`, `Float` — the corresponding primitive types.
  - `List`, `Array`, `Maybe`, `Set` — a container holding the given item type.
  - `Dict` — a dictionary with the given key and value types.
  - `Tuple` — a tuple/triple. The list holds the element types in order.
  - `Record` — a record. Each entry is a `(fieldName, fieldType)` pair.
  - `CustomType` — a custom (tagged union) type. See [`CustomType_Variant`](#CustomType_Variant).
  - `Recursive` — a back-reference to an enclosing custom type by name,
    used to break cycles introduced by [`Schema.lazy`](Schema#lazy).
    To resolve the reference, look up `name` in the `Dict` returned by
    [`gatherNamed`](#gatherNamed).

-}
type Type m
    = Unit
    | String (Maybe StringOptions)
    | Uuid
    | Bool
    | Int
    | Float
    | List (Node m)
    | Array (Node m)
    | Maybe (Node m)
    | Dict (Node m) (Node m)
    | Set (Node m)
    | Tuple (List (Node m))
    | Record (List ( String, Node m ))
    | CustomType (CustomType_Args m)
    | Recursive String


type alias CustomType_Args m =
    { name : String
    , variants : List (CustomType_Variant m)
    }


{-| One variant of a custom type.

  - `name` — the variant's tag (e.g. `"Just"`, `"Nothing"`).
  - `meta` — the meta value attached to the variant itself (distinct
    from the meta on each argument).
  - `args` — the variant's arguments, in declaration order. An empty
    list means a zero-argument variant like `Nothing`.

-}
type alias CustomType_Variant m =
    { name : String
    , meta : m
    , args : List (Node m)
    }


{-| A type paired with the meta value you supplied when building the
schema. `Node` is the currency of this module: every nested position in
a `Type` is itself a `Node`, so each level carries its own meta.
-}
type alias Node m =
    { type_ : Type m
    , meta : m
    }


{-| Extra constraints and hints carried on a `String` node. Produced by
[`Schema.stringWith`](Schema#stringWith) and consumed by downstream
tools — for example, `maxLength`/`minLength` become JSON Schema
constraints, and `sanitize` is applied by the schema's encoder and
decoder.

  - `multiline` — hint that the value may contain newlines (useful for
    rendering a `<textarea>` vs `<input>`).
  - `maxLength` / `minLength` — length bounds, if any.
  - `translatable` — hint that the value is user-facing text eligible
    for translation.
  - `sensitive` — hint that the value should be masked in logs, forms,
    or diagnostics (passwords, tokens, …).
  - `sanitize` — normalization steps applied both when encoding and
    decoding. See [`Sanitize`](#Sanitize).

Build one by starting from [`defaultStringOpts`](#defaultStringOpts)
and overriding the fields you care about.

-}
type alias StringOptions =
    { multiline : Bool
    , maxLength : Maybe Int
    , minLength : Maybe Int
    , translatable : Bool
    , sensitive : Bool
    , sanitize : List Sanitize
    }


{-| A `StringOptions` with every flag off, no length bounds, and no
sanitization. Use this as the starting point when calling
[`Schema.stringWith`](Schema#stringWith):

    Schema.stringWith ()
        { Schema.Type.defaultStringOpts
            | maxLength = Just 80
            , sanitize = [ Schema.Type.Trim ]
        }

-}
defaultStringOpts : StringOptions
defaultStringOpts =
    { multiline = False
    , maxLength = Nothing
    , minLength = Nothing
    , translatable = False
    , sensitive = False
    , sanitize = []
    }


{-| A single string-normalization step.

  - `Trim` — remove leading and trailing whitespace (`String.trim`).
  - `Lowercase` — convert to lower case (`String.toLower`).
  - `Uppercase` — convert to upper case (`String.toUpper`).

When listed in [`StringOptions.sanitize`](#StringOptions), the ops are
applied in order on both encode and decode, so the stored and in-memory
representations stay in sync.

-}
type Sanitize
    = Trim
    | Lowercase
    | Uppercase


{-| Apply a single [`Sanitize`](#Sanitize) operation to a string.

    sanitize Trim "  hi  " --> "hi"

-}
sanitize : Sanitize -> String -> String
sanitize op str =
    case op of
        Trim ->
            String.trim str

        Lowercase ->
            String.toLower str

        Uppercase ->
            String.toUpper str


{-| Apply a list of [`Sanitize`](#Sanitize) operations in order. This is
the function `Schema.stringWith` uses internally on the value flowing
through its encoder and decoder.

    sanitizeAll [ Trim, Lowercase ] "  Hello  " --> "hello"

-}
sanitizeAll : List Sanitize -> String -> String
sanitizeAll ops str =
    List.foldl sanitize str ops


{-| Walk a `Node` and collect every `CustomType` encountered, keyed by
its name. This is the companion lookup table for
[`Recursive`](#Type) references — when you hit a `Recursive name` while
traversing, look the name up in this dict to recover the original
`Node`.

Typical use, when writing your own consumer of the type tree:

    let
        named =
            gatherNamed rootNode
    in
    traverse named rootNode

Both `Schema.Type.JsonSchema` and `Schema.Fuzzer` use this pattern.

-}
gatherNamed : Node m -> Dict String (Node m)
gatherNamed node =
    case node.type_ of
        Unit ->
            Dict.empty

        String _ ->
            Dict.empty

        Uuid ->
            Dict.empty

        Bool ->
            Dict.empty

        Int ->
            Dict.empty

        Float ->
            Dict.empty

        List itemType ->
            gatherNamed itemType

        Array itemType ->
            gatherNamed itemType

        Maybe itemType ->
            gatherNamed itemType

        Dict keyType valueType ->
            gatherNamed valueType

        Set itemType ->
            gatherNamed itemType

        Tuple types ->
            List.foldl
                (\t acc -> Dict.union acc (gatherNamed t))
                Dict.empty
                types

        Record fields ->
            List.foldl
                (\( _, fieldType ) acc -> Dict.union acc (gatherNamed fieldType))
                Dict.empty
                fields

        CustomType args ->
            List.foldl
                (\variant acc ->
                    Dict.union acc
                        (List.foldl
                            (\arg acc2 -> Dict.union acc2 (gatherNamed arg))
                            Dict.empty
                            variant.args
                        )
                )
                (Dict.singleton args.name node)
                args.variants

        Recursive name ->
            Dict.empty


{-| `True` when `node` is a custom type that references itself anywhere
inside one of its variants (directly or through containers like `List`,
`Maybe`, records, other custom types, …). For any non-custom-type node
this returns `False`.

Useful when rendering or generating code for a type and you need to
decide whether to emit a forward declaration, `Schema.lazy`, or a
JSON Schema `$ref`.

-}
isRecursive : Node m -> Bool
isRecursive node =
    case node.type_ of
        CustomType args ->
            List.any
                (.args
                    >> List.any (isRecursiveHelp args.name)
                )
                args.variants

        _ ->
            False


isRecursiveHelp : String -> Node m -> Bool
isRecursiveHelp name node =
    case node.type_ of
        Unit ->
            False

        String _ ->
            False

        Uuid ->
            False

        Bool ->
            False

        Int ->
            False

        Float ->
            False

        List itemType ->
            isRecursiveHelp name itemType

        Array itemType ->
            isRecursiveHelp name itemType

        Maybe itemType ->
            isRecursiveHelp name itemType

        Dict keyType valueType ->
            isRecursiveHelp name valueType

        Set itemType ->
            isRecursiveHelp name itemType

        Tuple types ->
            List.any (isRecursiveHelp name) types

        Record fields ->
            List.any (isRecursiveHelp name << Tuple.second) fields

        CustomType args ->
            List.any (List.any (isRecursiveHelp name) << .args) args.variants

        Recursive n ->
            n == name



-- JSON Encoding


typeName : Node m -> String
typeName node =
    case node.type_ of
        Unit ->
            "Unit"

        String _ ->
            "String"

        Uuid ->
            "Uuid"

        Bool ->
            "Bool"

        Int ->
            "Int"

        Float ->
            "Float"

        List t ->
            "List (" ++ typeName t ++ ")"

        Array t ->
            "Array (" ++ typeName t ++ ")"

        Maybe t ->
            "Maybe (" ++ typeName t ++ ")"

        Dict k v ->
            "Dict (" ++ typeName k ++ ") (" ++ typeName v ++ ")"

        Set t ->
            "Set (" ++ typeName t ++ ")"

        Tuple ts ->
            "( " ++ String.join ", " (List.map typeName ts) ++ " )"

        Record fields ->
            List.map (\( n, t ) -> n ++ " : " ++ typeName t) fields
                |> String.join ", "
                |> (\s -> "{ " ++ s ++ " }")

        CustomType args ->
            args.name

        Recursive name ->
            name


encode : Node m -> Value
encode node =
    case node.type_ of
        Unit ->
            JE.object
                [ ( "type", JE.string "Unit" )
                ]

        String opts ->
            JE.object
                [ ( "type", JE.string "String" )
                ]

        Uuid ->
            JE.object
                [ ( "type", JE.string "Uuid" )
                ]

        Bool ->
            JE.object
                [ ( "type", JE.string "Bool" )
                ]

        Int ->
            JE.object
                [ ( "type", JE.string "Int" )
                ]

        Float ->
            JE.object
                [ ( "type", JE.string "Float" )
                ]

        List itemType ->
            JE.object
                [ ( "type", JE.string "List" )
                , ( "itemType", encode itemType )
                ]

        Array itemType ->
            JE.object
                [ ( "type", JE.string "Array" )
                , ( "itemType", encode itemType )
                ]

        Maybe itemType ->
            JE.object
                [ ( "type", JE.string "Maybe" )
                , ( "itemType", encode itemType )
                ]

        Dict keyType valueType ->
            JE.object
                [ ( "type", JE.string "Dict" )
                , ( "keyType", encode keyType )
                , ( "valueType", encode valueType )
                ]

        Set itemType ->
            JE.object
                [ ( "type", JE.string "Set" )
                , ( "itemType", encode itemType )
                ]

        Tuple types ->
            JE.object
                [ ( "type", JE.string "Tuple" )
                , ( "args", JE.list encode types )
                ]

        Record fields ->
            JE.object
                [ ( "type", JE.string "Record" )
                , ( "fields"
                  , JE.list
                        (\field ->
                            JE.object
                                [ ( "name", JE.string (Tuple.first field) )
                                , ( "type", encode (Tuple.second field) )
                                ]
                        )
                        fields
                  )
                ]

        CustomType args ->
            JE.object
                [ ( "type", JE.string "CustomType" )
                , ( "name", JE.string args.name )
                , ( "args"
                  , JE.list
                        (\arg ->
                            JE.object
                                [ ( "name", JE.string arg.name )
                                , ( "args", JE.list encode arg.args )
                                ]
                        )
                        args.variants
                  )
                ]

        Recursive name ->
            JE.object
                [ ( "type", JE.string "Recursive" )
                , ( "name", JE.string name )
                ]


encode_Sanitize : Sanitize -> Value
encode_Sanitize op =
    case op of
        Trim ->
            JE.string "Trim"

        Lowercase ->
            JE.string "Lowercase"

        Uppercase ->
            JE.string "Uppercase"
