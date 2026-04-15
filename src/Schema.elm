module Schema exposing
    ( Schema
    , string, stringWith, uuid, bool, int, float, null, const
    , maybe, list, array, dict, set, pair, triple
    , RecordBuilder, record, field, buildRecord
    , CustomBuilder, custom, variant0, variant1, variant2, variant3, buildCustom
    , lazy
    , toType
    , encode, decoder
    , newVersion, dropVersions
    )

{-| Define a schema for an Elm type and get a JSON encoder, decoder,
and introspectable type description from a single value. See the
[README][readme] for an overview.

Every builder takes a _meta_ value as its first argument. This is
whatever you want to attach to that node — a label, a description,
or a record of form-hint fields — and it flows unchanged into the
[`Schema.Type.Node`](Schema-Type#Node) tree returned by
[`toType`](#toType). If you don't need it, pass `()`.

[readme]: https://package.elm-lang.org/packages/albertdahlin/elm-schema/latest/

@docs Schema


## Scalar Types

@docs string, stringWith, uuid, bool, int, float, null, const


## Composite Types

@docs maybe, list, array, dict, set, pair, triple


## Records

    type alias Record =
        { id : String
        , value : Int
        }

    schema_Record : Schema () Record
    schema_Record =
        Schema.record Record
            |> Schema.field "id" .id (Schema.string ())
            |> Schema.field "value" .value (Schema.int ())
            |> Schema.buildRecord ()

@docs RecordBuilder, record, field, buildRecord


## Custom Types

    type CustomType
        = Zero
        | One String
        | Two String Int
        | Three String Int Float

    schema_CustomType : Schema () CustomType
    schema_CustomType =
        Schema.custom "CustomType"
            (\zero toOne toTwo toThree value ->
                case value of
                    Zero ->
                        zero

                    One str ->
                        toOne str

                    Two str i ->
                        toTwo str i

                    Three str i f ->
                        toThree str i f
            )
            |> Schema.variant0 "Zero" () Zero
            |> Schema.variant1 "One" () One (Schema.string ())
            |> Schema.variant2 "Two" () Two (Schema.string ()) (Schema.int ())
            |> Schema.variant3 "Three" () Three (Schema.string ()) (Schema.int ()) (Schema.float ())
            |> Schema.buildCustom ()

@docs CustomBuilder, custom, variant0, variant1, variant2, variant3, buildCustom


## Recursive Types

    type Tree a
        = Node a (List (Tree a))

    schema_Tree : String -> Schema () a -> Schema () (Tree a)
    schema_Tree typeName nodeSchema =
        Schema.custom typeName
            (\nodeConstructor value ->
                case value of
                    Node a children ->
                        nodeConstructor a children
            )
            |> Schema.variant2 "Node"
                ()
                Node
                nodeSchema
                (Schema.list ()
                    (Schema.lazy () typeName
                        (\_ ->
                            schema_Tree
                                typeName
                                nodeSchema
                        )
                    )
                )
            |> Schema.buildCustom ()


### Important!

The `typeName` passed to `Schema.lazy` must match the one
passed to `Schema.custom`. That's how recursion is detected
when generating type descriptions.
In this case we have a parametrized recursive type (`Tree a`), the
name must be different for each instantiation of `a`.

    schema_TreeInt : Schema () (Tree Int)
    schema_TreeInt =
        schema_Tree "Tree_Int" (Schema.int ())

    schema_TreeString : Schema () (Tree String)
    schema_TreeString =
        schema_Tree "Tree_String" (Schema.string ())

@docs lazy


## Type Description

@docs toType


## JSON Encode / Decode

@docs encode, decoder


## Versioning

See the [README][readme] for the wire format and decode dispatch
rules.

@docs newVersion, dropVersions

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Schema.Type as ExtType
import Schema.Type.JsonSchema as JsonSchema
import Set exposing (Set)



-- SCHEMA


type Type m
    = Unit {- null -}
    | String (Maybe ExtType.StringOptions)
    | Uuid
    | Bool
    | Int
    | Float
    | List (Meta m)
    | Array (Meta m)
    | Maybe (Meta m)
    | Dict (Meta m) (Meta m)
    | Set (Meta m)
    | Tuple (List (Meta m))
    | Record (List ( String, Meta m ))
    | Custom String (List { name : String, meta : m, args : List (Meta m) })
    | Lazy String (() -> Meta m)


type alias Meta m =
    { type_ : Type m
    , meta : m
    }


emptyMeta : m -> Type m -> Meta m
emptyMeta m type_ =
    { type_ = type_
    , meta = m
    }


{-| A schema for Elm values of type `a`, carrying meta values of type
`m` on every node. Build one with the functions below and consume it
with [`encode`](#encode), [`decoder`](#decoder), or
[`toType`](#toType).
-}
type Schema m a
    = Schema
        { meta : Meta m
        , decoder : Decoder a
        , version : Maybe (Version a)
        , encode : a -> Value
        }


type alias Version a =
    { tag : String
    , taggedDecoders : Dict String (Decoder a)
    , untaggedDecoder : Decoder a
    }



-- SCALAR TYPES


{-| A `()` schema. Encodes to JSON `null`; decodes any JSON value as `()`.
-}
null : m -> Schema m ()
null m =
    Schema
        { meta = emptyMeta m Unit
        , decoder = Decode.succeed ()
        , version = Nothing
        , encode = always Encode.null
        }


{-| A schema that carries a fixed value. Encodes to JSON `null`;
decodes any JSON value as the given constant. Useful as a payload for
variants or fields that have no runtime data.
-}
const : m -> a -> Schema m a
const m a =
    Schema
        { meta = emptyMeta m Unit
        , decoder = Decode.succeed a
        , version = Nothing
        , encode = always Encode.null
        }


{-| A basic string schema. Use `stringWith` to customize options.
-}
string : m -> Schema m String
string m =
    Schema
        { meta = emptyMeta m (String Nothing)
        , decoder = Decode.string
        , version = Nothing
        , encode = Encode.string
        }


{-| A customizable string schema.
-}
stringWith : m -> ExtType.StringOptions -> Schema m String
stringWith m opts =
    Schema
        { meta = emptyMeta m (String (Just opts))
        , decoder = Decode.string |> Decode.map (ExtType.sanitizeAll opts.sanitize)
        , version = Nothing
        , encode = ExtType.sanitizeAll opts.sanitize >> Encode.string
        }


{-| A string schema tagged as a UUID. Encodes and decodes like
[`string`](#string), but downstream tooling treats it specially —
for example, the JSON Schema generator emits `"format": "uuid"`.
-}
uuid : m -> Schema m String
uuid m =
    Schema
        { meta = emptyMeta m Uuid
        , decoder = Decode.string
        , version = Nothing
        , encode = Encode.string
        }


{-| A `Bool` schema, backed by JSON `true`/`false`.
-}
bool : m -> Schema m Bool
bool m =
    Schema
        { meta = emptyMeta m Bool
        , decoder = Decode.bool
        , version = Nothing
        , encode = Encode.bool
        }


{-| An `Int` schema, backed by JSON numbers.
-}
int : m -> Schema m Int
int m =
    Schema
        { meta = emptyMeta m Int
        , decoder = Decode.int
        , version = Nothing
        , encode = Encode.int
        }


{-| Does not support NaN or Infinity.
-}
float : m -> Schema m Float
float m =
    Schema
        { meta = emptyMeta m Float
        , decoder = Decode.float
        , version = Nothing
        , encode = Encode.float
        }



-- COMPOSITE TYPES


{-| A `List` of values, encoded as a JSON array.
-}
list : m -> Schema m a -> Schema m (List a)
list m s =
    Schema
        { meta = emptyMeta m (List (unwrapType s))
        , decoder = Decode.list (decoder s)
        , version = Nothing
        , encode = Encode.list (encode s)
        }


{-| An `Array` of values, encoded as a JSON array.
-}
array : m -> Schema m a -> Schema m (Array a)
array m s =
    Schema
        { meta = emptyMeta m (List (unwrapType s))
        , decoder = Decode.array (decoder s)
        , version = Nothing
        , encode = Encode.array (encode s)
        }


{-| An optional value. Encodes `Nothing` as JSON `null` and `Just x`
as the inner schema's encoding of `x`.
-}
maybe : m -> Schema m a -> Schema m (Maybe a)
maybe m s =
    Schema
        { meta = emptyMeta m (Maybe (unwrapType s))
        , decoder = Decode.nullable (decoder s)
        , version = Nothing
        , encode =
            \v ->
                case v of
                    Just value ->
                        encode s value

                    Nothing ->
                        Encode.null
        }


{-| A 2-tuple. Encodes as a JSON array `[a, b]`. The decoder also
accepts an object with keys `"0"` and `"1"` — the shape produced by
[`Schema.Type.JsonSchema`](Schema-Type-JsonSchema).
-}
pair : m -> Schema m a -> Schema m b -> Schema m ( a, b )
pair m sa sb =
    Schema
        { meta = emptyMeta m (Tuple [ unwrapType sa, unwrapType sb ])
        , decoder =
            Decode.oneOf
                [ Decode.map2 Tuple.pair
                    (Decode.index 0 (decoder sa))
                    (Decode.index 1 (decoder sb))
                , Decode.map2 Tuple.pair
                    (Decode.field "0" (decoder sa))
                    (Decode.field "1" (decoder sb))
                ]
        , version = Nothing
        , encode =
            \( a, b ) ->
                Encode.list identity
                    [ encode sa a
                    , encode sb b
                    ]
        }


{-| A 3-tuple. Encodes as a JSON array `[a, b, c]`. Like
[`pair`](#pair), the decoder also accepts the object form with keys
`"0"`, `"1"`, `"2"`.
-}
triple :
    m
    -> Schema m a
    -> Schema m b
    -> Schema m c
    -> Schema m ( a, b, c )
triple m sa sb sc =
    Schema
        { meta = emptyMeta m (Tuple [ unwrapType sa, unwrapType sb, unwrapType sc ])
        , decoder =
            Decode.oneOf
                [ Decode.map3 (\a b c -> ( a, b, c ))
                    (Decode.index 0 (decoder sa))
                    (Decode.index 1 (decoder sb))
                    (Decode.index 2 (decoder sc))
                , Decode.map3 (\a b c -> ( a, b, c ))
                    (Decode.field "0" (decoder sa))
                    (Decode.field "1" (decoder sb))
                    (Decode.field "2" (decoder sc))
                ]
        , version = Nothing
        , encode =
            \( a, b, c ) ->
                Encode.list identity
                    [ encode sa a
                    , encode sb b
                    , encode sc c
                    ]
        }


{-| A dictionary. Encoded as a JSON array of `{"k": …, "v": …}`
objects rather than an object, so keys are not restricted to strings.
-}
dict : m -> Schema m comparable -> Schema m v -> Schema m (Dict comparable v)
dict m sk sv =
    Schema
        { meta = emptyMeta m (Dict (unwrapType sk) (unwrapType sv))
        , decoder =
            Decode.map2 Tuple.pair
                (Decode.field "k" (decoder sk))
                (Decode.field "v" (decoder sv))
                |> Decode.list
                |> Decode.map Dict.fromList
        , version = Nothing
        , encode =
            Dict.toList
                >> Encode.list
                    (\( k, v ) ->
                        Encode.object
                            [ ( "k", encode sk k )
                            , ( "v", encode sv v )
                            ]
                    )
        }


{-| A `Set`, encoded as a JSON array. Duplicates and ordering are
collapsed by `Set.fromList` on decode.
-}
set : m -> Schema m comparable -> Schema m (Set comparable)
set m s =
    Schema
        { meta = emptyMeta m (Set (unwrapType s))
        , decoder = Decode.list (decoder s) |> Decode.map Set.fromList
        , version = Nothing
        , encode = Set.toList >> Encode.list (encode s)
        }



-- RECORDS


{-| Intermediate builder produced by [`record`](#record). Chain
[`field`](#field) calls to describe each field, then close with
[`buildRecord`](#buildRecord).
-}
type RecordBuilder m constructor record
    = RecordBuilder
        { fields : List ( String, Meta m )
        , decoder : Decoder constructor
        , encode : record -> List ( String, Value )
        }


{-| Start a record schema from your type alias constructor. Chain
[`field`](#field) for each field in declaration order and finish
with [`buildRecord`](#buildRecord).

    Schema.record User
        |> Schema.field "id" .id (Schema.string ())
        |> Schema.field "age" .age (Schema.int ())
        |> Schema.buildRecord ()

-}
record : constructor -> RecordBuilder m constructor record
record ctor =
    RecordBuilder
        { fields = []
        , decoder = Decode.succeed ctor
        , encode = \_ -> []
        }


{-| Add a field to a record builder. Arguments, in order: the JSON
field name, a getter from your record, and the schema for the field's
type.
-}
field :
    String
    -> (record -> field)
    -> Schema m field
    -> RecordBuilder m (field -> constructor) record
    -> RecordBuilder m constructor record
field name get s (RecordBuilder r) =
    RecordBuilder
        { fields = ( name, unwrapType s ) :: r.fields
        , decoder =
            Decode.map2
                (\value rec -> rec value)
                (Decode.field name (decoder s))
                r.decoder
        , encode =
            \entity ->
                ( name, encode s (get entity) ) :: r.encode entity
        }


{-| Close a record builder. The first argument is the meta value
attached to the record itself (distinct from each field's meta).
-}
buildRecord : m -> RecordBuilder m a a -> Schema m a
buildRecord m (RecordBuilder r) =
    Schema
        { meta = emptyMeta m (Record (List.reverse r.fields))
        , decoder = r.decoder
        , version = Nothing
        , encode = \entity -> Encode.object (r.encode entity |> List.reverse)
        }



-- BUILD CUSTOM TYPES


{-| Intermediate builder produced by [`custom`](#custom). Chain
`variantN` calls for each constructor, then close with
[`buildCustom`](#buildCustom).
-}
type CustomBuilder m match t
    = CustomBuilder
        { ctor : List { name : String, meta : m, args : List (Meta m) }
        , decoder : Dict String (Decoder t)
        , match : match
        , name : String
        }


{-| Start a custom type schema.

The `String` is the type's name. It appears in
[`Schema.Type`](Schema-Type) output and — crucially — is how
[`lazy`](#lazy) detects recursion, so it must match the name used in
any `Schema.lazy` call that refers to this type.

The second argument is a _match_ function: it receives one
constructor function per variant (in the same order `variantN` calls
are chained) followed by a value of your type, and returns a
`Json.Encode.Value`. Pattern-match on the value and forward each
variant's arguments to its corresponding constructor.

See the module header for a full example.
-}
custom : String -> match -> CustomBuilder m match t
custom name match =
    CustomBuilder
        { ctor = []
        , decoder = Dict.empty
        , match = match
        , name = name
        }


variant name m match decoderArgs args (CustomBuilder c) =
    let
        enc v =
            Encode.object
                [ ( "tag", Encode.string name )
                , ( "args", Encode.list identity v )
                ]
    in
    CustomBuilder
        { ctor = { name = name, meta = m, args = args } :: c.ctor
        , decoder =
            Dict.insert name
                decoderArgs
                c.decoder
        , match = c.match <| match enc
        , name = c.name
        }


{-| A zero-argument variant. Arguments: the tag name, the variant's
meta, and the constructor value itself.
-}
variant0 :
    String
    -> m
    -> v
    -> CustomBuilder m (Value -> a) v
    -> CustomBuilder m a v
variant0 name m val =
    variant
        name
        m
        (\c -> c [])
        (Decode.succeed val)
        []


{-| A one-argument variant. Arguments: the tag name, the variant's
meta, the constructor function, and a schema for the argument.
-}
variant1 :
    String
    -> m
    -> (a -> v)
    -> Schema m a
    -> CustomBuilder m ((a -> Value) -> b) v
    -> CustomBuilder m b v
variant1 name m ctor s =
    variant
        name
        m
        (\c a ->
            c
                [ encode s a
                ]
        )
        (Decode.oneOf
            [ Decode.map ctor (Decode.index 0 (decoder s))
            , Decode.map ctor (Decode.field "0" (decoder s))
            ]
        )
        [ unwrapType s ]


{-| A two-argument variant. Arguments: the tag name, the variant's
meta, the constructor, and a schema for each argument in order.
-}
variant2 :
    String
    -> m
    -> (a -> b -> v)
    -> Schema m a
    -> Schema m b
    -> CustomBuilder m ((a -> b -> Value) -> c) v
    -> CustomBuilder m c v
variant2 name m ctor sa sb =
    variant
        name
        m
        (\c a b ->
            c
                [ encode sa a
                , encode sb b
                ]
        )
        (Decode.oneOf
            [ Decode.map2 ctor
                (Decode.index 0 (decoder sa))
                (Decode.index 1 (decoder sb))
            , Decode.map2 ctor
                (Decode.field "0" (decoder sa))
                (Decode.field "1" (decoder sb))
            ]
        )
        [ unwrapType sa
        , unwrapType sb
        ]


{-| A three-argument variant. Arguments: the tag name, the variant's
meta, the constructor, and a schema for each argument in order.
-}
variant3 :
    String
    -> m
    -> (a -> b -> c -> v)
    -> Schema m a
    -> Schema m b
    -> Schema m c
    -> CustomBuilder m ((a -> b -> c -> Value) -> d) v
    -> CustomBuilder m d v
variant3 name m ctor sa sb sc =
    variant
        name
        m
        (\c a b d ->
            c
                [ encode sa a
                , encode sb b
                , encode sc d
                ]
        )
        (Decode.oneOf
            [ Decode.map3 ctor
                (Decode.index 0 (decoder sa))
                (Decode.index 1 (decoder sb))
                (Decode.index 2 (decoder sc))
            , Decode.map3 ctor
                (Decode.field "0" (decoder sa))
                (Decode.field "1" (decoder sb))
                (Decode.field "2" (decoder sc))
            ]
        )
        [ unwrapType sa
        , unwrapType sb
        , unwrapType sc
        ]


{-| Close a custom type builder. The first argument is the meta value
for the type itself.

Values encode as `{"tag": "<variantName>", "args": [a, b, …]}`.
On decode, `args` may be either a JSON array or an object indexed
`"0"`, `"1"`, … (the shape `Schema.Type.JsonSchema` emits). The
decoder dispatches on `"tag"` and fails with `"Unknown variant:
<tag>"` for unrecognised tags.
-}
buildCustom : m -> CustomBuilder m (a -> Value) a -> Schema m a
buildCustom m (CustomBuilder c) =
    Schema
        { meta = emptyMeta m (Custom c.name (List.reverse c.ctor))
        , decoder =
            Decode.field "tag" Decode.string
                |> Decode.andThen
                    (\tag ->
                        case Dict.get tag c.decoder of
                            Just dec ->
                                Decode.field "args" dec

                            Nothing ->
                                Decode.fail ("Unknown variant: " ++ tag)
                    )
        , version = Nothing
        , encode = \v -> c.match v
        }



-- TYPE DESCRIPTION


{-| Extract the type description as a
[`Schema.Type.Node`](Schema-Type#Node). Feed it to
[`Schema.Type.JsonSchema.fromType`](Schema-Type-JsonSchema#fromType)
for a JSON Schema document, or to
[`Schema.Fuzzer.fromType`](Schema-Fuzzer#fromType) for a fuzzer.

[`lazy`](#lazy) references are resolved into
[`Schema.Type.Recursive`](Schema-Type#Type) nodes when they close a
cycle, so the returned tree is always finite.
-}
toType : Schema m a -> ExtType.Node m
toType (Schema s) =
    toExternalType Set.empty s.meta


toExtMeta : Meta m -> ExtType.Type m -> ExtType.Node m
toExtMeta meta type_ =
    { type_ = type_
    , meta = meta.meta
    }


toExternalType : Set String -> Meta m -> ExtType.Node m
toExternalType namesSeen meta =
    case meta.type_ of
        Custom name vs ->
            let
                seen =
                    Set.insert name namesSeen
            in
            { name = name
            , variants =
                List.map
                    (\v ->
                        { args = List.map (toExternalType seen) v.args
                        , name = v.name
                        , meta = v.meta
                        }
                    )
                    vs
            }
                |> ExtType.CustomType
                |> toExtMeta meta

        Lazy name next ->
            if Set.member name namesSeen then
                ExtType.Recursive name
                    |> toExtMeta meta

            else
                toExternalType namesSeen (next ())

        List t1 ->
            ExtType.List (toExternalType namesSeen t1)
                |> toExtMeta meta

        Array t1 ->
            ExtType.Array (toExternalType namesSeen t1)
                |> toExtMeta meta

        Maybe t1 ->
            ExtType.Maybe (toExternalType namesSeen t1)
                |> toExtMeta meta

        Dict k v ->
            ExtType.Dict
                (toExternalType namesSeen k)
                (toExternalType namesSeen v)
                |> toExtMeta meta

        Set t1 ->
            ExtType.Set (toExternalType namesSeen t1)
                |> toExtMeta meta

        Tuple types ->
            List.map (toExternalType namesSeen) types
                |> ExtType.Tuple
                |> toExtMeta meta

        Record fields ->
            ExtType.Record
                (List.map
                    (\( name, t1 ) -> ( name, toExternalType namesSeen t1 ))
                    fields
                )
                |> toExtMeta meta

        Unit ->
            ExtType.Unit
                |> toExtMeta meta

        String s ->
            ExtType.String s
                |> toExtMeta meta

        Uuid ->
            ExtType.Uuid
                |> toExtMeta meta

        Bool ->
            ExtType.Bool
                |> toExtMeta meta

        Int ->
            ExtType.Int
                |> toExtMeta meta

        Float ->
            ExtType.Float
                |> toExtMeta meta



-- VERSIONING


{-| Attach a migration on top of an existing schema. Arguments: the
new version's tag, a migration function `old -> new`, the new
schema, and the old schema. Designed to compose with `|>`:

    schema_v2 : Schema () UserV2
    schema_v2 =
        schema_v1
            |> Schema.newVersion "v2" migrateV1toV2 schema_v2Base

    schema_v3 : Schema () UserV3
    schema_v3 =
        schema_v2
            |> Schema.newVersion "v3" migrateV2toV3 schema_v3Base

Each call preserves every prior version's decoder, composing them
through the migration chain so the resulting schema's decoder can
read every version written to date. Encoding always writes the
_latest_ tag.

Attach `newVersion` last — piping the result through another builder
(`list`, `record`, etc.) drops the version chain, since those
builders always reset `version` to `Nothing`.
-}
newVersion : String -> (old -> new) -> Schema m new -> Schema m old -> Schema m new
newVersion tag oldToNew (Schema new) (Schema old) =
    let
        version =
            case old.version of
                Just previousVersion ->
                    { tag = tag
                    , taggedDecoders =
                        previousVersion.taggedDecoders
                            |> Dict.map (\_ -> Decode.map oldToNew)
                            |> Dict.insert previousVersion.tag (Decode.map oldToNew old.decoder)
                    , untaggedDecoder = Decode.map oldToNew previousVersion.untaggedDecoder
                    }

                Nothing ->
                    { tag = tag
                    , taggedDecoders = Dict.empty
                    , untaggedDecoder = Decode.map oldToNew old.decoder
                    }
    in
    Schema
        { meta = new.meta
        , decoder = new.decoder
        , version = Just version
        , encode = new.encode
        }


{-| Return an otherwise-identical schema with its version chain
removed. The resulting decoder tries, in order: the payload nested
under `"#val"`, the raw payload, and finally `Decode.succeed`
with the supplied default — so it cannot fail.

`dropVersions` does **not** run the migration chain; it only strips
the version envelope (tolerating its absence). Use it when you're
done migrating and want a best-effort reader; use
[`newVersion`](#newVersion) when you need older tagged payloads to
be migrated forward.
-}
dropVersions : a -> Schema m a -> Schema m a
dropVersions default (Schema s) =
    case s.version of
        Just version ->
            Schema
                { s
                    | version = Nothing
                    , decoder =
                        Decode.oneOf
                            [ Decode.field versionField_val s.decoder
                            , s.decoder
                            , Decode.succeed default
                            ]
                }

        Nothing ->
            Schema s


versionField_tag : String
versionField_tag =
    "#tag"


versionField_val : String
versionField_val =
    "#val"


unwrapType : Schema m a -> Meta m
unwrapType (Schema s) =
    s.meta


unwrapVersion : Schema m a -> Maybe (Version a)
unwrapVersion sc =
    case sc of
        Schema s ->
            s.version



-- RECURSIVE TYPES


{-| Break a recursive schema by deferring evaluation. The `String`
must match the name passed to [`custom`](#custom) on the enclosing
type — that's how [`toType`](#toType) detects the cycle and emits a
reference instead of looping forever.

For parametrized recursive types such as `Tree a`, give each
instantiation a distinct name (see the module header example).
-}
lazy : m -> String -> (() -> Schema m a) -> Schema m a
lazy m name fn =
    Schema
        { meta = emptyMeta m (Lazy name (\_ -> unwrapType (fn ())))
        , decoder = Decode.lazy (\_ -> decoder (fn ()))
        , version = Nothing
        , encode = \v -> encode (fn ()) v
        }



-- JSON ENCODE / DECODER


encodeTagged : String -> Value -> Value
encodeTagged tag value =
    Encode.object
        [ ( versionField_tag, Encode.string tag )
        , ( versionField_val, value )
        ]


{-| Encode a value through the schema. If the schema has a version
chain attached (via [`newVersion`](#newVersion)), the output is
wrapped as `{"#tag": "<latest>", "#val": <payload>}`; otherwise the
raw payload is returned.
-}
encode : Schema m a -> a -> Value
encode (Schema s) value =
    case s.version of
        Just version ->
            encodeTagged version.tag (s.encode value)

        Nothing ->
            s.encode value


decoderTagged : Decoder a -> Version a -> Decoder a
decoderTagged latestDecoder version =
    Decode.oneOf
        [ Decode.field versionField_tag Decode.string
            |> Decode.andThen
                (\tag ->
                    if tag == version.tag then
                        Decode.field versionField_val latestDecoder

                    else
                        case Dict.get tag version.taggedDecoders of
                            Just previousDecoder ->
                                Decode.field versionField_val previousDecoder

                            Nothing ->
                                Decode.fail ("Unknown version: " ++ tag)
                )
        , version.untaggedDecoder
        ]


{-| Extract the JSON decoder.

For a versioned schema, this reads `"#tag"` and dispatches to the
matching decoder — the current one if the tag is the latest, an
older one from the migration chain otherwise, or falls back to the
un-tagged decoder for values written before versioning was
introduced. Unknown tags fail with `"Unknown version: <tag>"`.

For a non-versioned schema, this is lenient about the wrapper:
either the raw payload or a `{"#val": …}`-wrapped value decodes
successfully.
-}
decoder : Schema m a -> Decoder a
decoder (Schema s) =
    case s.version of
        Just version ->
            decoderTagged s.decoder version

        Nothing ->
            Decode.oneOf
                [ s.decoder
                , Decode.field versionField_val s.decoder
                ]
