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

{-| SCHEMA
A library to define schemas for data types, with JSON encoding/decoding and type description.

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

    schema_Record : Schema Record
    schema_Record =
        Schema.record Record
            |> Schema.field "id" .id Schema.string
            |> Schema.field "value" .value Schema.int
            |> Schema.buildRecord

@docs RecordBuilder, record, field, buildRecord


## Custom Types

    type CustomType
        = Zero
        | One String
        | Two String Int
        | Three String Int Float

    schema_CustomType : Schema CustomType
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
            |> Schema.variant0 "Zero" Zero
            |> Schema.variant1 "One" One Schema.string
            |> Schema.variant2 "Two" Two Schema.string Schema.int
            |> Schema.variant3 "Three" Three Schema.string Schema.int Schema.float
            |> Schema.buildCustom

@docs CustomBuilder, custom, variant0, variant1, variant2, variant3, buildCustom


## Recrusive Types

    type Tree a
        = Node a (List (Tree a))

    schema_Tree : String -> Schema a -> Schema (Tree a)
    schema_Tree typeName nodeSchema =
        Schema.custom typeName
            (\nodeConstructor value ->
                case value of
                    Node a children ->
                        nodeConstructor a children
            )
            |> Schema.variant2 "Node"
                Node
                nodeSchema
                (Schema.list
                    (Schema.lazy typeName
                        (\_ ->
                            schema_Tree
                                typeName
                                nodeSchema
                        )
                    )
                )
            |> Schema.buildCustom


### Important!

The `typeName` passed to `Schema.lazy` must match the one
passed to `Schema.custom`. That's how recursion is detected
when generating type descriptions.
In this case we have a parametrized recursive type (`Tree a`), the
name must be different for each instantiation of `a`.

    schema_TreeInt : Schema (Tree Int)
    schema_TreeInt =
        schema_Tree "Tree_Int" Schema.int

    schema_TreeString : Schema (Tree String)
    schema_TreeString =
        schema_Tree "Tree_String" Schema.string

@docs lazy


## Type Description

@docs toType, label, description


## JSON Encode / Decode

@docs encode, decoder
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


{-| TODO: document
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


{-| TODO: document
-}
null : m -> Schema m ()
null m =
    Schema
        { meta = emptyMeta m Unit
        , decoder = Decode.succeed ()
        , version = Nothing
        , encode = always Encode.null
        }


{-| TODO: document
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


{-| TODO: document
-}
uuid : m -> Schema m String
uuid m =
    Schema
        { meta = emptyMeta m Uuid
        , decoder = Decode.string
        , version = Nothing
        , encode = Encode.string
        }


{-| TODO: document
-}
bool : m -> Schema m Bool
bool m =
    Schema
        { meta = emptyMeta m Bool
        , decoder = Decode.bool
        , version = Nothing
        , encode = Encode.bool
        }


{-| TODO: document
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


{-| TODO: document
-}
list : m -> Schema m a -> Schema m (List a)
list m s =
    Schema
        { meta = emptyMeta m (List (unwrapType s))
        , decoder = Decode.list (decoder s)
        , version = Nothing
        , encode = Encode.list (encode s)
        }


{-| TODO: document
-}
array : m -> Schema m a -> Schema m (Array a)
array m s =
    Schema
        { meta = emptyMeta m (List (unwrapType s))
        , decoder = Decode.array (decoder s)
        , version = Nothing
        , encode = Encode.array (encode s)
        }


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
-}
type RecordBuilder m constructor record
    = RecordBuilder
        { fields : List ( String, Meta m )
        , decoder : Decoder constructor
        , encode : record -> List ( String, Value )
        }


{-| TODO: document
-}
record : constructor -> RecordBuilder m constructor record
record ctor =
    RecordBuilder
        { fields = []
        , decoder = Decode.succeed ctor
        , encode = \_ -> []
        }


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
-}
type CustomBuilder m match t
    = CustomBuilder
        { ctor : List { name : String, meta : m, args : List (Meta m) }
        , decoder : Dict String (Decoder t)
        , match : match
        , name : String
        }


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
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


{-| TODO: document
-}
toType : Schema m a -> ExtType.Node m
toType (Schema s) =
    toExternalType Set.empty s.meta


toExtMeta : Meta m -> ExtType.Type m -> ExtType.Node m
toExtMeta meta type_ =
    { type_ = type_
    , meta = meta.meta
    }


{-| TODO: document
-}
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


{-| TODO: document
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


{-| TODO: document
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


{-| -}
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


{-| TODO: document
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


{-| TODO: document
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
