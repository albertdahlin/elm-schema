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


type Type
    = Unit {- null -}
    | String (Maybe ExtType.StringOptions)
    | Uuid
    | Bool
    | Int
    | Float
    | List Meta
    | Array Meta
    | Maybe Meta
    | Dict Meta Meta
    | Set Meta
    | Tuple (List Meta)
    | Record (List ( String, Meta ))
    | Custom String (List { name : String, args : List Meta })
    | Lazy String (() -> Meta)


type alias Meta =
    { type_ : Type
    }


emptyMeta : Type -> Meta
emptyMeta type_ =
    { type_ = type_
    }


{-| TODO: document
-}
type Schema a
    = Schema
        { meta : Meta
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
null : Schema ()
null =
    Schema
        { meta = emptyMeta Unit
        , decoder = Decode.succeed ()
        , version = Nothing
        , encode = always Encode.null
        }


{-| TODO: document
-}
const : a -> Schema a
const a =
    Schema
        { meta = emptyMeta Unit
        , decoder = Decode.succeed a
        , version = Nothing
        , encode = always Encode.null
        }


{-| A basic string schema. Use `stringWith` to customize options.
-}
string : Schema String
string =
    Schema
        { meta = emptyMeta (String Nothing)
        , decoder = Decode.string
        , version = Nothing
        , encode = Encode.string
        }


{-| A customizable string schema.
-}
stringWith : ExtType.StringOptions -> Schema String
stringWith opts =
    Schema
        { meta = emptyMeta (String (Just opts))
        , decoder = Decode.string |> Decode.map (ExtType.sanitizeAll opts.sanitize)
        , version = Nothing
        , encode = ExtType.sanitizeAll opts.sanitize >> Encode.string
        }


{-| TODO: document
-}
uuid : Schema String
uuid =
    Schema
        { meta = emptyMeta Uuid
        , decoder = Decode.string
        , version = Nothing
        , encode = Encode.string
        }


{-| TODO: document
-}
bool : Schema Bool
bool =
    Schema
        { meta = emptyMeta Bool
        , decoder = Decode.bool
        , version = Nothing
        , encode = Encode.bool
        }


{-| TODO: document
-}
int : Schema Int
int =
    Schema
        { meta = emptyMeta Int
        , decoder = Decode.int
        , version = Nothing
        , encode = Encode.int
        }


{-| Does not support NaN or Infinity.
-}
float : Schema Float
float =
    Schema
        { meta = emptyMeta Float
        , decoder = Decode.float
        , version = Nothing
        , encode = Encode.float
        }



-- COMPOSITE TYPES


{-| TODO: document
-}
list : Schema a -> Schema (List a)
list s =
    Schema
        { meta = emptyMeta (List (unwrapType s))
        , decoder = Decode.list (decoder s)
        , version = Nothing
        , encode = Encode.list (encode s)
        }


{-| TODO: document
-}
array : Schema a -> Schema (Array a)
array s =
    Schema
        { meta = emptyMeta (List (unwrapType s))
        , decoder = Decode.array (decoder s)
        , version = Nothing
        , encode = Encode.array (encode s)
        }


{-| TODO: document
-}
maybe : Schema a -> Schema (Maybe a)
maybe s =
    Schema
        { meta = emptyMeta (Maybe (unwrapType s))
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
pair : Schema a -> Schema b -> Schema ( a, b )
pair sa sb =
    Schema
        { meta = emptyMeta (Tuple [ unwrapType sa, unwrapType sb ])
        , decoder =
            Decode.map2 Tuple.pair
                (Decode.index 0 (decoder sa))
                (Decode.index 1 (decoder sb))
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
    Schema a
    -> Schema b
    -> Schema c
    -> Schema ( a, b, c )
triple sa sb sc =
    Schema
        { meta = emptyMeta (Tuple [ unwrapType sa, unwrapType sb, unwrapType sc ])
        , decoder =
            Decode.map3 (\a b c -> ( a, b, c ))
                (Decode.index 0 (decoder sa))
                (Decode.index 1 (decoder sb))
                (Decode.index 2 (decoder sc))
        , version = Nothing
        , encode =
            \( a, b, c ) ->
                Encode.list
                    identity
                    [ encode sa a
                    , encode sb b
                    , encode sc c
                    ]
        }


{-| TODO: document
-}
dict : Schema comparable -> Schema v -> Schema (Dict comparable v)
dict sk sv =
    Schema
        { meta = emptyMeta (Dict (unwrapType sk) (unwrapType sv))
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
set : Schema comparable -> Schema (Set comparable)
set s =
    Schema
        { meta = emptyMeta (Set (unwrapType s))
        , decoder = Decode.list (decoder s) |> Decode.map Set.fromList
        , version = Nothing
        , encode = Set.toList >> Encode.list (encode s)
        }



-- RECORDS


{-| TODO: document
-}
type RecordBuilder constructor record
    = RecordBuilder
        { fields : List ( String, Meta )
        , decoder : Decoder constructor
        , encode : record -> List ( String, Value )
        }


{-| TODO: document
-}
record : constructor -> RecordBuilder constructor record
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
    -> Schema field
    -> RecordBuilder (field -> constructor) record
    -> RecordBuilder constructor record
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
buildRecord : RecordBuilder a a -> Schema a
buildRecord (RecordBuilder r) =
    Schema
        { meta = emptyMeta (Record (List.reverse r.fields))
        , decoder = r.decoder
        , version = Nothing
        , encode = \entity -> Encode.object (r.encode entity |> List.reverse)
        }



-- BUILD CUSTOM TYPES


{-| TODO: document
-}
type CustomBuilder match t
    = CustomBuilder
        { ctor : List { name : String, args : List Meta }
        , decoder : Dict String (Decoder t)
        , match : match
        , name : String
        }


{-| TODO: document
-}
custom : String -> match -> CustomBuilder match t
custom name match =
    CustomBuilder
        { ctor = []
        , decoder = Dict.empty
        , match = match
        , name = name
        }


variant name match decoderArgs args (CustomBuilder c) =
    let
        enc v =
            Encode.object
                [ ( "tag", Encode.string name )
                , ( "args", Encode.list identity v )
                ]
    in
    CustomBuilder
        { ctor = { name = name, args = args } :: c.ctor
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
    -> v
    -> CustomBuilder (Value -> a) v
    -> CustomBuilder a v
variant0 name val =
    variant
        name
        (\c -> c [])
        (Decode.succeed val)
        []


{-| TODO: document
-}
variant1 :
    String
    -> (a -> v)
    -> Schema a
    -> CustomBuilder ((a -> Value) -> b) v
    -> CustomBuilder b v
variant1 name ctor s =
    variant
        name
        (\c a ->
            c
                [ encode s a
                ]
        )
        (Decode.map ctor (Decode.index 0 (decoder s)))
        [ unwrapType s ]


{-| TODO: document
-}
variant2 :
    String
    -> (a -> b -> v)
    -> Schema a
    -> Schema b
    -> CustomBuilder ((a -> b -> Value) -> c) v
    -> CustomBuilder c v
variant2 name ctor sa sb =
    variant
        name
        (\c a b ->
            c
                [ encode sa a
                , encode sb b
                ]
        )
        (Decode.map2 ctor
            (Decode.index 0 (decoder sa))
            (Decode.index 1 (decoder sb))
        )
        [ unwrapType sa
        , unwrapType sb
        ]


{-| TODO: document
-}
variant3 :
    String
    -> (a -> b -> c -> v)
    -> Schema a
    -> Schema b
    -> Schema c
    -> CustomBuilder ((a -> b -> c -> Value) -> d) v
    -> CustomBuilder d v
variant3 name ctor sa sb sc =
    variant
        name
        (\c a b d ->
            c
                [ encode sa a
                , encode sb b
                , encode sc d
                ]
        )
        (Decode.map3 ctor
            (Decode.index 0 (decoder sa))
            (Decode.index 1 (decoder sb))
            (Decode.index 2 (decoder sc))
        )
        [ unwrapType sa
        , unwrapType sb
        , unwrapType sc
        ]


{-| TODO: document
-}
buildCustom : CustomBuilder (a -> Value) a -> Schema a
buildCustom (CustomBuilder c) =
    Schema
        { meta = emptyMeta (Custom c.name (List.reverse c.ctor))
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
toType : Schema a -> ExtType.Node
toType (Schema s) =
    toExternalType Set.empty s.meta


toExtMeta : Meta -> ExtType.Type -> ExtType.Node
toExtMeta meta type_ =
    { type_ = type_
    }


{-| TODO: document
-}
toExternalType : Set String -> Meta -> ExtType.Node
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
newVersion : String -> (old -> new) -> Schema new -> Schema old -> Schema new
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
dropVersions : a -> Schema a -> Schema a
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


unwrapType : Schema a -> Meta
unwrapType (Schema s) =
    s.meta


unwrapVersion : Schema a -> Maybe (Version a)
unwrapVersion sc =
    case sc of
        Schema s ->
            s.version



-- RECURSIVE TYPES


{-| -}
lazy : String -> (() -> Schema a) -> Schema a
lazy name fn =
    Schema
        { meta = emptyMeta (Lazy name (\_ -> unwrapType (fn ())))
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
encode : Schema a -> a -> Value
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
decoder : Schema a -> Decoder a
decoder (Schema s) =
    case s.version of
        Just version ->
            decoderTagged s.decoder version

        Nothing ->
            Decode.oneOf
                [ s.decoder
                , Decode.field versionField_val s.decoder
                ]
