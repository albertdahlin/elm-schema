module SchemaTest exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Decode as JD exposing (Decoder)
import Schema exposing (Schema)
import Schema.Fuzzer as Fuzzer
import Schema.Type as Type
import Set exposing (Set)
import Test exposing (Test)



-- SIMPLE RECORD


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



-- CUSTOM TYPE


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



-- RECURSIVE TYPES


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



-- MUTUALLY RECURSIVE TYPES


type RA
    = RA Int (List RB)


type RB
    = RB String (List RA)


schema_RA : Schema RA
schema_RA =
    Schema.custom "RA"
        (\raConstructor value ->
            case value of
                RA i rbs ->
                    raConstructor i rbs
        )
        |> Schema.variant2 "RA"
            RA
            Schema.int
            (Schema.list (Schema.lazy "RB" (\_ -> schema_RB)))
        |> Schema.buildCustom


schema_RB : Schema RB
schema_RB =
    Schema.custom "RB"
        (\rbConstructor value ->
            case value of
                RB str ras ->
                    rbConstructor str ras
        )
        |> Schema.variant2 "RB"
            RB
            Schema.string
            (Schema.list (Schema.lazy "RA" (\_ -> schema_RA)))
        |> Schema.buildCustom


testIsRecursive : Test
testIsRecursive =
    Test.describe "isRecursive"
        [ Test.test "schema_RA is recursive" <|
            \_ ->
                Expect.equal (Type.isRecursive <| Schema.toType schema_RA) True
        , Test.test "schema_RB is recursive" <|
            \_ ->
                Expect.equal (Type.isRecursive <| Schema.toType schema_RB) True
        , Test.test "schema_CustomType is not recursive" <|
            \_ ->
                Expect.equal (Type.isRecursive <| Schema.toType schema_CustomType) False
        ]



-- ALL TOGETHER


type alias All =
    { id : String
    , value : Int
    , float : Float
    , bool : Bool
    , maybe : Maybe Int
    , list : List String
    , dict : Dict (List String) Int
    , array : Array String
    , set : Set (List Int)
    , record : Record
    , custom : CustomType
    , pair : ( String, Int )
    , triple : ( String, Int, Float )
    , recursive : RA
    }


schema_All : Schema All
schema_All =
    Schema.record All
        |> Schema.field "id" .id Schema.string
        |> Schema.field "value" .value Schema.int
        |> Schema.field "float" .float Schema.float
        |> Schema.field "bool" .bool Schema.bool
        |> Schema.field "maybe" .maybe (Schema.maybe Schema.int)
        |> Schema.field "list" .list (Schema.list Schema.string)
        |> Schema.field "dict" .dict (Schema.dict (Schema.list Schema.string) Schema.int)
        |> Schema.field "array" .array (Schema.array Schema.string)
        |> Schema.field "set" .set (Schema.set (Schema.list Schema.int))
        |> Schema.field "record" .record schema_Record
        |> Schema.field "custom" .custom schema_CustomType
        |> Schema.field "pair" .pair (Schema.pair Schema.string Schema.int)
        |> Schema.field "triple" .triple (Schema.triple Schema.string Schema.int Schema.float)
        |> Schema.field "recursive" .recursive schema_RA
        |> Schema.buildRecord


fuzzer_RA : Int -> Fuzzer RA
fuzzer_RA len =
    Fuzz.map2 RA
        Fuzz.int
        (Fuzz.listOfLength len (Fuzz.lazy (\_ -> fuzzer_RB (len - 1))))


fuzzer_RB : Int -> Fuzzer RB
fuzzer_RB len =
    Fuzz.map2 RB
        Fuzz.string
        (Fuzz.listOfLength len (Fuzz.lazy (\_ -> fuzzer_RA (len - 1))))


jsonEncodeDecodeAll : Test
jsonEncodeDecodeAll =
    Test.fuzz
        (Fuzz.constant All
            |> Fuzz.andMap Fuzz.string
            |> Fuzz.andMap Fuzz.int
            |> Fuzz.andMap Fuzz.niceFloat
            |> Fuzz.andMap Fuzz.bool
            |> Fuzz.andMap (Fuzz.maybe Fuzz.int)
            |> Fuzz.andMap (Fuzz.list Fuzz.string)
            |> Fuzz.andMap
                (Fuzz.pair (Fuzz.list Fuzz.string) Fuzz.int
                    |> Fuzz.list
                    |> Fuzz.map Dict.fromList
                )
            |> Fuzz.andMap (Fuzz.array Fuzz.string)
            |> Fuzz.andMap (Fuzz.list (Fuzz.list Fuzz.int) |> Fuzz.map Set.fromList)
            |> Fuzz.andMap
                (Fuzz.map2 Record
                    Fuzz.string
                    Fuzz.int
                )
            |> Fuzz.andMap
                (Fuzz.oneOf
                    [ Fuzz.map (\_ -> Zero) (Fuzz.constant ())
                    , Fuzz.map One Fuzz.string
                    , Fuzz.map2 Two Fuzz.string Fuzz.int
                    , Fuzz.map3 Three Fuzz.string Fuzz.int Fuzz.niceFloat
                    ]
                )
            |> Fuzz.andMap (Fuzz.pair Fuzz.string Fuzz.int)
            |> Fuzz.andMap (Fuzz.triple Fuzz.string Fuzz.int Fuzz.niceFloat)
            |> Fuzz.andMap (fuzzer_RA 5)
        )
        "Encode and Decode all types using schema_All"
        (\r ->
            Schema.encode schema_All r
                |> JD.decodeValue (Schema.decoder schema_All)
                |> Expect.equal (Ok r)
        )


type alias V1 =
    { id : String
    , value : Int
    }


schema_V1 : Schema V1
schema_V1 =
    Schema.record V1
        |> Schema.field "id" .id Schema.string
        |> Schema.field "value" .value Schema.int
        |> Schema.buildRecord


fuzz_V1 : Fuzzer V1
fuzz_V1 =
    Fuzz.map2 V1
        Fuzz.string
        Fuzz.int


type alias V2 =
    { id : String
    , value : Int
    , newField : Float
    }


schema_V2 : Schema V2
schema_V2 =
    schema_V1
        |> Schema.newVersion "v2"
            v1_to_v2
            (Schema.record V2
                |> Schema.field "id" .id Schema.string
                |> Schema.field "value" .value Schema.int
                |> Schema.field "newField" .newField Schema.float
                |> Schema.buildRecord
            )


fuzz_V2 : Fuzzer V2
fuzz_V2 =
    Fuzz.map3 V2
        Fuzz.string
        Fuzz.int
        Fuzz.niceFloat


v1_to_v2 : V1 -> V2
v1_to_v2 v1 =
    { id = v1.id
    , value = v1.value
    , newField = 0.0
    }


versions : Test
versions =
    Test.describe "Versions"
        [ Test.fuzz
            fuzz_V1
            "Encode V1 and Decode V2"
            (\v1 ->
                Schema.encode schema_V1 v1
                    |> JD.decodeValue (Schema.decoder schema_V2)
                    |> Expect.equal (Ok <| v1_to_v2 v1)
            )
        , Test.fuzz
            fuzz_V2
            "Encode V2 and Decode V2"
            (\v2 ->
                Schema.encode schema_V2 v2
                    |> JD.decodeValue (Schema.decoder schema_V2)
                    |> Expect.equal (Ok v2)
            )
        , Test.fuzz
            fuzz_V2
            "Encode V2 and Decode V1"
            (\v2 ->
                Schema.encode schema_V2 v2
                    |> JD.decodeValue (Schema.decoder schema_V1)
                    |> Expect.equal (Ok { id = v2.id, value = v2.value })
            )
        ]


testSchemaFuzzer : String -> Schema a -> Test
testSchemaFuzzer name schema =
    Test.fuzz
        (Fuzzer.fromSchema schema)
        ("Schema Fuzzer: " ++ name)
        (\v ->
            Schema.encode schema v
                |> JD.decodeValue (Schema.decoder schema)
                |> Expect.equal (Ok v)
        )


typeFuzzers : Test
typeFuzzers =
    Test.describe "Type Fuzzers"
        [ testSchemaFuzzer "String" Schema.string
        , testSchemaFuzzer "Int" Schema.int
        , testSchemaFuzzer "Float" Schema.float
        , testSchemaFuzzer "Bool" Schema.bool
        , testSchemaFuzzer "List String" (Schema.list Schema.string)
        , testSchemaFuzzer "Array String" (Schema.array Schema.string)
        , testSchemaFuzzer "Maybe String" (Schema.maybe Schema.string)
        , testSchemaFuzzer "Dict String Int" (Schema.dict Schema.string Schema.int)
        , testSchemaFuzzer "Set Int" (Schema.set Schema.int)
        , testSchemaFuzzer "Pair String Int" (Schema.pair Schema.string Schema.int)
        , testSchemaFuzzer "Triple String Int Float" (Schema.triple Schema.string Schema.int Schema.float)
        , testSchemaFuzzer "Record" schema_Record
        , testSchemaFuzzer "CustomType" schema_CustomType
        , testSchemaFuzzer "Recursive RA" schema_RA
        , testSchemaFuzzer "All" schema_All
        ]
