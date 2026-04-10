module FormTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Schema exposing (Schema)
import Schema.Fuzzer as Fuzzer
import Schema.Type as Type
import Schema.Type.Value as Value
import Set exposing (Set)
import Test exposing (Test)


type alias Product =
    { id : String
    , sku : String
    , name : String
    , price : Float
    , tags : List String
    , specialPrice : Maybe Float
    , size : Size
    , description : Html
    , pair : ( Int, String )
    , triple : ( Int, String, Bool )
    , set : Set Int
    , dict : Dict ( String, Int ) Float
    }


type Size
    = Small Int
    | Medium String
    | Large Bool


schema_Size : Schema () Size
schema_Size =
    Schema.custom "Size"
        (\small medium large value ->
            case value of
                Small i ->
                    small i

                Medium s ->
                    medium s

                Large b ->
                    large b
        )
        |> Schema.variant1 "Small" Small (Schema.int ())
        |> Schema.variant1 "Medium" Medium (Schema.string ())
        |> Schema.variant1 "Large" Large (Schema.bool ())
        |> Schema.buildCustom ()


type Html
    = Element String (List Html)
    | Text String


schema_Html : Schema () Html
schema_Html =
    Schema.custom "Html"
        (\element text value ->
            case value of
                Element tag children ->
                    element tag children

                Text str ->
                    text str
        )
        |> Schema.variant2 "Element"
            Element
            (Schema.string ())
            (Schema.list () (Schema.lazy () "Html" (\_ -> schema_Html)))
        |> Schema.variant1 "Text" Text (Schema.string ())
        |> Schema.buildCustom ()


schema_Product : Schema () Product
schema_Product =
    Schema.record Product
        |> Schema.field "id" .id (Schema.uuid ())
        |> Schema.field "sku" .sku (Schema.string ())
        |> Schema.field "name" .name (Schema.string ())
        |> Schema.field "price" .price (Schema.float ())
        |> Schema.field "tags" .tags (Schema.list () (Schema.string ()))
        |> Schema.field "specialPrice" .specialPrice (Schema.maybe () (Schema.float ()))
        |> Schema.field "size" .size schema_Size
        |> Schema.field "description" .description schema_Html
        |> Schema.field "pair" .pair (Schema.pair () (Schema.int ()) (Schema.string ()))
        |> Schema.field "triple" .triple (Schema.triple () (Schema.int ()) (Schema.string ()) (Schema.bool ()))
        |> Schema.field "set" .set (Schema.set () (Schema.int ()))
        |> Schema.field "dict"
            .dict
            (Schema.dict ()
                (Schema.pair () (Schema.string ()) (Schema.int ()))
                (Schema.float ())
            )
        |> Schema.buildRecord ()


tests : Test
tests =
    Test.fuzz
        (Fuzzer.fromSchema schema_Product)
        "Product -> JsonValue -> TypeValue -> JsonValue -> Product"
        (\product ->
            let
                type_ =
                    Schema.toType schema_Product

                jsonValue =
                    Schema.encode schema_Product product

                result =
                    JD.decodeValue (Value.decoder type_) jsonValue
                        |> Result.andThen
                            (\typeValue ->
                                Value.encode typeValue
                                    |> JD.decodeValue (Schema.decoder schema_Product)
                            )
            in
            Expect.equal (Ok product) result
        )
