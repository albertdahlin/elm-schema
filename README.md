# elm-schema

Define a schema once, and get:

- a JSON **encoder** and **decoder** for your Elm type
- an introspectable **type description** (`Schema.Type.Node`)
- a JSON Schema document for [OpenAI Structured Outputs][openai-so]
- a random-value **fuzzer** for property tests
- opt-in **schema versioning** with migrations

No code generation, no compiler plugins — just a small builder API.

[openai-so]: https://platform.openai.com/docs/guides/structured-outputs


## Installation

```sh
elm install albertdahlin/elm-schema
```


## A tiny example

```elm
import Schema exposing (Schema)

type alias User =
    { id : String
    , age : Int
    }

schema_User : Schema () User
schema_User =
    Schema.record User
        |> Schema.field "id" .id (Schema.string ())
        |> Schema.field "value" .age (Schema.int ())
        |> Schema.buildRecord ()
```

From that single value you can now derive the pieces you need:

```elm
import Json.Decode
import Json.Encode

-- Encode / decode
Schema.encode schema_User { id = "u-1", age = 30 }
    --> {"id":"u-1","age":30}

Json.Decode.decodeString (Schema.decoder schema_User) "..."

-- Introspect the type tree
Schema.toType schema_User
    --> Schema.Type.Node { type_ = Record [...], meta = () }
```


## Custom types

Custom types are built with a `match` function plus one call per
variant. Variants encode as `{"tag": "VariantName", "args": [...]}`.

```elm
type Shape
    = Circle Float
    | Rect Float Float

schema_Shape : Schema () Shape
schema_Shape =
    Schema.custom "Shape"
        (\toCircle toRect value ->
            case value of
                Circle r     -> toCircle r
                Rect w h     -> toRect w h
        )
        |> Schema.variant1 "Circle" () Circle (Schema.float ())
        |> Schema.variant2 "Rect"   () Rect   (Schema.float ()) (Schema.float ())
        |> Schema.buildCustom ()
```


## Recursive types

Use `Schema.lazy` to break the cycle. The name passed to `lazy` must
match the one passed to `Schema.custom`, and parametrized recursive
types need a distinct name per instantiation.

```elm
type Tree a
    = Node a (List (Tree a))

schema_Tree : String -> Schema () a -> Schema () (Tree a)
schema_Tree typeName nodeSchema =
    Schema.custom typeName
        (\toNode (Node a children) -> toNode a children)
        |> Schema.variant2 "Node"
            ()
            Node
            nodeSchema
            (Schema.list ()
                (Schema.lazy () typeName
                    (\_ -> schema_Tree typeName nodeSchema)
                )
            )
        |> Schema.buildCustom ()

schema_TreeInt    = schema_Tree "Tree_Int"    (Schema.int ())
schema_TreeString = schema_Tree "Tree_String" (Schema.string ())
```


## JSON Schema for OpenAI Structured Outputs

`Schema.Type.JsonSchema.fromType` turns a type node into a JSON Schema
document targeting [OpenAI Structured Outputs][openai-so] — a
constrained subset of JSON Schema, not plain Draft 7. Specifically:

- every object sets `additionalProperties: false` and lists every
  property in `required`
- named types (records and custom types) are emitted once under
  `$defs` and referenced via `$ref`
- custom types are encoded as an `anyOf` over their variants, each
  variant being an object with a `tag` const and an `args` object
- `Maybe` is encoded as `anyOf [ …, { "type": "null" } ]`

```elm
import Schema.Type.JsonSchema

Schema.toType schema_User
    |> Schema.Type.JsonSchema.fromType "User"
    -- : Json.Encode.Value
```

The consumer expects a meta record with at least `name` and
`description` fields, so you typically carry those on your schema:

```elm
meta : String -> String -> { name : String, description : String }
meta n d =
    { name = n, description = d }

schema_User =
    Schema.record User
        |> Schema.field "id"  .id  (Schema.string (meta "id" "User identifier"))
        |> Schema.field "age" .age (Schema.int    (meta "age" "Age in years"))
        |> Schema.buildRecord (meta "User" "A user record")
```


## Fuzz testing

`Schema.Fuzzer.fromSchema` produces an `elm-test` fuzzer that generates
random Elm values by first generating JSON matching the type tree and
then decoding it through the schema — so a round-trip is guaranteed.

```elm
import Schema.Fuzzer
import Test exposing (fuzz)
import Expect

roundTrip : Test
roundTrip =
    fuzz (Schema.Fuzzer.fromSchema schema_User) "encode/decode round-trip" <|
        \user ->
            Schema.encode schema_User user
                |> Json.Decode.decodeValue (Schema.decoder schema_User)
                |> Expect.equal (Ok user)
```


## Versioning and migrations

`Schema.newVersion` chains an old schema to a new one with a migration
function and a version tag. Encoded values are wrapped as
`{"#tag": "v2", "#val": ...}` so the decoder can dispatch on version.
Old, untagged values still decode through the original schema.

```elm
schema_UserV1 : Schema () UserV1
schema_UserV1 = ...

schema_UserV2 : Schema () UserV2
schema_UserV2 =
    Schema.record UserV2
        |> ...
        |> Schema.buildRecord ()
        |> Schema.newVersion "v2" migrateV1ToV2 schema_UserV1

migrateV1ToV2 : UserV1 -> UserV2
migrateV1ToV2 v1 = ...
```

If you want to strip the versioning wrapper from the decoder (for
example when reading a snapshot you know is current), use
`Schema.dropVersions` with a default value.


## Meta

Every schema builder takes a _meta_ value as its first argument. This
is whatever you want to attach to that node — commonly a record like
`{ name : String, description : String }` — and it flows unchanged
into the `Schema.Type.Node` tree returned by `Schema.toType`.

If you don't need it, pass `()`.


## Exposed modules

- [`Schema`](https://package.elm-lang.org/packages/albertdahlin/elm-schema/latest/Schema) —
  build schemas, encode, decode, version.
- [`Schema.Type`](https://package.elm-lang.org/packages/albertdahlin/elm-schema/latest/Schema-Type) —
  the introspectable type tree.
- [`Schema.Type.JsonSchema`](https://package.elm-lang.org/packages/albertdahlin/elm-schema/latest/Schema-Type-JsonSchema) —
  JSON Schema (Draft 7) generation.
- [`Schema.Fuzzer`](https://package.elm-lang.org/packages/albertdahlin/elm-schema/latest/Schema-Fuzzer) —
  `elm-test` fuzzers from a schema.


## License

MIT
