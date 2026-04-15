module Schema.Fuzzer exposing (fromSchema, fromType)

{-| Generate [`elm-test`][elm-test] fuzzers from a
[`Schema`](Schema) or a [`Schema.Type.Node`](Schema-Type#Node). Useful
for property-based testing (encode/decode round-trips, invariants
over your type), and for seeding data in demos and tests.

The generator works at the JSON level: it produces a
`Json.Encode.Value` shaped like the schema and then, for
[`fromSchema`](#fromSchema), decodes it through the schema's
decoder. That guarantees the generated Elm values always round-trip
through the schema.

Generation is depth-bounded (initial budget: 10), so recursive types
and large containers don't blow up. Once the budget runs out,
container types produce empty values, `Maybe` produces `null`, and
recursive references likewise stop descending.

Known limitations:

  - String schemas ignore [`StringOptions`](Schema-Type#StringOptions)
    — `minLength`/`maxLength`/`sanitize` are not respected by the
    generator.
  - `Set` is fuzzed as a plain list; collisions are possible and
    will be collapsed by `Set.fromList` on decode, so the generated
    set may be smaller than the list.
  - UUID is fuzzed as an arbitrary string, not as a valid UUID.

[elm-test]: https://package.elm-lang.org/packages/elm-explorations/test/latest/

@docs fromSchema, fromType

-}

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Schema exposing (Schema)
import Schema.Type as Type exposing (Type)


{-| Produce a `Fuzzer` of values of your Elm type.

Internally: generate JSON via [`fromType`](#fromType), then decode it
through the schema's decoder. A decode failure becomes
`Fuzz.invalid` with the decoder's error message — so if you see
invalid fuzzer output, it means the schema's encoder and decoder
disagree, which is worth treating as a bug.

    import Test exposing (Test, fuzz)
    import Expect
    import Schema
    import Schema.Fuzzer

    roundTrip : Test
    roundTrip =
        fuzz (Schema.Fuzzer.fromSchema schema_User)
            "encode/decode round-trip"
            (\user ->
                Schema.encode schema_User user
                    |> Json.Decode.decodeValue (Schema.decoder schema_User)
                    |> Expect.equal (Ok user)
            )

-}
fromSchema : Schema m a -> Fuzzer a
fromSchema schema =
    Schema.toType schema
        |> fromType
        |> Fuzz.andThen
            (\val ->
                case JD.decodeValue (Schema.decoder schema) val of
                    Ok v ->
                        Fuzz.constant v

                    Err e ->
                        "Could not decode generated value from type Schema.Fuzzer.fromType: \n"
                            ++ JD.errorToString e
                            |> Fuzz.invalid
            )


{-| Produce a `Fuzzer` of JSON values shaped like the given type
tree.

This is the lower-level primitive that [`fromSchema`](#fromSchema)
builds on. Reach for it directly when you only need the JSON — for
example when testing an encoder that wasn't built with this package,
or when feeding generated payloads to something outside Elm.

Named types (records, custom types) reachable from the root are
resolved via [`Schema.Type.gatherNamed`](Schema-Type#gatherNamed), so
`Recursive` references are followed correctly up to the internal
depth budget.
-}
fromType : Type.Node m -> Fuzzer Value
fromType meta =
    fromTypeHelp 10 (Type.gatherNamed meta) meta


fromTypeHelp : Int -> Dict String (Type.Node m) -> Type.Node m -> Fuzzer Value
fromTypeHelp limit namedTypes meta =
    case meta.type_ of
        Type.Unit ->
            Fuzz.constant JE.null

        Type.String opt ->
            Fuzz.string
                |> Fuzz.map JE.string

        Type.Uuid ->
            Fuzz.string
                |> Fuzz.map JE.string

        Type.Bool ->
            Fuzz.bool
                |> Fuzz.map JE.bool

        Type.Int ->
            Fuzz.int
                |> Fuzz.map JE.int

        Type.Float ->
            Fuzz.niceFloat
                |> Fuzz.map JE.float

        Type.List itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.listOfLengthBetween 0 limit
                    |> Fuzz.map (JE.list identity)

        Type.Array itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Maybe justType ->
            if limit <= 0 then
                Fuzz.constant JE.null

            else
                Fuzz.oneOf
                    [ Fuzz.constant JE.null
                    , fromTypeHelp limit namedTypes justType
                    ]

        Type.Dict key value ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                Fuzz.map2
                    (\k v ->
                        [ ( "k", k )
                        , ( "v", v )
                        ]
                            |> JE.object
                    )
                    (fromTypeHelp limit namedTypes key)
                    (fromTypeHelp limit namedTypes value)
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Set itemType ->
            if limit <= 0 then
                Fuzz.constant (JE.list identity [])

            else
                fromTypeHelp limit namedTypes itemType
                    |> Fuzz.list
                    |> Fuzz.map (JE.list identity)

        Type.Tuple types ->
            types
                |> List.map (fromTypeHelp limit namedTypes)
                |> Fuzz.sequence
                |> Fuzz.map (JE.list identity)

        Type.Record fields ->
            fields
                |> List.map
                    (\( n, t ) ->
                        fromTypeHelp limit namedTypes t
                            |> Fuzz.map (\v -> ( n, v ))
                    )
                |> Fuzz.sequence
                |> Fuzz.map JE.object

        Type.CustomType args ->
            List.map (variantFuzzers (limit - 1) namedTypes) args.variants
                |> Fuzz.oneOf

        Type.Recursive name ->
            case Dict.get name namedTypes of
                Just t ->
                    fromTypeHelp (limit - 1) namedTypes t

                Nothing ->
                    Fuzz.invalid ("Could not find recursive type: " ++ name)


variantFuzzers : Int -> Dict String (Type.Node m) -> Type.CustomType_Variant m -> Fuzzer Value
variantFuzzers limit namedTypes variant =
    List.map (fromTypeHelp limit namedTypes) variant.args
        |> Fuzz.sequence
        |> Fuzz.map (JE.list identity)
        |> Fuzz.map
            (\args ->
                JE.object
                    [ ( "tag", JE.string variant.name )
                    , ( "args", args )
                    ]
            )
