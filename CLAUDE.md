# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`albertdahlin/elm-schema` is an Elm package (v1.1.0) that defines schemas for data types, providing:
- JSON encoding/decoding derived from a single schema definition
- Type descriptions (introspectable `Schema.Type.Node` tree)
- JSON Schema generation targeting OpenAI Structured Outputs
- Fuzz testing via schema-derived fuzzers
- Schema versioning with migration support

## Commands

- **Run all tests:** `npx elm-test`
- **Run a single test file:** `npx elm-test tests/SchemaTest.elm`

## Architecture

The package has two parallel type hierarchies:

1. **`Schema.Schema a`** (internal, in `src/Schema.elm`) — opaque type that bundles a type's `Meta` (structure description), JSON encoder, decoder, and optional version chain. This is the user-facing API. The internal `Type`/`Meta` types mirror `Schema.Type` but are private.

2. **`Schema.Type.Node` / `Schema.Type.Type`** (public, in `src/Schema/Type.elm`) — a plain data structure describing a type's shape, produced by `Schema.toType`. Consumed by:
   - `Schema.Type.JsonSchema.fromType` — generates JSON Schema output targeting OpenAI Structured Outputs (a constrained subset of JSON Schema: every object sets `additionalProperties: false` and lists every property as `required`; named types are emitted under `$defs` and referenced via `$ref`)
   - `Schema.Type.Value` — a generic `Value` union type with its own encoder/decoder, used for type-agnostic data manipulation (e.g., form editing)
   - `Schema.Fuzzer.fromType` — generates `Json.Encode.Value` fuzzers from the type tree

**Schema definition pattern** — uses a pipeline/builder pattern:
- Records: `Schema.record Ctor |> Schema.field ... |> Schema.buildRecord`
- Custom types: `Schema.custom "Name" matchFn |> Schema.variant{0,1,2,3} ... |> Schema.buildCustom`
- Recursive types: `Schema.lazy "TypeName" (\_ -> schema)` where the name must match the `Schema.custom` name

**Custom type JSON format** — variants encode as `{"tag": "VariantName", "args": [...]}`.

**Dict JSON format** — dicts encode as a list of `{"k": ..., "v": ...}` objects (not JS object keys), supporting non-string keys.

## Versioning

A `Schema m a` optionally carries a `Version a` record that enables decoding older on-disk shapes. The public API is two functions:

- `Schema.newVersion : String -> (old -> new) -> Schema m new -> Schema m old -> Schema m new`
- `Schema.dropVersions : a -> Schema m a -> Schema m a`

### Wire format

Versioned values are wrapped: `{"#tag": "<tag>", "#val": <payload>}`. The field names are hard-coded as `"#tag"` and `"#val"` (constants `versionField_tag` / `versionField_val` in `src/Schema.elm`). Only the *latest* tag is ever written on encode.

### Building a chain

`newVersion` takes the new tag, a migration function `old -> new`, the new schema, and the previous schema (as the last argument so it composes naturally with `|>`). Chain multiple calls to support >2 versions:

    schema_v1 : Schema m T1
    schema_v2 : Schema m T2
    schema_v2 = ... |> Schema.newVersion "v2" migrateV1toV2 schema_v2Base schema_v1

    schema_v3 : Schema m T3
    schema_v3 = ... |> Schema.newVersion "v3" migrateV2toV3 schema_v3Base schema_v2

### Version record internals

Each `newVersion` rebuilds a `Version` record holding:

- `tag` — the latest tag. Used by the encoder.
- `taggedDecoders : Dict String (Decoder a)` — every *previous* tag's decoder, each composed through all subsequent migrations so it produces the latest type. On each `newVersion` call, existing entries are `Decode.map`'d forward and the immediately-prior tag is inserted.
- `untaggedDecoder : Decoder a` — the very first (unversioned) schema's decoder, also mapped through the full migration chain. Set on the first `newVersion` call and preserved (mapped forward) on subsequent calls.

### Decode dispatch

`decoder` on a versioned schema (`decoderTagged` internally) uses `Decode.oneOf`:

1. Read `"#tag"`. If it matches the latest tag, decode `"#val"` with the current decoder. Otherwise look up the tag in `taggedDecoders` — hit decodes `"#val"` with that older decoder, miss fails with `"Unknown version: <tag>"`.
2. If there is no `"#tag"` field at all, fall through to `untaggedDecoder` — this handles values written before versioning was introduced.

`decoder` on a *non-versioned* schema is also lenient: it tries the raw payload first, then the payload nested under `"#val"`. This means `Schema.decoder` on any schema can read a `{"#val": ...}` wrapper even if you never called `newVersion`.

### `dropVersions`

`dropVersions default schema` returns an otherwise-identical schema with `version = Nothing` and a decoder that tries, in order: `"#val"`-nested decode, raw decode, then `Decode.succeed default`. It does **not** run the migration chain — it just strips the version wrapper (and tolerates its absence), and is meant for cases where you only need to read the current shape and want a guaranteed-success decoder. Use `newVersion` if you need the migration chain; use `dropVersions` if you're done migrating and want a best-effort reader.

### Gotchas

- Only `newVersion` sets `version = Just …`; every builder constructor (`string`, `record`, etc.) sets `version = Nothing`. So if you pipe a versioned schema through another builder combinator, the version chain is lost. Attach `newVersion` last.
- Encoding a versioned schema always wraps. If you need a raw-encoded value (e.g. for interop), encode through a `dropVersions`'d copy or strip `version` manually.
- The untagged fallback only works for values written by the *first* schema in the chain. Mid-chain unwrapped values are not recoverable.

## Key Constraints

- `elm-explorations/test` is a regular dependency (not test-only) because `Schema.Fuzzer` is an exposed module.
- `Schema.Fuzzer.fromSchema` works by generating JSON via `fromType`, then decoding through the schema's decoder — it does not construct Elm values directly.
