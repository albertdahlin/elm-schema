# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`albertdahlin/elm-schema` is an Elm package (v1.1.0) that defines schemas for data types, providing:
- JSON encoding/decoding derived from a single schema definition
- Type descriptions (introspectable `Schema.Type.Node` tree)
- JSON Schema (Draft 7) generation
- Fuzz testing via schema-derived fuzzers
- Schema versioning with migration support

## Commands

- **Run all tests:** `npx elm-test`
- **Run a single test file:** `npx elm-test tests/SchemaTest.elm`

## Architecture

The package has two parallel type hierarchies:

1. **`Schema.Schema a`** (internal, in `src/Schema.elm`) — opaque type that bundles a type's `Meta` (structure description), JSON encoder, decoder, and optional version chain. This is the user-facing API. The internal `Type`/`Meta` types mirror `Schema.Type` but are private.

2. **`Schema.Type.Node` / `Schema.Type.Type`** (public, in `src/Schema/Type.elm`) — a plain data structure describing a type's shape, produced by `Schema.toType`. Consumed by:
   - `Schema.Type.JsonSchema.fromType` — generates JSON Schema Draft 7 output
   - `Schema.Type.Value` — a generic `Value` union type with its own encoder/decoder, used for type-agnostic data manipulation (e.g., form editing)
   - `Schema.Fuzzer.fromType` — generates `Json.Encode.Value` fuzzers from the type tree

**Schema definition pattern** — uses a pipeline/builder pattern:
- Records: `Schema.record Ctor |> Schema.field ... |> Schema.buildRecord`
- Custom types: `Schema.custom "Name" matchFn |> Schema.variant{0,1,2,3} ... |> Schema.buildCustom`
- Recursive types: `Schema.lazy "TypeName" (\_ -> schema)` where the name must match the `Schema.custom` name

**Versioning** — `Schema.newVersion` chains old→new schemas with a migration function and version tag. Encoded values are wrapped in `{"#tag": "v2", "#val": ...}`. Old untagged values decode via `untaggedDecoder`.

**Custom type JSON format** — variants encode as `{"tag": "VariantName", "args": [...]}`.

**Dict JSON format** — dicts encode as a list of `{"k": ..., "v": ...}` objects (not JS object keys), supporting non-string keys.

## Key Constraints

- `elm-explorations/test` is a regular dependency (not test-only) because `Schema.Fuzzer` is an exposed module.
- `Schema.Fuzzer.fromSchema` works by generating JSON via `fromType`, then decoding through the schema's decoder — it does not construct Elm values directly.
