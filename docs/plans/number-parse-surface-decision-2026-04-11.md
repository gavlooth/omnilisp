# Number Parse Surface Decision - 2026-04-11

## Decision

Use `parse-number` as the canonical permissive number parse API.

- `parse-number` parses a string into either `Integer` or `Double`.
- `parse-number` returns `nil` on parse failure, integer overflow/underflow, or
  non-finite double results.
- `Number` remains a non-callable abstract/meta type descriptor for annotation
  and dispatch.
- `string->number` is not retained as a public compatibility alias during this
  pre-alpha cleanup phase.

## Rationale

- `Integer` and `Double` constructors are target-specific coercions and raise
  deterministic `type/arg-mismatch` payloads on invalid input.
- The current parse operation is intentionally permissive and maybe-valued; it
  does not have constructor semantics because success may produce either numeric
  concrete type, and failure is represented as `nil`.
- `Number` is already documented and tested as an abstract/meta type descriptor,
  not a value-position constructor.

## Rejected Options

- Make `(Number ...)` callable: rejected because it would conflict with the
  current abstract type descriptor contract for `Number`.
- Keep `string->number` public: rejected because constructor/coercion cleanup is
  removing arrow-style conversion aliases from the pre-alpha surface.
- Split into `parse-integer` and `parse-double` only: deferred because the
  current shipped operation intentionally returns either numeric concrete type.

## Migration Notes

- Register `parse-number` on the runtime and compiler primitive surfaces.
- Migrate tests, examples, and current docs from `string->number` to
  `parse-number`.
- Keep the internal C helper name `prim_string_to_number` because the target
  constructors reuse the parser implementation.
