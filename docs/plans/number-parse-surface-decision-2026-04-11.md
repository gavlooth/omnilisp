# Number Parse Surface Decision - 2026-04-11

## Decision

Use `parse-number` as the canonical permissive number parse API.

- `parse-number` parses a string into `Integer`, `BigInteger`, `Float64`, or
  `BigFloat`.
- `parse-number` returns `nil` on parse failure. Decimal integer
  overflow/underflow promotes to `BigInteger`; valid floating inputs that
  overflow `Float64` promote to `BigFloat`.
- `Number` remains a non-callable abstract/meta type descriptor for annotation
  and dispatch.
- `string->number` is not retained as a public compatibility alias during this
  pre-alpha cleanup phase.

## Rationale

- `Integer` and `Float64` constructors are target-specific coercions and raise
  deterministic `type/arg-mismatch` payloads on invalid input.
- The current parse operation is intentionally permissive and maybe-valued; it
  does not have constructor semantics because success may produce one of several
  numeric concrete types, and failure is represented as `nil`.
- `Number` is already documented and tested as an abstract/meta type descriptor,
  not a value-position constructor.

## Rejected Options

- Make `(Number ...)` callable: rejected because it would conflict with the
  current abstract type descriptor contract for `Number`.
- Keep `string->number` public: rejected because constructor/coercion cleanup is
  removing arrow-style conversion aliases from the pre-alpha surface.
- Split into `parse-integer` and `parse-double` only: deferred because the
  current shipped operation intentionally returns multiple numeric concrete
  types.

## Update - 2026-04-15

`BigInteger` and `BigFloat` are now shipped concrete numeric types.
`parse-number` keeps its permissive maybe-valued contract, but syntactically
valid decimal integer overflow/underflow now returns `BigInteger` instead of
`nil`, and syntactically valid floating inputs that overflow `Float64` return
`BigFloat`. Malformed strings still return `nil`.

## Migration Notes

- Register `parse-number` on the runtime and compiler primitive surfaces.
- Migrate tests, examples, and current docs from `string->number` to
  `parse-number`.
- Keep the internal C helper name `prim_string_to_number` because the target
  constructors reuse the parser implementation.
