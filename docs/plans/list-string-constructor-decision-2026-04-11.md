# List/String Constructor Decision - 2026-04-11

## Decision

Use constructor-first list/string conversion.

- `List(String)` is the canonical string-to-list conversion surface.
- `String(List)` is the canonical list-to-string conversion surface.
- `string->list` and `list->string` are not retained as public compatibility
  aliases during this pre-alpha cleanup phase.

## Semantics

- `List(String)` returns a proper list of one-codepoint strings.
- `String(List)` concatenates a proper list of string fragments.
- `String(nil)` returns the empty string because `nil` is the empty list.
- `String(List)` accepts string fragments, not only one-codepoint strings; this
  preserves the existing `list->string` concatenation behavior.
- `String(List)` rejects non-string list elements.
- `String(List)` rejects improper lists.

## Rejected Options

- Keep `string->list` and `list->string` public: rejected because constructor
  and multiple-dispatch surfaces are now the canonical conversion model.
- Enforce exactly one codepoint per `String(List)` element: rejected for this
  slice because the existing implementation already treats the operation as
  string-fragment concatenation, and tightening it would be a separate semantic
  migration.
- Use grapheme clusters for `List(String)`: rejected for this slice because the
  current UTF-8 string indexing and `char-at` contract are codepoint-based.

## Migration Notes

- Update docs and tests to use `List` and `String` constructor forms.
- Remove the public primitive/compiler-map entries for `string->list` and
  `list->string`.
- Keep internal C helper functions available for constructor implementation and
  lower-level memory regression tests.
