# As-Pattern Status

`'as` patterns are implemented in the current Omni parser/compiler pipeline.

Omni uses quoted marker symbols for this feature, not a bare infix form.

Canonical shape:

```lisp
(match [1 2]
  (([x y] 'as pair) pair))
```

Semantics:
- match the value against the inner pattern
- bind destructured variables from the inner pattern
- also bind the whole matched value to the trailing name

Rejected surface form:

```lisp
[[x y] as pair]
```

That shape is not canonical Omni syntax. The language already uses quoted symbols such as `'as` and `'all` as explicit markers in surface forms, and new syntax should stay consistent with that rule.

Implemented components:
- the pattern AST includes `PAT_AS`
- `src/lisp/parser_pattern_match.c3` parses `(pattern 'as name)`
- `src/lisp/eval_pattern_matching.c3` binds both inner variables and the whole matched value
- `src/lisp/compiler_native_effect_compilation_flat_style.c3` supports `PAT_AS` in native pattern checks/bindings
- macro-side pattern variable collection and warmup paths recurse through `PAT_AS`

Practical consequence:
- `'as` pattern examples work in `match` and macro pattern clauses
- bare `as` is rejected with an explicit parser error

Scope note:
- this is no longer an open feature gap
- the earlier audit note in `report.md` is now superseded by the implementation
