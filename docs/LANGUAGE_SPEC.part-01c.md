### 2.2 Truthiness

Normative predicate contract:
- Predicate positions in `if`, `when`, and `match` guards use the same
  truthiness rules.
- **Falsy:** `nil`, `false`
- **Truthy:** everything else.

| Predicate input | Example | Truthiness | Notes |
|---|---|---|---|
| `nil` | `nil` | falsy | Absence value |
| `false` | `false` | falsy | Boolean false |
| `Void` | `#<void>` | truthy | Command/effect completion token |
| numbers | `0`, `-1`, `3.14` | truthy | Zero is still truthy |
| strings | `""`, `"omni"` | truthy | Empty string is truthy |
| collections | `'()`, `[]`, `{}` | truthy | Empty collections are truthy |

### 2.3 `Void` vs `Nil` Contract

Normative rule:
- `Void` means successful command/effect completion with no payload.
- `Nil` means absence/query-miss (or falsey result in predicate-style APIs).
- `Void` is an operational completion token, not a data/absence sentinel.
- APIs should not encode query-miss/optional absence using `Void`; use `Nil`.
- `Void` is truthy under Omni truthiness rules.

Contract examples:

```lisp
(type-of (block (define x 1) (set! x 2)))   ; => 'Void
(type-of (let (d {'a 1}) (remove! d 'a)))   ; => 'Void
(if (block (define x 1) (set! x 2)) 1 0)    ; => 1  (Void is truthy)

(type-of (ref {'a 1} 'missing))             ; => 'Nil
(type-of (has? {'a 1} 'missing))            ; => 'Nil
```

### 2.4 Equality

`=` performs structural equality:
- Integers and Float64 values: numeric comparison
- Strings: character-by-character
- Symbols: identity (interned)
- Lists: recursive structural equality
- Other types: identity

---
