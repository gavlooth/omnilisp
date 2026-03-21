# Type System Syntax Reference

This document describes Omni's Julia-style type system syntax.
Most features below are implemented (see Implementation Status at the end).

## Design Principles

1. **Reuse existing constructs** - quote, metadata maps, define attributes
2. **Type position vs value position** - context determines interpretation
3. **Minimal new tokens** - no special syntax like `{T}` for generics
4. **Lisp-like** - parentheses for application, including type application
5. **Flat metadata** - bounds inline in metadata dict, explicit grouping only when ambiguous
6. **Clarity over terseness** - canonical language-facing names prefer descriptive forms; shorthands remain compatibility aliases unless explicitly removed

---

## Type System Syntax

### 1.1 Type Annotations

```lisp
; Simple type annotation
^Integer x
^String name

; Compound types use parentheses
^(List Integer) items
^(Dictionary String Integer) cache
^(Lambda Integer Integer) successor      ; function type
```

### 1.2 Function Types

Use `Lambda` to match value-level syntax:

```lisp
; Value level
(lambda (x) (* x 2))

; Type level
^(Lambda Integer Integer) inc            ; Integer -> Integer
^(Lambda Integer Integer Integer) add    ; (Integer, Integer) -> Integer
^(Lambda A B) f                  ; polymorphic
```

### 1.2.1 Callable Type Symbols in Value Position

Type symbols may also be callable in value position when they define a
constructor or coercion surface.

```lisp
; User-defined nominal constructors
(Point 1 2)
(Box 42)

; Builtin coercion constructors
(Integer 3.9)    ; => 3
(Double 3)       ; => 3.0
(String 3)       ; => "3"
(Symbol "name")  ; => 'name
(Boolean 0)      ; => true
(Boolean nil)    ; => nil
(Nil nil)        ; => nil
(Closure (lambda (x) x))
(Coroutine (lambda () 1))

; Runtime object constructor / conversion surface
(List [1 2 3])
(Array '(1 2 3))
(Dictionary 'a 1 'b 2)
(Iterator [1 2 3])
(TimePoint 'date 2026 3 7)
(Set 1 2 3)
(Iterator (lambda () nil))
```

Current rule:

- nominal user-defined types and union variants use the type symbol itself as
  the constructor in value position,
- selected builtin/runtime types may also expose callable constructor/coercion
  behavior through the same symbol,
- descriptive names are canonical (`Integer`, `Boolean`, `Dictionary`),
- shorthand names only exist when explicitly approved; the current builtin
  exception is `Dict` alongside canonical `Dictionary`,
- no lowercase compatibility wrappers are part of the canonical constructor
  surface.

Current exception note:

- Omni does not currently define a builtin `Empty` or bottom type.
- `Nil` is the language-level empty/false value type.
- `Void` is now a real builtin singleton type/value; `(Void)` constructs the
  runtime no-result value, and FFI `^Void` returns map to that same value.
- Existing side-effecting helpers may still return `nil` for compatibility;
  migrating ordinary no-result surfaces to `Void` is a separate language
  follow-up, not an implicit compatibility break.

### 1.2.2 Lambda Call-Boundary Checking (Design + Current Runtime Contract)

Status: implemented (`L3.3`) for argument compatibility. Return-type
compatibility is an explicit non-goal in current syntax/runtime.

Supported annotation shapes in callable parameter positions:

- `^Lambda` - requires a closure/function-valued argument.
- `^(Lambda A1 ... An R)` - currently requires a closure/function-valued
  argument (shape gate). Embedded `A1..An/R` are not yet enforced as full
  higher-order signature contracts.
- Standard typed-parameter forms continue to be enforced at invocation
  boundaries:
  - simple nominal (`^Integer`, `^Point`, ...)
  - value literal (`^(Value lit)`)
  - metadata constraint (`^{'T Bound}`)
  - unresolved type-variable forms (`^T`) with repeated-symbol unification.

Call-boundary argument checking rules:

1. For typed closures, fixed parameters are validated before body evaluation
   (including variadic closures: fixed-prefix args are checked).
2. Match relation reuses dispatch compatibility:
   - literal exact match,
   - exact nominal match,
   - subtype compatibility,
   - `Any` fallback.
3. Repeated type variables unify by runtime inferred type.
4. Metadata constraints are enforced against unified bindings.

Deterministic diagnostics:

- Failure code: `type/arg-mismatch` (or `type/arity` for arity-form mismatches).
- Payload `data` includes deterministic keys:
  - `failure`
  - `param-index`
  - `expected`
  - `actual`
  - `expected-arity`
  - `actual-arity`

Explicit non-goals (current release):

- Lambda return-type annotations on closure definitions are not yet part of the
  surface syntax; return compatibility is therefore not runtime-enforced.
- `^(Lambda A1 ... An R)` does not yet enforce full higher-order
  argument/return compatibility of the passed closure signature.
- No higher-rank/higher-kinded lambda typing or type inference beyond existing
  dispatch-style type-variable unification.

### 1.3 Abstract Type Hierarchy

```lisp
; Root abstract type
(define [abstract] Number)

; With parent (parenthesized form)
(define [abstract] (Real Number))
(define [abstract] (Integer Real))
```

### 1.4 Concrete Struct Types

```lisp
; Simple struct
(define [type] Point
  (^Float x)
  (^Float y))

; Inheritance (parenthesized form)
(define [type] (Point3D Point)
  (^Float z))
```

`[struct]` is accepted as an alias for `[type]`:

```lisp
(define [struct] Vec2 (^Integer x) (^Integer y))
```

### 1.5 Parametric Types

```lisp
; Single parameter
(define [type] (Box T)
  (^T value))

; Multiple parameters
(define [type] (Pair A B)
  (^A first)
  (^B second))

; Type application in annotations
^(Box Integer) intBox
^(Pair String Integer) entry
```

**Construction** — infer type from arguments:

```lisp
(Box 42)                    ; infers Box{Integer}
(Pair "key" 42)             ; infers Pair{String, Integer}
(^(Box Integer) (Box 42))   ; explicit when needed
```

### 1.5.1 Constructor Type-Application Checking (Design Contract)

Status: implemented (`L3.2`), with runtime-inference fallback when nested
constructor args are not materialized on runtime values.

This section defines how constructor annotations such as `^(Box Integer)` are
checked in evaluation.

#### A. Applicability gate

For an annotation `^(Ctor A1 A2 ... An)` applied to runtime value `v`:

1. `v` must be an instance value with a registered constructor/type id.
2. The instance constructor must be `Ctor` (or a constructor whose nominal
   type parent chain includes `Ctor` when parent compatibility is explicitly
   enabled for that check site).
3. Runtime inferred type-arg vector for `v` must exist (current source:
   `type-args` inference path).

Failure at this stage is a constructor mismatch diagnostic.

#### B. Arity matching

Let:
- expected arity = number of annotation type args (`n`)
- actual arity = inferred runtime type-arg count on `v`

Rules:
- if `n != actual arity`, raise canonical arity mismatch diagnostic.
- no implicit fill/defaulting for missing type args.
- no truncation of extra runtime type args.

#### C. Type-arg matching

For each index `i` in `0..n-1`, compare `expected[i]` vs `actual[i]`.

Match relation:
- exact nominal type match succeeds.
- parent/subtype match succeeds only where the check site explicitly requests
  subtype-compatible comparison; default is invariant matching for constructor
  parameters.
- `Any` matches any runtime type arg.
- nested constructor applications compare recursively using the same rules.

Failure at any index raises canonical type-arg mismatch diagnostic with
deterministic index metadata.

#### D. Nested constructor behavior

Nested expected args are checked recursively, for example:

```lisp
(^(Box (List Int)) v)
```

Normative recursion rules:
- recurse constructor-by-constructor until leaf nominal args.
- preserve the same arity/type-arg mismatch diagnostics at the first failing
  nested position.
- nested failure payloads include both top-level arg index and nested path
  segments for deterministic debugging.

#### E. Canonical diagnostics

All constructor type-application failures MUST use canonical payloaded `raise`
shape:

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Required diagnostic codes:
- `type/ctor-arity-mismatch`
- `type/ctor-type-arg-mismatch`

Recommended deterministic `data` fields:
- `ctor` (symbol)
- `expected-args` (list of type symbols/forms)
- `actual-args` (list of inferred type symbols/forms)
- `arg-index` (int, for arg mismatch)
- `arg-path` (list, for nested mismatch path; top-level index first)

Message templates should be deterministic and mention constructor + first
failing index/path.

### 1.6 Type Constraints

Bounds are specified directly in metadata (flat by default):

```lisp
; Single constraint - flat in metadata
^{'T Number}                            ; T <: Number

; With type annotation
^{'type (Lambda T T) 'T Number}         ; type + bound

; Multiple constraints
^{'T Number 'U Comparable}

; On a method
(define [method] ^{'T Number}
  double (^T x)
  (+ x x))

; Constraint on parametric type
(define [type] ^{'T Comparable}
  (SortedSet T)
  (^(List T) items))
```

**Explicit `'with` only when ambiguous** (rare):

```lisp
; If type variable conflicts with metadata key
^{'type (Lambda pure pure) 'with {'pure Number} 'pure true}

; For visual grouping (optional style choice)
^{'type (Lambda T U V)
  'with {'T Number 'U Comparable 'V T}}
```

### 1.7 Union Types / Sum Types

```lisp
; Simple union (enum-like)
(define [union] Flag True False)

; Parametric union
(define [union] (Option T)
  None
  (Some T))

; Multiple type parameters
(define [union] (Either E A)
  (Left E)
  (Right A))

; Construction
None                  ; nullary variant
(Some 42)             ; variant with value

; Pattern matching
(match opt
  (None default)
  ((Some x) (use x)))
```

### 1.8 Type Aliases

```lisp
; Simple alias
(define [alias] Text String)

; Parametric alias
(define [alias] (Maybe T) (Option T))

; Complex alias
(define [alias] (Result T) (Either String T))
```

---

## Metadata Value Disambiguation

When metadata is flat, the **value type** determines meaning:

| Value Type | Meaning | Example |
|------------|---------|---------|
| Type name (symbol) | Type bound | `'T Number` → T <: Number |
| Boolean | Metadata flag | `'pure true` |
| String | Metadata info | `'doc "description"` |

```lisp
; All flat, unambiguous
^{'type (Lambda T T) 'T Number 'pure true 'inline true}
;        ↑ type        ↑ bound   ↑ flag    ↑ flag
```

---

## Summary: Final Syntax

| Feature | Syntax | Example |
|---------|--------|---------|
| Type annotation | `^Type` | `^Integer x` |
| Compound type | `^(Ctor Args)` | `^(List Integer) xs` |
| Function type | `^(Lambda Args Ret)` | `^(Lambda Integer Integer) f` |
| Type bound | `'T Type` (flat) | `^{'T Number}` |
| Multiple bounds | `'T Type 'U Type` | `^{'T Number 'U Eq}` |
| Explicit grouping | `'with {...}` | `^{'with {'T Number}}` |
| Abstract type | `(define [abstract] ...)` | `(define [abstract] Number)` |
| Struct type | `(define [type] ...)` (`[struct]` alias) | `(define [type] Point ...)` |
| Parametric type | `(define [type] (Name T) ...)` | `(define [type] (Box T) ...)` |
| Union type | `(define [union] ...)` | `(define [union] (Option T) ...)` |
| Type alias | `(define [alias] ...)` | `(define [alias] Text String)` |

---

## Julia Parity Matrix (Release-Critical)

Status tags:
- `done`: implemented and documented with tests.
- `partial`: implemented in core behavior, but semantics/tests/docs are incomplete.
- `missing`: not implemented yet.

| Area | Julia Baseline | Omni Current Behavior | Status | Evidence / Anchor |
|------|----------------|-----------------------|--------|-------------------|
| Method specificity ordering | More-specific method wins | Score model: `Value=1000`, `exact=100`, `subtype=10`, `any=1`; highest score wins | `done` | `src/lisp/eval_dispatch_types.c3` (`method_match_score`) |
| Ambiguity handling | Ambiguity is diagnosed deterministically | Equal best score raises `ambiguous method call` with explicit equal-specificity detail (no implicit tie-break winner) | `done` | `src/lisp/eval_dispatch_types.c3` (`find_best_method`); `src/lisp/tests_advanced_type_effect_ffi_groups.c3` |
| Parametric type behavior | Parametric construction + constraints + substitution | Parametric constructors + `type-args` inference are implemented; method type variables unify by symbol at dispatch (`(^T a) (^T b)` requires same runtime type), with bounds checked via metadata constraints | `done` | `src/lisp/eval_type_evaluators.c3`; `src/lisp/eval_dispatch_types.c3`; `src/lisp/tests_advanced_type_effect_ffi_groups.c3` |
| Variance policy | Explicit variance model in parametric dispatch | Explicit invariant policy: type params are invariant; `+T/-T` markers are rejected with deterministic parser errors | `done` (invariant policy) | `src/lisp/parser_type_defs.c3`; `src/lisp/tests_advanced_type_effect_ffi_groups.c3` |
| Union participation in dispatch applicability | Union/variant types participate in subtype dispatch | Union variants are registered as subtype of union; dispatch uses subtype chain so variant-specialized methods outrank union-parent methods | `done` | `src/lisp/eval_type_evaluators.c3` (`eval_defunion_register_variant_type`); `src/lisp/tests_advanced_type_effect_ffi_groups.c3` |
| Numeric conversion in dispatch | Numeric conversion should be explicit at call sites | Dispatch uses exact-or-subtype applicability only; cross-numeric matching requires constructor conversion such as `(Double 7)` | `done` | `src/lisp/eval_dispatch_match.c3`; `src/lisp/tests_advanced_type_dispatch_groups.c3` |
| `where`-style constraints | `where` clauses on methods/types | Omni uses metadata constraints (`^{'T Number}`), not Julia `where` syntax | `done` (intentional difference) | This document §1.6; `src/lisp/tests_advanced_type_effect_ffi_groups.c3` (constraint tests) |
| Match/union exhaustiveness | Exhaustiveness diagnostics | Runtime missing-case diagnostics are normative; compile-time exhaustiveness checker is currently an explicit non-goal | `done` (runtime policy) | `src/lisp/eval_dispatch_types.c3` (`format_match_error`); `src/lisp/tests_runtime_feature_groups.c3` |

### Parity Position: Julia vs Omni

| Topic | Par with Julia | Intentionally Different |
|-------|----------------|-------------------------|
| Multiple dispatch core | Yes (specificity-based) | No |
| Typed method fallback behavior | Yes (fallback possible) | No |
| Ambiguity policy | Partial (deterministic detection) | Yes: error on equal score, no implicit winner |
| Parametric type declaration | Partial | Yes: metadata-bound constraints instead of `where` |
| Numeric conversion in dispatch | Partial | Yes: Omni requires explicit constructor conversion (`(Double 7)`) instead of implicit dispatch-time widening |
| Exhaustiveness checking | Partial | Yes: runtime diagnostics first, compile-time check pending |

### Test Anchors

- Dispatch core and constraints: `src/lisp/tests_advanced_type_effect_ffi_groups.c3`
- Dispatch diagnostics + match diagnostics: `src/lisp/tests_runtime_feature_groups.c3`
- Type reference behavior: `src/lisp/eval_dispatch_types.c3`, `src/lisp/eval_type_evaluators.c3`

### Exhaustiveness Policy (CP-13)

- Omni currently enforces **runtime** match exhaustiveness diagnostics, not compile-time rejection.
- A match expression is accepted syntactically even if variants are missing.
- If execution reaches an uncovered case without wildcard fallback, runtime raises a diagnostic that includes missing variants for union scrutinees.
- Wildcard (`_`) is the canonical explicit opt-out for compile-time exhaustiveness.

---

## Complete Example

```lisp
; Abstract hierarchy
(define [abstract] Number)
(define [abstract] (Real Number))

; Parametric struct (type params stored, not substituted)
(define [type] (Box T)
  (^T value))

; Union type
(define [union] (Option T)
  None
  (Some T))

; Multiple dispatch
(define (double (^Integer x)) (+ x x))
(define (double (^Double x)) (+ x x))

; Union pattern matching
(match (Some 42)
  (None "empty")
  ((Some x) x))          ; => 42

(double 21)              ; => 42 (dispatches to Integer version)
```

---

## Implementation Status (as of 2026-03-09)

### Implemented (Phases 1-7)
- [x] TypeRegistry wired to Interp with FNV-1a hash lookup
- [x] `infer_value_type()` maps runtime values to TypeId
- [x] `[type]` struct definitions with typed fields and constructors (`[struct]` alias supported)
- [x] `[abstract]` type definitions with parent hierarchy
- [x] `[union]` ADT definitions with variant constructors
- [x] `[alias]` type alias definitions
- [x] `^Type` annotation parsing (simple, compound `^(List Integer)`, Value `^(Value 42)`)
- [x] `^{'T Number}` flat metadata dict parsing in parser
- [x] Multiple dispatch via MethodTable (typed `define` creates dispatch entries)
- [x] Value dispatch `^(Value literal)` for value-level pattern matching (literals: int/symbol/string/bool)
- [x] Multi-argument dispatch
- [x] Dispatch scoring: Value=1000, exact=100, subtype=10, any=1
- [x] Struct field access via dot-path: `point.x`, `line.start.y`
- [x] Struct field mutation: `(set! point.x 99)`, nested paths
- [x] Constructor pattern matching: `(match opt (None 0) ((Some x) x))`
- [x] Nullary constructor auto-detection in patterns (e.g., `None`)
- [x] I/O effects: print/println/etc. go through `signal` with fast path
- [x] `type-of`, `is?`, `instance?`, `type-args` introspection primitives
- [x] Parametric types: `(define [type] (Box T) (^T value))` with type param collection
- [x] Type arg inference: `(type-args (Box 42))` → `'(Integer)` — inferred from field values
- [x] Constrained dispatch: `^{'T Number}` enforced at dispatch — arg type must be subtype of bound
- [x] Parent vs type-param disambiguation at eval time (first symbol = registered type → parent)
- [x] Constructor type-application checking (`(^(Ctor ...) value)`) with canonical mismatch/arity diagnostics and deterministic payload fields (`ctor`, `expected-args`, `actual-args`, `arg-index`, `arg-path`)  
  Regression anchors: `src/lisp/tests_advanced_type_effect_ffi_groups.c3` (`run_advanced_type_parametric_ctor_annotation_tests`)
- [x] Lambda typed call-boundary argument checking with deterministic mismatch payload fields (`failure`, `param-index`, `expected`, `actual`, `expected-arity`, `actual-arity`) and cross-coverage for dispatch/union/explicit-conversion behavior  
  Regression anchors: `src/lisp/tests_advanced_type_effect_ffi_groups.c3` (`run_advanced_type_lambda_call_boundary_tests`)
- [x] 100+ type/dispatch/effect tests all passing

### Implementation Notes

**Dispatch mechanism**: When a typed `define` is encountered, the evaluator checks if the name is already bound. If bound to a closure, it promotes to a MethodTable with the existing closure as fallback. If already a MethodTable, adds a new entry. This means dispatch is backwards-compatible — untyped defines work exactly as before.

**Constructor patterns**: The parser generates PAT_CONSTRUCTOR for `(ConstructorName sub-patterns...)`. In match_pattern, it checks if the value is an INSTANCE whose type matches the constructor name, then recursively matches sub-patterns against fields. Nullary constructors like `None` are detected in the PAT_VAR case by looking up the symbol in the type registry.

**I/O effects**: Primitives are registered as `__raw-print`, `__raw-println`, etc. Stdlib wrappers (`print`, `println`, etc.) call `(signal io/print x)`. In `eval_perform` (internal C3 function), before searching the handler stack, a fast path checks if the tag is an I/O effect symbol and no user handler is installed -- if so, calls the raw primitive directly.
