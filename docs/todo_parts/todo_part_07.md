# Active TODO Index Part 07

Source: `TODO.md`

- [x] `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098` consolidate compiler
  primitive classification around the primitive hash table
  - closure evidence:
    - free-variable and delegation primitive checks now use one shared
      hash-backed classifier instead of duplicated hardcoded name arrays.
    - legacy non-hash exceptions for `ForeignHandle` and `__ui-ftxui-run`
      remain excluded from closure capture without adding value-position
      lowering.
    - regression coverage now verifies hash-only primitives such as `Dict` and
      `json-parse` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103` replace the public FFI pointer
  annotation surface with `^ForeignHandle`
  - closure evidence:
    - `docs/LANGUAGE_SPEC.md` now documents `^ForeignHandle` as the opaque
      foreign-handle annotation and only accepts live `FFI_HANDLE` values or
      `nil`.
    - `docs/reference/09-concurrency-ffi.md` now states that opaque foreign C
      values use `^ForeignHandle` rather than raw integer addresses.
    - `docs/PROJECT_TOOLING.md` now maps non-string opaque foreign bindings to
      `^ForeignHandle` in bindgen output.
    - runtime FFI call packing now rejects raw integer addresses for
      foreign-handle arguments and expects `nil` or live `FFI_HANDLE` values.
    - non-null foreign-handle returns now produce non-owning `FFI_HANDLE` values
      instead of raw address integers.
    - the audit/remediation and AOT runtime/linking notes now use
      `ForeignHandle` instead of the stale `Pointer` exception wording where
      current-state docs describe the live surface.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=43 fail=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` carry `ForeignHandle`
  metadata policy through the AOT runtime bridge
  - closure evidence:
    - AOT generated declarations now carry `ForeignHandle` policy descriptors
      into `aot::ffi_declare_fn`.
    - Parameter policy descriptors preserve handle family and nullability.
    - Return policy descriptors preserve handle name, ownership, and finalizer
      symbol, and the AOT bridge resolves owned-return finalizers before
      installing the bound primitive.
    - AOT lowering rejects owned `ForeignHandle` parameter policy fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=209 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104` implement the interpreter/JIT
  ForeignHandle metadata dictionary policy
  - closure evidence:
    - `^ForeignHandle` remains the simple default foreign-handle annotation.
    - FFI-local metadata dictionaries now accept the refinements:
      `^{'name File 'ownership owned 'finalizer fclose}` implies
      `ForeignHandle`, and the explicit `^{'type ForeignHandle ...}` form is
      also accepted.
    - runtime FFI now enforces handle family and non-null policy at call
      packing, resolves owned return finalizers with `dlsym`, and wraps pointer
      returns in named `FFI_HANDLE` boxes.
    - FFI AST serialization now preserves metadata dictionary annotations for
      compiler roundtrip coverage.
    - AOT policy propagation was split into
      `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` and is now closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=204 fail=0`

- [x] `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096` fix generated
  e2e C3 primitive capture name sanitization
  - closure evidence:
    - shared compiler symbol-value emission now routes primitive references and
      closure capture initializers through the same primitive-cache path.
    - runtime primitives exposed by the e2e corpus now have compiler primitive,
      free-variable, and hash-table coverage: `error`, `error?`,
      `error-message`, `is?`, `instance?`, and `type-args`.
    - AOT `Coroutine` now accepts `aot::make_closure` wrapper thunks without
      inspecting JIT-only closure payload fields.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `Coroutine` type probe returns `Coroutine`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-NUMBER-PARSE-SURFACE-085` canonicalize permissive numeric parsing
  as `parse-number`
  - decision note: `docs/plans/number-parse-surface-decision-2026-04-11.md`
    selects `parse-number` and keeps `Number` non-callable.
  - closure evidence:
    - public runtime/compiler primitive surfaces now register `parse-number`
      instead of `string->number`.
    - live tests, examples, and docs now use `parse-number`.
    - `Number` remains an abstract/meta type descriptor for annotation and
      dispatch, not a value-position constructor.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `parse-number` int/double/nil results, removed
      `string->number` binding, live `parse-number` binding, and non-callable
      `Number`
    - bounded `advanced-macro-hygiene-string-number` subgroup: `pass=9 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`
    - `./build/main --check examples/deduce_crud_server.omni`
    - `./build/main --check examples/finwatch/smoke_test.omni`
    - broader Docker `scripts/run_e2e.sh` reached generated-source parity but
      failed the generated C3 build on primitive capture names `error` /
      `error?`; subsequently closed as
      `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`.

- [x] `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084` canonicalize list/string
  conversion through constructors
  - decision note: `docs/plans/list-string-constructor-decision-2026-04-11.md`
    selects `List(String)` and `String(List)` as the canonical public surface.
  - closure evidence:
    - `List(String)` now reuses the UTF-8 codepoint splitter and returns a
      proper list of one-codepoint strings.
    - `String(List)` now concatenates proper lists of string fragments and
      treats `nil` as the empty list / empty string.
    - public `string->list` and `list->string` primitive/compiler aliases were
      removed; internal C helpers remain for runtime implementation and memory
      regression tests.
    - docs and Lisp-level tests now use constructor forms.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `List(String)`, `String(List)`, `String(nil)`,
      non-string list element rejection, and removed public arrow bindings
    - bounded `advanced-unicode-iterator` subgroup: `pass=138 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `limit-busting` slice: `pass=17 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`

- [x] `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094` decide byte versus codepoint
  semantics for generic string `length` and `ref`
  - closure evidence:
    - selected codepoint/character semantics for generic string sequence
      operations to match `string-length`, `char-at`, and `List(String)`.
    - kept byte count explicit through `string-byte-length`.
    - changed generic `ref` and postfix `.[index]` on strings to return
      single-character strings instead of byte integers.
    - added non-ASCII regressions for generic `length`, `ref`, and postfix
      indexing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `length`, `string-length`, `string-byte-length`, `ref`,
      `char-at`, and postfix `.[index]` on non-ASCII strings
    - bounded `advanced-unicode-iterator` subgroup: `pass=136 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=142 fail=0`

- [x] `AUDIT-CONS-REF-SPEC-PARITY-095` reconcile cons/list `ref` behavior with
  the language spec
  - closure evidence:
    - kept the existing tested runtime behavior as the contract rather than
      narrowing `ref` on cons cells back to pair-only `0`/`1` access.
    - updated `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`, and
      `docs/reference/11-appendix-primitives.md` to document cons/list chain
      indexing, negative indexes, and dotted terminal tail length/indexing.
  - validation:
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`

- [x] `AUDIT-LIST-PREDICATE-CONTRACT-093` reconcile `list?` proper-list
  semantics with the stdlib implementation
  - closure evidence:
    - registered the existing strict `prim_is_list` primitive as public `list?`.
    - removed the stdlib override that treated every pair as a list.
    - added regressions proving `(list? (cons 1 2))` is false.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `advanced-type-dispatch-mutation-chain` subgroup:
      `pass=237 fail=0`

- [x] `AUDIT-LIST-WALKER-IMPROPER-LIST-092` normalize improper-list handling
  across public list walkers
  - closure evidence:
    - added a private proper-list guard and applied it across the public stdlib
      list walkers covered by the audit: `map`, `filter`, `foldl`, `foldr`,
      `append`, `take`, `drop`, `zip`, `for-each`, `any?`, `every?`,
      `flatten`, `partition`, `remove` via `filter`, `find`, and `nth`.
    - mirrored the guard in the compiler stdlib prelude for the overlapping
      walker definitions.
    - guarded nested list traversal in `flatten` so improper inner lists no
      longer fall through to low-level `car`/`cdr` failures.
    - left pair/iterator protocols such as `stream-take` and iterator `drop`
      outside this proper-list-only lane.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after partial-success slice:
      `pass=275 fail=0`
    - bounded `compiler` slice after partial-success slice: `pass=196 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after remaining walker slice:
      `pass=285 fail=0`
    - bounded `compiler` slice after remaining walker slice: `pass=196 fail=0`

- [x] `AUDIT-EVAL-VALUE-TO-EXPR-FAIL-CLOSED-096` harden the runtime `eval`
  data-to-expression conversion path
  - closure evidence:
    - value-to-expression conversion now enforces structural arity for malformed
      special-form data instead of defaulting missing operands to `nil` or
      ignoring extras.
    - `define`, two-operand `set!`, `capture`, and `signal` now reject
      non-symbol names/tags instead of coercing them to symbol id `0`.
    - multi-argument `set!` data forms now lower through normal call dispatch,
      preserving parser-equivalent generic collection setter behavior.
    - lambda and let data forms now preserve parser-equivalent implicit block
      bodies, and `macroexpand` now surfaces structural conversion errors for
      malformed cons forms.
    - added eval and macroexpand regressions in the advanced stdlib numeric
      introspection group.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after arity/name slice:
      `pass=265 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after block/macroexpand slice:
      `pass=268 fail=0`

- [x] `AUDIT-NAMED-LET-INIT-ERROR-PROPAGATION-097` preserve initializer
  error values through named `let`/tail multi-argument calls
  - closure evidence:
    - `make_cons` now treats successfully ESCAPE-promoted `ERROR` values as
      first-class cons elements instead of always interpreting an `ERROR` tag
      result as promotion failure.
    - `append` now checks the intermediate `(reverse a)` value before entering
      its named-let loop, preserving the original proper-list error for
      improper left input.
    - added core regressions for tail multi-argument calls that ignore or
      return an error-valued argument, plus the append improper-left regression.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `c3c build --sanitize=address --warn-deprecation=no`: green
    - direct tail multi-arg/append probes: green
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`
    - bounded `tco-recycling` slice: `pass=11 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-PIKA-REGEX-STRESS-CACHE-090` fix repeatable bounded `pika` regex
  stress/cache failures
  - closure evidence:
    - direct Pika large-pattern controls passed, while named-let
      multi-argument recursion with string parameters reproduced the same
      `arg list too short` failure without invoking regex internals.
    - `make_cons` now permits identity promotion only for values already owned
      by the current target scope chain or ESCAPE lane, and still rejects
      identity returns for current TEMP values that failed to promote.
    - added a memory-lifetime regression covering named-let string argument-list
      promotion under the TCO/ESCAPE-lane path.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `pika` slice: `pass=83 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-HTTP-CRUD-DICT-PROMOTION-089` fix repeatable bounded `http` slice
  failures after constructor cleanup
  - closure evidence:
    - `pipeline/decode-request` now uses line-based request splitting so LF-only
      and CRLF requests do not treat header lines as JSON bodies.
    - duplicate-post payload construction now uses `ref` for optional response
      error fields, so success responses do not inject error values into a
      dictionary literal.
    - `prim_dict` now propagates error-valued literal key/value arguments before
      insertion instead of reporting a misleading backing-storage OOM.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `http` slice: `pass=29 fail=0`

- [x] `AUDIT-CONSTRUCTOR-DISPATCH-SURFACE-083` remove high-confidence
  noncanonical constructor/conversion aliases and vector terminology
  - closure evidence:
    - `number->string`, `symbol->string`, `string->symbol`,
      `exact->inexact`, and `inexact->exact` are no longer public primitive
      registrations; `String`, `Symbol`, `Float64`, and `Integer` are the
      canonical constructor/coercion surfaces.
    - `set-size` and `set->list` are no longer public primitive
      registrations; `length` handles set cardinality and `List(Set ...)`
      materializes deterministic canonical set element order.
    - fast-dev-only `Int`, `Bool`, and duplicate `filesystem-*` primitive
      aliases were removed.
    - schema validation now uses `array-of` instead of `vector-of`.
    - `lib/immer.omni` now exposes `persistent-array`,
      `persistent-dictionary`, and `persistent-set` names instead of
      `vector`, `hash-map`, and `hash-set`.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `string-type` slice: `pass=40 fail=0`
    - bounded `advanced` slice: `pass=1189 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`
    - quick eval confirmed removed set aliases, `length`/`List(Set ...)`, and
      `array-of` schema behavior.

- [x] `AUDIT-RUNTIME-INTERN-RAISEPAYLOAD-GUARDS-081` guard runtime intern and
  unhandled-effect raise payload paths
  - closure evidence:
    - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
      symbol interning before defining the constant.
    - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
      interning as a non-match.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now uses
      non-raising dictionary allocation for unhandled-effect payloads and
      checks payload-key interning before publication.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`
