# Memory Changelog Index Part 12

Source: `memory/CHANGELOG.md`

    primitives surfaced by the e2e corpus, including `error`, `error?`,
    `error-message`, `is?`, `instance?`, and `type-args`.
  - Preserved user binding shadowing by keeping primitive detection table-based,
    not dynamic environment-based.
  - Allowed `Coroutine` to accept AOT closure wrapper thunks by treating
    `aot::make_closure` wrappers as root-lifetime closure thunks without
    inspecting JIT-only `closure_val` fields.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probe: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(type-of (Coroutine (lambda () 1)))'` -> `Coroutine`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Canonicalized permissive numeric parsing as `parse-number`:
  - `parse-number` now replaces the public `string->number` arrow alias on the
    runtime/compiler primitive surfaces.
  - `Number` remains a non-callable abstract/meta type descriptor; permissive
    maybe-valued parsing is intentionally not constructor semantics.
  - Added `docs/plans/number-parse-surface-decision-2026-04-11.md` and
    migrated live tests, docs, and examples to `parse-number`.
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

- Canonicalized list/string conversion through constructors:
  - `List(String)` is now the public string-to-list surface and returns a
    proper list of UTF-8 codepoint strings.
  - `String(List)` is now the public list-to-string surface and concatenates a
    proper list of string fragments; `String(nil)` returns the empty string.
  - Removed public `string->list` and `list->string` primitive/compiler aliases;
    internal helper wrappers remain for runtime allocation regressions.
  - Added `docs/plans/list-string-constructor-decision-2026-04-11.md` and
    migrated public docs/Lisp-level tests to constructor forms.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `List(String)`, `String(List)`, `String(nil)`,
      non-string list element rejection, and removed public arrow bindings
    - bounded `advanced-unicode-iterator` subgroup: `pass=138 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `limit-busting` slice: `pass=17 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`

- Aligned generic string sequence operations with codepoint semantics:
  - `length` on strings now returns UTF-8 codepoint count, matching
    `string-length`; byte count remains explicit via `string-byte-length`.
  - `ref` and postfix `.[index]` on strings now return a single-character string
    by codepoint index, matching `char-at` and `List(String)` element shape.
  - Added non-ASCII regressions for generic `length`, `ref`, and postfix string
    indexing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `length`, `string-length`, `string-byte-length`, `ref`,
      `char-at`, and postfix `.[index]` on non-ASCII strings
    - bounded `advanced-unicode-iterator` subgroup: `pass=136 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=142 fail=0`

- Closed the cons/list `ref` specification parity audit:
  - Updated the language spec and collection reference to document the
    already-tested runtime contract for cons/list chains: positive and negative
    indexing across the full cons chain, dotted terminal tails addressable as
    the final element, and `length` counting a non-`nil` dotted tail as one
    terminal element.
  - validation:
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`

- Aligned `list?` with the documented proper-list contract:
  - Registered the existing strict `prim_is_list` implementation as the public
    `list?` predicate and removed the stdlib override that treated any pair,
    including improper lists, as a list.
  - Added predicate regressions for improper lists in the advanced stdlib and
    type/effect predicate groups.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `advanced-type-dispatch-mutation-chain` subgroup:
      `pass=237 fail=0`

- Completed public stdlib list-walker improper-list normalization:
  - Added a private stdlib proper-list guard and applied it to the audited
    public list walkers: `map`, `filter`, `foldl`, `foldr`, `append`, `take`,
    `drop`, `zip`, `for-each`, `any?`, `every?`, `flatten`, `partition`,
    `remove` via `filter`, `find`, and `nth`.
  - Mirrored the guard and protected definitions in the compiler stdlib prelude
    for the overlapping walker set.
  - Added regressions for partial-success, short-circuit false-positive, and
    nested-`flatten` improper-list cases.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after partial-success slice:
      `pass=275 fail=0`
    - bounded `compiler` slice after partial-success slice: `pass=196 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after remaining walker slice:
      `pass=285 fail=0`
    - bounded `compiler` slice after remaining walker slice: `pass=196 fail=0`

- Completed the runtime `eval` data-to-expression fail-closed lane:
  - Lambda and let data forms now preserve parser-equivalent implicit block
    bodies instead of truncating to the first body form.
  - `macroexpand` now surfaces structural conversion failures for malformed cons
    forms instead of returning the original malformed form.
  - Added eval regressions for lambda/let multi-body parity and macroexpand
    conversion-failure surfacing.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup: `pass=268 fail=0`

- Hardened the runtime `eval` data-to-expression conversion path for malformed
  special forms:
  - `if`, `quote`, `define`, `set!`, `checkpoint`, `capture`, quasiquote,
    `unquote`, `unquote-splicing`, and `signal` now enforce structural arity
    during value-to-expression reconstruction instead of defaulting missing
    operands to `nil` or ignoring extras.
  - `define`, two-operand `set!`, `capture`, and `signal` now reject non-symbol
    names/tags instead of coercing them to symbol id `0`.
  - Multi-argument `set!` data forms now lower through normal call dispatch,
    matching the parser's generic collection setter shape.
  - Added eval regressions for malformed special-form cases and generic
    multi-argument `set!` lowering in the advanced stdlib numeric
    introspection group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=265 fail=0`

- Fixed tail multi-argument calls carrying error-valued arguments through
  ESCAPE-lane cons promotion:
  - `make_cons` now distinguishes successful promotion of first-class `ERROR`
    values from promotion failures when building ESCAPE-lane cons cells.
  - `append` now guards the intermediate `(reverse a)` result so improper left
    lists preserve the original proper-list error instead of masking it as
    `arg list too short`.
  - Added core regressions for tail multi-argument named-let calls carrying
    error-valued arguments and updated the append improper-left regression.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - direct tail multi-arg/append probes
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`
    - bounded `tco-recycling` slice: `pass=11 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- Fixed `reverse` on improper lists found by the collection walker audit:
  - `__reverse-list` now rejects dotted tails instead of silently truncating
    them, preventing `append` from losing left-side tail data through `reverse`.
  - Added advanced stdlib regressions for `reverse` proper-list rejection and
    `append` improper-left fail-closed behavior. A follow-up item tracks named
    `let` preserving initializer error payloads instead of masking them as an
    argument-list failure.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `reverse`/`append` improper-list probes
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`

- Removed name-only AOT list/dictionary constructor fast paths found by the
  constructor/dispatch audit:
  - AOT call lowering now routes `list`, `Dictionary`, and `Dict` through the
    normal callable dispatch path instead of bypassing bindings by symbol name.
  - Added compiler regressions for shadowed `list` and `Dictionary` bindings.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `compiler` slice: `pass=196 fail=0`

- Fixed fixed-arity primitive fallback calls ignoring trailing arguments:
  - Shared multi-argument primitive application now rejects extra or missing
    arguments for primitives registered with fixed arity, while preserving
    explicit variadic primitives registered with arity `-1`.
  - Added regressions for arithmetic, constructor, and generic collection calls
    that previously consumed only the first registered arguments.
  - Updated stale destructuring examples to use explicit binary arithmetic
    nesting.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for fixed-arity rejection and variadic `string-append`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=254 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`

- Aligned `length` with dotted cons `ref` behavior found during the
  cons-chain walker audit:
  - Dotted terminal tails now count as addressable sequence elements, matching
    positive and negative `ref` indexing on dotted cons chains.
  - Added regressions in the advanced collections generic operations group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `length` probes for dotted cons chains
    - bounded `advanced-collections-module` subgroup: `pass=137 fail=0`

- Fixed negative `ref` indexing on dotted cons pairs found during the
  cons-chain walker audit:
  - Dotted terminal tails now participate in the negative-index length used by
    `ref`, matching the existing positive `(ref (cons a b) 1)` pair behavior.
  - Added a regression in the advanced collections generic operations group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `ref` probes for dotted pair negative indexing
    - bounded `advanced-collections-module` subgroup: `pass=135 fail=0`

- Fixed Pika grammar start-rule selection found while reconciling the reference
  docs with the live grammar surface:
  - Forward-reference placeholders can no longer become the grammar start rule
    ahead of the first declared rule.
  - Added a regression where the first declared rule references later `number`
    and `op` rules before they are registered.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct Pika grammar probe for first-declared start rule with forward
      `number` and `op` references
    - bounded `pika` slice: `pass=90 fail=0`

- Hardened Pika grammar clause validation found during the follow-up
  list-walker audit:
  - Grammar rule and clause forms now reject improper lists instead of
    compiling only the cons-prefix shape.
  - Unary grammar operators (`many`, `some`, `opt`, `not`, `and`, `scan`) now
    enforce exact operand arity, and `end` enforces zero operands.
  - Added regressions in the Pika runtime grammar coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct malformed Pika grammar checks for extra operands, improper clause
      tails, extra rule fields, empty unary operators, and `end` operands
    - bounded `pika` slice: `pass=89 fail=0`

- Hardened sequence-pattern matching found during the follow-up list-walker
  audit:
  - Interpreted sequence patterns now match only arrays or proper lists, so
    improper cons chains and scalars no longer satisfy sequence patterns via
    cons-prefix length truncation.
  - Added regressions in the advanced core semantics match coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct match checks for improper single, improper rest, and scalar empty-sequence inputs
    - bounded `advanced-core-semantics-match` subgroup: `pass=71 fail=0`

- Hardened `eval` value-to-expression conversion found during the
  follow-up list-walker audit:
  - `eval` now rejects improper list program forms before special-form or
    call lowering can truncate dotted tails.
  - Lambda parameter conversion now rejects non-symbol and improper parameter
    lists instead of silently dropping malformed elements.
  - `eval` now preserves the underlying runtime error message for these
    conversion failures.
  - Added regressions in the advanced stdlib introspection coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct eval checks for constructed improper call, `and`, and lambda-params forms
    - bounded `advanced-stdlib-numeric-introspection-lazy-tco` subgroup: `pass=32 fail=0`

- Hardened `apply` malformed-argument-list handling found during the
  follow-up list-walker audit:
  - `apply` now rejects non-list and improper-list argument inputs at the
    primitive boundary instead of truncating dotted tails or treating scalar
    inputs as zero-argument calls.
  - Added regressions in the advanced stdlib introspection coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct eval checks for scalar and improper-list argument inputs
    - bounded `advanced-stdlib-numeric-introspection-lazy-tco` subgroup: `pass=29 fail=0`

- Hardened quasiquote splicing malformed-tail handling found during the
  follow-up list-walker audit:
  - JIT quasiquote `,@` expansion now rejects improper list splice
    values instead of dropping the non-list tail.
  - Added a regression in the advanced macro-hygiene quasiquote
    coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-macro-hygiene-quasi-pattern` subgroup: `pass=20 fail=0`

- Hardened `string-join` malformed-element handling found during the
  follow-up string conversion audit:
  - `string-join` now rejects non-string list elements instead of
    silently omitting them from the joined output.
  - Added a regression in the advanced stdlib string/predicate/format
    coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup: `pass=60 fail=0`

- Hardened collection/string conversion fail-closed behavior found during the
  follow-up constructor-surface audit:
  - `list->string` now rejects non-string list elements instead of silently
    dropping them from the output.
  - `Array` conversion from a list now rejects improper list tails instead of
    truncating at the first non-cons tail.
  - `sort`, `sort-by`, and internal list-to-array conversion now reject
    non-list/improper-list inputs instead of returning truncated data.
  - Added regressions in the advanced core constructor/string and stdlib
    sort coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-unicode-iterator` subgroup: `pass=132 fail=0`
    - bounded `advanced-stdlib-numeric-array-set` subgroup: `pass=37 fail=0`
    - bounded `advanced-stdlib-numeric-sort-bitwise-hof` subgroup: `pass=21 fail=0`

- Fixed the repeatable Pika stress/cache slice failures at the shared runtime
  boundary:
  - The regex engine and cache passed direct large-pattern controls; the failing
    Pika cases were dynamic pattern builders that exercised named-let
    multi-argument recursion with string parameters.
  - `make_cons` now distinguishes legal identity promotion for values already
    in a surviving target scope chain from illegal identity returns for current
    TEMP-lane values that failed to promote into ESCAPE.
  - This keeps ESCAPE-lane argument-list construction from turning reusable
    string literals into a false `cons: failed to promote ...` error, which
    later surfaced as `arg list too short`.
  - Added a memory-lifetime regression for named-let string argument-list
    promotion.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `pika` slice: `pass=83 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- Fixed the repeatable HTTP CRUD slice failures:
  - `examples/deduce_crud_server.omni` now decodes CRLF and LF requests via a
    line-based header/body split, avoiding the previous `string-split "\n\n"`
    assumption even though `string-split` is a single-byte delimiter helper.
  - The duplicate-post regression no longer reads a missing `r1.error` path as
    a hard field error; it uses `ref` so successful responses carry `nil` for
    absent error fields.
  - `prim_dict` now propagates error-valued dictionary literal keys/values
    before insertion instead of mapping that non-allocation failure to the
    misleading `Dictionary: out of memory while growing backing storage`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `http` slice: `pass=29 fail=0`

- Closed the constructor/generic-dispatch surface cleanup follow-up:
  - Public conversion aliases `number->string`, `symbol->string`,
    `string->symbol`, `exact->inexact`, and `inexact->exact` are no longer
    registered; `String`, `Symbol`, `Double`, and `Integer` remain the
    canonical callable constructor/coercion surfaces and dispatch to the same
    underlying helpers.
  - `Set` materialization now follows the constructor/generic convention:
    `length` replaces public `set-size`, `List` replaces public `set->list`,
    and `List(Set ...)` returns the deterministic canonical set element order.
  - The fast-dev primitive table no longer exposes drift aliases `Int`,
    `Bool`, or the duplicate `filesystem-*` names; it now mirrors the main
    constructor surface for `Integer`/`Boolean` and keeps the existing `fs-*`
    primitive family.
  - Schema validation now uses `array-of` instead of `vector-of` for array
    schemas in runtime, fast-dev, and docs.
  - `lib/immer.omni` no longer exposes Omni-facing `vector`, `hash-map`, or
    `hash-set` names. The wrapper surface now uses
    `persistent-array`, `persistent-dictionary`, and `persistent-set`
    terminology while leaving the underlying Immer C ABI names intact.
  - The broken `lib/immer.omni` generic `count`/`conj`/`into` facade was
    removed because it depended on predicates that are not defined for the
    current opaque FFI handles. A typed wrapper/dispatch lane is tracked
    separately in `TODO.md`.
  - residuals recorded in `TODO.md`:
    - `string->list` / `list->string` need an explicit `List(String)` /
      `String(List)` semantic decision before removal because char,
      grapheme, and list-element coercion behavior are product-visible.
    - `string->number` remains public until the product chooses a canonical
      parse API or a `Number` constructor/coercion contract.
    - lowercase `list` remains public for now because docs explicitly kept it
      as an idiomatic helper and removing it has broad compatibility impact.
    - repeated bounded `http` and `pika` slice failures are tracked as
      separate defect lanes because they reproduce independently of the
      constructor cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `string-type` slice: `pass=40 fail=0`
    - bounded `advanced` slice: `pass=1189 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`
    - quick eval:
      `(bound? 'set-size) => nil`, `(bound? 'set->list) => nil`,
      `(length (Set 1 2 2 3)) => 3`,
      `(List (Set 3 1 2 2)) => (1 2 3)`,
      `(validate '(array-of int) [1 2 3]) => true`,
      `(validate '(array-of int) [1 "x"]) => nil`

- Closed the runtime intern and raise-payload guard follow-up:
  - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
    symbol interning before defining the language constant.
  - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
    interning as a non-match instead of probing with invalid symbols.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now builds
    unhandled-effect raise payload dictionaries through the non-raising
    hashmap helper and rejects invalid payload key interning before
    publication.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`

- Closed the runtime result-key and optional diagnostic payload interning guard
  follow-up:
  - `src/lisp/eval_dispatch_error_payloads.c3` now rejects invalid payload
    keys and invalid symbol payload values in the shared optional dispatch
    payload setter, so failed interning omits ancillary dispatch diagnostics
    instead of publishing `SYMBOL(INVALID_SYMBOL_ID)` values.
  - `src/lisp/async_process_spawn.c3` now interns process-spawn result keys
    before constructing key symbols and closes spawned resources on key
    interning failure.
  - `src/lisp/http_url_response.c3` now rejects failed HTTP response-field
    key interning before publishing response payload dictionaries.
  - `src/lisp/prim_ui_ftxui_helpers.c3` now rejects failed UI dictionary
    lookup-key interning before probing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
      `pass=65 fail=0`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- Closed the goal-directed deduce explain invalid-symbol guard follow-up:
  - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now builds
    goal-directed blocker and shape symbols through a checked helper, so
    symbol interning failure returns the existing explain OOM error instead
    of publishing `SYMBOL(INVALID_SYMBOL_ID)` payload values.
  - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses the same helper
    for goal-directed shape and execution-path payload values.
  - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
    key interning before constructing temporary dictionary key symbols.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`

- Closed the goal-directed deduce diagnostic payload no-preseed follow-up:
  - `src/lisp/deduce_rule_eval_analyze_setup.c3` now builds
    goal-directed selector analysis error payload dictionaries through
    `make_hashmap_no_raise(...)` and local checked insertion, so ancillary
    payload allocation failure cannot publish a nested dictionary raise before
    the intended `deduce/analyze-out-of-memory` fallback.
  - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
    now applies the same non-raising payload construction contract to
    goal-directed selector and relation surface diagnostics while preserving
    the existing `deduce/out-of-memory` fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- Closed the AOT/module/deduce/collection sentinel and rollback lane:
  - AOT runtime bridge binding/lookup/import/FFI paths now use checked symbol
    interning before environment lookup/define/set, and environment hash and
    barrier mutation paths now reject `INVALID_SYMBOL_ID` before probing or
    mutating bindings.
  - Module setup/import now rolls back newly published modules when body,
    path-copy, or top-level file evaluation fails; module hash rebuilds skip
    tombstones, and implicit module file loading reacquires module entries by
    index after nested loads can grow the module table.
  - Deduce persisted rule catalog/signature restore now distinguishes missing
    DBIs from other LMDB open failures, rejects invalid restored symbols, and
    rolls back partially restored rule signatures/schemas on mid-restore
    failure.
  - Raise payload construction, method-table root cloning, and array/dict/set
    primitives now fail closed on invalid symbol IDs, method-table entry
    allocation overflow, or nullable backing storage.
  - validation:
    - bounded advanced slice: `pass=1183 fail=0`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded string-type slice: `pass=40 fail=0`
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the runtime promotion allocation-staging lane:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_escape_structured.c3`, and
    `src/lisp/eval_promotion_root_clone_basic.c3`
    now reject overflowing array, hashmap, method-table, method-signature, and
    closure-parameter allocation sizes before boundary copy or ESCAPE/root
    clone publication.
  - `src/lisp/eval_promotion_escape_structured.c3`
    now resets staged method signatures on dependent allocation failure and
    delays closure result wrapper publication until fallible clone/env work
    succeeds.
  - `src/lisp/eval_env_copy_frame_helpers.c3`
    now allocates non-inline binding storage before publishing the copied
    frame and frees it if frame allocation fails.
  - `src/lisp/eval_pattern_matching.c3`
    now rejects overflowing sequence element collection buffers.
  - validation:
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the deduce SCC allocation-bound lane:
  - `src/lisp/deduce_rule_eval_scc_plan.c3`
    now checks SCC square matrix sizing and stratum relaxation bound arithmetic
    before allocation or fixpoint iteration.
  - `src/lisp/deduce_rule_eval_validation.c3`
    now checks reachability matrix square sizing before allocation.
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
    `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
    and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
    now guard proof-key vector, aggregate batch growth, and decoded delta
    entry allocations.
  - `src/lisp/deduce_schema_query_input_shape.c3`,
    `src/lisp/deduce_schema_query_input_roles.c3`, and
    `src/lisp/deduce_schema_query_input_constraints.c3`
    now reject overflowing `count + 1` capacity requests before calling
    relation ensure-capacity helpers.
  - validation:
    - bounded deduce slice: `pass=330 fail=0`

- Closed the AOT/FFI allocation-staging lane:
  - `src/lisp/aot_type_definitions.c3`
    now initializes cleanup-owned type fields and union variants before any
    later fallible allocation can trigger deferred cleanup, and checks AOT
    field/variant/type-parameter allocation sizes before allocation.
  - `src/lisp/eval_ffi_bound_call.c3`
    now rejects unsupported/narrowing argument counts and overflowing libffi
    staging buffer sizes before preparing runtime call storage.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the parser AST array allocation-helper lane:
  - `src/lisp/parser_parser.c3`
    now exposes `Parser.alloc_ast_array_bytes(...)`, which checks
    `elem_size * count` before delegating to AST arena allocation.
  - Dynamic parser AST array allocations across calls, relation definitions,
    literals, patterns, type annotations, module bodies, lambda params,
    named-let rewrites, path expressions, macro clauses, blocks, and pipe
    rewrites now route through the checked helper.
  - `src/lisp/parser_set_pipe_helpers.c3`
    now rejects overflowing `arg_count + 1` before growing pipe call
    arguments.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the parser AST argument size-guard lane:
  - `src/lisp/parser_define_relation_attr_helpers.c3`
    now checks relation role/count arithmetic and `Expr*` allocation sizing
    before building `__define-relation` call arguments.
  - `src/lisp/parser_application_helpers.c3`
    now checks generic call argument `Expr*` allocation sizing before copying
    argument pointers.
  - `src/lisp/parser_ffi_helpers.c3`
    now rejects overflowing FFI parameter counts before `+ 1` capacity checks.
  - `src/lisp/parser_import_helpers_specs.c3`
    now rejects overflowing selective-import counts before `+ 1` capacity
    checks.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the shared/async string size-guard lane:
  - `src/lisp/scheduler_shared_handles_blob.c3`
    now rejects max-sized blob copies before `len + 1` allocation.
  - `src/lisp/async_process_signal_handles.c3`
    now rejects max-sized C-string copies, cons sequence count overflow, and
    argv/env `char*` table byte-size overflow before allocation.
  - `src/lisp/tls_offload_connect.c3`
    now rejects max-sized TLS duplicate strings before `len + 1` allocation.
  - `src/lisp/prim_io_fs_handles.c3`
    now rejects invalid/overflowing filesystem result array growth before
    replacing array backing storage.
  - `src/lisp/scheduler_offload_network.c3`
    now rejects max-sized compression bounds before allocating the output
    scratch buffer.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
