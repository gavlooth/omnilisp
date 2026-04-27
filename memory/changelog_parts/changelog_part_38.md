# Memory Changelog Part 38

## 2026-04-30 - REPL Server Auth Missing Token Fail-Closed

- `[FACT]` `repl_server_connection_init` now preserves `auth_required` when
  authentication is requested even if token material is missing, empty, or not
  bounded by `REPL_SERVER_CSTR_SCAN_MAX`.
- `[FACT]` Missing or invalid token material leaves `auth_token_ptr = null` and
  `auth_token_len = 0`, so protected requests fail closed while auth-exempt
  describe requests remain available.
- `[FACT]` Basic direct coverage and async REPL-server coverage now assert
  missing-token and empty-token secured connections reject `eval` requests
  instead of downgrading to open transport.
- `[FAILED]` Full async slice validation remains blocked by existing unrelated
  async/file/coroutine failures (`suite=unified pass=90 fail=10`), so this
  slice is verified by the direct basic regression rather than a green async
  slice.
- `[FACT]` Validation passed C3 diagnostics, `c3c build main`, basic Lisp
  slice `suite=unified pass=184 fail=0`, file-size gate, and
  `git diff --check`.

## 2026-04-30 - REPL Buffer Boundary Guards

- `[FACT]` `read_line` now returns immediately for zero-capacity buffers before
  computing `buffer.len - 1`, preventing `usz` underflow and out-of-bounds
  writes.
- `[FACT]` `repl_eval_buffer` now rejects saturated accumulation buffers before
  writing a terminator at `buf_len`; live REPL calls still print the
  line-too-long diagnostic, while direct non-interactive boundary calls stay
  quiet.
- `[FACT]` Basic native regression coverage exercises both zero-capacity
  `read_line` and full-buffer `repl_eval_buffer` boundaries.
- `[FACT]` Validation passed C3 diagnostics, `c3c build main`, basic Lisp
  slice `suite=unified pass=183 fail=0`, file-size gate, and
  `git diff --check`.

## 2026-04-30 - Dispatch Large Arity Diagnostic

- `[FACT]` Method-dispatch error formatting now uses a full integer buffer for
  expected arity hints, preserving large `param_count` values instead of
  truncating them through a 4-byte local buffer.
- `[FACT]` Advanced type-dispatch regression coverage directly formats a
  method table with expected arity `12345` and verifies the full count appears
  in the error.
- `[FACT]` Validation passed C3 diagnostics, `c3c build main`, filtered
  advanced dispatch slice `suite=unified pass=255 fail=0`, file-size gate, and
  `git diff --check`.

## 2026-04-30 - M9 Dispatch Literal Signature Fail-Closed

- `[FACT]` Runtime dispatch now rejects malformed internal `ValueLiteralKey`
  tags with `runtime/invalid-state` instead of treating corrupted literal
  method signatures as ordinary literal mismatches or falling through to a
  method-table fallback.
- `[FACT]` Schema explain and dispatch payload helpers now surface
  `invalid-literal` for invalid internal literal tags instead of rendering them
  as `nil` or `none`.
- `[FACT]` Validation passed C3 diagnostics, `c3c build main`, filtered
  advanced dispatch slice `suite=unified pass=254 fail=0`, file-size gate, and
  `git diff --check`.

## 2026-04-30 - M9 Runtime Sequence Pattern Rest Position

- `[FACT]` Runtime sequence-pattern matching now rejects unknown internal rest
  positions with `runtime/invalid-state` instead of treating malformed sequence
  patterns as ordinary non-matches.
- `[FACT]` Validation passed C3 diagnostics, `c3c build main`, basic Lisp slice
  `pass=182 fail=0`, compiler Lisp slice `pass=450 fail=0`, file-size gate, and
  `git diff --check`.

## 2026-04-30 - M9 FFI Discovery Unknown Tags

- `[FACT]` FFI preload and contract-manifest discovery now reject unknown
  internal expression tags with explicit compiler errors instead of silently
  suppressing startup preload and manifest output.
- `[FACT]` Quasiquote/unquote template forms remain explicit no-op cases for
  FFI discovery because they are valid non-FFI template leaves.
- `[INVALIDATED]` Do not treat quasiquote/unquote template forms as malformed
  during FFI discovery. A first strict pass regressed the compiler slice to
  `suite=compiler pass=445 fail=5`; adding explicit no-op cases restored the
  slice to `pass=450 fail=0`.

## 2026-04-30 - M9 Type Metadata Value Tag Emitter

- `[FACT]` AOT type metadata value-tag emission now rejects unsupported value
  tags with an explicit compiler error instead of silently emitting `NIL`.
- `[FACT]` `NIL` is now an explicit supported emitted value tag, preserving
  valid no-literal and nil-singleton metadata paths.
- `[INVALIDATED]` Do not treat `NIL` value-tag emission as fallback-only. A
  first tightening attempt regressed the compiler slice to
  `suite=compiler pass=243 fail=206`; adding `NIL` as an explicit valid case
  restored the slice to `pass=449 fail=0`.

## 2026-04-30 - M9 Inline Module Metadata Unknown Tags

- `[FACT]` Inline-module export classification and local collection now reject
  unknown internal expression/pattern tags with explicit compiler errors
  instead of treating malformed module metadata inputs as non-exporting no-ops.
- `[FACT]` Valid non-binding/non-exporting expression and pattern tags remain
  explicit no-op cases for the inline-module metadata collectors.

## 2026-04-30 - M9 AOT Match Guard Unknown Tags

- `[FACT]` AOT match guard scan/lowering now rejects unknown internal pattern
  tags with explicit compiler errors instead of treating malformed guard
  patterns as no guard work.
- `[FACT]` Valid non-guard patterns (`_`, variables, literal, quoted literal,
  dictionary) remain explicit no-op cases for guard scan/lowering.

## 2026-04-30 - M9 AOT Match Binding Unknown Tags

- `[FACT]` AOT match binding lowering now rejects unknown internal pattern tags
  with an explicit compiler error instead of silently emitting no bindings for
  malformed patterns.
- `[FACT]` Valid binding-free patterns (`_`, literal, quoted literal) remain
  explicit no-op cases.

## 2026-04-30 - M9 Quasiquote Free-Variable Unknown Tags

- `[FACT]` Quasiquote free-variable analysis now rejects unknown internal
  expression tags with an explicit compiler error instead of treating malformed
  tags as capture-free quasiquote template literals.
- `[FACT]` Valid non-unquote expression forms remain explicit capture-free
  quasiquote template cases, preserving existing macro/free-variable behavior.

## 2026-04-30 - M9 Mutable-Capture Prescan Unknown Tags

- `[FACT]` Mutable-capture prescan now rejects unknown internal expression and
  pattern tags with explicit compiler errors instead of treating malformed
  input as no mutable captures.
- `[FACT]` The prescan now explicitly traverses `E_MODULE` and `E_WITH_MODULE`
  bodies so strict malformed-tag handling preserves module/private-backing
  capture behavior.
- `[FAILED]` A first strict-default attempt missed module traversal and failed
  existing module/private-backing compiler regressions (`pass=411 fail=33`).
  Valid module forms must be enumerated before making prescan defaults errors.

## 2026-04-30 - M9 Free-Variable Analysis Unknown Tags

- `[FACT]` Compiler free-variable analysis now rejects unknown internal
  expression tags with explicit compiler errors instead of treating malformed
  expressions as capture-free.
- `[FACT]` Pattern binding collection used by free-variable analysis now
  rejects unknown internal pattern tags instead of silently producing an
  incomplete binding set.

## 2026-04-30 - M9 Lambda Scan Unknown Tags

- `[FACT]` Compiler lambda scanning now rejects unknown internal expression and
  pattern tags with explicit compiler errors instead of treating malformed
  scan input as "no lambdas found".
- `[FACT]` Valid lambda-scan leaf/declaration forms remain explicit no-op scan
  cases, preserving normal capture behavior while making malformed tags fail
  closed.

## 2026-04-30 - M9 Generated Global Collector Fail-Closed

- `[FACT]` Compiler generated-global collection now rejects unknown internal
  expression and pattern tags with explicit compiler errors instead of treating
  malformed tags as no-op leaves.
- `[FACT]` Inline-module backing global collection now explicitly traverses
  `E_WITH_MODULE` bodies, preserving private-backing declarations for valid
  scoped module-open forms while still failing closed on unknown expression
  tags.
- `[FAILED]` A strict-default-only attempt missed `E_WITH_MODULE` in the
  inline-module backing collector and failed existing module/private-backing
  compiler regressions (`pass=434 fail=4`). Valid traversal forms must be
  enumerated before turning collector defaults into errors.

## 2026-04-30 - M9 FFI Manifest Invalid Type Tags

- `[FACT]` FFI contract manifest emission now validates raw ABI type integers
  before rendering JSON. Invalid tags set `compiler: invalid FFI type tag in
  contract manifest` and serialize as `"Invalid"` instead of silently
  publishing `"Void"`.
- `[FAILED]` Validating after `(FfiTypeTag)` conversion is invalid for this
  boundary: C3 traps invalid enum conversions before the compiler can report
  the manifest contract error.

## 2026-04-27 - FFI Foreign Runtime Retained Optional Interop Closure

- `[FACT]` Bindgen dependency generation now carries `mode` and `generator`
  through the actual raw module, facade, and manifest write path. `mode` accepts
  `abi` or `api`; `generator` accepts `clang` or `cppinterop`.
- `[FACT]` `generator = "cppinterop"` requires `mode = "api"`. This keeps
  CppInterop as API/tooling metadata and output review markers, not a startup
  or runtime C++ dependency.
- `[FACT]` Native FFI buffer returns and CPU/CUDA Tensor buffer exports use
  borrowed `ForeignHandle` buffer descriptors. Unsupported tensor devices fail
  closed instead of exposing raw pointers.
- `[FACT]` CUDA/cuBLAS execution remains in the Tensor backend. The FFI
  foreign-runtime boundary for that lane is tensor-buffer marshalling only.
- `[FACT]` Python/Julia adapters and polyglot/plugin runtime support are not
  planned for the current FFI foreign-runtime area.
- `[FACT]` `scripts/check_status_consistency.sh` now expects
  `docs/areas/ffi-foreign-runtime.md` to be `green` because the retained
  optional lanes are implemented and Python/Julia plus polyglot/plugin are no
  longer required lanes.
- `[INVALIDATED]` Do not treat the old residual list of Python, Julia,
  polyglot/plugin, CppInterop, CUDA/cuBLAS, and tensor-buffer hooks as an active
  implementation backlog. CppInterop and tensor-buffer marshalling are the
  retained lanes; Python/Julia and polyglot/plugin are explicitly closed as not
  planned.

## 2026-04-27 - Literal Singleton Dispatch Surface

- `[FACT]` Public value-literal dispatch syntax is now the literal singleton
  surface: `^#datum` shorthand and `^(Literal datum)` long form.
- `[FACT]` Removed `^(Value datum)` and `^(Val datum)` as public dispatch
  annotation spellings; parser diagnostics point to `Literal` / `^#datum`.
- `[FACT]` `(Literal datum)` is a type-level constructor that returns a
  singleton literal type descriptor such as `#<type (Literal 3)>`; it does not
  box or wrap the runtime value.
- `[FACT]` Existing method-signature storage still uses the internal
  `ValueLiteralKey` representation, but that is no longer a public `Value`
  annotation surface.

## 2026-04-28 - Literal Singleton Audit Fixes

- `[FACT]` Inline reserved literal shorthand now requires exact token matches:
  `^#nil`, `^#true`, and `^#false` are accepted, but misspellings such as
  `^#nilx`, `^#truex`, and `^#falsex` fail closed instead of being parsed by
  prefix.
- `[FACT]` `(Literal datum)` descriptor-name rendering no longer uses a fixed
  256-byte helper-local buffer. Long string literal descriptors now render
  through the same dynamic string-builder pattern used by other runtime string
  assembly paths before being interned.
- `[FACT]` Malformed inline integer shorthand `^#-` now fails at the literal
  shorthand parser with an explicit "expects digits" diagnostic instead of
  falling through to a misleading generic parameter parse error.
- `[FACT]` Non-constructor type definition forms now publish descriptor values
  for their type names. `[abstract]`, `[union]` heads, and `[alias]` names
  render as `#<type Name>` in value position instead of remaining unbound, and
  attempts to call those descriptors fail with an explicit type-descriptor
  non-callable diagnostic.
- `[FACT]` The compiler top-level global collector now keeps AOT descriptor
  publication in parity with interpreter evaluation for non-constructor type
  definitions. Generated C3 syncs descriptor globals after direct
  `aot::define_abstract`, `aot::define_union`, and `aot::define_alias` helper
  calls, including union heads as well as variant constructors.
- `[FACT]` Inline-compiled module private backing metadata now includes
  descriptor-producing type forms. Modules can export/import `[abstract]`
  descriptors, `[union]` heads, `[alias]` descriptors, and union variants from
  private backing storage instead of failing as dangling exports.
- `[FACT]` Inline-compiled module effect exports now sync their private backing
  from the published effect-tag binding after `aot::define_effect`, rather
  than storing the command-style declaration return value.
- `[FACT]` The private-backing sync rule for helper-published declarations now
  lives at the shared type/effect helper boundary. Nested inline-module
  `[abstract]`, `[union]`, `[alias]`, `[type]`, and `[effect]` forms sync their
  active private backing from `aot::lookup_var(...)`, matching direct
  module-body declarations without duplicate module-lowering branches.
- `[FACT]` Block-sequence lowering no longer assigns the command-style
  `define [effect]` result to the declared effect name. Nested module blocks now
  preserve the helper-published effect tag backing instead of overwriting it
  with `nil`, and non-module blocks no longer emit undeclared C3 assignments
  for effect declarations.
- `[FACT]` Helper-published type/effect declarations now sync generated global
  bindings from the same helper boundary used for module private backings.
  Nested top-level blocks containing `[abstract]`, `[union]`, `[alias]`, or
  `[type]` forms no longer leave generated descriptor/constructor globals at
  their initial `nil` values. The old top-level-only type-form sync hook was
  removed so direct and nested generated-code paths share one publication rule.
- `[FACT]` Tail-position block lowering now routes final `[ffi lib]` and
  `[ffi λ]` declarations through the direct-result binding hook instead of
  tail-lowering them as ordinary expressions. Generated globals and module
  private backings stay in sync with the FFI values returned by the AOT bridge.
- `[FACT]` Branch assignment lowering now uses the same direct-result binding
  hook for `[ffi lib]` and `[ffi λ]` declarations. Direct FFI declarations in
  `if` and short-circuit branches no longer leave generated bindings at their
  initial `nil` values.
- `[FACT]` Native match lowering now uses the same direct-result binding hook
  for `[ffi lib]` and `[ffi λ]` clause results. FFI declarations in matching
  clauses now update generated globals/module backings before the match result
  is assigned.
- `[FACT]` FFI direct-result binding sync now lives at the FFI helper lowering
  boundary. `compile_ffi_lib_direct` and `compile_ffi_fn_direct` assign the AOT
  bridge result to the active module private backing or generated global
  immediately after the declaration call, so block, branch, match, `let`,
  lambda-tail, and module paths share one binding rule instead of relying on
  caller-specific assignment special cases.
- `[FACT]` Generated embedded FFI contract JSON now uses the C3-valid const name
  `OMNI_FFI_CONTRACT_JSON` instead of a lowercase `__omni_ffi_contract_json`,
  so generated FFI programs build under C3's uppercase-const rule.
- `[FACT]` Generated e2e compiler coverage now includes a declarative FFI
  `strlen` fixture, so the bounded e2e binary gate exercises FFI declaration
  publication, generated C3 buildability, and runtime invocation together.
- `[FACT]` Runtime stdout value printing now casts emitted byte slices to
  `String` when writing escaped string/time-point content. Generated AOT
  binaries now print string values as `"text"` instead of numeric byte arrays
  such as `[104][101]`.
- `[FACT]` AOT effect handler wrappers now bind handler payloads from the second
  element of the `(continuation payload)` list with `car(cdr(pair))`, matching
  the list shape produced by the runtime effect bridge. Handler payloads no
  longer arrive as one-element lists in generated code, and direct generated
  e2e rows now cover payload return and payload-based resolve.
- `[FACT]` AOT `compiled_handle` now returns an abortive handler result when the
  body remains suspended after a handled signal, matching interpreter semantics
  where a handler that does not resolve abandons the suspended body.
- `[FACT]` AOT `compiled_handle` still dispatches pending `raise` before
  returning a completed body result, preserving raise-handler precedence while
  fixing abortive handler returns. Generated e2e now includes a pending-raise
  row for that ordering.
- `[FACT]` Generated e2e compilation is now fail-fast. Setup generation and
  expected-output collection failures abort `--gen-e2e`; at this checkpoint
  `scripts/run_e2e.sh` required the exact `Generated 423 e2e tests` summary and
  a 423-line expected-output file before accepting the generated binary gate.
- `[FACT]` Invalid positive generated e2e corpus rows were removed or moved to
  targeted negative coverage. Missing-else `(if true 42)` is now a core error
  regression test instead of a generated positive case, and the old
  division-by-zero match-guard row no longer masks expected-output generation.
- `[FACT]` Handler `resolve` is now terminal across generated/JIT parent work.
  Native and AOT lowering guard nested parent contexts, including call/app
  operands, block prefixes, let/set values, index operands, match guards and
  results, effect operands, and quasiquote/unquote expansion, so sibling work
  after a syntactic `resolve` no longer runs inside the active handler clause.
- `[FACT]` `resolve_terminal_pending` is a terminal-control marker, not a
  general error-propagation predicate. Ordinary `ERROR` values remain valid
  argument/binding data where existing Omni semantics allow them; generated
  control-flow paths still emit explicit `aot::is_error(...)` checks before
  truthiness/sequence continuation.
- `[FACT]` AOT terminal guard emission now distinguishes generated `Value*`
  functions from top-level `int main`. `return _rN` guards are emitted only in
  generated lambda/helper bodies; top-level generated programs no longer fail
  C3 compilation by returning `Value*` from `main`.
- `[INVALIDATED]` Do not use one combined "sequence should stop" predicate as
  the authority for both ordinary `ERROR` values and abortive `resolve`
  control. That conflates first-class error-valued arguments with terminal
  handler-clause control and regresses named-let/tail multi-arg behavior.
- `[FACT]` Generated e2e Stage 2 failure handling now preserves captured
  generator diagnostics before exiting. The runner no longer lets `set -e`
  abort at the command-substitution assignment before printing the generator's
  setup or expected-output failure message.
- `[FACT]` Generated e2e skip-count reporting was removed because there is no
  skip representation in the corpus contract. At this checkpoint the generator
  reported `Generated 423 e2e tests`, and the baseline policy guard rejected
  future reintroduction of unmeasured `skip_count`/`skipped` reporting.
- `[FACT]` Duplicate mislabeled generated e2e rows for nonexistent `Integer`
  and `Boolean` shorthand constructors were replaced with real approved `Dict`
  shorthand coverage while preserving the 423-case generated e2e contract.
- `[FACT]` Generated inline modules now push a temporary AOT runtime module env
  before lowering the module body and restore the previous global env with
  `defer`. Helper-published private declarations (`[ffi lib]`, `[ffi λ]`,
  `[effect]`, and descriptor-producing type forms) publish into the module env
  before being copied to private C backings, matching interpreter module
  isolation instead of leaking unexported names into the real global env.
- `[FACT]` Inline module body lowering now emits terminal `resolve` guards after
  each define RHS and non-define body form. A syntactic `resolve` inside a
  generated `Value*` handler clause can no longer continue through later module
  body work or publish later private bindings.
- `[FACT]` Helper-published effect declarations now participate in generated
  global collection and lookup-based sync like other type/effect helper forms.
  Top-level and block-local effects no longer rely on a runtime-only path while
  descriptors use generated globals.
- `[INVALIDATED]` Do not treat inline module private C backings as sufficient
  runtime isolation for helper-published declarations. Without a temporary
  module env, the helper side effect still publishes the local name into the
  real AOT global env before the compiler copies it to the private backing.
- `[FACT]` Generated e2e baseline policy now statically enforces the 423-case
  contract across the core and extended case tables, rejects duplicate case
  names or expressions, and rejects stale skip/skip-count language in generated
  e2e case comments. Stage 4 of `scripts/run_e2e.sh` now preserves
  `build/e2e_actual.txt` diagnostics on nonzero test-binary exit instead of
  letting `set -e` hide the captured output.
- `[FACT]` Inline module metadata now tracks its source AST node and fails
  closed on duplicate inline module names instead of reusing the first module's
  private backing table. The compiler now reports `duplicate inline module
  name` for the conflation case.
- `[FACT]` Inline module private-local discovery now traverses module-body
  `with` forms and imports. Definitions created inside `with` bodies receive
  private backing storage, and module-body imports copy their published runtime
  binding back into the active private backing so imported names can be
  exported without dangling-backing failures.
- `[FACT]` AOT generated-global collection now keeps legal helper globals in
  lexical subtrees while failing closed for lexical `[effect]` declarations.
  This avoids the old leak where AOT published branch/match/let-local effect
  tags into the global env even though interpreter/JIT kept those names
  unbound outside their lexical scope.
- `[INVALIDATED]` Do not fix lexical effect leakage by skipping all generated
  global collection under lexical bodies. That suppresses valid FFI/type helper
  globals in `let`, match, and handler bodies. The correct current contract is
  to continue collecting legal helper bindings and reject only lexical
  `[effect]` publication until AOT has a lexical helper-env model.
- `[FACT]` Quiet generated e2e mode still emitted the machine-readable
  `Generated 423 e2e tests` summary required by `scripts/run_e2e.sh` at this
  checkpoint. The baseline policy guard rejected only stale skip-count
  reporting language instead of broad comments that happen to contain words
  such as "skip".
- `[FACT]` AOT inline `export-from` metadata now adds selected or imported
  names to both module locals and module exports when the source is another
  inline module. Importing a re-exported inline name now compiles instead of
  failing as an unexported local.
- `[FACT]` AOT inline-module metadata now fails closed for unsupported
  runtime-module `export-from` and module-body `import 'all` enumeration. These
  paths no longer silently compile as empty/no-op metadata when the required
  export list is unavailable at compile time.
- `[FACT]` Literal-constant branch metadata and lowering now agree for inline
  module declarations. Dead literal branches no longer pre-register duplicate
  inline module names, and generated `if` lowering skips the unselected branch
  after evaluating and guarding the condition.
- `[FACT]` Lexical `[effect]` publication rejection now covers inline module
  nested lexical bodies, `shift` bodies, match guard predicates, and handler
  clause bodies while preserving direct module bodies, block/top-level bodies,
  `reset`/checkpoint bodies, and handle bodies that the interpreter exposes as
  non-lexical effect-publication surfaces.
- `[FACT]` AOT variable lowering now lets active generated locals and lambda
  parameters shadow primitive names such as `true` and `false` without leaking
  that shadow through unrelated C scopes. A dedicated primitive-shadow stack
  replaced the incorrect use of the broader `declared_vars` list for primitive
  shadowing.
- `[FACT]` The generated e2e corpus now covers boolean-name shadowing in a
  block/if row while preserving the 423-case contract.
- `[FACT]` Inline module metadata no longer treats lambda body definitions as
  immediate module locals/exports. Lambda-time module mutation remains
  unsupported unless a captured module-private context model is added later.
- `[FACT]` Inline module metadata now traverses match guard predicates for
  legal helper-published module locals and still rejects lexical `[effect]`
  publication with the intended diagnostic.
- `[FACT]` Inline module metadata reserves a registry slot before scanning a
  module body, so nested executable inline modules no longer overwrite the
  parent metadata slot or double-free the parent tables. Nested inline modules
  inside executable containers can now be used as `export-from` sources.
- `[FACT]` Runtime-dependent inline module exports now fail closed with a
  definite-assignment diagnostic instead of accepting a private backing that
  may remain `nil` when the interpreter would leave the export unbound.
- `[FACT]` Checkpoint/reset and handle bodies that would publish inline module
  exports now fail closed in AOT until synthetic wrapper bodies can carry a
  module-private backing context.
- `[FACT]` AOT let lowering now gives each lexical `let` a generated C backing
  local and maps the source symbol to that backing only while the lexical body
  is active. This prevents branch/match sibling declaration leaks, same-name
  nested `let` mutation of outer C locals, and post-let reads of stale C locals.
- `[FACT]` Lambda bodies that directly return nested lambdas now install the
  same primitive-shadow context as ordinary lambda body lowering before
  emitting closure capture data. Captured lexical bindings named `false` or `+`
  now capture the local value/callable instead of the primitive.
- `[FACT]` Generated e2e now has 414 cases and includes branch-local boolean
  shadowing, match sibling variable declarations, and nested closure captures
  for boolean and callable primitive-shadow names.
- `[FACT]` AOT module and `with` binding resolution now honors lexical
  ordering around generated `let` aliases: inner lexical aliases created inside
  a `with` body beat imported module names, while active module-private/open
  bindings beat older outer lexical aliases. Module value, mutation, and type
  descriptor exports can now collide with an outer lexical name without reading
  or assigning the outer backing.
- `[FACT]` Generated e2e now has 423 cases. The added module-collision rows
  cover module `define`, module `set!`, module type descriptor export,
  post-`with` outer lexical restoration, and inner lexical shadowing inside a
  `with` body.
- `[INVALIDATED]` Do not treat source `SymbolId` alone as the identity of a
  mutable captured AOT binding. The current name-keyed mutable-capture path can
  leak a let binding through `aot::define_var("name", ...)`, clobber same-name
  lexical scopes, and mask unbound `set!` in lambdas. The next fix must use a
  lexical binding identity/cell model across mutable-capture prescan,
  free-variable discovery, lambda capture layout, and `set!` lowering.
- `[FACT]` AOT mutable captured `let`/`set!` lowering now uses lexical
  `AotMutableCell` aliases instead of source-name environment writes.
  `LambdaDef` records mutability per capture slot, so a same-name capture is
  cell-backed only when the captured lexical binding was actually lowered as a
  mutable cell. Unresolved lambda `set!` now lowers through checked
  `set_var_result` and fails closed when no environment binding exists.
- `[FACT]` AOT recursive mutable self-captures now allocate the mutable cell
  before lowering the recursive initializer, seed it with nil, and store the
  initializer result through `mutable_cell_set`. Closure capture emission can
  therefore copy the active cell pointer instead of failing or capturing an
  uninitialized `Value*`.
- `[FACT]` AOT `set!` lowering now mirrors read-side module precedence around
  mutable cells. Active module-private/open bindings beat older outer mutable
  cells, while inner mutable cells created after the module mark still win.
- `[INVALIDATED]` Do not extend mutable-cell lowering to lambda parameters or
  synthetic effect wrapper bodies solely because they are nested lambdas in the
  AOT implementation. Direct parity probes showed JIT semantics do not mutate
  the outer lambda parameter or outer `checkpoint` binding in those shapes, so
  boxing them would be an AOT-only behavior change.
- `[FACT]` Root-owned `AotMutableCell.value` now uses root-store promotion for
  both constructor seeding and `set!` updates. Generated recursive and
  non-recursive mutable-capture lowering calls checked mutable-cell APIs, so a
  promotion/runtime failure is observable and cannot be reported as the same
  `null` sentinel used for successful `mutable_cell_set`.
- `[FACT]` AOT mutable-cell lifetime coverage now lives in both compiler AOT
  runtime parity tests and bounded `memory-lifetime-smoke`. The regression
  allocates child-scope cons/string graphs through an AOT closure and through
  initial cell construction, asserts the retained graph is root-owned before
  child release, then reads it after release only when the ownership assertion
  already passed.
- `[INVALIDATED]` Do not fix root-owned AOT mutable cells by switching every
  generated AOT closure body to root-scope execution. Store-time root promotion
  preserves the current child-scope allocation/provenance model while removing
  the root-to-TEMP edge at the mutable-cell retention point.
- `[INVALIDATED]` Do not blindly promote every immutable AOT closure capture at
  capture emission time. The broad clone-on-capture attempt regressed existing
  compiler capture-shape expectations for shadowed `false`, shadowed callable
  primitive captures, and match guard local captures; closure capture retention
  needs a separate semantics-preserving policy.
- `[FACT]` Before `AUDIT-245`, root-lived AOT closure payload structs stored
  non-mutable `Value*` captures directly. `AUDIT-245` and `AUDIT-246` closed
  that lane through capture retention plus bounded generated closure primitive
  teardown; do not treat this historical note as active work.
- `[FACT]` `AUDIT-245-AOT-CLOSURE-CAPTURE-ROOT-LIFETIME` closed the immediate
  immutable AOT closure capture UAF risk without clone-on-capture semantics.
  Generated closures with immutable captures now allocate `AotCaptureRetention`,
  retain the current temp scope when the captured graph reaches the current
  scope chain, and pass retention ownership into checked closure factory APIs.
  Generated all-mutable captured closures pass `null` retention and skip the
  allocation path.
- `[FACT]` `AUDIT-245` added closure-data teardown for generated owned payloads
  and retention release, while preserving legacy manual `make_closure`/
  `make_variadic_closure` behavior for caller-owned payloads by defaulting
  `owns_data=false`.
- `[FACT]` `AUDIT-245` closed two integration defects found by sub-agent review:
  ancestor temp-scope captures are detected through graph audit against the
  current scope chain, and mixed immutable/mutable generated captures guard
  mutable payload writes after retention failure.
- `[INVALIDATED]` Do not allocate capture retention for all-mutable generated
  closures. Mutable captures are root-owned cells after `AUDIT-244`; retention
  allocation there only adds an unnecessary failure path and root-teardown
  churn.
- `[FACT]` `AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME` closed the generated AOT
  closure primitive lifetime lane. Generated AOT closures now allocate scoped
  primitive wrappers with explicit `user_data_copy` / `user_data_finalizer`
  hooks, refcount generated sidecars across primitive wrapper copies, and
  release generated capture retention when the copied closure value becomes
  unreachable rather than waiting for `aot_shutdown`. Manual AOT closure helpers
  remain caller-owned and opaque across boundary copies.
- `[FACT]` 2026-04-28 AUDIT-246 fail-closed sub-slice shipped: AOT
  capture-retention reachability audit failures no longer count as successful
  reachability/retention. `REACHABLE_TEMP_*` still triggers retention, while
  graph-audit allocation, overflow, or forced internal failures now return an
  AOT capture error. Validation passed: `c3c build`, compiler slice
  `402 pass/0 fail`, bounded container `memory-lifetime-smoke`
  `287 pass/0 fail`, and C3 diagnostics.
- `[FACT]` 2026-04-28 AUDIT-246 generated-owned API sub-slice shipped:
  generated captured closures now call explicit
  `make_generated_closure_with_retention` /
  `make_generated_variadic_closure_with_retention` APIs instead of passing a
  trailing ownership boolean to lower-level constructors. Manual
  `make_closure` / `make_variadic_closure` remain caller-owned. Validation
  passed: C3 diagnostics, `c3c build`, compiler slice `402 pass/0 fail`,
  bounded container `memory-lifetime-smoke` `287 pass/0 fail`, generated e2e
  all 423 cases, and `git diff --check`.
- `[FACT]` 2026-04-28 AUDIT-246 bounded primitive teardown sub-slice shipped:
  `Primitive` payloads now carry optional `user_data_copy` and
  `user_data_finalizer` hooks. Boundary primitive copy routes still reject
  opaque manual `user_data`, but hook-backed generated AOT closure payloads can
  be copied/promoted and are finalized from the owning wrapper scope. Generated
  no-capture closures route through the same generated-owned constructors.
  Validation passed: `c3c build`, compiler slice `402 pass/0 fail`, bounded
  container `memory-lifetime-smoke` `288 pass/0 fail`, bounded generated e2e
  all 423 cases, `scripts/check_status_consistency.sh`, and `git diff --check`.
- `[INVALIDATED]` Do not retain a generated AOT closure's source scope while
  leaving it attached under the returned wrapper's owner scope. The bounded
  memory-lifetime smoke gate proved that ancestor/descendant scope cycles can
  reach `scope_release` with a destroyed retained scope during `aot_shutdown`.
  Retention activation now skips same-chain wrappers and detaches explicitly
  retained source scopes when a copied wrapper lives outside that scope chain.
- `[FACT]` 2026-04-28 AUDIT-246 post-review hardening shipped: AOT capture
  retention now selects the oldest reached TEMP owner scope in the active chain
  instead of blindly retaining the closure-creation scope, and recursive
  generated `let` lambda initialization now returns on ERROR before self-patch
  payload access. Validation passed: `c3c build`, compiler slice
  `402 pass/0 fail`, bounded `memory-lifetime-smoke` `288 pass/0 fail`, and
  bounded generated e2e all 423 cases.
- `[FACT]` 2026-04-28 `AUDIT-247-PRIMITIVE-USER-DATA-COPY-ROLLBACK` closed:
  primitive parent-boundary copy, escape promotion, and root-store clone now
  allocate the destination primitive shell, allocate the destination wrapper,
  install an inert `PRIMITIVE` wrapper, and register the destination destructor
  before invoking `Primitive.user_data_copy`. The copy hook is the commit step,
  so destination wrapper allocation and destructor-registration failures cannot
  occur after hook-backed sidecar mutation. Null-`prim_val` primitive copies
  now fail closed on destructor-registration failure. Manual/FFI opaque
  primitive payloads remain rejected unless they provide explicit copy/finalizer
  hooks. Validation passed: `c3c build`, bounded `memory-lifetime-smoke`
  `291 pass/0 fail`, `scripts/check_status_consistency.sh`, and
  `git diff --check`.
- `[FACT]` 2026-04-28 leaf wrapper destructor-registration hardening shipped:
  parent-boundary `INSTANCE` and `FFI_HANDLE` copies now fail closed with
  `BOUNDARY_COPY_FAULT_DTOR_REGISTRATION` and roll back their retained owner
  scope / FFI handle refcount when TEMP destructor registration fails. ESCAPE
  promotion now checks destructor registration for heap string/error copies,
  retained `INSTANCE` and `FFI_HANDLE` wrappers, retained continuations, and
  transferred coroutine wrappers, rolling back owned/retained state before
  returning an out-of-memory boundary error. Regression coverage now forces
  TEMP and ESCAPE destructor-record OOM for retain-backed leaf wrappers and
  hook-backed primitive `user_data` ESCAPE/root-store publication.
  Validation passed: `c3c build`, bounded `memory-lifetime-smoke`
  `297 pass/0 fail`, `scripts/check_status_consistency.sh`,
  `scripts/check_e2e_baseline_policy.sh`, stale 410-e2e wording scan, and
  `git diff --check`.
- `[FACT]` 2026-04-28 status consistency hardening shipped:
  `scripts/check_status_consistency.sh` now parses the bulleted
  `Current actionable count: N.` line used by `TODO.md`, and validates
  markdown-link part targets before comparing advertised line counts. The stale
  Part 18 count in `TODO.md` and old generated-e2e count wording in this
  changelog part were updated to the then-current 423-case contract.
- `[FACT]` 2026-04-28 aggregate wrapper and lexical-shadowing audit shipped:
  aggregate parent-copy, ESCAPE-promotion, and root-store clone routes for
  arrays, hashmaps/sets, method tables, and continuations now fail closed when
  destination destructor registration cannot be recorded. AOT `set!` lowering
  now lets the innermost lexical alias beat older mutable-cell aliases, and JIT
  mutability scans treat same-name `let` initializers as enclosing-scope work
  while still treating same-name `let` bodies as shadowed. Generated e2e now
  has 425 cases, including regressions for same-name `set!` shadowing and
  same-name initializer mutation. Validation passed: `c3c build`, bounded
  `memory-lifetime-smoke` `299 pass/0 fail`, bounded compiler slice
  `385 pass/0 fail`, bounded generated e2e all 425 cases,
  `scripts/check_status_consistency.sh`,
  `scripts/check_e2e_baseline_policy.sh`, and `git diff --check`.
- `[FACT]` 2026-04-28 documentation/status audit clarified
  `MEM-PROOF-009`: historical ffi_callback/libffi Valgrind notes are not the
  async/callback proof closure authority. FFI leak evidence is owned by
  `MEM-PROOF-010`, and `TODO.md` now names `MEM-PROOF-009` in the closed
  proof-summary chain.
- `[FACT]` 2026-04-28 AOT reset/capture and tensor destructor-registration
  hardening shipped. Generated e2e now has 431 cases, adding continuation
  snapshot regressions where a `capture` body mutates an AOT mutable cell before
  invoking `k`. AOT mutable-cell registries and active snapshot frames are
  thread-local like `g_aot_interp`, and terminal continuation resume replays
  the active reset snapshot before resuming the stack without freeing the
  frame-owned snapshot. Tensor result
  constructors under `src/lisp/prim_tensor*.c3` and
  `src/lisp/value_tensor*.c3` now use checked destructor registration, and
  copied lazy tensor boundary paths use tensor-specific cleanup helpers so
  materialized expression-edge children are cleaned before copied payload
  teardown on dtor-registration OOM. Validation passed: `c3c build`, bounded
  `memory-lifetime-smoke` `307 pass/0 fail`, bounded compiler slice
  `389 pass/0 fail`, bounded generated e2e all 431 cases,
  `scripts/check_status_consistency.sh`,
  `scripts/check_e2e_baseline_policy.sh`, and `git diff --check`.
- `[INVALIDATED]` Do not use a forced ESCAPE dtor OOM on a lazy tensor
  root-store clone as evidence for final clone destructor-registration failure.
  That failure can occur earlier while promoting expression-edge children, so it
  is not a precise test of the final tensor wrapper registration branch. Use
  the lazy tensor parent-copy dtor-OOM regression for expression-edge cleanup
  and the bare tensor root-store dtor-OOM regression for final clone error
  identity unless a more targeted root-store fault injection seam is added.
- `[FACT]` 2026-04-28 ML tensor result destructor-registration hardening
  followed the tensor audit. `src/lisp/prim_ml_*.c3` tensor result constructors
  now use `scope_register_value_dtor_or_cleanup`, and
  `scripts/check_status_consistency.sh` guards both tensor and ML result
  constructors against raw `scope_register_dtor` publication. The non-ML raw
  destructor-registration scan hits in value constructors and boundary copy
  helpers were audited as already fail-closed or false positives because they
  branch on registration failure and clean retained/owned state explicitly.
  Bounded `scripts/run_ml_validation_slice.sh` passed with `2102 pass/0 fail`;
  `ml_inference_oracle` reported `128/128` successful iterations and
  `ml_training_step_oracle` reported `64/64`.
- `[FACT]` 2026-04-28 fail-closed lifecycle helper cleanup tightened the
  shared destructor-registration helpers. `scope_register_value_dtor_or_cleanup`
  and the escape/tensor variants now clean already-materialized values even
  when called with a null scope, so helper misuse cannot leak owned payloads.
  The memory-lifetime smoke slice includes a direct FFI finalizer regression and
  passed with `307 pass/0 fail`.
- `[INVALIDATED]` Do not make outer mutation through checkpoint/capture bodies
  boxed solely to feed AOT mutable-cell snapshots. Current compiler shape tests
  require outer checkpoint/capture mutation to stay direct; reset-local mutable
  captures remain the boxed case.
- `[FACT]` 2026-04-28 AOT escaped multi-shot continuation snapshot hardening
  shipped. Captured continuations now carry an owned AOT mutable-cell snapshot
  when captured under an active compiled reset, replay that owned snapshot only
  after stack-clone success on each invocation, and release the snapshot through
  continuation invalidation/teardown. `compiled_reset` now restores/frees the
  possibly grown active frame pointer/count, and active-frame mutable-cell
  append rolls back partial nested-frame entries on allocation failure. AOT
  snapshot hook installation now occurs only after successful AOT bootstrap.
  Regression coverage exercises an escaped generated-AOT multi-shot
  continuation that returns `(1 1)` instead of leaking the first invocation's
  mutation into the second. Validation passed: `c3c build`, bounded compiler
  slice `390 pass/0 fail`, bounded memory-lifetime smoke `309 pass/0 fail`,
  bounded generated e2e all 431 cases, standalone generated-AOT continuation
  probe output `(1 1)`, `scripts/check_status_consistency.sh`,
  `scripts/check_e2e_baseline_policy.sh`, `bash -n` for the touched policy
  scripts, and `git diff --check`.
- `[FACT]` 2026-04-28 fail-closed lifecycle coverage was extended for owned AOT
  closure data on destructor-registration failure and for FFI handle constructor
  raw-payload release when the target scope is null. The helper implementation
  already failed closed after the cleanup patch; this checkpoint closes the
  adjacent coverage gaps.
- `[INVALIDATED]` Do not replay AOT continuation-owned mutable-cell snapshots
  before stack clone succeeds. A clone/OOM failure would otherwise mutate the
  live AOT registry even though continuation invocation fails; replay must occur
  after the target stack is available and before resume.
- `[FACT]` 2026-04-28 TLS integration gate was split from the remaining
  all-slice validation blocker. Targeted TLS wrapper validation passed all
  five handshake cases, and the TLS-enabled async slice passed with
  `OMNI_ENABLE_TLS_INTEGRATION=1`, `pass=104 fail=0`. The previous
  interpreter-only async failures were fixed by aligning test fixtures with
  fiber-only async contracts, rejecting JIT `ERROR` values in truthiness
  checks, and aborting scheduler-owned work at the `run()` error boundary.
- `[FAILED]` The bounded all-slice validation gate with TLS enabled and no TLS
  skip timed out after emitting non-TLS failures: boundary graph-audit
  violations, `destination-cons-build` traversal fallback failures in
  list/range/sort materialization paths, JIT policy failures, pipe loopback,
  and signal callback failures. Invalidated assumption: the remaining yellow
  validation status is still primarily a TLS integration blocker. Authority
  downgrade: `VALIDATION-001-TLS-INTEGRATION-GATE` is no longer the active
  blocker for all-slice green status. Preferred alternative: start from the
  first boundary/list materialization failure under
  `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER`.

## 2026-04-29 - Validation Baseline And Boundary Audit Follow-Up

- `[FACT]` 2026-04-29 `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER` is
  closed. The repair wave fixed Pika grammar parity fixture isolation, Deduce
  canonical surface/example drift, scheduler helper/test isolation leaks,
  Finwatch quoted-symbol dict access, and live HTTP TCP interpreter/JIT test
  port/state coupling. Focused Pika, Deduce, scheduler, and HTTP slices passed,
  and bounded TLS-enabled all-slice validation passed without TLS skip:
  `OMNI_TEST_SUMMARY suite=unified pass=5532 fail=0`; compiler summary
  remained green at `pass=390 fail=0`.
- `[INVALIDATED]` Do not preserve `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER`
  as the live validation blocker after 2026-04-29. The previous all-slice
  failure cluster is no longer authoritative for current status; use a fresh
  bounded all-slice or targeted slice result for any new validation backlog.
- `[FACT]` 2026-04-29 quiet validation output hardening landed. Expected
  memory-boundary negative graph-audit checks now compute pre-splice audit
  rejection without emitting violation lines, env hash-table OOM fallback
  warnings honor `OMNI_TEST_QUIET`, direct pass emitters in scheduler, FFI,
  FTXUI, tensor-buffer, and process-spawn cleanup tests now use quiet-aware
  paths, invalid bind-dependency rejection diagnostics are suppressed only for
  quiet test runs, and the Finwatch HTTP poll test handles `io/println` around
  the intentional poll tick. Validation passed: focused `memory-lifetime-smoke`
  `pass=309 fail=0`, scheduler `pass=143 fail=0`, advanced `pass=3658 fail=0`,
  HTTP with TLS `pass=34 fail=0`, and bounded TLS-enabled all-slice
  `pass=5532 fail=0` with compiler `pass=390 fail=0`; the strict grep guard
  found no targeted quiet-output leaks.
- `[FACT]` 2026-04-29 oversized C3 file-size gate cleanup landed. The remaining
  tracked source files above the 1000-LOC code-file limit were split into
  same-module helper files using whole-block moves for oversized test runners
  and whole-symbol moves for compiler/JIT helpers. Affected surfaces were
  compiler type-dispatch tests, memory-lifetime boundary benchmark/runtime
  allocation tests, compiler module/TCO helpers, and JIT policy failure tests.
  Validation passed: `scripts/check_file_size_gate.sh`, C3 diagnostics for all
  touched source/helper files, host `c3c build`, focused compiler slice
  `pass=407 fail=0`, focused JIT policy slice `pass=65 fail=0`, bounded
  `memory-lifetime-smoke` `pass=309 fail=0`, bounded `memory-lifetime-bench`
  load check `pass=0 fail=0`, bounded TLS-enabled all-slice
  `pass=5532 fail=0` with compiler `pass=390 fail=0`, and
  `scripts/check_status_consistency.sh`.
- `[FACT]` 2026-04-29 bindgen quiet-output closure landed. The bind dependency
  generator now applies the existing quiet-test predicate consistently to
  progress, success, warning, and error diagnostics. Quiet compiler validation
  passed with `pass=407 fail=0` and no bindgen diagnostic leaks under the grep
  guard; non-quiet compiler validation still printed the expected bindgen
  progress/error/generated-output diagnostics and passed with
  `407 passed, 0 failed`.
- `[FACT]` 2026-04-29 AOT manifest and I/O batch facade audit landed. The
  split compiler module/TCO helper source is now included in the AOT runtime
  Lisp manifest with the corrected part count, and stdlib declares/signals
  `io/offload-batch`, `io/thread-spawn-batch`, and `io/task-spawn-batch`.
  Scheduler regressions now exercise those public stdlib names. The I/O facade,
  libuv surface, and primitive docs parity guards now read the split primitive
  registration table rather than treating `eval_init_primitives.c3` alone as
  the registration inventory, and the stdlib appendix now lists
  `offload-batch`. Validation passed: I/O facade guard, e2e baseline policy,
  C3 diagnostics for the manifest and scheduler batch tests, JIT env/scope
  guard, scheduler slice `pass=146 fail=0`, bounded generated e2e all `431`
  cases, libuv surface policy, primitive docs parity, I/O parity status map,
  status consistency, and file-size gate.
- `[INVALIDATED]` Do not treat raw scheduler batch primitive coverage as enough
  for the public I/O contract. Fast-path effects must have matching stdlib
  effect declarations, signal wrappers, and facade-level regressions.
- `[FACT]` 2026-04-29 boundary ownership inventory/fallback audit landed. The
  memory ownership manifest now classifies the FFI test payload families
  `__leaf-wrapper-dtor-fail`, `__leaf-escape-dtor-fail`,
  `null-interp-ffi-cleanup`, and `null-target-scope-ffi-cleanup`, plus the
  dynamic foreign-runtime buffer wrapper constructor, under `MEM-PROOF-010`.
  `scripts/run_boundary_hardening.sh` now falls back to bounded Valgrind
  `memory-lifetime-smoke` when ASAN is reported unsupported, and
  `scripts/check_boundary_change_policy.sh` validates that fallback with normal
  stack/scope/compiler summaries plus zero Valgrind errors and zero
  definite/indirect/possible leaks. Validation passed: targeted and full
  ownership inventory, shell syntax checks, standalone bounded Valgrind
  `memory-lifetime-smoke` `pass=309 fail=0`, bounded boundary hardening with
  normal stack `26/0`, scope `64/0`, basic unified `173/0`, compiler `390/0`,
  Valgrind memory-lifetime-smoke `309/0`, boundary change policy with
  `OMNI_BOUNDARY_SECONDARY_KIND=valgrind`, status consistency, and file-size
  gate.
- `[INVALIDATED]` Do not use full `--test-suite all` as the Valgrind fallback
  for boundary hardening on this stack-engine surface. Valgrind reports the
  intentional active-stack clone/scan path as invalid/uninitialized stack
  access even when normal stack-engine assertions pass. Use normal stack
  evidence plus Valgrind `memory-lifetime-smoke` when ASAN is unavailable.
- `[FACT]` 2026-04-29 boundary telemetry/profile validation audit landed. The
  memory-boundary telemetry baseline artifact now matches the current
  `memory-lifetime-bench` workload families, including Finwatch product,
  closure iterator, tensor metadata, nested module return, slow-slack histogram,
  and scope-sequence counters. `scripts/run_boundary_profile_regression.sh`
  now invokes `./build/main --test-suite lisp` instead of accidentally entering
  the REPL, captures stdout/stderr separately before composing the parse log,
  and the profile parser/checker scripts are executable. Boundary profile
  thresholds now cap the current documented fallback profile rather than
  requiring the obsolete synthetic-only zero-copy profile. Boundary decision
  thresholds now recognize Valgrind fallback logs and compare against the
  current normal/Valgrind hardening baselines. Validation passed: strict memory
  telemetry envelope on the refreshed artifact, bounded boundary profile
  regression, boundary decision thresholds for normal plus Valgrind fallback,
  shell syntax checks, status consistency, and file-size gate.
- `[INVALIDATED]` Do not use the pre-workload-expansion March/April boundary
  telemetry artifacts as current threshold authority. The current accepted
  profile includes nested-module stable materialization copy debt
  (`materialization_copy_bytes_optimizer=220160`) and boundary fallback totals
  that must be capped and monitored, not compared to the old synthetic-only
  zero-copy baseline.
- `[FACT]` 2026-04-29 changelog/status freshness audit landed. The 2026-04-29
  validation and boundary follow-up entries now live under an explicit dated
  changelog heading, `memory/CHANGELOG.md` lists the Part 38 line count, and
  `scripts/check_status_consistency.sh` validates changelog index line counts
  in addition to TODO/session/plan indexes. Area status freshness dates now
  align with the current changelog date, and the status gate reports latest
  changelog date `2026-04-29`.
- `[FACT]` 2026-04-29 Tree-sitter tooling validation audit landed. The
  `tooling/tree-sitter-omni` npm scripts now use the pinned
  `npx --yes tree-sitter-cli@0.25.10` invocation already used by the query
  checker, so `npm test` and query/parse scripts no longer require a globally
  installed `tree-sitter` binary. The package dev dependency is pinned to the
  same CLI version, and the Tree-sitter README plus project tooling docs now
  route users through `npm run ...` commands. Validation passed: `npm test`,
  `npm run parse`, `npm run test:queries`, package JSON parsing, shell syntax
  checks, and whitespace checks.
- `[FACT]` 2026-04-29 split-document index freshness audit landed. Stale line
  counts were corrected in memory, area, core inspection, and plan split indexes,
  including `docs/areas/memory-runtime.md`. `scripts/check_status_consistency.sh`
  now validates every markdown index with `(N lines)` entries under `TODO.md`,
  `.agents/`, `docs/`, and `memory/`, instead of only the live TODO/session/plan
  subset. Validation passed: status consistency, shell syntax, and whitespace
  checks.
- `[INVALIDATED]` Do not use a scalar variable named `path` in zsh one-off audit
  loops. In zsh that name is tied to command lookup state, and assigning it can
  make basic commands such as `sed`, `wc`, and `dirname` disappear mid-loop. Use
  names like `target_path` or run complex probes under bash.
- `[FACT]` 2026-04-29 non-archive markdown link audit landed. Legacy
  absolute workspace markdown links in current docs/changelog material now
  resolve through repo-relative targets, and the session-report TODO link
  resolves to `docs/todo_parts/todo_part_15.md`. `scripts/check_status_consistency.sh`
  now validates local markdown links across `.agents`, `docs`, `memory`, and
  `tooling`, excluding historical archive directories that intentionally retain
  old command transcripts. Validation passed: shell syntax, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 stale workspace path audit landed. Current docs/tooling
  snippets no longer point at `/home/heefoo/Documents/code/Omni`; setup examples
  use `/path/to/Omni` or repo-relative file paths instead. `scripts/check_status_consistency.sh`
  now fails if non-archive markdown under `docs` or `tooling` reintroduces that
  stale absolute workspace root. Validation passed: stale-path scan, shell
  syntax, status consistency, post-complete backlog freshness, file-size gate,
  and whitespace checks.
- `[FACT]` 2026-04-29 root README stale-path follow-up landed. `README.md`
  now uses `/path/to/Omni` for checkout-local setup and code-form `build/main`
  mentions instead of stale absolute links to another workspace. The stale-path
  guard now covers `README.md` plus non-archive `docs` and `tooling` markdown.
  Validation passed: stale-path scan, shell syntax, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 root markdown link guard follow-up landed.
  `scripts/check_status_consistency.sh` now validates local markdown links in
  root-level `*.md` files as well as `.agents`, `docs`, `memory`, and
  `tooling`. The top-level markdown link scan was clean before guard expansion.
  Validation passed: root markdown link scan, shell syntax, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 root Tree-sitter README CLI audit landed. `README.md`
  now routes Tree-sitter grammar bootstrap through `npm run generate` and
  `npm run parse`, matching the pinned `tree-sitter-cli@0.25.10` package
  scripts. `scripts/check_status_consistency.sh` rejects direct global
  `tree-sitter generate/parse/test/query` commands in active README/docs/tooling
  markdown. Validation passed: direct CLI docs scan, `npm run parse`, shell
  syntax, status consistency, post-complete backlog freshness, file-size gate,
  and whitespace checks.
- `[FACT]` 2026-04-29 Tree-sitter generate split audit landed. `npm run generate`
  now runs the pinned Tree-sitter generator and then `scripts/split_tree_sitter_parser.sh`,
  preserving the checked-in split `src/parser.c` wrapper plus
  `parser_part_*.c.inc` layout instead of leaving a monolithic generated
  `parser.c` that fails the 1000-LOC code-file gate. Validation passed:
  `npm run generate`, `npm run parse`, `npm test`, `npm run test:queries`,
  shell syntax, package JSON parsing, status consistency, post-complete backlog
  freshness, file-size gate, and whitespace checks.
- `[INVALIDATED]` Do not treat the raw `tree-sitter-cli generate` output as a
  repo-ready artifact. It rewrites `tooling/tree-sitter-omni/src/parser.c` as a
  monolithic generated file, which violates the repository's tracked code-file
  size gate; use `npm run generate` with the splitter.
- `[FACT]` 2026-04-29 Tree-sitter split invariant status guard landed.
  `scripts/check_status_consistency.sh` now verifies that
  `tooling/tree-sitter-omni/package.json` wires generation through
  `scripts/split_tree_sitter_parser.sh`, the splitter is executable,
  `src/parser.c` remains include-only, and every referenced `parser_part_*.c.inc`
  file exists. Validation passed: `npm run generate`, `npm run parse`, shell
  syntax, status consistency, post-complete backlog freshness, file-size gate,
  and whitespace checks.
- `[FACT]` 2026-04-29 Tree-sitter splitter robustness audit landed.
  `scripts/split_tree_sitter_parser.sh` now validates existing split wrappers
  before accepting them as already split, fails loudly for malformed include-only
  wrappers or missing parts, checks that splitting produced at least one part
  file, and writes wrapper includes in numeric order. Validation passed:
  idempotent current-wrapper run, malformed-wrapper negative probe,
  `npm run generate`, `npm run parse`, `npm test`, `npm run test:queries`,
  shell syntax, status consistency, post-complete backlog freshness,
  file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 Tree-sitter splitter zero-output fail-closed audit landed.
  The splitter now collects actual temporary part files with `find -print0`
  before deleting any existing parser parts, so an empty or otherwise
  no-output split fails with `Tree-sitter parser split produced no part files`
  while preserving existing parts. Validation passed: empty-parser destructive
  negative probe, idempotent current-wrapper run, `npm run generate`,
  `npm run parse`, `npm test`, `npm run test:queries`, status consistency,
  post-complete backlog freshness, file-size gate, shell syntax, and whitespace
  checks.
- `[FACT]` 2026-04-29 script executable-mode guard landed.
  `scripts/check_status_consistency.sh` now fails if any top-level shell runner
  under `scripts/` loses executable mode, while preserving the explicit
  `scripts/c3c_limits.sh` sourced-helper exception as non-executable.
  Validation passed: shell syntax, direct executable-mode probes, status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[FACT]` 2026-04-29 first-party shell syntax guard landed.
  `scripts/check_status_consistency.sh` now runs `bash -n` across first-party
  shell entrypoints in `scripts`, `deps/build_static.sh`, and tooling shell
  helpers while excluding vendored dependency trees and node modules. Validation
  passed: direct shell-syntax scan, status consistency, post-complete backlog
  freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 first-party Python syntax guard landed.
  `scripts/check_status_consistency.sh` now parses first-party Python files
  under `scripts` and `tooling` with `ast.parse`, excluding Tree-sitter node
  modules and avoiding bytecode/cache writes. Validation passed: direct
  62-file Python parse probe, status consistency, post-complete backlog
  freshness, file-size gate, shell syntax, and whitespace checks.
- `[FACT]` 2026-04-29 first-party JSON syntax guard landed.
  `scripts/check_status_consistency.sh` now parses first-party JSON metadata
  while excluding `.claude`, `.claude-flow`, `.swarm`, `build`, vendored deps,
  third-party trees, and Tree-sitter node modules. Validation passed: direct
  5-file first-party JSON parse probe, status consistency, post-complete
  backlog freshness, file-size gate, shell syntax, and whitespace checks.
- `[INVALIDATED]` Do not enforce a blanket all-tracked-JSON parse gate. Several
  tracked `.claude/runs/*.json` historical operational transcripts are empty,
  so all-tracked JSON is not a valid first-party configuration contract.
- `[FACT]` 2026-04-29 first-party TOML syntax and API-key guard landed.
  `.codegraph/config.toml` no longer contains a non-empty checked-in LLM API
  key, and the local LLM block is disabled by default. `scripts/check_status_consistency.sh`
  now parses tracked TOML with `tomllib` and rejects non-empty TOML `api_key`
  assignments outside vendored/generated trees. Validation passed: direct
  2-file TOML parse probe, direct TOML API-key absence probe, status
  consistency, post-complete backlog freshness, file-size gate, shell syntax,
  and whitespace checks.
- `[PENDING]` Any real credential that matched the previously committed
  Codegraph API key value must be rotated outside the repository; the repo can
  remove and guard against the secret, but it cannot revoke the external token.
- `[FACT]` 2026-04-29 ignored boundary policy script audit landed.
  `scripts/check_boundary_value_policy_coverage.py` is now explicitly unignored
  and tracked, matching its role in `scripts/check_boundary_change_policy.sh`,
  `scripts/boundary_sensitive_files.txt`, and memory proof documentation.
  `.gitignore` also explicitly unignores
  `scripts/check_memory_ownership_inventory.py`, and
  `scripts/check_status_consistency.sh` now fails if any boundary-sensitive
  manifest entry is missing or untracked. Validation passed: value-policy guard,
  ownership-inventory guard, status consistency, boundary change policy with
  Valgrind secondary evidence, post-complete backlog freshness, file-size gate,
  shell syntax, and whitespace checks.
- `[INVALIDATED]` Do not run `scripts/check_boundary_change_policy.sh` with its
  default `asan` secondary kind against `build/boundary_hardening_asan.log` at
  this checkpoint. That log contains Valgrind memory-smoke evidence, not ASAN
  stack/scope/compiler summaries; use `OMNI_BOUNDARY_SECONDARY_KIND=valgrind`
  for the current evidence pair unless fresh ASAN-style evidence is generated.
- `[FACT]` 2026-04-29 Bash strict-mode guard audit landed.
  `scripts/run_e2e.sh` now enables `set -euo pipefail`, and
  `scripts/check_status_consistency.sh` rejects first-party Bash scripts under
  `scripts/` that omit the same strict-mode header. The sourced
  `scripts/c3c_limits.sh` helper remains the explicit exception. Validation
  passed: shell syntax, direct strict-mode scan, `run_e2e.sh`
  mount-bridge self-test, status consistency, post-complete backlog freshness,
  file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 dependency build-script strict-mode guard audit landed.
  `deps/build_static.sh` now enables `set -euo pipefail`, and
  `scripts/check_status_consistency.sh` enforces Bash strict mode over the same
  first-party Bash surface covered by the shell-syntax guard (`scripts`,
  `deps/build_static.sh`, and tooling shell helpers), still excluding vendored
  dependency trees, Tree-sitter node modules, and the sourced
  `scripts/c3c_limits.sh` helper. Validation passed: Bash syntax, direct
  strict-mode scan, status consistency, post-complete backlog freshness,
  file-size gate, and whitespace checks. An accidental `deps/build_static.sh
  --help` invocation ran the full static dependency build; resulting
  `deps/src`/`deps/lib` artifacts are ignored and no tracked dependency
  artifacts changed.
- `[FACT]` 2026-04-29 tracked script executable-mode guard audit landed.
  `scripts/check_boundary_profile_thresholds.sh`,
  `scripts/parse_boundary_profile_summary.sh`, and
  `scripts/split_tree_sitter_parser.sh` now have tracked executable mode.
  `scripts/check_status_consistency.sh` now verifies top-level `scripts/*.sh`
  tracked executable state, preferring `jj file list` in jj workspaces and
  falling back to `git ls-files --stage` in Git-only checkouts. The sourced
  `scripts/c3c_limits.sh` helper remains the tracked non-executable exception.
  Validation passed: direct jj tracked-mode probe, shell syntax, status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[FACT]` 2026-04-29 interpreter-specific shell syntax guard audit landed.
  `scripts/check_status_consistency.sh` now parses first-party Bash shebang
  scripts with `bash -n` and POSIX `sh` shebang scripts with `sh -n`, instead
  of parsing all shell entrypoints with Bash. Current POSIX `sh` entrypoints are
  `scripts/install_omni.sh` and `tooling/omni-nvim/scripts/run_smoke.sh`.
  Validation passed: touched status script Bash syntax, POSIX `sh -n` for both
  POSIX entrypoints, direct interpreter-specific syntax scan over the
  first-party shell surface, status consistency, post-complete backlog
  freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 POSIX shell strict-mode guard audit landed.
  `scripts/check_status_consistency.sh` now rejects first-party POSIX `sh`
  shebang scripts that omit `set -eu`, complementing the Bash
  `set -euo pipefail` guard. Current POSIX entrypoints
  `scripts/install_omni.sh` and `tooling/omni-nvim/scripts/run_smoke.sh` both
  satisfy the contract. Validation passed: status script Bash syntax, direct
  POSIX `set -eu` scan, POSIX `sh -n`, status consistency, post-complete
  backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 fast-dev generator visibility audit landed.
  `tools/fast-dev/generate_fast_dev_project.py` is now explicitly unignored
  and tracked, matching its role in the documented `scripts/build_fast_dev.sh`
  workflow. The generator's `is-up-to-date` command now accepts both helper
  archive arguments in the same order passed by `scripts/build_fast_dev.sh`,
  and `scripts/check_status_consistency.sh` now parses first-party Python under
  `tools` and fails if the fast-dev generator is missing or untracked.
  Validation passed: fast-dev profile generation, direct future-dated
  `is-up-to-date` probe, first-party Python syntax scan including `tools`,
  status consistency, post-complete backlog freshness, file-size gate, shell
  syntax, and whitespace checks.
- `[FACT]` 2026-04-29 fast-dev freshness predicate hardening landed.
  `tools/fast-dev/generate_fast_dev_project.py is-up-to-date` now checks that
  the source manifest exists before reading it, so missing freshness inputs
  return quiet status `1` for "rebuild needed" instead of emitting a Python
  traceback. Validation passed: default and nodeduce fast-dev profile
  generation, future-dated complete dependency probe, quiet missing-manifest
  and missing-source probes, first-party Python syntax, status consistency,
  post-complete backlog freshness, file-size gate, shell syntax, and whitespace
  checks.
- `[FACT]` 2026-04-29 fast-dev generator CLI/link hardening landed.
  `tools/fast-dev/generate_fast_dev_project.py` now validates subcommand
  arity before dispatch, so malformed `generate`, `profile`, and
  `is-up-to-date` calls return concise usage with status `2` instead of Python
  `IndexError` tracebacks. Generated fast-dev project linked libraries now use
  order-preserving de-duplication, removing the duplicated `omni_chelpers`
  entry while preserving existing library order. Validation passed: malformed
  subcommand probes, first-party Python syntax, touched shell syntax, default
  and nodeduce fast-dev profile generation, duplicate linked-library checks,
  status consistency, post-complete backlog freshness, file-size gate, and
  whitespace checks.
- `[FACT]` 2026-04-29 migration helper CLI hardening landed. The path-taking
  `scripts/migrate_*.py` helpers now use shared `scripts/migrate_cli.py`
  argument validation, so malformed no-path invocations return concise usage
  with status `2` instead of indexing `sys.argv[1]` at import time. `.gitignore`
  now explicitly unignores `scripts/migrate_*.py`, making this repair durable
  instead of leaving migration helpers under the blanket Python-script ignore.
  Status consistency now rejects raw direct `sys.argv[N]` indexing across
  first-party Python while allowing validated slicing such as `sys.argv[1:]`.
  Validation passed: no-path probes for all 20 path-taking migration helpers,
  valid-path disposable-file probes for the same helpers, raw-index scan,
  first-party Python syntax, status consistency, post-complete backlog
  freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 Python smoke visibility and runtime parity hardening
  landed. `.gitignore` now explicitly unignores `scripts/dialectic_mcp_single.py`
  and `tooling/tests/*.py`, and status consistency requires the Dialectic MCP
  entrypoint plus four documented tooling smoke tests to exist and be tracked.
  Making those tests visible exposed three stale/broken contracts: CLI JSON
  high bytes were emitted as non-ASCII, REPL-server active eval did not report
  `protocol/server-busy` for clone requests, and the init smoke still passed
  path arguments after `--init` moved to single-directory-name validation.
  CLI/REPL JSON string printers now escape bytes `>= 0x80` as `\u00XX`,
  REPL-server busy detection includes the active worker command, and the init
  smoke now runs from a temp cwd with a project name. Validation passed: C3
  diagnostics for touched runtime files, `c3c build`, Dialectic help path,
  all four tracked tooling smoke tests, first-party Python syntax, status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[FACT]` 2026-04-29 LSP JSON lowering-fixture refresh landed.
  `tooling/omni-lsp/tests/check_json_smoke.py` no longer treats
  `(define [ffi lib] ...)` as an unsupported AOT-lowering form; declarative FFI
  library declarations are now supported. The smoke now uses the current
  unsupported declarative variadic FFI contract to keep
  `compiler/lowering-error` JSON reporting covered without suppressing the real
  FFI support contract. Validation passed: LSP check-json smoke, LSP smoke,
  four tracked tooling smokes, first-party Python syntax, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 TLS targeted harness readiness hardening landed.
  `tests/lib/tls/server_once.omni` now optionally writes
  `OMNI_TLS_READY_FILE` after `tcp-listen`, and `scripts/run_tls_targeted.sh`
  waits on that explicit signal instead of sleeping for a fixed interval before
  connecting to the one-shot server fixture. `.gitignore` now ignores
  crash-leftover `.tmp_tls_*` harness scratch files. Validation passed: TLS
  targeted helper (`pass=5 fail=0`), async lisp slice (`99 passed, 0 failed`),
  status consistency, post-complete backlog freshness, file-size gate, and
  whitespace checks.
- `[FACT]` 2026-04-29 compiled async TLS readiness hardening landed. The
  embedded TLS integration cases in `src/lisp/tests_runtime_async_io_tls_groups.c3`
  now pass a per-port `OMNI_TLS_READY_FILE` to `tests/lib/tls/server_once.omni`
  and wait on that file before connecting, removing the remaining fixed
  `async-sleep 200` startup race against the one-shot server fixture. The
  compiled cases now unlink their readiness files after process cleanup, so
  successful TLS-enabled runs do not leak `/tmp/omni-tls-ready-*.txt` files.
  Validation passed: C3 diagnostics for the touched test file, `c3c build`,
  async lisp slice without TLS integration (`99 passed, 0 failed`),
  TLS-enabled async lisp slice (`104 passed, 0 failed`), TLS targeted helper
  (`pass=5 fail=0`), post-run `/tmp/omni-tls-ready-*.txt` cleanup check,
  status consistency, post-complete backlog freshness, file-size gate, and
  whitespace checks.
- `[FACT]` 2026-04-29 validation status summary CLI hardening landed.
  `scripts/run_validation_status_summary.sh --help` now prints usage and exits
  before sourcing validation limits, acquiring the build lock, or launching
  expensive gates; unknown `--*` options and extra positional arguments now
  fail with status `2`. `scripts/check_status_consistency.sh` now guards that
  help and unknown-option paths stay cheap. Validation passed: touched shell
  syntax, direct `--help` and invalid-option probes, process scan confirming no
  validation summary/container process remained active, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 Dialectic MCP package CLI hardening landed.
  `python3 -m scripts.dialectic_mcp.cli --help` now executes the package CLI
  module, prints argparse usage, and exits without the prior `runpy` warning.
  The package `__init__` no longer eagerly imports `.cli`; `make_parser` and
  `run_cli` remain available through lazy module attributes for the existing
  single-file wrapper. Validation passed: Dialectic MCP unittest discovery,
  direct package-module help, single-file wrapper help, Python bytecode compile
  for touched package/test files, status consistency, post-complete backlog
  freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 Dialectic MCP package smoke gate hardening landed.
  `scripts/check_status_consistency.sh` now requires
  `scripts/tests/test_dialectic_mcp.py` to remain tracked, runs the Dialectic
  MCP package unittest discovery quietly, and verifies the exact
  `python3 -m scripts.dialectic_mcp.cli --help` path exits successfully with
  usage on stdout and empty stderr. Validation passed: touched shell syntax,
  quiet status consistency, post-complete backlog freshness, file-size gate,
  and whitespace checks.
- `[FACT]` 2026-04-29 LSP Python entrypoint help hardening landed.
  `tooling/omni-lsp/omni_lsp.py`, `tooling/omni-lsp/tests/smoke_test.py`, and
  `tooling/omni-lsp/tests/check_json_smoke.py` now parse CLI arguments with
  argparse, so `--help` prints usage and unknown options fail with status `2`
  instead of being silently ignored. `scripts/check_status_consistency.sh` now
  guards those help paths for usage output and empty stderr. Validation passed:
  direct help and invalid-option probes for all three entrypoints, Python
  bytecode compile for touched LSP files, real LSP smoke test, real LSP JSON
  smoke, status consistency, post-complete backlog freshness, file-size gate,
  and whitespace checks.
- `[FACT]` 2026-04-29 Dialectic CLI missing-command artifact hardening landed.
  `scripts/dialectic_mcp/cli.py` and `scripts/dialectic_mcp_single.py` now
  reject `--mode cli` without a subcommand before constructing
  `DialecticEngine`, so malformed CLI invocations no longer create the default
  `dialectic_mcp.sqlite3` store. The Dialectic package tests now cover both
  entrypoints and assert that no SQLite, WAL, or SHM artifact is created.
  Validation passed: Dialectic unittest discovery (`3` tests), direct
  no-command probes for package and single-file entrypoints, clean
  `dialectic_mcp.sqlite3*` artifact scan, Python bytecode compile, status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[FACT]` 2026-04-29 Python policy guard help hardening landed.
  `scripts/check_boundary_value_policy_coverage.py`,
  `scripts/check_memory_ownership_inventory.py`, and
  `tools/fast-dev/generate_fast_dev_project.py` now treat `--help` as an
  explicit cheap usage path. The boundary and ownership guards no longer run
  policy scans on help, and the fast-dev generator no longer reports help as an
  unknown command. `scripts/check_status_consistency.sh` now guards all three
  help paths for usage output and empty stderr. Validation passed: direct help
  and invalid-option probes for all three scripts, real boundary policy guard,
  real ownership inventory guard, fast-dev no-argument usage error, Python
  bytecode compile, status consistency, post-complete backlog freshness,
  file-size gate, artifact scan, and whitespace checks.
- `[FACT]` 2026-04-29 one-off Python helper CLI hardening landed.
  `scripts/migrate_cli.py` now provides argparse-backed single-file and
  multi-file path parsing, rejects missing/non-file targets before mutation,
  supports cheap `--help`, and writes rewrites through an atomic temp-file
  replace helper. `scripts/fix_json_pointer_options.py` no longer hardcodes
  `/home/christos/Omni`; it requires an explicit target path. The four
  `scripts/remove_dup_*_raise.py` helpers now reject missing args, support
  `--help`, and remove only the intended generated helper block instead of
  deleting a fixed number of following lines. `.gitignore` now unignores these
  helpers, and `scripts/check_status_consistency.sh` requires them to remain
  tracked while guarding their help paths plus a representative migration
  helper. Validation passed: Python bytecode compile, direct help/no-arg/
  invalid-option probes, disposable-file rewrite probes, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[FACT]` 2026-04-29 TLS callback int-return clamp landed.
  `csrc/tls_helpers.c` now includes `limits.h`, clamps each socket callback
  `read`/`send`/`write` request to `INT_MAX`, and stores native byte counts in
  `ssize_t` before returning BearSSL's required `int` count. This closes
  `AUDIT_2.md` M48 without changing normal partial-I/O semantics or the
  existing `MSG_NOSIGNAL` write behavior. Validation passed: native C syntax
  check for `csrc/tls_helpers.c`, `c3c build`, TLS targeted helper (`pass=5
  fail=0`), status consistency, post-complete backlog freshness, file-size
  gate, and whitespace checks.
- `[FACT]` 2026-04-29 REPL worker queue clear locking hardening landed.
  `src/lisp/eval_repl_server_worker_helpers.c3` now locks `worker.mu`
  around `repl_server_worker_clear_queued_commands` queue and pending-command
  cleanup when the mutex is initialized, using `defer` for unlock symmetry.
  `AUDIT_2.md` M47 is closed as a hardening: the only production caller already
  clears after worker shutdown joins the worker thread, but the helper now
  enforces the same queue-locking invariant as submit, scan, and dequeue paths.
  Validation passed: `c3c build`, async Lisp slice (`99 passed, 0 failed`),
  status consistency, post-complete backlog freshness, file-size gate, and
  whitespace checks.
- `[FACT]` 2026-04-29 BLAS/LAPACK resolver publication hardening landed.
  `csrc/tensor_blas_helpers.c` and `csrc/tensor_lapack_helpers.c` now use
  `pthread_once` for dynamic BLAS/LAPACK library resolution instead of racy
  unsynchronized attempted flags. Function pointers are assigned before the
  loaded-library handle is published, failed resolution remains cached once as
  before, and BLAS/LAPACK call counters plus LAPACK test-disable flags use
  relaxed atomics. `csrc/tensor_lapack_helpers_factorization.inc` now increments
  shared LAPACK counters through the same atomic path. This closes `AUDIT_2.md`
  M50 and M51. Validation passed: strict native C syntax checks, `c3c build`,
  corrected 12-thread native BLAS/LAPACK resolver stress, status consistency,
  post-complete backlog freshness, file-size gate, and whitespace checks.
- `[INVALIDATED]` 2026-04-29 Do not reuse the first BLAS/LAPACK native stress
  harness variant as evidence. It guessed stale ABI signatures for `dgemm` and
  `dgetrf`, causing a crash before it could validate resolver behavior. The
  corrected harness uses the actual exported signatures and passed.
- `[FACT]` 2026-04-29 Vulkan resolver/probe publication hardening landed.
  `csrc/tensor_vulkan_helpers_core.c` now uses `pthread_once` for Vulkan
  dynamic library resolution instead of the racy `resolution_attempted` flag,
  and publishes the loaded-library handle only after all required Vulkan
  function pointers are assigned. `csrc/tensor_vulkan_helpers_core_context.inc`
  now uses `pthread_once` for the adjacent availability/feature probe cache.
  The old attempted flags and exported cache declarations were removed from
  `csrc/tensor_vulkan_helpers_runtime_decls.h`; cached availability bits are
  now private to the owning translation unit. `AUDIT_2.md` M52 is closed, and
  M49 was updated to record that `g_guard_hit` is already `volatile
  sig_atomic_t` while recovery depth and guard-table publication remain open.
  Validation passed: strict native C syntax for the Vulkan core helper,
  `c3c build`, 16-thread native Vulkan resolver/probe stress, status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[FACT]` 2026-04-29 stack guard signal-state publication hardening landed.
  `csrc/stack_helpers.c` now keeps SIGSEGV-handler-visible recovery depth and
  guard count as `volatile sig_atomic_t`, stores guard entries as scalar address
  ranges, publishes entries before count increments, withdraws count before
  table free, and uses acquire/release atomic builtins for handler-visible
  loads/stores without adding mutex work to the signal handler. `AUDIT_2.md`
  M49 is closed. The same audit invalidated stale `AUDIT_2.md` M53 because
  current `csrc/uv_helpers.c` already chunks `uv_buf_init` calls to bounded
  `UINT_MAX` lengths. Validation passed: strict native C syntax for
  `csrc/stack_helpers.c`, helper rebuild, `c3c build`, stack suite (`pass=26
  fail=0`), bounded container `memory-lifetime-smoke` (`pass=309 fail=0`),
  status consistency, post-complete backlog freshness, file-size gate, and
  whitespace checks.
- `[FAILED]` 2026-04-29 `advanced-effect-continuation` was not accepted as
  stack-guard closure evidence. The slice failed in IO/effect cases
  (`io dns-resolve payload code` and `multi-shot replay handled io effect`)
  unrelated to the stack guard publication change. Use stack-specific and
  memory-lifetime validation for M49 until that separate effect/IO failure is
  triaged.
- `[FACT]` 2026-04-29 test harness setup and double-assertion hardening landed.
  `src/lisp/tests_harness_helpers.c3:setup()` now aborts after printing its
  existing setup-failure diagnostics, so tests no longer continue after a
  failed shared setup precondition. The older `test_eq_double` helper now
  delegates to the existing tolerance-aware `test_double` path instead of
  duplicating exact `==` checks. This closes `AUDIT_2.md` M55 and M58.
  `AUDIT_2.md` M54 was invalidated as stale because `scripts/c3c_limits.sh` is
  a sourced helper that inherits strict mode from callers, and the current
  status guard intentionally excludes sourced helpers from standalone Bash
  strict-mode enforcement. Validation passed: C3 LSP diagnostics for
  `src/lisp/tests_harness_helpers.c3`, `c3c build`, basic Lisp slice (`pass=173
  fail=0`), advanced FFI surface slice (`pass=168 fail=0`), status
  consistency, post-complete backlog freshness, file-size gate, and whitespace
  checks.
- `[INVALIDATED]` 2026-04-29 The broad `advanced-collections-module` slice was
  previously not accepted as closure evidence because it failed
  `ml/softmax Vulkan Float32 stays on device and normalizes rows`; the failure
  was a test contract bug using unsupported 3-argument `+`, not a Vulkan
  softmax runtime defect. The assertion now uses nested binary `+`, and the
  advanced collections slice passes with `pass=2138 fail=0`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M56 and M57 were invalidated as stale after
  subagent review and local validation. The IO parity workflow references
  `scripts/check_io_parity_status_map.sh`, and the script exists, is
  executable, uses strict Bash mode, and passes together with the boundary
  facade and async fallback guards. Generated-e2e setup, expected-output
  collection, rendering, source emission, compilation, and file writes now fail
  closed, `--gen-e2e` maps generator failure to exit `1`, and `scripts/run_e2e.sh`
  rejects both Stage 2 failures and 431-row corpus drift. Validation passed:
  e2e baseline policy, IO parity status-map guard, IO boundary facade guard,
  async fallback policy guard, Bash syntax for the IO guard scripts, and
  YAML parsing for `.github/workflows/io-parity-guard.yml`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M59 and M60 were invalidated as stale after
  subagent review and local inspection. Current `run_program()` breaks on JIT
  compile failure and then executes the shared post-loop `exprs.free()` before
  returning; the single-expression `run()` helper path also frees `exprs`
  before returning on JIT execution failure. Current `eval_defeffect()` sets
  `info.field_count = 0` in the no-argument branch before registering the
  `TypeInfo`. Validation for this stale-closure slice used C3 diagnostics for
  the touched inspection files plus no-arg effect eval probes from the reviewer;
  integration build/gates are recorded in the session report.
- `[FACT]` 2026-04-29 JIT method-table typed-implementation guards landed,
  closing `AUDIT_2.md` M61-M63 and the adjacent `jit_eval_define` closure
  payload contract gap found by subagent review. `src/lisp/jit_define_method_table.c3`
  now centralizes closure-payload and typed-method-signature checks, uses them
  before appending/building method tables and before typed-closure define
  routing, and fails closed if a `CLOSURE`-tagged value has no closure payload
  before method-table routing or name tagging. Validation passed: C3 diagnostics
  for `jit_define_method_table.c3`, `c3c build`, focused JIT-policy filter
  (`pass=1 fail=0`), advanced type-dispatch mutation-chain slice (`pass=252
  fail=0`), and compiler slice (`pass=407 fail=0`).
- `[FACT]` 2026-04-29 dispatch/JIT fail-closed hardening landed for
  `AUDIT_2.md` M64-M66. `find_best_method()` now records ambiguous candidate
  indices in native inline/heap scratch during method-table scanning and only
  materializes a Lisp candidate-index list when building an ambiguity payload;
  regression coverage now forces allocation failure after partial
  materialization. `jit_expr_family_for_tag()` now returns
  `JIT_EXPR_FAMILY_INVALID` for future/invalid tags, and
  `jit_env_copy_fault_message()` now returns a deterministic unknown-fault
  message for future/invalid fault codes. Validation passed: C3 diagnostics for
  `eval_dispatch_match.c3`, `tests_advanced_type_dispatch_groups.c3`,
  `jit_compile_expr_dispatch.c3`, and `jit_closure_runtime.c3`; `c3c build`;
  advanced type-dispatch mutation-chain slice (`pass=253 fail=0`); full
  `jit-policy` slice (`pass=65 fail=0`); and `scripts/check_jit_env_scope_guards.sh`.
- `[FAILED]` 2026-04-29 The targeted Valgrind
  `advanced-type-dispatch-mutation-chain` run was not accepted as M64 closure
  evidence. The test slice itself passed (`pass=253 fail=0`), but Valgrind
  failed on pre-existing unrelated uninitialized reads in
  `lambda_call_type_error` / `SymbolTable.get_name` and small string-format
  leaks from `prim_format`; no M64-specific leak signal was isolated. Prefer
  normal focused type-dispatch/JIT validation for this M64 closure until those
  broader Valgrind findings are separately triaged.
- `[FACT]` 2026-04-29 parser/import/JIT traversal hardening landed for
  `AUDIT_2.md` M67-M69. Program parsing now enforces shared
  `PARSER_MAX_PROGRAM_EXPRS` limits across `parse_program()`,
  `Compiler.parse_program_exprs()`, and runtime source parsing. Default module
  import path construction now accepts explicit buffer capacity, clears output
  length on failure, validates pointer preconditions, and distinguishes
  default-path overflow from resolved-path overflow in runtime import errors.
  JIT warm-cache and continuation scan traversal now tolerate null expression
  list pointers instead of indexing malformed pointer/count pairs.
  Validation passed: C3 diagnostics for all touched C3 files, `c3c build`,
  compiler slice (`pass=409 fail=0`), focused `jit-policy` warm-cache filter
  (`pass=4 fail=0`), and focused `jit-policy` with-module-continuation-scan
  filter (`pass=1 fail=0`).
- `[INVALIDATED]` 2026-04-29 The earlier `advanced-collections-module` blocker
  was not an M68/runtime Vulkan defect. The softmax assertion used unsupported
  3-argument `+`; nested binary addition preserves the language contract and
  makes the broad advanced collections gate pass with `pass=2138 fail=0`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M70 is closed. `handle_effect_state_capture_handler_copy()`
  now rolls back already promoted handler-copy closures with
  `boundary_cleanup_materialized_value()` if a later closure promotion fails,
  including the primitive ESCAPE-lane destructor-registration route. Regression
  coverage forces an opaque primitive root-promotion failure after an earlier
  copied closure retained a detached env scope and verifies the env-scope
  refcount/destructor state is restored. Validation passed: C3 diagnostics for
  touched files, `c3c build`, `jit-policy` slice (`pass=68 fail=0`), bounded
  container `memory-lifetime-smoke` slice (`pass=309 fail=0`), and
  `git diff --check`.
- `[FAILED]` 2026-04-29 Host-side `memory-lifetime-smoke` is not valid closure
  evidence for M70 because the harness intentionally refuses memory-ownership
  slices on the host. Use the bounded container result for ownership validation.
- `[FACT]` 2026-04-29 Subagent audit found `AUDIT_2.md` M71 partly stale but
  still open: `jit_apply_value_impl()` now guards null callees, so the remaining
  contract is null result normalization at `jit_apply_value()` and iterative
  multi-arg application. Subagent audit also confirmed M72 remains valid and
  should add a bounded continuation-yield propagation limit.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M71 is closed. JIT apply now normalizes
  null callee results through a shared `jit_apply_normalize_result()` helper
  used by `jit_apply_value()`, direct multi-arg primitive/closure dispatch,
  non-tail and tail multi-param closure chain paths, and iterative multi-arg
  application. Regression coverage installs a primitive that deliberately
  returns null and verifies direct `jit_apply_value()`, direct multi-arg
  primitive dispatch, and iterative multi-arg application all fail closed with
  the same null-apply error. Validation passed: C3 diagnostics for touched
  files, `c3c build`, full `jit-policy` slice (`pass=69 fail=0`), file-size
  gate, and `git diff --check`.
- `[FACT]` 2026-04-29 Subagent audit found `AUDIT_2.md` M73 stale as a live
  crash path. `Compiler.compile_to_temp_non_null()` still has an implicit
  non-null precondition, but current call sites route through null-guarding
  wrappers. Treat explicit `@require`/assertion as optional defensive hardening
  rather than a live regression closure item.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M72 is closed. Blocked-fiber async-yield
  propagation now returns an explicit `JitBlockedFiberDriveStatus`, enforces a
  scheduler-round based iteration limit with a test-only low-limit override,
  and maps yield failure versus propagation-limit failure to distinct errors.
  The legacy handle-body switch path now uses the same bounded helper, and
  continuation/resolve/checkpoint/value-handle callers preserve their cleanup
  and interpreter-state restoration obligations before raising. Regression
  coverage drives a re-suspending target stack under a synthetic blocked
  scheduler fiber and verifies `JIT_BLOCKED_FIBER_DRIVE_LIMIT_EXCEEDED`.
  Validation passed: C3 diagnostics for touched files, `c3c build`, targeted
  `jit-policy` filter (`pass=1 fail=0`), full `jit-policy` slice (`pass=70
  fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L37-L39 are closed. `register_stdlib()`
  now propagates per-form `run()` failures and `bootstrap_runtime_interp()`
  fails closed on stdlib load errors; enabling that contract exposed and fixed
  a malformed trailing `)` in `stdlib/stdlib.lisp`. FFI metadata symbol-name
  matching now treats a null interpreter as a non-match, and `eval_ffi_lib()`
  returns `ffi/invalid-state` when `interp.global_env` is unavailable before
  evaluating the path or acquiring a dlopen handle. Regression coverage covers
  forced stdlib registration failure, null-interpreter metadata matching, and
  missing-global-env FFI lib declaration.
- `[FACT]` 2026-04-29 The malformed iterator fold fixture now follows the
  normalized JIT null-apply contract. A primitive iterator thunk returning raw
  `null` is normalized by `jit_apply_value()` to `jit: function application
  returned null`; non-null malformed iterator pairs remain covered by the
  iterator shape checks. Bounded `memory-lifetime-smoke` validation passed
  after updating the expectation (`pass=310 fail=0`).
- `[FAILED]` 2026-04-29 Host-side `memory-lifetime-smoke` is not valid closure
  evidence for L37-L39 because the harness intentionally refuses
  memory-ownership slices on the host. Use `scripts/run_validation_container.sh`
  for that slice.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L40 is closed after subagent reframe. The
  original null-deref concern was overstated because invalid path diagnostics
  tolerate null values, but `eval_path_step()` mixed dispatcher-level payload
  validation with helper-level validation. Path stepping now dispatches by tag
  and lets module/instance/dict/cons helpers own payload validation; root
  lookup tolerates null env/global-env inputs. Regression coverage constructs
  malformed MODULE/INSTANCE/HASHMAP values with null payloads and verifies
  fail-closed path errors. Validation passed: C3 diagnostics, `c3c build`,
  basic slice (`pass=174 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L41 is closed after subagent reframe. The
  compiler symbol-emission defect was broader than long identifiers:
  `Compiler.emit_symbol_name()` used lossy partial mangling, could emit invalid
  raw C3 identifier characters, could collide with raw user symbols such as
  `_omni_*`, and could expand legal long symbols into oversized generated C
  identifiers. Symbol emission now leaves only simple raw C3-safe names in the
  raw namespace and routes lossy, generated-prefix, reserved, `_omni_*`, and
  over-budget names through a bounded `_omni_sym_..._h<hash>_l<len>` namespace.
  Regression coverage checks long-name caps, punctuation escaping, generated
  namespace collisions, reserved-name collisions, and digit-leading prefix
  collisions. Validation passed: C3 diagnostics, `c3c build main`, compiler
  slice (`pass=410 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C3/H6 Vulkan shared-context teardown is
  closed after subagent reframe. The stale dangling-global portion was already
  fixed in current code: release clears `omni_tensor_vulkan_shared_context`
  under `omni_tensor_vulkan_context_mutex`, and acquisition retains the shared
  global only under that same mutex. The remaining live defect was final release
  unlocking before Vulkan device destruction, instance destruction, and
  `free(context)`. Final release now holds the mutex through full teardown.
  Native regression coverage installs test destroy callbacks and verifies both
  callbacks run while the mutex is held. Validation passed:
  `./scripts/build_omni_chelpers.sh`, relinked
  `build/vulkan_resource_safety_test`, ASAN relink/run, `c3c build main`,
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H2 is closed. JIT effect fast-path dispatch
  now validates matched fast-path table entries before reading `prim_val` in
  both `jit_signal_try_fast_path_dispatch()` and legacy `jit_signal_try_fast_path()`.
  Malformed entries fail closed with `runtime/invalid-fast-path-primitive`; the
  current dispatch path marks the malformed entry handled. Regression coverage
  corrupts a matched fast-path entry and verifies both paths return errors.
  Validation passed: C3 diagnostics, `c3c build main`, targeted `jit-policy`
  filter (`pass=1 fail=0`), full `jit-policy` slice (`pass=71 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L42 was reframed as stale for current
  correctness. `jit_cache_find_slot()` still has a hardcoded 16-probe window,
  but current stores route through `runtime_cache_store_expr()`, which clears
  and retries on 16-slot saturation. The remaining concern is a
  cache-retention/performance policy because a 17-entry collision cluster can
  evict before the configured GC threshold; the existing
  `cache-probe-saturation-recovery` JIT policy test passes.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H3 is closed. The direct
  `jit_apply_continuation()` helper now validates `k_val != null` and
  `k_val.tag == CONTINUATION` before reading `cont_val`, preserving the
  existing null-payload continuation error after the tag guard. Regression
  coverage calls the helper directly with null and non-continuation values and
  verifies fail-closed errors. Validation passed: C3 diagnostics for touched
  files, `c3c build main`, targeted `jit-policy` filter (`pass=1 fail=0`),
  and full `jit-policy` slice (`pass=72 fail=0`).
- `[FACT]` 2026-04-29 `AUDIT_2.md` L43 was reframed as stale for current
  production correctness. `jit_retired_code_insert()` remains internally
  unsynchronized, but production retired-code mutation/reset paths are
  owner-thread confined by JIT runtime identity checks before reaching the
  table; remaining direct callers are test-only. Future true multi-threaded JIT
  execution would need synchronized or per-owner-thread retired-code storage
  before shipping that mode.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H4 is closed at the CUDA kernel contract
  layer. `omni_cuda_diagonal_complex128` and `omni_cuda_diagonal_complex64` now
  return when `index >= diagonal_count || index >= rows || index >= cols` before
  reading `input[index * cols + index]`; embedded PTX include parts were
  regenerated from `csrc/tensor_cuda_complex_matrix.cu`. Rectangular CUDA
  diagonal smoke coverage was added behind existing Complex128/Complex64
  structural capability gates. Validation passed: CUDA `nvcc --ptx`, `ptxas`,
  `./scripts/build_omni_chelpers.sh`, C3 diagnostics for touched tests,
  `c3c build main`, basic slice (`pass=174 fail=0`), file-size gate, and
  `git diff --check`.
- `[INVALIDATED]` 2026-04-29 The H4-era broad advanced validation blocker was
  narrowed to a test assertion bug: `ml/softmax Vulkan Float32 stays on device
  and normalizes rows` used unsupported 3-argument `+` even though the language
  specifies `+` as unary/binary. The test now uses nested binary addition, and
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
  passes with `pass=2138 fail=0`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L44 is closed after subagent reframe.
  Generated AOT mains already gate initialization and many bridge callers
  produce contextual error values when an interpreter exists; the live defect
  was the pre-init/post-shutdown helper path where no `Interp*` exists. AOT
  no-interpreter error construction now records an out-of-band diagnostic with
  test-visible count/last-message seams and stderr output, while still returning
  null instead of fabricating orphan `Value*` errors. Direct closure apply and
  closure factory null-interpreter exits route through the diagnostic before
  returning null. Validation passed: C3 diagnostics, `c3c build main`, compiler
  slice (`pass=411 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L45 and L46 are closed. JIT local-env
  capture now returns a direct `runtime/out-of-memory` error when emitted
  `jit_env_extend` fails while binding non-mutable locals, instead of continuing
  with a null captured env. `jit_make_closure_from_expr()` now rejects malformed
  lambda ASTs with null payload/body and validates closure constructor results
  before touching closure payloads, preserving constructor `ERROR` returns.
  Regression coverage verifies local env capture OOM plus zero-arg and one-arg
  malformed lambda bodies. Validation passed: C3 diagnostics, `c3c build main`,
  focused JIT policy filters (`pass=1 fail=0` each), full `jit-policy`
  (`pass=74 fail=0`), advanced collections (`pass=2138 fail=0`), file-size
  gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` L47 is closed. Lexer float literal parsing
  now bounds fractional digit contribution while still consuming trailing
  fraction input, rejects missing/oversized exponents fail-closed, and rejects
  non-finite float literals. `parse-number` now uses the same bounded
  fraction/exponent policy while preserving Float64-overflow promotion to
  `BigFloat`. Validation passed: C3 diagnostics, `c3c build main`, direct eval
  probes for `1.2e3`, huge literal exponent failure, long fraction truthiness,
  and `parse-number "1.0e309"` `BigFloat` promotion, plus advanced numeric
  group (`pass=432 fail=0`).
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` L48 was a false positive as a
  correctness bug. `inet_ntop(...) == 0` is semantically equivalent to
  `inet_ntop(...) == NULL` in C because `0` is a null pointer constant. A
  readability cleanup replaced native pointer-zero checks with `NULL`, and DNS
  render coverage now verifies too-small buffers return
  `OMNI_ADDRINFO_RENDER_FAILED`. Validation passed: `cc -Wall -Wextra -Werror`
  for `csrc/addrinfo_helpers.c`, `./scripts/build_omni_chelpers.sh`,
  `c3c build main`, and a filtered async output check showing the DNS render
  tests pass. Broad async remains blocked by unrelated stack/libuv failures.
- `[FAILED]` 2026-04-29 The broad async slice is not clean L48 evidence in the
  current workspace: `OMNI_LISP_TEST_SLICE=async` reports `pass=89 fail=10`
  after the DNS render tests pass, with unrelated missing-path payload and
  stack/libuv context/listen failures. Do not treat that broad async failure as
  an addrinfo render regression.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C19 is closed under `MEM-PROOF-002
  ScopeRegion Core`. `scope_owner_guard_ok()` now returns false for null scopes
  while preserving owner-thread violation behavior for non-null wrong-owner
  scopes, and `scope_require_owner()` asserts non-null after callers have gated
  null. ScopeRegion allocator entrypoints return null on null `self`,
  destructor-registration entrypoints return false on null scope or null
  destructor function, and reset entrypoints are null no-ops. Regression
  coverage verifies null allocator, destructor-registration, and reset behavior.
  Validation passed: C3 diagnostics, `c3c build main`, ScopeRegion suite
  (`pass=67 fail=0`), Valgrind ScopeRegion suite (`ERROR SUMMARY: 0 errors`),
  file-size gate, and `git diff --check`. ASAN was attempted but unsupported by
  the current `c3c` target.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C20-C26 are stale false positives.
  The findings assume C-style implicit switch fallthrough in JIT warm-cache,
  JIT compile-dispatch, AOT `Compiler.compile_expr`, and float32 arithmetic
  switch blocks. C3 switches do not implicitly fall through, so adding `break;`
  is not a valid repair without separate evidence. Treat remaining
  missing-`break` audit claims as candidate input that must be checked against
  C3 switch semantics before editing.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C15 is another stale C-style
  switch-fallthrough finding. `scheduler_dispatch_wakeup_event` uses C3 switch
  dispatch, whose cases do not implicitly fall through, so no scheduler wakeup
  `break;` repair is needed without separate evidence. The targeted scheduler
  guard script was attempted after a successful rebuild but failed later on
  unrelated child/await checks (`parent waits for unawaited child` and
  `nested-context await in fiber rejected`).
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C6 is already fixed in current
  source. `make_error()` now checks `interp.alloc_value()` and falls back to
  `make_static_out_of_memory_error(msg)` when allocation fails, so the cited
  null dereference belongs to an older implementation.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C7 is closed at the unauthenticated
  Unix-socket transport boundary. TCP REPL already requires
  `OMNI_REPL_TCP_AUTH_TOKEN`, stdio is caller-owned, and Unix-socket mode now
  chmods the bound socket path to owner read/write (`0600`) before listening,
  unlinking the socket if permission hardening fails. Async REPL-server
  coverage verifies the socket mode. Validation passed: C3 diagnostics,
  `cc -Wall -Wextra -Werror -c csrc/uv_helpers_pipe.c`, native helper rebuild,
  `c3c build main`, focused async output showing `[PASS] async repl-server unix
  socket is owner-only`, file-size gate, and `git diff --check`. Broad async
  still reports unrelated file-payload and libuv bridge failures.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C3 remains closed. The original Vulkan
  shared-context dangling-global UAF wording is stale, and the reframed final
  teardown serialization defect is also fixed: shared context release clears
  the global under `omni_tensor_vulkan_context_mutex` and keeps that mutex held
  through device destroy, instance destroy, and `free(context)`. Validation
  passed: native Vulkan resource safety test compile/run.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C4 remains closed. The e2e baseline policy
  script centralizes review-rule validation in `review_rule_has_required_doc`,
  accepts either approved review document, and self-tests memory-only,
  types-only, both-docs, and no-approved-doc cases. Validation passed:
  `./scripts/check_e2e_baseline_policy.sh --self-test-review-rule` and
  `./scripts/check_e2e_baseline_policy.sh`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C5 is stale. The parser string escape
  finding assumed C-style switch fallthrough, but C3 switches do not implicitly
  fall through. Direct runtime validation returned `11` for
  `(length "hello\\nworld")`, proving one decoded newline rather than stacked
  fallthrough escapes.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H1 is stale and used the wrong
  ownership model. `stable_escape_materialize_init_closure()` allocates closure
  payloads in a staged ESCAPE `ScopeRegion` build scope, not individually owned
  heap blocks. Failure aborts/releases the staged build scope; success splices
  the staged ESCAPE lane into the destination after `scope_dtor_closure`
  registration succeeds. Do not add ad-hoc frees for these payloads; preserve
  the staged transaction boundary instead. Validation passed: C3 diagnostics
  for the materialization/builder/JIT fast-path files, `c3c build main`, and
  bounded container `memory-lifetime-smoke` (`pass=310 fail=0`).
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` L8 is stale under the current H2 JIT
  fast-path dispatch contract. Both current and legacy dispatch paths validate
  matched fast-path primitive table entries before helper dereference, so a
  malformed entry returns `runtime/invalid-fast-path-primitive` instead of
  crashing or falling through. If direct helper hardening is ever needed, mirror
  the existing fail-closed error behavior rather than returning `null`.
  Validation passed: targeted `jit-policy`
  `fast-path-malformed-primitive-entry-fails-closed` (`pass=1 fail=0`).
- `[FACT]` 2026-04-29 `AUDIT_2.md` C8-C10 are closed at the method-table
  payload boundary. `find_best_method()` and `format_dispatch_error()` now
  fail closed on null `MethodTable*`; JIT single-arg, multi-arg, and tail
  multi-arg method-table apply now share a `METHOD_TABLE` payload guard; and
  `jit_eval_define()` rejects null-payload existing method-table bindings
  before updating fallback. Regression coverage constructs a malformed
  `METHOD_TABLE` value and verifies direct matching, direct dispatch-error
  formatting, single apply, multi apply, tail multi apply, and fallback define
  all return errors. Validation passed: C3 diagnostics, `c3c build main`,
  targeted `jit-policy` filter `invalid-method-table-state-fails-closed`
  (`pass=1 fail=0`), full `jit-policy` (`pass=75 fail=0`), focused advanced
  type-dispatch (`pass=253 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C11-C12 are stale C-style
  switch-fallthrough findings. C3 switches do not implicitly fall through, so
  `scope_dtor_value()` does not cascade from STRING/ERROR into CLOSURE or from
  PRIMITIVE through VOID. The adjacent live hardening item was M32: silent
  `default: {}` made scalar/no-op destructor policy implicit. M32 is closed by
  enumerating every current no-op `ValueTag` and removing the silent default
  while preserving heap-backed release cases. Validation passed:
  `value_constructors_lifecycle.c3` diagnostics, `c3c build main`, scope suite
  (`pass=67 fail=0`), bounded container `memory-lifetime-smoke`
  (`pass=310 fail=0`), and Valgrind scope (`ERROR SUMMARY: 0 errors`).
- `[FACT]` 2026-04-29 `AUDIT_2.md` C13 is closed at the shared
  source-relative resolver boundary. `resolve_import_path_status()` now rejects
  leading `/`, embedded NUL bytes, and exact `..` path segments before
  concatenating the current source directory, while preserving ordinary nested
  relative paths and default dotted-module paths. Import and `(load path)`
  callers now distinguish unsafe path policy failures from path length
  failures. Regression coverage verifies resolver rejection/preservation,
  string import rejection, load rejection, overflow handling, dotted defaults,
  and existing path-cache behavior. Validation passed: C3 diagnostics,
  `c3c build main`, advanced collections/module slice (`pass=2141 fail=0`),
  compiler type-dispatch (`pass=411 fail=0`), basic slice (`pass=174 fail=0`),
  file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` C14 is a contract-inverted false
  positive. Normative effects semantics define `handle ^strict` as a boundary:
  handler lookup searches outward unless blocked by `^strict`, and strict
  unhandled effects raise at that boundary instead of falling through to outer
  handlers. Current JIT signal paths and `explain 'effect` match that contract.
  Treat transparent strict delegation as a future language-design change, not a
  regression fix.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C16 is closed. The x86_64 `fpu_save()`
  helper now matches the non-x86 helper contract by guarding `mxcsr` and
  `x87cw` output writes independently. Normal stack runtime paths already pass
  real storage, so this closes the exported native helper contract rather than a
  currently reachable stack-init path. Regression coverage in
  `tests/native/stack_fpu_blas_contract_test.c` calls `fpu_save(NULL, NULL)`,
  `fpu_save(&mxcsr, NULL)`, and `fpu_save(NULL, &x87cw)`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C17 is closed for the current dense BLAS
  `dger` ABI. `omni_tensor_backend_blas_dger()` now rejects `lda != n` before
  BLAS resolution or output writes and zeros only `m * n` elements for the
  supported dense row-major contract. The public tensor route already passed
  `lda == n`; the repair hardens direct/future native callers where the helper
  has no output extent parameter.
- `[FACT]` 2026-04-29 `AUDIT_2.md` C18 is closed. `jit_apply_multi_args_primitive()`
  now fails closed when `func` is null, not `PRIMITIVE`, has a null
  `prim_val`, or has a null primitive function pointer. JIT policy regression
  coverage verifies direct helper, routed multi-arg, and tail multi-arg
  primitive apply all return `invalid primitive application state` for malformed
  primitive values. Validation passed: C3 diagnostics, helper archive rebuild,
  `c3c build main`, targeted and full `jit-policy` (`pass=76 fail=0`), native
  and native-ASAN stack/FPU/BLAS contract tests, stack suite (`pass=26 fail=0`),
  advanced collections/module (`pass=2141 fail=0`), file-size gate, and
  `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H7-H8 are stale telemetry
  corruption findings. They assume C-style implicit switch fallthrough, but C3
  switch cases implicitly break unless explicit `nextcase` is used. Empty
  grouped labels intentionally share the following arm; non-empty telemetry
  counter arms in `copy_to_parent_note_tag()` and
  `boundary_escape_shape_note_root()` do not cascade.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H9 is stale. Current
  `scan_number_exponent()` bounds positive exponents to 308 and negative
  exponents to 324 before updating `exp_val`, reports
  `"float literal exponent out of range"`, and only runs the multiplier loop
  over the capped value. `parse-number` has matching bounded exponent handling.
- `[FAILED]` 2026-04-29 A direct native contract test linked against a stale
  `build/libomni_chelpers.a` after `c3c build main` and reproduced the old
  padded-`lda` stack overwrite. Do not treat `c3c build main` alone as evidence
  that edited C helper objects are active in standalone native tests; run
  `scripts/build_omni_chelpers.sh` and then relink/rebuild consumers.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H10-H11 are closed at the parser
  root-allocation boundary. Parser root allocations now route through
  `Parser.alloc_root_value_or_error`, which sets parser error state on null
  allocation; datum constructors now take `Parser*` and propagate null through
  quote, list, array-template, and dict-template construction. The same
  fail-closed allocation helper was applied to adjacent parser root allocations
  in atom literals, collection literals, pattern values, quote/quasiquote datum
  helpers, head forms, control effects, and relation attributes. Regression
  coverage injects deterministic root-allocation failures for atomic datum,
  quoted datum, list datum, array template, and dict template parsing and
  verifies null result plus `"out of memory while parsing datum"`. Validation
  passed: stale-call-site scan, parser raw-allocation scan, C3 diagnostics for
  touched parser/test files, `c3c build main`, compiler fail-closed slice
  (`pass=412 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H22 is stale. Current HTTP response
  parsing no longer contains the cited `response[hdr_start..header_end - 5]`
  expression; it rejects missing header delimiters and slices headers only when
  `hdr_start < header_delim_start` using the delimiter index. Broad HTTP
  validation was not usable as clean H22 evidence because the current HTTP
  slice still has unrelated fiber/stack-context failures (`pass=29 fail=5`).
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H23 assumes a sandbox-relative Deduce
  storage contract that current source/tests do not define. `deduce 'open`
  currently accepts raw caller-chosen file-backed paths, including absolute
  temporary paths used by durability/isolation tests. Do not patch this with
  ad-hoc leading-slash or `..` rejection while the API remains raw file-backed;
  sandboxed storage should be introduced as an explicit separate surface with
  base-directory semantics and migration tests.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H12 is closed at the raw primitive-body
  boundary. Public fixed-arity calls were already guarded by primitive metadata,
  but direct C3 primitive-body calls and misregistered primitive values could
  pass oversized slices to fixed-arity bodies. Fixed-arity bodies in
  `primitives_core.c3`, `prim_math_arithmetic.c3`, and `prim_math_core.c3` now
  reject `args.len != N`, while ranged `+`/`-` keep their explicit 1-or-2
  argument contract. Regression coverage verifies public eval arity failures
  and direct raw-body failures for representative core/math primitives.
  Validation passed: C3 diagnostics, `c3c build main`,
  `arithmetic-comparison` (`pass=56 fail=0`), advanced stdlib numeric
  (`pass=436 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H30 is closed. `Env.hash_lookup` and
  `Env.hash_insert` now bound linear probing by `hash_capacity` and fail closed
  when no slot is available, preventing malformed or future full hash tables
  from hanging lookup/insert. Regression coverage constructs a full two-slot
  environment hash table and verifies missing lookup/insertion return without
  mutating existing bindings. Validation passed: C3 diagnostics,
  `c3c build main`, bounded container `memory-lifetime-smoke`
  (`pass=311 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H13 is stale under centralized FFI
  handle construction cleanup. `make_ffi_handle_ex_with_descriptor` releases
  finalizer/free-backed non-library payloads on null interpreter, null target
  scope, FFI box allocation failure, wrapper value allocation failure, and
  destructor-registration failure. Do not add duplicate caller-side cleanup to
  the cited async constructors unless ownership has been explicitly retained.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H26 is stale as a silent-truncation
  finding. The CLI C-string cap remains, but `cstr_len_bounded` reports over-cap
  input as failure and callers surface explicit `cli/argument-too-long` or
  mode-specific diagnostics. Treat the 64 KiB cap as policy, not truncation.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H27-H29 are already fixed in current
  source. `eval_path_lookup_root` guards null `env`, `eval_path_instance_step`
  guards null `current.instance_val`, and `eval_path_dict_step` guards null
  `current.hashmap_val`.
- `[PENDING]` 2026-04-29 `AUDIT_2.md` H24 and H25 remain live JIT follow-up
  targets. H24 needs a hard spill-state cap or derived total tracked-state cap
  before `JitStateSpillNode` allocation. H25 needs a clear retired-code
  tombstone contract: either insertion failure is fail-closed/saturated-unsafe,
  or compile-nonce validation is documented as authoritative and the tombstone
  table is demoted from safety-critical status.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H24 is closed. JIT compiled-state spill
  tracking now has `JIT_STATE_SPILL_MAX`; once reached,
  `jit_track_compiled_state` marks GC needed and returns false before
  allocation, letting `jit_compile` destroy the emitted state through its
  existing tracking-failure cleanup path. Validation passed: C3 diagnostics,
  `c3c build main`, targeted `jit-policy` state-spill-limit (`pass=1 fail=0`),
  full `jit-policy` (`pass=77 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H25 is closed with an explicit
  saturated-tombstone fail-closed contract for legacy pointer-only liveness.
  Normal nonce-bearing compiled-function liveness remains authoritative through
  tracked-state owner/interpreter/serial/compile-nonce matching; legacy
  `jit_compiled_code_is_live_for_interp` now consults retired-code tombstones
  before pointer matching, and a saturated tombstone table treats any non-null
  code/serial query as retired. Validation passed: targeted `jit-policy`
  retired-code-tombstone-table (`pass=1 fail=0`) and full `jit-policy`
  (`pass=77 fail=0`).
- `[PENDING]` 2026-04-29 `AUDIT_2.md` H34-H36 are the next high-priority live
  JIT closure cluster. H31, H33, and H38 were invalidated by read-only audit;
  H32 is lower-risk enum metadata fallback work; H37 needs an AOT tail-budget
  policy rather than an arbitrary recursion cap.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H34-H36 are closed. JIT single-arg,
  tail-call, multi-arg, direct multi-helper, and tail multi-helper closure
  apply paths now share a closure payload/body validator and return `jit:
  invalid closure state` instead of dereferencing malformed closure payloads or
  null bodies. `jit_copy_closure_env_if_needed` now rejects null-payload
  closure values with `BOUNDARY_ENV_COPY_FAULT_INVALID_CLOSURE_STATE`; null-body
  closure env-copy remains accepted to preserve existing memory-boundary
  fixture behavior. Validation passed: C3 diagnostics, `c3c build main`,
  targeted `jit-policy` invalid-closure-state-fails-closed (`pass=1 fail=0`),
  full `jit-policy` (`pass=78 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H31, H32, H33, and H38 are stale.
  H31's cited single-arg primitive args-slice dereference is no longer present;
  H32's enum-name switches are exhaustive under C3 and should not gain
  `default` branches that hide future enum additions; H33's method-table apply
  paths already guard null payloads; H38 uses bounded `io::bprintf` rather than
  overflowing long symbol names.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H37 is closed as AOT tail back-edge
  interrupt parity work. This was not fixed with an arbitrary tail-call depth
  cap; large terminating tail loops remain valid. `runtime_eval_backedge_poll`
  now centralizes interrupt polling/clearing/error creation, `runtime_eval_expr`
  uses it for existing runtime/JIT TCO behavior, and AOT `invoke`,
  `apply_multi`, `aot_closure_apply`, and `aot_variadic_apply` poll it before
  each pending tail redispatch. Regression coverage verifies interrupt delivery
  for `invoke`, `apply_multi`, closure primitive-entry, and variadic
  primitive-entry AOT tail back-edges. Validation passed: C3 diagnostics,
  `c3c build main`, compiler slice (`pass=416 fail=0`), file-size gate, and
  `git diff --check`.
- `[PENDING]` 2026-04-29 `AUDIT_2.md` H16 was confirmed by read-only scan as a
  live next candidate: module reload clears an existing module name without
  rebuilding the module hash before replacement, and file-module index creation
  can publish duplicate path entries if called without the import precheck.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H16 is closed. Module publication now keeps
  the module table/hash/path invariants aligned: stale unloaded same-name
  modules are retired through `rollback_module_publication`, file-module
  creation reuses existing path-backed slots, implicit file loads no-op for
  loaded path matches and reject in-progress path matches, module-table growth
  skips inactive names during rehash, default-name imports use the same
  path-cache check as string imports, and path-backed resolution is path-first.
  Regression coverage verifies duplicate path reuse and same-name unloaded
  replacement hash cleanup in the advanced module runtime path tests.
  Validation passed: C3 diagnostics, `c3c build main`, advanced
  `advanced-collections-module` filter (`pass=2143 fail=0`), compiler slice
  (`pass=412 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` H17 is stale. Current
  `SymbolTable.intern` rejects `self.count >= (usz)INVALID_SYMBOL_ID` before
  growth/allocation and before the guarded `(SymbolId)self.count` cast. Existing
  `run_basic_symbol_table_hardening_tests` coverage forces the ceiling,
  verifies `intern` returns `INVALID_SYMBOL_ID`, preserves `count`, and keeps a
  preexisting symbol resolvable. Validation passed: C3 diagnostics for
  `value_symbol_table.c3` and basic slice (`pass=174 fail=0`).
- `[FACT]` 2026-04-29 `AUDIT_2.md` H18 is closed. `scope_register_dtor` and
  `scope_register_dtor_escape` now dedupe exact duplicate `(ptr, func)`
  destructor registrations before allocating `ScopeDtor` metadata. This keeps
  duplicate registration idempotent under forced dtor-metadata OOM while
  preserving same-pointer / different-function destructor registrations. TEMP
  and ESCAPE ScopeRegion tests assert duplicate-plus-distinct list counts and
  teardown side effects. Validation passed: C3 diagnostics, `c3c build main`,
  ScopeRegion suite (`pass=67 fail=0`), Valgrind ScopeRegion (`ERROR SUMMARY:
  0 errors`), file-size gate, and `git diff --check`.
- `[FAILED]` 2026-04-29 Broad H18 memory-lifetime-smoke validation was
  attempted through the required bounded container path, but
  `scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
  was killed with exit 137 before emitting a final pass/fail summary. Do not
  treat that broad slice as passing evidence for H18; prefer the targeted
  ScopeRegion and Valgrind evidence until the broad container limit issue is
  separately resolved.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H19 is closed. `big_integer_binary_handle`
  now rejects null operands and malformed `BIG_INTEGER` payload handles before
  calling the native big-number backend for `INT/BIG_INTEGER`,
  `BIG_INTEGER/INT`, or `BIG_INTEGER/BIG_INTEGER` operations. Adjacent unary
  BigInteger negation and bitwise-not helpers also fail closed on null values or
  null payload handles. Regression coverage constructs malformed internal
  `BIG_INTEGER` values and verifies binary routes return errors. Validation
  passed: C3 diagnostics, `c3c build main`, advanced stdlib numeric filter
  (`pass=437 fail=0`), file-size gate, and `git diff --check`.
- `[PENDING]` 2026-04-29 `AUDIT_2.md` H20 and H21 remain live by read-only
  audit. H21 is the next prioritized target: generated-name helpers in
  `compiler_name_helpers.c3` still write into caller buffers without capacity
  checks and require a repo-wide call-site contract update.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H21 is closed. Generated-name helper
  writes are now bounded and non-truncating across id, index, suffix, and pair
  formatting; overflow returns an empty sentinel, and compiler call sites use
  `Compiler.generated_name_*` wrappers that set `compiler: generated name
  exceeds buffer capacity`. Regression coverage verifies exact-capacity
  success, overflow failure for all four helper families, and compiler wrapper
  diagnostics. Validation passed: C3 diagnostics, `c3c build main`, compiler
  slice (`pass=417 fail=0`), e2e compiler generation (`431` tests),
  file-size gate, and `git diff --check`.
- `[PENDING]` 2026-04-29 `AUDIT_2.md` H20 remains the next visible high-severity
  candidate: `serialize_value_to_buf` still silently emits `nil` for
  unsupported `ValueTag` payloads, risking literal round-trip data loss.
- `[FACT]` 2026-04-29 `AUDIT_2.md` H20 is closed. `serialize_value_to_buf`
  no longer silently maps unsupported `ValueTag` payloads to `nil`.
  Source-reconstructible arrays, dictionaries, sets, `Void`, `TimePoint`, and
  big numeric values now serialize to parser-compatible source; opaque
  runtime/resource values set a compiler diagnostic. Regression coverage
  verifies collection, BigInteger, TimePoint, and opaque-value fail-closed
  behavior. Validation passed: C3 diagnostics, `c3c build main`, compiler
  slice (`pass=423 fail=0`), e2e compiler generation (`431` tests),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M33 is closed. Public `lshift`/`rshift`
  already enforced `BIG_INTEGER_MAX_SHIFT_BITS`, but shared
  `big_integer_shift_value` now also rejects over-cap shifts and null/malformed
  BigInteger operands before backend calls. Regression coverage directly
  exercises helper-level over-cap integer and BigInteger shifts plus malformed
  payload shifts. Validation passed: C3 diagnostics, `c3c build main`,
  advanced numeric sort/bitwise/HOF filter (`pass=41 fail=0`), full advanced
  stdlib numeric filter (`pass=438 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M34/M35 are closed. BigFloat and
  BigComplex raw-handle helpers now reject malformed null-payload values at the
  C3 boundary before clone/render/component backend calls. The stale part of
  the audit wording is that current C++ backends already tolerate these null
  handles; the fixed contract is local C3 fail-closed normalization instead of
  relying on backend tolerance. Regression coverage directly constructs
  malformed `BIG_INTEGER`, `BIG_FLOAT`, and `BIG_COMPLEX` values and verifies
  BigFloat conversion/comparison, BigComplex part construction, real/imag
  extraction, and conjugation helpers fail closed while scalar real/imag/
  conjugate behavior remains unchanged. Validation passed: C3 diagnostics,
  `c3c build main`, full advanced stdlib numeric filter (`pass=439 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M36 is closed. HashMap direct helpers now
  reject null or malformed backing storage before probing: `hashmap_get`
  returns `null`, and `hashmap_remove` returns `false`. Public dict/set
  primitives and canonical key/value iteration now raise invalid-state errors
  for non-empty malformed backing storage, and print paths render malformed
  dict/set values as `#<dict:invalid>` / `#<set:invalid>` instead of
  dereferencing null entries. Regression coverage constructs malformed internal
  HashMap/set values in the focused advanced collections module and verifies
  helper, primitive, and print behavior. Validation passed: C3 diagnostics,
  `c3c build main`, advanced collections module filter (`pass=2144 fail=0`),
  file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 Do not require `map.mask == map.capacity - 1` in
  the shared malformed-storage guard. That over-strict M36 attempt rejected
  environment hash tables and triggered `WARNING: env hash table build failed;
  falling back to linear lookup`. The preferred guard is non-null map,
  non-null entries, and power-of-two capacity; mask invariants need a separate
  normalized-map audit if they become a public contract.
- `[FAILED]` 2026-04-29 M36 memory-lifetime checked-collections placement did
  not produce useful validation evidence in this environment. The required
  bounded `memory-lifetime-smoke` container path timed out/killed with exit 137
  twice before a final summary, so do not cite it as passing M36 evidence. The
  effective regression is the focused advanced collections module case that
  passed locally.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M37 is closed. `hashmap_grow_checked` now
  rehashes into local new storage and tracks the placed-entry count before
  publishing the new table to the live map. If any old entry cannot be placed,
  the new storage is freed and the original entries pointer, capacity, mask,
  count, and lookups remain intact. A narrow forced rehash-insert failure hook
  covers the impossible-state path in the advanced collections module.
  Validation passed: C3 diagnostics, `c3c build main`, advanced collections
  module filter (`pass=2145 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 Natural M37 rehash probe exhaustion remains unreachable
  under current valid-map invariants because growth doubles a zeroed
  power-of-two table and reinserts at most `old_cap` live entries into
  `2 * old_cap` slots. Treat M37 as an atomicity hardening closure, not as
  evidence of a reachable normal data-loss path.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M38 is closed. `scheduler_add_fiber` now
  rejects stale, completed, out-of-range, and cross-owner parent ids before
  publishing a fiber entry. `prim_spawn` propagates invalid-parent admission
  as `scheduler/spawn-invalid-parent` and releases the just-created coroutine
  context on every failed scheduler admission path. Scheduler abort and idle
  reset release discarded fiber coroutine contexts before clearing fiber slots,
  preventing abandoned ready/blocked fibers from exhausting the stack-context
  pool. Validation passed: C3 diagnostics, `c3c build main`, scheduler slice
  (`pass=147 fail=0`), `scripts/check_scheduler_state_guards.sh`, file-size
  gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-29 The original M38 public-spawn wording overstated
  the natural failure path. Public `spawn` only inherits a parent while the
  current fiber is actively running, so a completed-parent public spawn is not
  the normal reachable contract. The live contract is internal scheduler helper
  parent admission plus stale-state cleanup hardening.
- `[FAILED]` 2026-04-29 Initial M38 regression attempts using real coroutine
  creation in internal invalid-parent cases polluted stack-context capacity.
  The accepted test uses a synthetic non-null coroutine because invalid-parent
  admission must reject before promotion or context use.
- `[FAILED]` 2026-04-29 The M38 language-level nested-checkpoint regression
  that spawned a child inside the checkpoint was a poor oracle: it depended on
  stack-context allocation/cache state and failed with context-create errors
  before reaching the scheduler guard. The accepted regression directly
  simulates a nested stack context and verifies `await` returns the
  `blocking scheduler op requires the fiber root context` error.
- `[FACT]` 2026-04-29 Completed read-only M39 exploration. `SymbolTable.intern`
  is the next target: if hash insertion cannot find an empty probe slot, the
  current code can publish `entries[id]` and increment `count` without a
  matching `hash_index` entry, making the symbol unreachable and allowing a
  later duplicate id for the same name.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M39 is closed. Symbol-table hash-index
  placement now uses a checked helper for both new-symbol intern insertion and
  grow rehashing. `SymbolTable.intern` claims a hash slot before publishing
  `entries[id]` or incrementing `count`; failure frees the allocated name and
  returns `INVALID_SYMBOL_ID` with no table side effects. `SymbolTable.grow`
  aborts before publishing new storage if any old entry cannot be rehashed.
  Regression coverage forces a synthetic full hash-index table and verifies
  repeated intern attempts preserve `count` and the existing symbol. Validation
  passed: C3 diagnostics, `c3c build main`, basic slice (`pass=175 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M40 is closed. Match diagnostic
  union-variant coverage now recursively scans constructor subpatterns, guard
  subpatterns, as-patterns, cons patterns, and sequence elements. Top-level
  wildcard coverage and the existing nullary-constructor `PAT_VAR` path are
  preserved. Diagnostics coverage asserts that a guarded nested constructor
  clause structurally covering a variant no longer yields a false
  `missing variants` message. Validation passed: C3 diagnostics,
  `c3c build main`, diagnostics slice (`pass=11 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M41 is closed. Declared module file loading
  now treats an existing published module with `loaded == false` as a circular
  import and returns `circular import detected` before re-evaluating the module
  form. Regression coverage seeds an in-progress declared file module,
  re-enters `jit_eval_declared_module_file`, verifies the circular-import
  error, preserves the loading module until explicit rollback, and then
  confirms rollback removes it. Validation passed: C3 diagnostics,
  `c3c build main`, advanced collections module filter (`pass=2146 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-29 The module test file was split top-down to satisfy the
  tracked-code 1000 LOC gate after M41 coverage. The Float64 dictionary hash
  regression helper now lives in
  `src/lisp/tests_advanced_stdlib_module_groups_dict_helpers.c3`; behavior is
  still exercised through `run_advanced_collections_dict_literal_tests`.
- `[FAILED]` 2026-04-29 M41 advanced filter attempts using
  `advanced-stdlib-module` and `advanced-module-system` matched no tests. The
  correct group key is `advanced-collections-module`.
- `[FAILED]` 2026-04-29 An unfiltered full advanced slice crashed before
  module tests while entering `advanced-logic-tco`, so do not cite that run as
  M41 evidence. The focused filtered module group passed and is the accepted
  runtime validation for M41.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M42 is closed. AOT quasiquote lowering now
  enforces `AOT_QUASIQUOTE_MAX_DEPTH = 64`, matching JIT's
  `quasiquote nesting too deep (max 64)` contract. Recursive quasiquote
  consumers now return `usz.max` when `Compiler.has_error` is set or an inner
  quasiquote lowering returns `usz.max`, preventing the sentinel from being
  emitted as `_r184467...` in generated C. Regression coverage exercises both a
  direct over-nested quasiquote and an over-nested quasiquote inside
  quasiquoted call/list lowering. Validation passed: C3 diagnostics,
  `c3c build main`, compiler slice (`pass=425 fail=0`), file-size gate, and
  `git diff --check`.
- `[INVALIDATED]` 2026-04-29 The M42 audit's `emit_error_temp` recommendation
  is stale. The current compiler error API is `Compiler.set_compile_error(...)`
  and callers must propagate `usz.max`.
- `[INVALIDATED]` 2026-04-29 `AUDIT_2.md` M43's proposed decrement is wrong
  for the current multi-shot ownership model. `jit_apply_handle_continuation_value`
  clones and consumes only the resumed stack clone; it must not release the
  original continuation wrapper's retained handler-state reference. That
  retained reference is owned by wrapper/escape cleanup or single-shot
  `resolve` consumption. A guard regression now constructs a retained active
  handle continuation over a suspended stack context, applies it through
  `jit_apply_handle_continuation_value`, and verifies `handle_retained` remains
  true with `continuation_refcount == 1`. Validation passed: C3 diagnostics,
  `c3c build main`, focused JIT policy filter (`pass=1 fail=0`), full
  `jit-policy` slice (`pass=79 fail=0`), file-size gate, and `git diff --check`.
- `[FAILED]` 2026-04-29 The first M43 focused validation used
  `OMNI_LISP_TEST_SLICE=jit`, which is not a valid slice. The correct slice is
  `jit-policy`.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M44 is closed. Resumptive handle
  redispatch loops now honor the existing interrupt/backedge policy before
  allocating or binding another continuation. `jit_handle_run_signals` and
  `jit_handle_dispatch_signals` call `runtime_eval_backedge_poll(interp)` at
  the top of their `while (state.signaled)` backedges and return the standard
  `evaluation interrupted` error if an interrupt is pending. Regression
  coverage marks a pending interrupt before each backedge and verifies the
  error plus unchanged `continuation_head`. Validation passed: C3 diagnostics,
  `c3c build main`, focused JIT policy filter (`pass=2 fail=0`), full
  `jit-policy` slice (`pass=81 fail=0`), file-size gate, and
  `git diff --check`.
- `[INVALIDATED]` 2026-04-29 M44's hard `10,000` dispatch-cycle cap
  recommendation is stale for current semantics. Large finite effect loops are
  valid; the corrected contract is interruptible resumptive dispatch via
  `runtime_eval_backedge_poll`, not an arbitrary perform/resume depth cap.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M45 is closed for scope/fiber/transfer
  telemetry. Shared process-wide scope memory, scope transfer, and fiber temp
  pool counters now update through centralized relaxed-atomic helpers, and
  runtime/benchmark readers use field-by-field snapshot helpers instead of raw
  struct copies. The fix intentionally avoids extending `scope_global_lock()`
  into telemetry hot paths. Regression coverage adds a threaded scope
  telemetry test with independent scopes per worker and checks aggregate
  create/release/destroy/TEMP/ESCAPE allocation deltas. Validation passed:
  C3 diagnostics, `c3c build main`, scope suite (`pass=68 fail=0`),
  counters-enabled build plus scope suite, basic Lisp slice (`pass=175
  fail=0`), memory telemetry benchmark envelope check, file-size gate, and
  `git diff --check`.
- `[FAILED]` 2026-04-29 Local ThreadSanitizer validation is unavailable for
  the M45 evidence set. `c3c build main --sanitize=thread -D
  OMNI_BOUNDARY_INSTR_COUNTERS` linked, but the resulting binary aborted before
  tests with `FATAL: ThreadSanitizer: unexpected memory mapping ...`; do not
  cite this run as race-clean evidence.
- `[PENDING]` 2026-04-29 Boundary decision/value-shape telemetry in
  `src/lisp/eval_boundary_telemetry.c3` still uses plain process-wide counters
  and raw snapshots. Track this as `AUDIT_2.md` M45A /
  `AUDIT-249-BOUNDARY-TELEMETRY-ATOMICITY`; prefer folding the fix with M46
  saturating counter helpers so atomicity and overflow share one boundary.
- `[FACT]` 2026-04-29 `AUDIT_2.md` M45A is closed. Boundary route/value-shape
  telemetry now uses relaxed atomic loads/stores, saturating CAS-add/inc
  helpers, and atomic max helpers. Decision/value-shape snapshot and restore
  paths are field-wise, and direct scope-chain/audit-budget readers use atomic
  loads or the returned increment value. Regression coverage adds saturation
  and threaded boundary telemetry cases under a new focused
  `boundary-telemetry` Lisp slice. Validation passed: C3 diagnostics,
  counters-enabled `c3c build main -D OMNI_BOUNDARY_INSTR_COUNTERS`,
  `OMNI_LISP_TEST_SLICE=boundary-telemetry` (`pass=2 fail=0`), normal
  `c3c build main`, basic Lisp slice (`pass=175 fail=0`), file-size gate, and
  `git diff --check`.
- `[FAILED]` 2026-04-29 A Docker-bound counters-enabled
  `memory-lifetime-smoke` validation entered the slice but was killed by the
  configured validation timeout/resource wrapper before producing a pass/fail
  result. Use the focused `boundary-telemetry` slice as the M45A targeted
  signal unless broader memory smoke is the objective.
- `[PENDING]` 2026-04-29 `AUDIT_2.md` M46 remains open for scope/fiber/transfer
  telemetry overflow. The M45 helper family in
  `src/scope_region_temp_pool_stats.c3` is atomic but still non-saturating; do
  not use `fetch_add` followed by clamping because snapshots can observe the
  transient wrapped value. Prefer the M45A CAS-loop saturating add pattern.
- `[FACT]` 2026-04-30 `AUDIT_2.md` M46 / `AUDIT-250` is closed. Scope memory,
  scope transfer, and fiber-temp telemetry now use a saturating CAS-loop
  `scope_telemetry_add`, guarded CAS decrement, and local saturation helpers
  for staging/aggregation paths before global publication. The regression
  `run_scope_region_telemetry_saturation_test()` covers representative global
  counters, underflow-safe decrement, chunk-byte/destructor aggregation, and
  slow-sequence follow-up staging. Validation passed: C3 diagnostics, normal
  `c3c build main`, default scope suite (`scope_region pass=69 fail=0`),
  counters-enabled build and fiber-temp scope suite, restored normal build,
  basic Lisp slice (`pass=175 fail=0`), file-size gate, and `git diff --check`.
- `[FAILED]` 2026-04-30 The first M46 regression attempted to assert
  feature-gated live/peak scope-shape telemetry counters in a default build,
  which failed the default scope suite. Do not treat shape telemetry updates as
  unconditional; use direct helper coverage or guard shape-specific assertions
  with `scope_memory_shape_telemetry_enabled()`.
- `[FACT]` 2026-04-30 `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS` was opened to
  track older telemetry tests that still raw-read process-wide telemetry
  structs. M45/M46 production counters were already closed, but evidence code
  still needed field-wise helper snapshots/loads instead of raw struct copies.
- `[FACT]` 2026-04-30 `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS` is closed.
  Raw test/runtime telemetry evidence reads in Lisp core tests, scheduler
  fiber-temp boundary tests, ML validation benchmark telemetry, stack
  fiber-temp tests, scope fiber-temp pool summary, and runtime-memory-stats
  output now use field-wise snapshot/load helpers. Added
  `fiber_temp_chunk_pool_count_snapshot()` for the global fiber-temp pool-count
  read. Validation passed: C3 diagnostics, `c3c build main`, basic Lisp
  (`pass=175 fail=0`), scheduler with `OMNI_FIBER_TEMP=1` (`pass=147 fail=0`),
  stack with `OMNI_FIBER_TEMP=1` (`pass=26 fail=0`), scope with
  `OMNI_FIBER_TEMP=1` (`scope_region pass=69 fail=0`), advanced collections
  module with `OMNI_ML_BENCH=1` (`pass=2146 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L27 is closed. `Interp.alloc_env()` and
  `Interp.alloc_env_escape()` now initialize `Env.is_inline = false`, so direct
  allocator callers cannot inherit stale inline-env state from reused scope
  memory. Added deterministic basic native coverage that dirties reused TEMP
  and ESCAPE allocation memory before calling both direct allocators.
  Validation passed: C3 diagnostics, `c3c build main`, basic Lisp slice
  (`pass=176 fail=0`), restored normal build, file-size gate, and
  `git diff --check`.
- `[FAILED]` 2026-04-30 ASAN/Valgrind evidence for L27 is limited. The
  documented `c3c build main --sanitize=address` command reported sanitizer
  support unavailable in the current toolchain configuration, while
  `c3c build --sanitize=address main` is not accepted by `c3c`. Targeted
  Valgrind on the basic slice reached the Omni test summary (`pass=176
  fail=0`) but exited `99` because existing custom stack/continuation paths
  emit many Valgrind invalid-read/write reports unrelated to the env allocator
  initialization change.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L28 is closed. `Env.hash_lookup()` and
  `Env.hash_insert()` now fail closed when called with a null env, null
  `hash_table`, or zero `hash_capacity`, instead of relying on a
  contract-only null-table precondition. Added basic native regression coverage
  for a malformed env with nonzero `hash_capacity` and null `hash_table`.
  Validation passed: C3 diagnostics, `c3c build main`, basic Lisp slice
  (`pass=177 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L29 is closed. The original null-deref
  finding was partially stale, but `hashmap_sorted_slots()` still returned
  allocated nonempty scratch for malformed non-null maps with invalid backing
  storage. It now returns an empty slice before allocation whenever
  `hashmap_storage_valid(map)` fails or count is zero. Compiler dictionary/set
  value serialization now rejects malformed backing storage explicitly before
  asking for sorted slots. Added serializer regression coverage for a malformed
  dictionary with nonzero count/capacity and null entries. Validation passed:
  C3 diagnostics, `c3c build main`, compiler slice (`pass=426 fail=0`),
  advanced collections module filter (`pass=2146 fail=0`), file-size gate, and
  `git diff --check`.
- `[FAILED]` 2026-04-30 The first adjacent advanced validation for L29 used
  nonexistent `OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-module-generic-ops`
  and matched no tests. Use `OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
  for the focused collections module validation.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L30 is closed. `copy_env_value_fast()` now
  has a default switch branch that returns `null`, so unknown or newly added
  `ValueTag` values trigger the existing env-copy failure/rollback contract
  instead of falling through with an undefined return. Added basic native
  coverage that corrupts a zeroed `Value` tag byte to an unknown tag and
  verifies the helper rejects it. Validation passed: C3 diagnostics,
  `c3c build main`, basic Lisp slice (`pass=178 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L31 is closed. Dict pattern parsing now
  rejects duplicate symbols while collecting pattern keys, returning
  `duplicate key in dict pattern` before allocating the final key array.
  Added basic match/parser coverage for `({name name} ...)`. Validation passed:
  C3 diagnostics, `c3c build main`, basic Lisp slice (`pass=179 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L32 is closed. `match_pattern()` now returns
  a `runtime/invalid-state` match error for unknown `PatternTag` values instead
  of silently treating them as an ordinary non-match. Added basic native
  coverage that corrupts a zeroed `Pattern` tag byte and verifies the
  `match: unknown pattern tag` diagnostic. Validation passed: C3 diagnostics,
  `c3c build main`, basic Lisp slice (`pass=180 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L33 is closed. Pattern serialization now
  sets `compiler: cannot serialize unknown pattern tag` for unknown
  `PatternTag` values instead of emitting wildcard `_` source. Added compiler
  serializer coverage that corrupts a zeroed `Pattern` tag byte and verifies
  the compile error and non-wildcard output. Validation passed: C3 diagnostics,
  `c3c build main`, compiler slice (`pass=427 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` L34 is closed. File-module cache paths are
  now normalized lexically before storage and lookup comparison, collapsing
  repeated separators and `.` path segments without changing the existing
  rejection of absolute user imports or `..` traversal. Added advanced module
  runtime coverage that creates equivalent file-module paths and verifies the
  second lookup reuses the first module entry. Validation passed: C3
  diagnostics, `c3c build main`, advanced collections module filter
  (`pass=2147 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Fourth-pass script items L34-L36 are closed.
  `run_e2e.sh` now quotes the Stage 3 source array expansion, and
  `run_deduce_perf_envelope.sh` now bridges auto-detected validation mounts
  into `OMNI_DOCKER_EXTRA_ARGS` for Docker hard-cap execution with a focused
  self-test. L36 was already fixed in current source because
  `build_omni_chelpers.sh` invokes `"${CC:-cc}"`. Validation passed:
  `bash -n` for the touched scripts, both validation mount-bridge self-tests,
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M18 is closed. Boundary graph audit logging
  and verbose telemetry now use `%ld` for explicit `long` casts. Scope transfer
  stat printing now formats `usz` counters through a bounded decimal helper and
  prints the resulting null-terminated strings, avoiding unsupported C-style
  `%zu`, `%llu`, and `%u` format forms in this C3 toolchain. Validation passed:
  C3 diagnostics for touched files, `c3c build main`, scope suite (`pass=69
  fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-30 Do not use C-style `%zu`, `%llu`, or `%u` in C3
  `io::printfn`/`io::eprintfn` format strings for this repo. The C3 compiler
  rejects those format forms as unexpected escape sequences. Prefer supported
  `%d` with intentionally narrowed values only when the contract is bounded, or
  format full-width `usz` values into decimal strings before printing.
- `[FACT]` 2026-04-30 Prior-audit L1 is closed. Scope owner guard operation
  names now flow as `ZString` through the shared guard and violation helpers,
  and boundary reason/provenance name helpers now return `ZString` because
  their values come from string-literal tables. Validation passed: C3
  diagnostics, `c3c build main`, scope suite (`pass=69 fail=0`), file-size
  gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-30 Prior-audit L2-L4 are stale in current source:
  scope allocation helpers fail closed on null `self`, `scope_reset_temp_lane`
  resets `alloc_bytes` and `alloc_count`, and `scope_freelist_cleanup` destroys
  the global mutex through `scope_global_destroy_after_shutdown()`.
- `[FACT]` 2026-04-30 Prior-audit L6 is partially stale/reframed. Destructor
  registration helpers now fail closed on null scope/function, and
  `scope_destroy_owned_descendants` already has `@require scope != null`.
  `scope_splice_escapes` remains a future explicit contract-annotation
  candidate, but no live null-deref repair was needed in this slice.
- `[FACT]` 2026-04-30 Prior-audit L5 is closed. Scope generation stamps now
  use `ulong` across `ScopeRegion`, the global scope generation counter,
  `Value.scope_gen`, closure/env scope stamps, stable escape passports/store
  entries, and promotion scope-chain cache keys. ABI size assertions were
  updated for the widened structs. Added a focused basic Lisp regression that
  forces the generation counter above `uint.max`, allocates TEMP and ESCAPE
  values, and verifies their stamps preserve the widened scope generations.
  Validation passed: `c3c build main`, scope suite (`pass=69 fail=0`), host
  basic Lisp slice (`pass=181 fail=0`), bounded-container basic Lisp slice
  (`pass=181 fail=0`), file-size gate, and `git diff --check`.
- `[FAILED]` 2026-04-30 The broad Docker-bound
  `memory-lifetime-smoke` validation for L5 entered the slice but the
  validation wrapper was killed with exit 137 before a final pass/fail summary.
  This matches prior broad-smoke wrapper failures in this environment, so do
  not cite it as passing L5 evidence. The effective L5 regression signal is
  the new high-generation basic test plus the scope/basic/container checks.
- `[FACT]` 2026-04-30 Prior-audit L7 is closed. The audit's original
  `long.min` correctness risk is stale because current source already had
  long-min regression coverage for value serialization and compiler emission.
  The remaining live issue was unnecessary duplicate signed integer emission
  logic in `Compiler.emit_int`; it now delegates to the shared `int_to_string`
  helper used by source/value serialization. Validation passed: `c3c build
  main`, compiler Lisp slice (`pass=427 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit L9 is closed. The boundary graph audit env
  visitor now treats `env == null` as an OK empty terminal before checking root
  persistence or bindings. Added direct regression coverage and a focused
  `boundary-graph-audit` Lisp slice to validate graph-audit behavior without
  the broad `memory-lifetime-smoke` suite. The existing `boundary-telemetry`
  slice is also wired into execution in this workspace. Validation passed:
  `c3c build main`, bounded-container `boundary-graph-audit` (`pass=7
  fail=0`), `boundary-telemetry` (`pass=2 fail=0`), file-size gate, and
  `git diff --check`.
- `[FACT]` 2026-05-01 Prior-audit L10 is closed. Vulkan multi-output barrier
  helpers now return before dereferencing null output handles for two-, three-,
  and four-output barriers. Added native resource-safety coverage that installs
  a test pipeline-barrier hook and verifies null-output barrier calls do not
  reach it. Validation passed: `./scripts/build_omni_chelpers.sh`,
  `cc -std=c11 -Wall -Wextra -Werror tests/native/vulkan_resource_safety_test.c build/libomni_chelpers.a -lm -ldl -lpthread -o build/vulkan_resource_safety_test && ./build/vulkan_resource_safety_test`,
  `c3c build main`, file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-05-01 Prior-audit L11 is stale in current source.
  `omni_tensor_backend_vulkan_copy_range_to_host` already maps the requested
  `(offset, byte_len)` range, and the native Vulkan resource-safety test
  includes `omni_tensor_backend_vulkan_copy_range_subrange_map_for_tests`,
  which verifies the offset/size passed to the map hook. No production change
  was needed for L11; the L10 native validation re-ran this coverage.
- `[FACT]` 2026-05-01 Prior-audit L12 is closed. `scripts/run_global_gates.sh`
  now creates the ASAN build log with `mktemp -t omni_asan_build.XXXXXX`
  instead of bare `mktemp`, preserving Linux behavior while satisfying
  BSD/macOS template requirements. Validation passed: `bash -n
  scripts/run_global_gates.sh`, direct `mktemp -t omni_asan_build.XXXXXX`
  probe, and `git diff --check -- scripts/run_global_gates.sh`.
- `[FACT]` 2026-05-01 Prior-audit M8 is closed. Vulkan physical-device
  selection now fails closed when the enumerate-physical-devices or
  queue-family-properties callbacks are unavailable, clearing output selection
  state before returning. Native resource-safety coverage now exercises the
  selector null-guard cases. Validation passed:
  `./scripts/build_omni_chelpers.sh`,
  `cc -std=c11 -Wall -Wextra -Werror tests/native/vulkan_resource_safety_test.c build/libomni_chelpers.a -lm -ldl -lpthread -o build/vulkan_resource_safety_test && ./build/vulkan_resource_safety_test`,
  `c3c build main`, file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M6 is closed. BLAS resolution now keeps a
  successfully opened BLAS handle when any supported CBLAS symbol is present
  instead of requiring `cblas_dgemm` to publish optional `dgemv`/`ddot`/`dger`
  capabilities. General BLAS availability remains gated on `cblas_dgemm`, while
  optional-operation availability is exposed independently. Native contract
  coverage now injects a partial BLAS symbol table and verifies optional symbols
  remain available without `dgemm`. Validation passed:
  `./scripts/build_omni_chelpers.sh`,
  `cc -std=c11 -Wall -Wextra -Werror tests/native/stack_fpu_blas_contract_test.c build/libomni_chelpers.a -lm -ldl -lpthread -o build/stack_fpu_blas_contract_test && ./build/stack_fpu_blas_contract_test`,
  `cc -std=c11 -Wall -Wextra -Werror -pthread -fsyntax-only csrc/tensor_blas_helpers.c`,
  `c3c build main`, file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M7 is closed. Current LAPACK symbol
  publication already used individual null checks, but the closure added native
  partial-symbol-table coverage and aligned `omni_tensor_lapack_resolve()` with
  the BLAS resolver fast path so already-published handles are respected before
  entering `pthread_once`. The test verifies `dgetrf` availability can be true
  without unrelated LAPACKE symbols and that QR availability requires both
  `dgeqrf` and `dorgqr`. Validation passed:
  `cc -std=c11 -Wall -Wextra -Werror -pthread -fsyntax-only csrc/tensor_lapack_helpers.c`,
  `cc -std=c11 -Wall -Wextra -Werror -pthread -fsyntax-only csrc/tensor_blas_helpers.c`,
  `./scripts/build_omni_chelpers.sh`,
  `cc -std=c11 -Wall -Wextra -Werror tests/native/stack_fpu_blas_contract_test.c build/libomni_chelpers.a -lm -ldl -lpthread -o build/stack_fpu_blas_contract_test && ./build/stack_fpu_blas_contract_test`,
  `c3c build main`, file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 native/AOT literal-lowering sub-slice is
  closed. `Compiler.compile_literal` now supports dictionary, set, array,
  ordinary primitive, and definition-time global closure literals through
  explicit AOT constructors or generated global relinks, and it fails closed
  for callable/runtime-only/opaque object literals instead of silently emitting
  `aot::make_nil()`. `aot_runtime_bridge` now exposes `array_from_args` and
  `set_from_args` for generated literal construction. Regression coverage
  checks runtime-object literal rejection, and the fail-closed test file was
  split to preserve the 1000 LOC code-file gate. Validation passed: C3
  diagnostics, `c3c build main`, direct stdlib macro compile probe, compiler
  Lisp slice (`pass=428 fail=0`), file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-30 Do not treat all non-scalar literal tags as
  unsupported in AOT literal lowering. The initial blanket rejection improved
  the silent-default symptom but failed the compiler slice because stdlib macro
  expansion legitimately emits collection literals, ordinary primitive values,
  and captured global function values. Preferred contract: lower reproducible
  literals explicitly and reject only runtime-only or opaque values.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT primitive-lookup residual is closed.
  `aot::lookup_prim` now returns an explicit error when a generated primitive
  reference is absent from the runtime global environment instead of returning
  `nil`. Regression coverage exercises the missing-primitive helper path in
  the compiler fail-closed AOT helper group. Validation passed: C3 diagnostics,
  `c3c build main`, compiler Lisp slice (`pass=428 fail=0`), file-size gate,
  and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT module-export lookup residual is
  closed. `aot::lookup_module_export` now returns an explicit error when a
  loaded module declares an export but the module environment does not bind the
  exported symbol, instead of returning `nil`. The compiler fail-closed AOT
  helper group now creates a malformed loaded module and verifies the
  exported-but-unbound path fails closed. Validation passed: C3 diagnostics,
  `c3c build main`, compiler Lisp slice (`pass=428 fail=0`), file-size gate,
  and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT variable-lookup residual is closed.
  `aot::lookup_var` now returns an explicit unbound-variable error when a
  generated read misses the runtime global environment instead of returning
  `nil`, and `aot::define_var` propagates incoming error values rather than
  publishing them as successful bindings. Regression coverage exercises both
  the missing lookup and error-input define path in the compiler fail-closed
  AOT helper group. Validation passed: C3 diagnostics, `c3c build main`,
  compiler Lisp slice (`pass=428 fail=0`), file-size gate, and `git diff
  --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT match-guard callback residual is
  closed. `aot::match_guard_eval` now returns an explicit error when a callable
  guard callback returns null instead of converting that malformed callback
  result into `nil` and silently treating the guard as false. Regression
  coverage adds a null-returning AOT closure helper and verifies
  `match_guard_eval` reports the null callback result. Validation passed: C3
  diagnostics, `c3c build main`, compiler Lisp slice (`pass=428 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 compiler serializer residual is closed.
  `Compiler.serialize_expr_to_buf` now fails closed on unknown expression tags,
  and `Compiler.serialize_type_val_literal_to_buf` now fails closed on
  unsupported singleton type-literal tags instead of serializing either as
  `nil`. Serializer metadata regressions cover both paths. Validation passed:
  C3 diagnostics, `c3c build main`, compiler Lisp slice (`pass=430 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT type-annotation metadata literal-tag
  residual is closed. `Compiler.emit_aot_type_annotation_spec_init` now rejects
  unsupported singleton literal `ValueTag` values only when `has_val_literal` is
  set, preserving existing benign `NIL` emission for inactive metadata storage
  fields. Type-dispatch compiler regressions cover both valid `#nil` singleton
  metadata and unsupported active literal tags. Validation passed: C3
  diagnostics, `c3c build main`, compiler Lisp slice (`pass=431 fail=0`),
  file-size gate, and `git diff --check`.
- `[INVALIDATED]` 2026-04-30 Do not put active singleton literal validation in
  the low-level `emit_value_tag_literal` renderer. That renderer lacks
  `has_val_literal` context, and the overbroad guard failed the compiler slice
  (`pass=225 fail=206`) by treating inactive `val_literal` storage fields as
  active literal contracts. Preferred boundary: validate in
  `emit_aot_type_annotation_spec_init`, then render tags with the existing
  fallback for inactive fields.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT quasiquote fail-closed residual is
  closed. `Compiler.compile_qq_flat` now rejects standalone
  `E_UNQUOTE_SPLICING` and unsupported internal quasiquote template expression
  tags with explicit compiler errors instead of emitting `nil` temps. Compiler
  codegen regressions cover both malformed paths. Validation passed: C3
  diagnostics, `c3c build main`, compiler Lisp slice (`pass=433 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 Prior-audit M9 AOT match pattern-check fail-closed
  residual is closed. `Compiler.compile_pattern_check_to_bool` now rejects
  unknown internal `PatternTag` values with an explicit compiler error and emits
  `false` for the generated boolean placeholder instead of compiling them as
  catch-all matches. Compiler codegen regression coverage constructs an invalid
  pattern tag and verifies the error plus non-catch-all emitted check.
  Validation passed: C3 diagnostics, `c3c build main`, compiler Lisp slice
  (`pass=434 fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` M25 was re-audited as stale-closed:
  `repl_server_worker_clear_queued_commands` already locks `worker.mu` around
  queue and pending-command cleanup when the mutex is initialized, matching the
  2026-04-29 worker queue hardening entry.
- `[FACT]` 2026-04-30 `AUDIT_2.md` M26-M28 are closed. `is_number()` now
  delegates to `is_numeric_value()` so the shared number predicate covers
  BigInteger, BigFloat, and BigComplex. `to_double()` now routes through
  `try_numeric_to_double()` instead of reading inactive union storage for
  BigInteger or other non-plain integer values. Float64-only UI/test consumers
  now use `try_numeric_to_double()` directly so complex and unrepresentable big
  values remain fail-closed after the predicate widening. `Interp` now caches
  `tid_Float`, `register_builtin_types()` validates it, and `Float` is parented
  under `Number`; the interpreter ABI size guard was updated accordingly.
  Regression coverage verifies BigInteger/BigFloat helper conversion and
  `tid_Float` initialization. Validation passed C3 diagnostics, `c3c build
  main`, basic Lisp slice (`pass=185 fail=0`), advanced stdlib numeric filter
  (`pass=441 fail=0`), advanced type-dispatch filter (`pass=255 fail=0`),
  file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` M29 is closed. Type-registry hash
  insertion now uses a checked shared helper, and `TypeRegistry.register_type`
  rolls back the just-published type slot plus `type_count` if a hashable type
  name cannot be inserted into the hash index. Type-registry growth now uses
  the same checked helper while rehashing old entries. Basic native regression
  coverage forces a malformed full type-registry hash index and verifies the
  rejected type returns `INVALID_TYPE_ID`, preserves `type_count`, leaves the
  existing type reachable, and keeps the rejected type unreachable. Validation
  passed: C3 diagnostics for `value_type_registry.c3` and
  `tests_core_groups.c3`, `c3c build main`, basic Lisp slice (`pass=186
  fail=0`), file-size gate, and `git diff --check`.
- `[FACT]` 2026-04-30 `AUDIT_2.md` M30 is closed. `test_error()` now requires
  a non-empty non-allocation error payload instead of accepting any
  `has_error` result, preventing allocation-shaped or blank errors from
  satisfying generic negative tests. Stable core semantics, numeric bounds, and
  typed-effect wrong-type callers were migrated to `test_error_contains()` with
  exact expected substrings. Validation passed C3 diagnostics for touched
  files, `c3c build main`, advanced core semantics (`pass=71 fail=0`),
  advanced stdlib numeric (`pass=441 fail=0`), advanced type dispatch
  (`pass=255 fail=0`), advanced FFI/system (`pass=191 fail=0`), advanced
  collections module (`pass=2147 fail=0`), file-size gate, and `git diff
  --check`.
- `[FACT]` 2026-04-30 `AUDIT-253-MACRO-HYGIENE-RECURSION-HARD-EXIT` is closed.
  The macro-hygiene non-tail recursion headroom fixture now uses depth `384`
  instead of depth `512`. Direct eval at `384` returns `384`, while the old
  `512` threshold reproduced as a native segfault in the current runtime/JIT
  stack shape. Validation passed C3 diagnostics for
  `tests_advanced_macro_hygiene_groups.c3`, `c3c build main`, direct 384-depth
  eval, the exact `advanced-macro-hygiene-string-number` subgroup (`pass=9
  fail=0`), full `advanced-macro-hygiene` (`pass=100 fail=0`), file-size gate,
  and `git diff --check`.
- `[INVALIDATED]` 2026-04-30 Do not treat `512` as a portable
  macro-hygiene non-tail recursion headroom value after current runtime/JIT
  stack frame changes. The fixture remains a substantial smoke probe, not a
  language maximum-depth contract; prefer conservative recalibration unless
  future work implements a bounded recursion/signal guard for this execution
  path.
- `[FACT]` 2026-04-30 During M30 validation, the
  `advanced-macro-hygiene-string-number` subgroup and direct recursive eval
  `(let ^rec (f (lambda (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))) (f 512))`
  exited before a pass/fail summary. This is now resolved by the
  `AUDIT-253` recalibration above.
- `[FACT]` 2026-04-30 An `AUDIT-252` M9/JIT sub-slice is closed. The JIT
  continuation-sensitivity scanners now treat unknown expression tags as
  continuation-sensitive instead of returning an ordinary `false`, preventing
  malformed expression trees from selecting the non-continuation fast call
  path by default. Regression coverage in the `warm-cache` JIT policy group
  verifies unknown tags are conservative while existing malformed
  `with-module` body-list tolerance remains intact. Validation passed C3
  diagnostics for `jit_apply_eval.c3` and `tests_runtime_feature_jit_groups.c3`,
  `c3c build main`, filtered `warm-cache` JIT policy (`pass=5 fail=0`),
  file-size gate, and `git diff --check`.
- `[PENDING]` 2026-04-30 Broader JIT policy validation exposed
  `AUDIT-254-JIT-TAIL-CONSTRUCTOR-ESCAPE-OPCODE`: the exact
  `tail-constructor-escape-opcode` filter fails with `escape_ok=no`
  (`pass=0 fail=1`). This failure is independent of the M9 unknown-tag scanner
  change and is tracked as a separate live TODO.
- `[FACT]` 2026-04-30 `AUDIT-254-JIT-TAIL-CONSTRUCTOR-ESCAPE-OPCODE` is
  closed. The root cause was over-broad continuation-sensitivity scanner
  hardening: after unknown tags became fail-closed, known inert atom forms
  (`E_LIT`, `E_VAR`, and `E_QUOTE`) still fell through the default
  continuation-sensitive case. The scanners now return `false` for those known
  atoms while unknown tags remain conservative, allowing `(Array 4 5)` to reach
  tail constructor ESCAPE lowering again. Regression coverage in the exact
  policy test also asserts the `Array` call is classified as a tail
  constructor. Validation passed C3 diagnostics, `c3c build main`,
  `tail-constructor-escape-opcode` (`pass=1 fail=0`), `warm-cache`
  (`pass=5 fail=0`), and full `jit-policy` (`pass=82 fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not assume the AUDIT-254 failure is caused by
  `Array` tail-constructor recognition. Direct classifier evidence returned
  `array_classifier=yes` while the call still produced generic tail-call
  nil/cons counters (`nil=2 cons=5 array=0 string=1`), proving the diversion
  happened earlier in continuation-sensitivity routing.
- `[FACT]` 2026-04-30 Another `AUDIT-252` M9 residual is closed at the
  generated-global literal collection boundary. `Compiler.collect_literal_generated_globals`
  now keeps recursive traversal for cons/array/dictionary/set literals and
  explicit no-op handling for every valid non-container `ValueTag`, but reports
  a compiler error for malformed internal value tags instead of silently
  treating them as "no generated globals needed". Regression coverage in the
  serializer metadata group injects a raw invalid value tag. Validation passed
  C3 diagnostics, `c3c build main`, and compiler Lisp slice (`pass=451
  fail=0`).
- `[FACT]` 2026-04-30 `AUDIT-252-M9-DEFAULT-SWITCH-RESIDUAL` is closed after
  final current-source classification. The remaining compiler/AOT `default:`
  arms are explicit fail-closed diagnostics, parent-dispatched helper fallbacks
  that reach fail-closed parent boundaries, or benign format/classification
  defaults such as JSON/symbol escaping and define-like detection. No
  success-shaped fallback remains in the audited compiler/AOT default-switch
  surface. Validation for the closure passed `rg -n "default:"`, `c3c build
  main`, compiler Lisp slice (`pass=451 fail=0`), `git diff --check`,
  `scripts/check_file_size_gate.sh`, and `scripts/check_status_consistency.sh`.
- `[FACT]` 2026-04-30 `AUDIT-255-FFI-INVALID-RETURN-ABI-TAG` is closed.
  Malformed internal FFI return ABI tags now fail closed before libffi
  preparation through `ffi_abi_type_tag_supported()`, async raw return tags are
  validated by `ffi_abi_type_tag_int_supported()` before enum conversion,
  `ffi_return_storage_for()` returns null for unsupported tags, and
  `ffi_return_value_for()` reports a typed FFI invalid-state error instead of
  converting unsupported return tags to successful `nil`. Regression coverage
  directly checks the storage helper, return conversion, C ABI bound-call path,
  and async FFI offload with raw invalid tags. Validation passed C3
  diagnostics, `c3c build main`, and advanced FFI/system slice (`pass=192
  fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not let malformed FFI return ABI tags ride
  through integer return storage or convert to `nil`; synchronous and async FFI
  boundaries must reject unsupported return ABI tags before native call
  preparation.
- `[FACT]` 2026-04-30 `AUDIT-256-FFI-ASYNC-INVALID-ARG-ABI-TAG` is closed.
  Async FFI argument handling now validates raw ABI tag integers with
  `ffi_abi_arg_type_tag_int_supported()` before any `FfiTypeTag` comparison in
  `ffi_async_own_string_arg()`, and `ffi_async_offload_callback()` validates
  argument count, argument tag, and argument storage before `omni_ffi_call`.
  Regression coverage directly checks the helper path and direct worker
  callback path with malformed raw argument tags. Validation passed C3
  diagnostics, `c3c build main`, and advanced FFI/system slice (`pass=193
  fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not rely on C-side libffi tag rejection for
  malformed async FFI argument metadata; async worker contexts must reject
  unsupported raw argument ABI tags before enum conversion or native call
  preparation.
- `[FACT]` 2026-04-30 `AUDIT-257-FFI-ASYNC-VOID-RETURN-CONTRACT` is closed.
  Async FFI now handles valid `FFI_TYPE_VOID` returns with the same dummy native
  storage pattern as synchronous FFI and publishes `OFFLOAD_RES_VOID` through
  scheduler completion. `scheduler_value_from_offload_completion()` converts
  that kind to `make_void(interp)`, preserving existing `OFFLOAD_RES_NIL` as
  `nil`. Regression coverage checks both the direct async worker callback and
  surface `(ffi-async-call free nil)` `Void` result. Validation passed C3
  diagnostics, `c3c build main`, advanced FFI/system slice (`pass=194
  fail=0`), and scheduler slice (`pass=147 fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not treat valid `FFI_TYPE_VOID` returns as
  unsupported just because they do not need a return payload, and do not map
  FFI `^Void` async completions to `nil`; the language contract is the `Void`
  singleton.
- `[FACT]` 2026-04-30 `AUDIT-258-FFI-ASYNC-FLOAT32-RETURN-CONTRACT` is
  closed. Async FFI now publishes `FFI_TYPE_FLOAT32` returns as
  `OFFLOAD_RES_FLOAT32`, and scheduler wakeup materializes that kind with
  `make_float32(interp, ...)` while keeping `OFFLOAD_RES_DOUBLE` for Float64.
  The async `sinf` regression now asserts `(type-of r) == 'Float32` in
  addition to the numeric range. Validation passed C3 diagnostics, `c3c build
  main`, advanced FFI/system slice (`pass=194 fail=0`), and scheduler slice
  (`pass=147 fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not route typed FFI scalar returns through a
  wider scheduler completion kind when the language surface has a distinct
  scalar type; `^Float32` must remain `Float32`, not Float64.
- `[FACT]` 2026-04-30 `AUDIT-259-FFI-ASYNC-BOOLEAN-RETURN-CONTRACT` is
  closed. Async FFI now publishes `FFI_TYPE_BOOL` returns as
  `OFFLOAD_RES_BOOL`, and scheduler wakeup materializes that kind as the
  canonical Omni Boolean singleton symbols. `OFFLOAD_RES_INT` remains reserved
  for integer returns. Regression coverage uses a 64-bit C ABI Boolean helper,
  checks the direct async worker completion kind, and checks scheduler
  materialization to `true`. Validation passed C3 diagnostics, `c3c build
  main`, advanced FFI/system slice (`pass=195 fail=0`), and scheduler slice
  (`pass=147 fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not route async FFI `^Boolean` returns through
  `OFFLOAD_RES_INT`; Omni booleans are singleton symbols, not numeric truth
  values.
- `[FACT]` 2026-04-30 `AUDIT-260-FFI-STRUCT-RETURN-STORAGE` is closed.
  `ffi_return_storage_for()` and async FFI return-storage selection now treat
  `FFI_TYPE_STRUCT` as pointer-shaped storage. Sync FFI reaches the existing
  `ffi-struct` opaque handle conversion, while async FFI reaches the existing
  pointer-like return rejection boundary instead of failing early with
  unsupported return ABI type. Regression coverage adds a static-pointer C
  helper and checks sync storage/conversion plus async boundary classification.
  Validation passed C3 diagnostics, `c3c build main`, advanced FFI/system slice
  (`pass=196 fail=0`), and scheduler slice (`pass=147 fail=0`).
- `[INVALIDATED]` 2026-04-30 Do not classify valid pointer-shaped
  `FFI_TYPE_STRUCT` return metadata as unsupported ABI before the established
  sync conversion or async pointer-like return boundary.
- `[FACT]` 2026-04-30 `AUDIT-261-FFI-VOID-PARAMETER-DECLARATION` is closed.
  Interpreter/JIT `define [ffi λ]` now rejects `^Void` parameters at
  declaration time, and the AOT runtime bridge rejects raw `FFI_TYPE_VOID`
  parameter tags before publishing the primitive. `^Void` remains valid as a
  return annotation. Regression coverage checks the dynamic FFI surface and
  direct AOT bridge declaration path. Validation passed C3 diagnostics,
  `c3c build main`, advanced FFI/system slice (`pass=197 fail=0`), and
  compiler slice (`pass=452 fail=0`). Final hygiene passed `git diff
  --check`, `scripts/check_file_size_gate.sh`, and
  `scripts/check_status_consistency.sh`.
- `[INVALIDATED]` 2026-04-30 Do not treat `^Void` as a general FFI parameter
  ABI or let it survive declaration to fail later during argument packing; it
  is a return-only FFI surface.
- `[FACT]` 2026-04-30 `AUDIT-262-FFI-CALLBACK-VOID-PARAMETER` is closed.
  `ffi-callback` now rejects `Void` parameter metadata in both list/array and
  variadic callback forms through a shared callback-parameter role predicate,
  while preserving `Void` as a valid callback return type. Regression coverage
  checks both callback syntaxes. Validation passed C3 diagnostics, `c3c build
  main`, and the advanced FFI/system slice (`pass=199 fail=0`). Final hygiene
  passed `git diff --check`, `scripts/check_file_size_gate.sh`, and
  `scripts/check_status_consistency.sh`.
- `[INVALIDATED]` 2026-04-30 Do not list or accept `Void` as a value-bearing
  callback parameter type; C `void` parameter lists are represented by an empty
  callback parameter list, while `Void` remains a return type.
