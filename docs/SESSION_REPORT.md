# Session Report Archive

This index points to the sequential parts that preserve the full historical report content in order.

- [Part 1](archive/session_report/part_00.md) (650 lines)
- [Part 2](archive/session_report/part_01.md) (649 lines)
- [Part 3](archive/session_report/part_02.md) (650 lines)
- [Part 4](archive/session_report/part_03.md) (650 lines)
- [Part 5](archive/session_report/part_04.md) (650 lines)
- [Part 6](archive/session_report/part_05.md) (650 lines)
- [Part 7](archive/session_report/part_06.md) (650 lines)
- [Part 8](archive/session_report/part_07.md) (650 lines)
- [Part 9](archive/session_report/part_08.md) (650 lines)
- [Part 10](archive/session_report/part_09.md) (650 lines)
- [Part 11](archive/session_report/part_10.md) (650 lines)
- [Part 12](archive/session_report/part_11.md) (650 lines)
- [Part 13](archive/session_report/part_12.md) (650 lines)
- [Part 14](archive/session_report/part_13.md) (650 lines)
- [Part 15](archive/session_report/part_14.md) (650 lines)
- [Part 16](archive/session_report/part_15.md) (650 lines)
- [Part 17](archive/session_report/part_16.md) (650 lines)
- [Part 18](archive/session_report/part_17.md) (650 lines)
- [Part 19](archive/session_report/part_18.md) (375 lines)

## Recent Checkpoints

### 2026-04-28T17:01:46+02:00 - AUDIT-243 Mutable Capture Residuals

Objective attempted: continue multi-agent audit of the AOT mutable-cell
lowering path after AUDIT-242 and close confirmed regressions without changing
public semantics.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: recursive mutable `let ^rec` now allocates
the cell before initializer lowering and seeds it through `mutable_cell_set`;
`set!` lowering now mirrors read-side module precedence so module/private/open
bindings beat older outer mutable cells. Hardened generated-e2e error
diagnostic slicing, made resolve terminal-control tests ordered, and tightened
an AUDIT-242 shape assertion.

Commands run:
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- C3 LSP diagnostics for touched compiler/test files

Key results: build passed; compiler slice passed with `399 fail=0`; generated
e2e passed with `ALL 423 e2e compiler tests passed!`; targeted diagnostics
reported no C3 issues.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not box lambda
parameters or synthetic effect wrapper bodies merely because AOT implements them
as lambdas. Direct parity probes showed JIT does not mutate the outer parameter
or outer `checkpoint` binding for those shapes.

Current best recommendation: next work should address
`AUDIT-244-AOT-MUTABLE-CELL-ROOT-LIFETIME` through memory-boundary promotion or
an explicit root-scope execution policy for AOT closures invoked from
JIT/interpreter scopes.

Unresolved issues: `AUDIT-244-AOT-MUTABLE-CELL-ROOT-LIFETIME` is open. It was
not patched in this pass because existing promotion helpers require boundary
scope provenance and a raw pointer-retention fix would violate the ownership
model.

Next actions: design the mutable-cell child-scope store policy, add a runtime
test where an AOT mutable closure is invoked from a JIT child scope, and run
bounded memory validation.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler changes; local `build/main` and `build/e2e_test`
were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T18:50:00+02:00 - AUDIT-246 Generated-Owned AOT Closure API Sub-Slice

Objective attempted: continue `AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME` by
making generated AOT closure ownership explicit before the primitive
finalizer/copy-promotion rewrite.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: generated captured closures now call
`make_generated_closure_with_retention` or
`make_generated_variadic_closure_with_retention` instead of passing a trailing
`true` ownership boolean to the lower-level constructors. Manual
`make_closure` / `make_variadic_closure` remain caller-owned.

Commands run:
- C3 LSP diagnostics for touched runtime/compiler/test files.
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_VALIDATION_TIMEOUT_SEC=600 scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `git diff --check`

Key results: build passed; compiler slice passed with `402 pass, 0 fail`;
bounded memory-lifetime smoke passed with `287 pass, 0 fail`; generated e2e
passed all 423 cases; whitespace check passed.

Current best recommendation: continue AUDIT-246 with primitive user-data
copy/finalizer hooks and generated-AOT-only primitive copy/promotion support.

Unresolved issues: generated AOT closure primitives are still root-owned and
still release retention from root closure-data teardown.

Next actions: implement scoped generated closure payload ownership while
preserving opaque-user-data refusal for ordinary and manual caller-owned
primitives.

Dependencies, blockers, or restart requirements: rebuild/restart required for
running processes to pick up runtime/compiler changes; `build/main` and
`build/e2e_test` were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T18:30:00+02:00 - AUDIT-246 AOT Closure Primitive Lifetime Sub-Slice

Objective attempted: continue `AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME` by
closing the capture-retention graph-audit failure seam before the broader
generated closure wrapper lifetime redesign.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: `capture_value_reaches_scope` now treats
only `REACHABLE_TEMP_*` graph-audit results as retention triggers; audit
allocation, overflow, or forced internal failures now return an AOT capture
error. A forced fail-closed regression was added to the compiler slice.

Commands run:
- C3 LSP diagnostics for touched runtime/test files.
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_VALIDATION_TIMEOUT_SEC=600 scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `git diff --check`

Key results: build passed; compiler slice passed with `402 pass, 0 fail`;
bounded memory-lifetime smoke passed with `287 pass, 0 fail`; whitespace check
passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` graph-audit
infrastructure failures are not successful retention signals and must not be
silently converted into scope retention.

Current best recommendation: continue AUDIT-246 with the scoped generated AOT
closure wrapper/payload redesign, including explicit copy/promotion support for
returned closures.

Unresolved issues: generated AOT closure payload retention still releases from
root closure-data teardown, so retained temp scopes can remain pinned until
`aot_shutdown`.

Next actions: implement a JIT-closure-like generated AOT closure ownership
boundary while preserving manual caller-owned `make_closure` payload
compatibility.

Dependencies, blockers, or restart requirements: rebuild/restart required for
running processes to pick up runtime changes; `build/main` was rebuilt during
validation.

Signature: Codex GPT-5

### 2026-04-28T18:10:26+02:00 - AUDIT-245 AOT Closure Capture Root Lifetime

Objective attempted: close the immediate immutable AOT closure capture
root-lifetime regression without clone-on-capture semantics drift.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: `AotCaptureRetention` now lets generated
AOT closures retain temp source scopes when immutable captures reach the current
scope chain; generated closure factories take retention ownership and release it
from closure-data teardown; all-mutable generated closure captures skip
retention allocation; mixed immutable/mutable captures guard payload writes
after retention failure. Tests now cover child-scope and ancestor-scope capture
survival, closure factory destructor-registration failure, and generated
code-shape expectations.

Commands run:
- C3 LSP diagnostics for touched runtime/compiler/test files.
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_VALIDATION_TIMEOUT_SEC=600 scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `git diff --check`

Key results: build passed; compiler slice passed with `402 pass, 0 fail`;
bounded memory-lifetime smoke passed with `287 pass, 0 fail`; generated e2e
passed all 423 cases; whitespace check passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` blind immutable
clone-on-capture is not the closure-lifetime fix; it regresses lexical
shadowing/capture shape and can break shared mutable value identity.
`[INVALIDATED]` all-mutable generated closure captures do not need retention
allocation because mutable cells are root-owned after `AUDIT-244`.

Current best recommendation: continue with
`AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME`, which should replace the remaining
root-owned AOT closure primitive/payload lifetime with bounded closure-value
teardown.

Unresolved issues: AOT closure capture retention releases from root closure-data
teardown today, so retained temp scopes can remain pinned until `aot_shutdown`.

Next actions: design and implement generated AOT closure value/payload teardown
that releases retention before shutdown while preserving manual caller-owned
payload compatibility.

Dependencies, blockers, or restart requirements: rebuild/restart required for
running processes to load the runtime/compiler changes; `build/main` and
`build/e2e_test` were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T00:00:00+02:00 - AUDIT-244 AOT Mutable Cell Root Lifetime

Objective attempted: close the AOT mutable-cell root-lifetime regression while
preserving current closure invocation semantics.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: `AotMutableCell` constructor seeding and
updates now promote retained values through root-store promotion; generated
mutable-capture initialization and `set!` lowering use checked cell APIs; AOT
runtime and memory-lifetime tests cover child-scope cons/string graphs stored
through root-owned cells. `AUDIT-244` was closed and `AUDIT-245` opened for
the separate immutable AOT closure payload capture policy.

Commands run:
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_VALIDATION_TIMEOUT_SEC=600 scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `scripts/check_status_consistency.sh`
- `git diff --check`
- C3 LSP diagnostics for touched runtime/compiler/test files.

Key results: build passed; compiler slice passed with `401 pass, 0 fail`;
bounded memory-lifetime smoke passed with `285 pass, 0 fail`; generated e2e
passed all 423 cases; status consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` running every AOT
closure under root scope is not the mutable-cell fix; store-time promotion is
the narrower boundary. `[INVALIDATED]` blind immutable clone-on-capture
regressed existing shadowed capture behavior and must be replaced with a
semantics-preserving `AUDIT-245` policy.

Current best recommendation: continue with `AUDIT-245`, starting with the
closure payload retention policy before touching capture emission again.

Unresolved issues: root-lived AOT closure payload structs still store
non-mutable `Value*` captures directly.

Next actions: design and validate immutable AOT closure capture retention across
child-scope arguments, trampoline direct-call paths, capture identity, and
promotion failure signaling.

Dependencies, blockers, or restart requirements: rebuild/restart required for
running processes to load the runtime/compiler changes; `build/main` was
rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T16:19:37+02:00 - AUDIT-242 Mutable Capture Binding Identity

Objective attempted: close the AOT mutable-capture regression where mutable
captured `let`/`set!` lowering was keyed by source symbol names instead of
lexical binding identity.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added AOT mutable cells, checked
environment `set!` fallback, active mutable-cell alias stacks, per-capture
`LambdaDef.capture_is_mutable` metadata, and scanner support for lexical
capture mutability. Rewired lambda capture layout, closure capture emission,
lambda-body reads/writes, mutable `let` lowering, and `set!` lowering to use
the indexed cell/value capture contract. Added AUDIT-242 compiler shape
regressions for mutable-cell lowering, lexical leak prevention, same-name
mutable and immutable capture identity, and unbound lambda `set!`.

Commands run:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_e2e_baseline_policy.sh`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `git diff --check`

Key results: build passed; e2e generation emitted 423 rows; compiler slice
passed with `397 fail=0`; generated e2e passed with
`ALL 423 e2e compiler tests passed!`; baseline policy and whitespace checks
passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Source
`SymbolId` membership in `Compiler.mutable_captures` remains non-authoritative
for binding identity. Per-capture lambda mutability and active lexical
mutable-cell aliases are now the authoritative generated-code boundary.

Current best recommendation: keep ordinary AOT lexical lets as generated
`Value*` aliases and use `AotMutableCell` only for the captured mutable lexical
binding being lowered. Do not add new source-name membership checks around
`mutable_captures`.

Unresolved issues: no open AUDIT-242 item remains. Broader high-memory or
memory-lifetime validation was not run because this slice touched compiler
lowering, not runtime ownership teardown.

Next actions: run `scripts/check_status_consistency.sh` before selecting a new
audit target from `TODO.md` or a fresh failing validation signal.

Dependencies, blockers, or restart requirements: rebuild/re-run generated e2e
for processes to pick up compiler changes; local `build/main` and
`build/e2e_test` were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T15:28:07+02:00 - Module Alias Precedence And Mutable Capture Audit

Objective attempted: continue multi-agent regression auditing of AOT lexical
alias/module binding behavior and close safe defects while preserving
interpreter/JIT semantics.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added module-binding lexical alias marks so
active module-private/open bindings beat older outer lexical aliases while
inner lexical aliases inside `with` still beat imported module bindings. Added
generated e2e rows and baseline-policy sentinels for module `define`, module
`set!`, module type-descriptor export, post-`with` outer lexical restoration,
and inner lexical shadowing inside `with`; generated e2e count is now 423.
Opened `AUDIT-242-AOT-MUTABLE-CAPTURE-BINDINGS` because the remaining mutable
captured `let` path is source-name-keyed and needs a binding-identity rewrite.

Commands run:
- `c3c build`
- `scripts/check_e2e_baseline_policy.sh`
- `git diff --check`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- targeted interpreter/AOT probes for module define, module set, module type,
  and inner-let `with` collisions

Key results: build passed; e2e baseline policy passed; whitespace check passed;
generated e2e passed with `ALL 423 e2e compiler tests passed!`. Subagent Hegel
confirmed the module/lexical precedence ordering and test gaps; subagent
Avicenna confirmed that mutable-capture lowering is still keyed by source name
and must not be fixed by another source-name patch.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not treat
source `SymbolId` membership in `mutable_captures` as a lexical binding
authority. It can leak `let` bindings through `aot::define_var("name", ...)`,
clobber same-name nested mutable captures, and hide unbound lambda `set!`
errors. The correct repair boundary spans mutable-capture prescan,
free-variable discovery, lambda capture layout, and `set!` lowering.

Current best recommendation: continue with
`AUDIT-242-AOT-MUTABLE-CAPTURE-BINDINGS` and implement a binding-identity or
mutable-cell model for AOT captured mutable lets before adding more mutable
capture e2e rows.

Unresolved issues: `AUDIT-242-AOT-MUTABLE-CAPTURE-BINDINGS` remains open.

Next actions: implement the mutable capture binding model, then validate with
targeted repros for lexical leak, same-name nested mutable capture, and unbound
lambda `set!`, followed by compiler slice and generated e2e.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up the compiler changes; local `build/main` and generated
e2e binaries were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T14:18:34+02:00 - Primitive Shadow And Module Metadata Audit

Objective attempted: continue multi-agent regression auditing on AOT primitive
shadowing, inline-module metadata, and generated e2e parity while preserving
existing interpreter/JIT behavior.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added an active primitive-shadow stack so
generated locals and lambda parameters can shadow primitive names without
poisoning later C scopes; kept `true`/`false` branch folding out of lexical
shadowing contexts; replaced one existing generated e2e row with a
boolean-shadowing block/if case while preserving 410 rows. Inline module
metadata no longer treats lambda bodies as immediate module locals, now visits
match guard predicates for helper-published locals, reserves metadata registry
slots before recursive body scans, supports nested executable inline-module
`export-from` sources, and fails closed for runtime-dependent exports plus
reset/handle body export publication.

Commands run:
- `c3c build`
- C3 LSP diagnostics for the touched compiler/test files
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `scripts/check_e2e_baseline_policy.sh --self-test-review-rule`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_e2e_baseline_policy.sh`
- `git diff --check`
- `scripts/check_status_consistency.sh`

Key results: build passed; compiler slice passed with `381 passed, 0 failed`;
advanced type-dispatch mutation-chain passed with `252 passed, 0 failed`;
advanced effect-continuation passed with `66 passed, 0 failed`; basic slice
passed with `173 passed, 0 failed`; generated e2e passed all 410 compiler tests;
policy, whitespace, and status-consistency checks passed. Direct AOT repros for
`let` and lambda parameter shadowing of `false` now print `1`; nested inline
`export-from` compiles; conditional and checkpoint inline module export repros
fail closed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not use
`declared_vars` to decide whether a primitive name is actively shadowed. It is
a redeclaration guard and persists beyond C block scopes, which caused later
unshadowed `false` references to emit `_omni_false` without an in-scope
declaration. `[INVALIDATED]` Do not delay incrementing `compiled_module_count`
until after body scanning when inline module metadata can recurse; nested
metadata reused the parent slot and triggered double-free/segfault behavior.

Current best recommendation: keep primitive shadowing explicit and stack-based.
Treat runtime-dependent and synthetic-wrapper inline module export publication
as fail-closed until there is a real definite-assignment and module-context
model.

Unresolved issues: AOT still does not support publishing inline module exports
from synthetic reset/handle wrapper bodies; this is intentionally fail-closed.

Next actions: none for this slice unless product requirements demand dynamic
inline module export definite-assignment analysis or reset/handle wrapper module
contexts.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler and generated e2e corpus changes; local binaries
were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T13:52:48+02:00 - E2E Quiet Re-Export Lexical Audit

Objective attempted: continue multi-agent regression auditing on generated e2e
contracts, AOT inline-module re-export metadata, literal-branch inline module
parity, and lexical effect publication leaks while preserving interpreter/JIT
semantics.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: kept the generated e2e count summary
visible in quiet mode, narrowed stale skip-count policy detection, added AOT
inline `export-from` re-export metadata, failed closed for unsupported runtime
module export enumeration, aligned literal-constant branch metadata with
generated lowering, and expanded lexical `[effect]` fail-closed handling for
inline-module nested lexical bodies, `shift`, match guards, and handler clause
bodies. Added compiler regressions for these contracts.

Commands run:
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_e2e_baseline_policy.sh --self-test-review-rule`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_e2e_baseline_policy.sh`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `git diff --check`
- `scripts/check_status_consistency.sh`

Key results: build passed; compiler slice passed with `378 passed, 0 failed`;
advanced type-dispatch mutation-chain passed with `252 passed, 0 failed`;
advanced effect-continuation passed with `66 passed, 0 failed`; basic slice
passed with `173 passed, 0 failed`; generated e2e passed all 410 compiler tests;
policy, whitespace, and status-consistency checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not treat
runtime module export enumeration as available during AOT inline-module metadata
collection. Runtime `export-from` and module-body `import 'all` now fail closed
until there is a compile-time export inventory. `[INVALIDATED]` Do not assume
all effect-bearing bodies are lexical; interpreter probes showed direct module
bodies, block/top-level bodies, `reset`/checkpoint bodies, and handle bodies
publish effects non-lexically, while inline nested lexical bodies, `shift`,
match guards, and handler clause bodies must reject lexical `[effect]`
publication in AOT.

Current best recommendation: keep AOT inline-module metadata conservative around
runtime export enumeration, but preserve legal helper collection and direct
effect-publication surfaces. Literal-constant branch selection should remain
shared between metadata discovery and lowering.

Unresolved issues: dynamic duplicate inline module names in runtime-dependent
branches remain fail-closed/path-insensitive by design; only literal-dead branch
parity was closed in this slice.

Next actions: none for this slice unless the product chooses to support
compile-time export inventories for runtime-loaded modules.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler and e2e generator changes; `c3c build` and the
e2e script rebuilt the local binaries during validation.

Signature: Codex GPT-5

### 2026-04-28T13:35:00+02:00 - E2E Policy And Inline Module Parity Audit

Objective attempted: continue multi-agent regression auditing on generated e2e
harness policy and AOT inline-module/helper-publication parity while preserving
existing runtime behavior.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: strengthened `scripts/run_e2e.sh` Stage 4
failure handling so nonzero generated test-binary exits print the captured
`build/e2e_actual.txt` path and head/tail diagnostics; added static generated
e2e case-table policy checks for exactly 410 rows, unique case names, unique
case expressions, and stale skip-comment language; removed stale skip comments
and duplicate generated e2e rows while preserving 410 cases. Inline module
metadata now records its source AST node and fails closed on duplicate module
names, discovers private locals inside module-body `with` and `import` forms,
and syncs module-body imports back into active private backings. AOT
generated-global collection now continues collecting legal helper globals in
lexical bodies while failing closed for lexical `[effect]` declarations that
would otherwise leak through the global helper env.

Commands run:
- C3 LSP diagnostics for touched compiler/runtime/test C3 files
- `c3c build`
- `bash -n scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_e2e_baseline_policy.sh --self-test-review-rule`
- `scripts/check_e2e_baseline_policy.sh`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e`
- `scripts/run_e2e.sh`
- `git diff --check`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `372 passed, 0 failed`;
advanced type/dispatch filtered slice passed with `252 passed, 0 failed`;
advanced effect-continuation filtered slice passed with `66 passed, 0 failed`;
basic slice passed with `173 passed, 0 failed`. Generated e2e reported
`Generated 410 e2e tests`, and `scripts/run_e2e.sh` reported `ALL 410 e2e
compiler tests passed`. E2E baseline policy, Stage 3 source parity,
review-rule self-test, whitespace, and status consistency checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` The e2e runner
string check alone is not enough policy authority for the 410-case contract;
the case tables must be counted and de-duplicated directly. `[INVALIDATED]`
Duplicate inline module names cannot be keyed by bare name and silently share
the first private backing table. `[INVALIDATED]` Skipping all generated-global
collection under lexical bodies is not a valid fix for effect leakage because
it suppresses legal FFI/type helper globals. The current safe contract is to
collect legal helpers and fail closed only for lexical `[effect]` publication
until AOT has a lexical helper-env model.

Current best recommendation: keep generated e2e policy tied to the source case
tables and keep inline module private backing decisions source-aware. For AOT
helper publication, prefer shared helper sync boundaries; lexical effect
declarations must remain fail-closed unless a real lexical helper-env runtime
model is implemented.

Unresolved issues: lexical `[effect]` declarations are intentionally
unsupported in AOT and now fail closed instead of leaking. This is not an open
correctness blocker for the current compiler contract, but future AOT support
would require a lexical helper-env design rather than global helper sync.

Next actions: continue auditing only against concrete failing contracts. No
new TODO-backed blocker was opened by this slice.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler/runtime changes; `build/main` and generated e2e
artifacts were rebuilt/regenerated during validation.

Signature: Codex GPT-5

### 2026-04-28T11:15:49+02:00 - Generated E2E FFI and AOT Effect Closure

Objective attempted: continue generated-code parity auditing into e2e binary
coverage, stdout value printing, and AOT effect handler semantics.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added a declarative FFI `strlen` setup/case
to `src/lisp/tests_e2e_generation_setups.c3` and
`src/lisp/tests_e2e_generation_cases_extended.c3`; fixed stdout byte-slice
printing in `src/lisp/value_print_buf.c3` and `src/lisp/value_print.c3`; fixed
generated AOT handler payload destructuring in
`src/lisp/compiler_lambda_scan_effect_wrappers.c3`; fixed abortive AOT handler
return semantics in `src/lisp/jit_runtime_effects_handle.c3` while preserving
pending `raise` dispatch precedence; added focused AOT effect parity
helpers/tests in `src/lisp/tests_compiler_core_groups.c3` and
`src/lisp/tests_compiler_core_groups_aot_runtime.c3`; added generated e2e rows
for direct handler payload return, payload-based resolve, and pending raise;
updated `memory/changelog_parts/changelog_part_38.md`.

Commands run:
- C3 LSP diagnostics for touched source/test files
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/run_validation_container.sh scripts/run_e2e.sh`

Key results: build passed. Compiler slice passed with `363 passed, 0 failed`;
default basic Lisp passed with `173 passed, 0 failed`; bounded Docker e2e
passed with `ALL 410 e2e compiler tests passed!`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` String-level
compiler assertions and manual temp binaries were not enough to prove generated
runtime parity. The bounded e2e binary gate exposed stdout string rendering and
AOT effect handler issues that the string assertions missed.

Current best recommendation: keep generated-code regressions represented in the
e2e binary corpus when they depend on emitted C3 buildability or runtime
interaction, and keep small AOT runtime parity tests for the underlying bridge
contract.

Unresolved issues: none in this slice. Full all-slice/high-memory validation
was not run; broad memory suites remain container-only.

Next actions: continue auditing generated-code parity or run broader
Docker-bound validation if the owner wants a global gate.

Dependencies, blockers, or restart requirements: rebuild required for compiler
and runtime changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T11:38:19+02:00 - FFI Helper-Boundary Direct-Result Sync

Objective attempted: consolidate the direct-result FFI declaration binding
fixes at the shared helper lowering boundary.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added
`emit_ffi_direct_result_binding_sync` in
`src/lisp/compiler_temp_ffi_forms.c3` and invoked it from
`compile_ffi_lib_direct` and `compile_ffi_fn_direct` immediately after AOT FFI
declaration calls. Removed caller-specific FFI assignment logic from
`src/lisp/compiler_temp_control_flow.c3`,
`src/lisp/compiler_native_match_compilation_flat_style.c3`,
`src/lisp/compiler_program_top_level.c3`, and
`src/lisp/compiler_tail_position_compilation_tco.c3`. Added a
`let`-body FFI regression in
`src/lisp/tests_compiler_core_groups_existing_feature.c3`. Updated
`memory/changelog_parts/changelog_part_38.md`,
`docs/areas/ffi-foreign-runtime.md`, and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for touched compiler/test files
- `git diff --check`
- `c3c build`
- generated-code probes for top-level, module-private, tail-block, and let-body
  FFI declarations
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp let-body FFI source> -o <temp output>` followed by running the temp binary with `LD_LIBRARY_PATH=/usr/local/lib`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `361 passed, 0 failed`;
generated-code probes showed helper-emitted assignments for generated globals
and module-private backings; the generated let-body FFI binary built and ran
successfully; focused advanced type/dispatch passed with `252 passed, 0 failed`;
default basic Lisp passed with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Direct-result FFI
binding should not be maintained through caller-specific patches in block,
branch, match, module, or top-level lowering. The FFI helper lowering boundary
is the durable publication point.

Current best recommendation: keep declaration publication boundaries attached
to the helpers that know whether a declaration returns a bindable value or
publishes through the runtime binding table.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T11:27:41+02:00 - Match Clause FFI Direct-Result Binding

Objective attempted: continue the FFI direct-result binding audit into native
match clause result lowering.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_native_match_compilation_flat_style.c3` so match clause
results that are `[ffi lib]` or `[ffi λ]` declarations use
`compile_block_child_to_temp`, preserving generated binding assignments before
the match result temp is set. Added a compiler regression in
`src/lisp/tests_compiler_core_groups_existing_feature.c3`. Updated
`memory/changelog_parts/changelog_part_38.md`,
`docs/areas/ffi-foreign-runtime.md`, and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for
  `src/lisp/compiler_native_match_compilation_flat_style.c3` and
  `src/lisp/tests_compiler_core_groups_existing_feature.c3`
- `git diff --check`
- `c3c build`
- generated-code probe for match-clause FFI declaration
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp match FFI source> -o <temp output>` followed by running the temp binary with `LD_LIBRARY_PATH=/usr/local/lib`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `360 passed, 0 failed`;
the generated-code probe showed `matchc = _r...` after the FFI declaration;
the generated match-FFI binary built and ran successfully; focused advanced
type/dispatch passed with `252 passed, 0 failed`; default basic Lisp passed
with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Native match
clause result lowering cannot compile direct-result FFI declarations as plain
expressions; it must preserve the declaration binding side effect.

Current best recommendation: keep extending direct-result declaration handling
through shared helper gates rather than adding case-specific assignments at
each call site.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T11:18:03+02:00 - Branch FFI Direct-Result Binding

Objective attempted: continue the FFI direct-result binding audit from tail
block children into branch assignment lowering.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_temp_control_flow.c3` so `compile_branch_assign` routes
`[ffi lib]` and `[ffi λ]` branch expressions through
`compile_block_child_to_temp`, preserving the generated binding assignment.
Added a compiler regression in
`src/lisp/tests_compiler_core_groups_existing_feature.c3`. Updated
`memory/changelog_parts/changelog_part_38.md`,
`docs/areas/ffi-foreign-runtime.md`, and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_temp_control_flow.c3` and
  `src/lisp/tests_compiler_core_groups_existing_feature.c3`
- `git diff --check`
- `c3c build`
- generated-code probe for direct `if` branch FFI declaration
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp branch FFI source> -o <temp output>` followed by running the temp binary with `LD_LIBRARY_PATH=/usr/local/lib`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `359 passed, 0 failed`;
the generated-code probe showed `branchc = _r...` after the FFI declaration;
the generated branch-FFI binary built and ran successfully; focused advanced
type/dispatch passed with `252 passed, 0 failed`; default basic Lisp passed
with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Branch lowering
cannot treat direct-result FFI declarations as ordinary branch expressions,
because the branch result alone does not update generated globals/module
backings.

Current best recommendation: keep `compiler_block_child_binds_direct_result`
as the shared gate for block and branch paths that need declaration binding
side effects.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T11:08:44+02:00 - Tail Block FFI Binding and Contract Const Fix

Objective attempted: continue the generated-code declaration audit into
tail-position block children and generated FFI binary buildability.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_temp_control_flow.c3` so final tail-position block children
that are `[ffi lib]` or `[ffi λ]` declarations still flow through
`compile_block_child_to_temp` and bind their direct AOT bridge result. Updated
`src/lisp/compiler_program_top_level_ffi_manifest.c3` so generated embedded FFI
contract JSON uses the C3-valid uppercase const name
`OMNI_FFI_CONTRACT_JSON`. Added/updated compiler regressions in
`src/lisp/tests_compiler_core_groups_existing_feature.c3`. Updated
`memory/changelog_parts/changelog_part_38.md`,
`docs/areas/ffi-foreign-runtime.md`, and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_temp_control_flow.c3`,
  `src/lisp/compiler_program_top_level_ffi_manifest.c3`, and
  `src/lisp/tests_compiler_core_groups_existing_feature.c3`
- `git diff --check`
- `c3c build`
- generated-code probe for final tail-position block FFI declarations
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp tail FFI source> -o <temp output>` followed by running the temp binary with `LD_LIBRARY_PATH=/usr/local/lib`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `358 passed, 0 failed`;
the generated-code probe showed `tailc = _r...` and `strlen = _r...` after the
FFI declarations; the generated FFI binary built and ran successfully; focused
advanced type/dispatch passed with `252 passed, 0 failed`; default basic Lisp
passed with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` The last child of
a tail-position block cannot always use tail lowering. Declaration forms that
return a bindable value, currently `[ffi lib]` and `[ffi λ]`, still require the
block-child binding side effect.

Current best recommendation: keep `compiler_block_child_binds_direct_result`
as the gate for declaration forms whose expression result must be copied into a
generated binding.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T10:47:36+02:00 - Nested Block Type-Form Global Sync

Objective attempted: continue the helper-published binding audit into nested
top-level blocks containing type forms.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: renamed the shared sync helper in
`src/lisp/compiler_temp_type_forms.c3` to
`emit_helper_published_binding_sync` and made it sync generated globals from
`aot::lookup_var(...)` when no active module private backing exists. Updated
type/effect helper call sites in `src/lisp/compiler_temp_type_forms_defs.c3`,
`src/lisp/compiler_temp_type_forms_defs_union.c3`,
`src/lisp/compiler_temp_type_forms_defs_abstract.c3`, and
`src/lisp/compiler_temp_type_forms_defs_misc.c3`. Removed the old
top-level-only `emit_type_form_global_sync` hook from
`src/lisp/compiler_program_top_level.c3` and
`src/lisp/compiler_program_top_level_globals.c3`. Added a compiler regression
in `src/lisp/tests_compiler_core_groups_type_dispatch.c3` for block-local
abstract, union, alias, and union constructor sync. Updated
`memory/changelog_parts/changelog_part_38.md`, `docs/areas/types-dispatch.md`,
and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for touched compiler helper/top-level/test files
- `git diff --check`
- `c3c build`
- generated-code probe for nested block type forms
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp block type-form source> -o <temp output>` followed by executing the temp binary
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `357 passed, 0 failed`;
the generated-code probe showed lookup syncs for `BlockShape`, `BlockMaybe`,
`BlockNone`, `BlockSome`, and `BlockAlias`; the generated-binary smoke built
and ran successfully; focused advanced type/dispatch passed with
`252 passed, 0 failed`; default basic Lisp passed with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` A top-level-only
post-pass is not a sufficient publication boundary for type forms. Nested
blocks need the same helper-published sync as direct top-level and module
private backing paths.

Current best recommendation: keep helper-published binding sync in
`emit_helper_published_binding_sync`, with module-private backing taking
precedence and generated global sync as the non-module path.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T10:31:12+02:00 - Block Effect Declaration Command-Result Guard

Objective attempted: continue the generated-code helper-published binding audit
into adjacent block sequencing paths.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: removed the generic block-child assignment
for `define [effect]` in `src/lisp/compiler_temp_control_flow.c3`, because the
effect declaration expression returns command-style `nil` while the usable
effect tag is published through the runtime binding table. Strengthened
`src/lisp/tests_compiler_core_groups_type_dispatch.c3` with non-module and
nested inline-module block regressions. Updated
`memory/changelog_parts/changelog_part_38.md`, `docs/areas/types-dispatch.md`,
and `.agents/PLAN.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_temp_control_flow.c3` and
  `src/lisp/tests_compiler_core_groups_type_dispatch.c3`
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build <temp block-effect source> -o <temp output>`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Compiler slice passed with `356 passed, 0 failed`;
the generated-binary smoke for `(block (define [effect] ...) effect-name)`
built successfully; focused advanced type/dispatch passed with
`252 passed, 0 failed`; default basic Lisp passed with `173 passed, 0 failed`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not treat
`define [effect]` like FFI declarations in block sequencing. FFI declarations
return the value to bind, but effect declarations publish a tag binding and
return `nil`.

Current best recommendation: keep effect binding publication at the
type/effect helper boundary and avoid generic command-result assignments for
effect declarations.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code parity or run Docker-bound e2e
for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T10:08:56+02:00 - Shared Module Helper-Published Binding Sync

Objective attempted: remove the direct-only module backing workaround and close
nested inline-module declaration parity for helper-published bindings.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added shared active module private-backing
sync in `src/lisp/compiler_temp_type_forms.c3` and wired it through direct
type/effect helper lowering in `src/lisp/compiler_temp_type_forms_defs.c3`,
`src/lisp/compiler_temp_type_forms_defs_union.c3`,
`src/lisp/compiler_temp_type_forms_defs_abstract.c3`, and
`src/lisp/compiler_temp_type_forms_defs_misc.c3`. Removed duplicate direct
module-body type/effect backing branches from
`src/lisp/compiler_tail_position_compilation_tco.c3`. Added nested inline-module
regressions in `src/lisp/tests_compiler_core_groups_type_dispatch.c3` for
effect and type-form declarations inside `if`/`block`. Updated
`memory/changelog_parts/changelog_part_38.md` and
`docs/areas/types-dispatch.md`.

Commands run:
- C3 LSP diagnostics for touched compiler helper/test files
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `355 passed, 0 failed`;
focused advanced type/dispatch slice passed with `252 passed, 0 failed`;
default basic Lisp slice passed with `173 passed, 0 failed`; status
consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` A direct
module-body special case is not the right boundary for helper-published
bindings. Nested declarations need the same sync, so the shared helper lowering
must publish into active module private backing when one exists.

Current best recommendation: keep helper-published binding sync in the helper
lowering layer, and keep module lowering focused on ordinary value-producing
forms.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code module parity or run
Docker-bound e2e for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T10:02:39+02:00 - Inline Module Effect Export Backing Sync

Objective attempted: continue the inline-module private backing audit into
effect declarations, which have the same helper-published binding shape as
non-constructor type descriptors.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_tail_position_compilation_tco.c3` so direct module-body
`[effect]` declarations populate the module private backing from
`aot::lookup_var(effect-name)` after `aot::define_effect`, rather than storing
the declaration expression's command-style return value. Added compiler
regression coverage in `src/lisp/tests_compiler_core_groups_type_dispatch.c3`.
Updated `memory/changelog_parts/changelog_part_38.md` and
`docs/areas/types-dispatch.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_tail_position_compilation_tco.c3`
  and `src/lisp/tests_compiler_core_groups_type_dispatch.c3`
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `354 passed, 0 failed`;
focused advanced type/dispatch slice passed with `252 passed, 0 failed`;
default basic Lisp slice passed with `173 passed, 0 failed`; status
consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not use the
command-style return value of `define [effect]` as the exported module binding.
The usable exported value is the effect tag symbol published by the helper.

Current best recommendation: for compiler helpers that publish a binding and
return a command/status value, module private backing sync should load the
published binding explicitly.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing nested/non-direct module declaration paths if
generated-code module parity remains the active focus.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T09:59:07+02:00 - Inline Module Type Descriptor Backings

Objective attempted: continue the generated-code descriptor audit into inline
module private backing/export handling.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_tail_position_compilation_tco.c3` so inline-compiled module
metadata reserves private backings for descriptor-producing type forms and
module lowering populates those backings from the globals published by direct
AOT type-form helpers. This covers `[type]`, `[abstract]`, `[union]` heads,
`[alias]`, and union variants. Added compiler regression coverage in
`src/lisp/tests_compiler_core_groups_type_dispatch.c3` for an inline module
exporting/importing an abstract descriptor, union head, alias descriptor, and
union constructor. Updated `memory/changelog_parts/changelog_part_38.md` and
`docs/areas/types-dispatch.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_tail_position_compilation_tco.c3`
  and `src/lisp/tests_compiler_core_groups_type_dispatch.c3`
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `353 passed, 0 failed`;
focused advanced type/dispatch slice passed with `252 passed, 0 failed`;
default basic Lisp slice passed with `173 passed, 0 failed`; status
consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: none beyond the prior AOT global
sync invalidation. This closes the same parity rule inside inline module
private backing tables.

Current best recommendation: keep module private backing metadata in lockstep
with every helper-published type-form symbol, not only ordinary `define`,
FFI/effect declarations, and union variants.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing generated-code type surfaces or run
Docker-bound e2e for broader generated-binary confidence.

Dependencies, blockers, or restart requirements: rebuild required for compiler
changes; the local `build/main` was rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T09:51:20+02:00 - AOT Type Descriptor Global Sync Parity

Objective attempted: close the generated-code parity gap adjacent to the
non-constructor type descriptor publication fix.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: updated
`src/lisp/compiler_program_top_level_globals.c3` so top-level generated-global
collection and post-type-form sync include `[abstract]` names, `[union]` heads,
and `[alias]` names in addition to concrete constructors and union variants.
Updated `src/lisp/tests_compiler_core_groups_type_dispatch.c3` so compiler
tests require `aot::lookup_var` syncs for `Entity`, `Maybe`, and `UserAlias`
after direct AOT helper lowering. Updated
`memory/changelog_parts/changelog_part_38.md` and
`docs/areas/types-dispatch.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/compiler_program_top_level_globals.c3` and
  `src/lisp/tests_compiler_core_groups_type_dispatch.c3`
- `git diff --check`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `352 passed, 0 failed`;
focused advanced type/dispatch slice passed with `252 passed, 0 failed`;
status consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not assume AOT
direct type-form helper lowering automatically makes newly published descriptor
values visible to generated code. The generated top-level global table must
collect and sync every helper-published global that later generated expressions
can reference.

Current best recommendation: keep interpreter and AOT direct helper behavior
paired: when an eval helper publishes a global, the compiler collector/syncer
must include that symbol for top-level generated code.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing adjacent generated-code type surfaces or run
Docker-bound e2e if broad binary parity confidence is required.

Dependencies, blockers, or restart requirements: rebuild required for generated
code and compiler changes; the local `build/main` was rebuilt during
validation.

Signature: Codex GPT-5

### 2026-04-28T09:43:08+02:00 - Non-Constructor Type Descriptor Publication

Objective attempted: continue the type/dispatch audit from the user-visible
descriptor contract and close missing value-position bindings for type forms
that do not have constructors.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: added shared root-owned `TYPE_INFO`
descriptor publication for `[abstract]`, `[union]` head, and `[alias]` type
definitions in `src/lisp/eval_type_evaluators.c3`. Removed an unused concrete
`[type]` descriptor allocation in `src/lisp/eval_type_declarations.c3` that
was never bound or returned. Added an explicit non-callable type-descriptor
apply error in `src/lisp/jit_apply_helpers.c3` and regression coverage in
`src/lisp/tests_advanced_type_parametric_groups.c3`. Updated
`memory/changelog_parts/changelog_part_38.md`,
`docs/areas/types-dispatch.md`, `docs/reference/04-type-system.md`, and
`docs/LANGUAGE_SPEC.part-02.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/eval_type_evaluators.c3`,
  `src/lisp/eval_type_declarations.c3`, `src/lisp/jit_apply_helpers.c3`, and
  `src/lisp/tests_advanced_type_parametric_groups.c3`
- `c3c build`
- direct repros for `[abstract]`, `[union]` head, and `[alias]` descriptor
  rendering plus abstract descriptor call rejection
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`
- `git diff --check`

Key results: build passed. Direct repros now render `#<type ShapeProbe>`,
`#<type MaybeProbe>`, and `#<type MyIntProbe>` instead of reporting unbound
variables; calling an abstract descriptor reports that it is a type descriptor,
not a function. Focused advanced type/dispatch validation passed with
`252 passed, 0 failed`; compiler slice passed with `352 passed, 0 failed`;
default basic Lisp slice passed with `173 passed, 0 failed`; status
consistency and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not assume
successful type registry insertion implies value-position descriptor
publication. `[abstract]`, `[union]` heads, and `[alias]` need explicit
root-owned descriptor binding because they do not publish constructor
primitives.

Current best recommendation: keep constructor-bearing concrete types bound to
their constructor primitives, and bind non-constructor type forms to `TYPE_INFO`
descriptor values with an explicit non-callable apply diagnostic.

Unresolved issues: bounded Docker e2e validation was not run in this pass.

Next actions: continue auditing adjacent type-definition surfaces, especially
AOT/runtime parity for descriptor publication if broader generated-binary
coverage is needed.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up the eval/apply changes; the local `build/main` was rebuilt
during validation.

Signature: Codex GPT-5

### 2026-04-28T09:26:17+02:00 - Literal Audit Status Date Normalization

Objective attempted: close the status-reporting regression exposed by the
literal singleton audit fixes.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: split the April 28 literal singleton audit
facts in `memory/changelog_parts/changelog_part_38.md` under their own
`2026-04-28` heading and refreshed the required area status `As of` headers in
`docs/areas/memory-runtime_parts/memory-runtime_part_01.md`,
`docs/areas/types-dispatch.md`, `docs/areas/ffi-foreign-runtime.md`, and
`docs/areas/validation-status.md`.

Commands run:
- `scripts/check_status_consistency.sh`
- `git diff --check`

Key results: status consistency now passes with latest changelog date
`2026-04-28`, TODO actionable count `0`, and all tracked area statuses green.
Whitespace validation passed.

Invalidated assumptions or failed approaches: none.

Current best recommendation: keep same-day audit facts under their real
calendar heading so freshness checks do not hide current work.

Unresolved issues: none for this status slice.

Next actions: continue the code audit from a concrete failing probe or
localized complexity signal.

Dependencies, blockers, or restart requirements: none; this was documentation
and status metadata only.

Signature: Codex GPT-5

### 2026-04-28T09:20:46+02:00 - Literal Shorthand Negative Sign Diagnostic

Objective attempted: continue adjacent parser audit on malformed literal
singleton shorthand.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: tightened
`src/lisp/parser_type_annotation_helpers.c3` so inline integer shorthand
`^#-` fails at the literal parser with an explicit digits-after-minus
diagnostic. Added regression coverage in
`src/lisp/tests_advanced_type_parametric_groups.c3` and updated
`memory/changelog_parts/changelog_part_38.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/parser_type_annotation_helpers.c3` and
  `src/lisp/tests_advanced_type_parametric_groups.c3`
- `git diff --check`
- `c3c build`
- direct repro with `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '((lambda ((^#- x)) x) 0)'`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Direct malformed shorthand now reports `Literal
shorthand integer annotation expects digits after '-'`. Focused advanced
type/dispatch validation passed with `250 passed, 0 failed`; compiler slice
passed with `352 passed, 0 failed`; default basic Lisp slice passed with
`173 passed, 0 failed`; `git diff --check` passed.

Invalidated assumptions or failed approaches: none beyond the already-recorded
literal shorthand parser exactness constraints.

Current best recommendation: keep malformed inline shorthand failures local to
the literal parser so parse errors identify the broken annotation rather than a
downstream parameter shape.

Unresolved issues: bounded end-to-end generation was not run in this pass; use
the Docker validation path if broad e2e confidence is required.

Next actions: continue broader audit from another concrete failing probe or
status signal; no open work remains for `^#-`.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up the parser diagnostic change; the local `build/main` was
rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T09:17:11+02:00 - Literal Descriptor Dynamic Name Rendering

Objective attempted: continue auditing the active literal singleton
type/dispatch surface for adjacent runtime regressions.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: replaced `(Literal datum)` descriptor-name
rendering in `src/lisp/primitives_meta_types.c3` with a `DString` builder
instead of returning a slice into a fixed helper-local `char[256]` buffer.
Added regression coverage in `src/lisp/tests_advanced_type_dispatch_groups.c3`
for a long string singleton descriptor that previously failed before interning.
Updated `memory/changelog_parts/changelog_part_38.md` and
`docs/areas/types-dispatch.md`.

Commands run:
- C3 LSP diagnostics for `src/lisp/primitives_meta_types.c3` and
  `src/lisp/tests_advanced_type_dispatch_groups.c3`
- `git diff --check`
- `c3c build`
- direct long-string repro with `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval ...`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. The direct long-string repro now returns `true`.
Focused advanced type/dispatch validation passed with `249 passed, 0 failed`;
compiler slice passed with `352 passed, 0 failed`; default basic Lisp slice
passed with `173 passed, 0 failed`; `git diff --check` passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not use fixed
small stack buffers as the authority for `(Literal datum)` descriptor names.
String singleton descriptors can exceed short diagnostic-buffer sizes and must
be assembled in dynamically sized storage before interning.

Current best recommendation: keep descriptor-name construction dynamic and
bounded by the symbol table's intern limits rather than by an incidental local
format buffer.

Unresolved issues: bounded end-to-end generation was not run in this pass; use
the Docker validation path if broad e2e confidence is required.

Next actions: continue adjacent auditing on literal singleton representation,
escaping, and parser/runtime parity.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up the primitive change; the local `build/main` was rebuilt
during validation.

Signature: Codex GPT-5

### 2026-04-28T12:50:00+02:00 - E2E Harness And Inline Module Isolation Audit

Objective attempted: continue the generated-code/runtime audit with three
parallel read-only agents and close the concrete defects they found without
regressing existing generated e2e or type/effect behavior.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: fixed `scripts/run_e2e.sh` Stage 2 so
generator diagnostics are printed even when `--gen-e2e` exits nonzero. Removed
dead `skip_count` plumbing and changed the generator contract to
`Generated 410 e2e tests`. Added baseline-policy guards against unmeasured skip
reporting and noncanonical `Integer`/`Boolean` shorthand rows. Replaced the
duplicate shorthand rows with real `Dict` shorthand coverage. Added AOT helpers
for push/pop of a temporary module env, made inline-module lowering run inside
that env with `defer` restoration, and emitted terminal `resolve` guards after
module body forms. Added generated-global collection/sync coverage for effect
declarations.

Commands run:
- `bash -n scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh`
- C3 LSP diagnostics for touched e2e, compiler, AOT bridge, and test files
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/run_e2e.sh`
- `git diff --check`
- `scripts/check_status_consistency.sh`
- `jj diff --stat`

Key results: build passed. Compiler slice passed with `367 passed, 0 failed`;
advanced type-dispatch slice passed with `252 passed, 0 failed`; advanced
effect-continuation slice passed with `66 passed, 0 failed`; basic slice passed
with `173 passed, 0 failed`. Generated e2e reported `Generated 410 e2e tests`,
and the bounded generated binary gate reported `ALL 410 e2e compiler tests
passed`. Baseline policy, status consistency, and whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` The generated e2e
`0 skipped` string was not measured state and should not be treated as a test
contract. `[INVALIDATED]` Inline module private C backings alone do not isolate
helper-published declarations; the helper side effect must run under a module
runtime env. `[INVALIDATED]` Inline module body lowering cannot skip terminal
guards just because module expressions normally evaluate to command-style
`Void`.

Current best recommendation: keep generated e2e count enforcement explicit but
avoid reporting unmeasured skipped state. Keep generated inline modules aligned
with interpreter module evaluation by entering a module env for the helper
declaration side effects and restoring it with `defer`.

Unresolved issues: no open blocker for this slice. A future runtime parity test
could exercise same-name unexported FFI declarations in two inline modules, but
the shared module-env lowering boundary is now in place and covered by
generated-code tests plus the bounded generated e2e gate.

Next actions: continue only if a new concrete failing contract is found; the
canonical TODO queue remains at zero actionable items.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler/AOT bridge changes; `build/main` and the
generated e2e binary were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T12:25:00+02:00 - Generated E2E Fail-Fast And Resolve Terminal Guards

Objective attempted: continue multi-agent audit/remediation on generated e2e,
AOT/JIT effect-handler semantics, and regressions introduced while hardening
handler `resolve` as terminal control.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: made generated e2e setup/output generation
fail-fast and made `scripts/run_e2e.sh` assert the exact 410-case/0-skipped
contract. Removed invalid positive e2e rows and added missing-else negative
coverage. Added `resolve_terminal_pending` and propagated terminal-aware guards
through JIT/AOT parent-work lowering, including calls, block/control flow,
let/set, index, match, effects, and quasiquote. Split terminal-control guards
from ordinary `ERROR` propagation so error-valued arguments and bindings remain
valid data where existing semantics allow them. Added compiler return-context
tracking so generated `return _rN` guards are emitted only from `Value*`
functions, not top-level `int main`.

Commands run:
- `c3c build`
- `bash -n scripts/run_e2e.sh`
- C3 LSP diagnostics for touched compiler/runtime/test files
- manual handler `resolve` probes for call sibling, `if` test, nested resolve,
  index operands, and quasiquote unquote
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e`
- `scripts/run_e2e.sh`
- `git diff --check`
- `scripts/check_status_consistency.sh`
- `jj status`

Key results: build passed. Compiler slice passed with `365 passed, 0 failed`;
advanced effect-continuation slice passed with `66 passed, 0 failed`; basic
slice passed with `173 passed, 0 failed`. Generated e2e reported
`Generated 410 e2e tests (0 skipped)`, and the bounded generated binary gate
reported `ALL 410 e2e compiler tests passed`. Status consistency and
whitespace checks passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` A combined
terminal/error sequence-stop predicate is not a valid shared contract. Ordinary
`ERROR` values can be first-class argument/binding data, while
`resolve_terminal_pending` is abortive handler-clause control. Also invalidated
emitting `return Value*` guards from every generated context; top-level
generated `main` returns `int` and needs different guard emission.

Current best recommendation: keep terminal-control guards separate from
ordinary error propagation. Use explicit `aot::is_error(...)`/JIT error checks
only at strict control-flow boundaries, and use terminal-only checks for parent
work that must stop after syntactic `resolve`.

Unresolved issues: none for this slice.

Next actions: continue auditing adjacent generated-code/runtime parity only by
opening a concrete TODO-backed item if a new failing contract is found.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up compiler/runtime changes; `build/main` was rebuilt during
validation. Generated e2e artifacts were regenerated during validation.

Signature: Codex GPT-5

### 2026-04-28T15:03:03+02:00 - Primitive Shadow And Let Scope AOT Audit

Objective attempted: continue multi-agent regression auditing on generated
AOT code while maintaining interpreter/JIT behavior.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: hardened inline-module export metadata to
fail closed for runtime-dependent match, short-circuit, checkpoint guard, and
handle clause export publication; fixed stale primitive-shadow state after
shadowing `false`; moved nested-lambda closure capture emission under the active
primitive-shadow context; added generated lexical backing aliases for `let`
locals so C3 declarations do not leak or shadow source names; restored
declaration state across branch and match-clause generated C scopes; expanded
generated e2e coverage and row-count policy to 414 cases.

Commands run:
- `c3c build`
- C3 LSP diagnostics for touched compiler/test files
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_QUIET=1 scripts/run_e2e.sh`
- `scripts/check_e2e_baseline_policy.sh --self-test-review-rule`
- `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- `scripts/check_e2e_baseline_policy.sh`
- targeted `/tmp` `--build` repros for branch boolean shadowing, nested closure
  boolean/callable capture, match sibling declarations, same-name `let`,
  same-name `let ^rec`, same-name closure capture, mutable same-name capture,
  and post-let lexical leak
- `git diff --check`
- `scripts/check_status_consistency.sh`

Key results: build passed. Compiler slice passed with `393 passed, 0 failed`.
Generated e2e passed with `ALL 414 e2e compiler tests passed!`. E2e baseline
policy, whitespace check, and status consistency passed. Targeted AOT binaries
returned the expected values for branch shadowing (`2`), closure captures (`9`
and `88`), match sibling declarations (`2`), same-name `let` (`1`), same-name
recursive `let` (`99`), same-name closure capture (`3`), and mutable same-name
capture (`1`). The former post-let stale local leak no longer prints `1`.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not treat C
block braces alone as sufficient for lexical `let` shadowing in generated C3.
C3 rejects same-name local shadow declarations, so lexical lets need generated
backing aliases rather than source-name locals. `[INVALIDATED]` Do not let
possible inline-module export bindings inside runtime-dependent match,
short-circuit, checkpoint guard, or handle clause paths create public export
backings; they must fail closed until definite-assignment lowering exists.

Current best recommendation: keep lexical source names mapped to generated
backings during AOT lowering and keep primitive-shadow tracking separate from
declaration tracking. Broader inline-module export support should be added only
with definite-assignment analysis and module-private context propagation for
synthetic wrappers.

Unresolved issues: full high-memory or sanitizer validation was not run; this
slice touched compiler lowering and generated e2e, not runtime memory lanes.

Next actions: continue auditing only from concrete failing contracts found by
targeted probes or TODO-backed items.

Dependencies, blockers, or restart requirements: running processes need a
rebuild to pick up compiler changes. `build/main` and generated e2e artifacts
were rebuilt during validation.

Signature: Codex GPT-5

### 2026-04-28T09:06:54+02:00 - Literal Singleton Shorthand Exactness

Objective attempted: continue regression auditing on the active type/dispatch
surface while preserving the literal singleton behavior landed on 2026-04-27.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: tightened inline literal singleton
annotation parsing so reserved shorthand tokens require exact matches. `^#nil`,
`^#true`, and `^#false` remain accepted; misspellings such as `^#nilx`,
`^#truex`, and `^#falsex` now fail closed. Removed an unused duplicate inline
literal parser helper and added regression tests in the advanced type parser
matrix. Updated `memory/changelog_parts/changelog_part_38.md`,
`docs/areas/types-dispatch.md`, and `.agents/PLAN.md` with the closed
checkpoint.

Commands run:
- `jj status`
- `c3c build`
- C3 LSP diagnostics for `src/lisp/parser_type_annotation_helpers.c3`,
  `src/lisp/primitives_meta_types.c3`, and
  `src/lisp/tests_advanced_type_parametric_groups.c3`
- direct repros with `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval ...`
  for `^#nilx`, `^#truex`, and `^#falsex`
- `git diff --check`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed. Direct repros now report `Literal shorthand symbol
annotations use ^#'symbol` instead of returning `77`. Focused advanced
type/dispatch validation passed with `248 passed, 0 failed`; compiler slice
passed with `352 passed, 0 failed`; default basic Lisp slice passed with
`173 passed, 0 failed`; `git diff --check` passed.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Do not assume the
inline `^#` parser can recognize reserved words by prefix. Literal shorthand
must parse whole tokens only, or misspelled annotations can silently become
valid dispatch routes.

Current best recommendation: keep the `^#datum` shorthand exact and fail closed
for unquoted symbol-like inline tokens. Use `^#'symbol` for symbol literals and
`^(Literal datum)` for the long form.

Unresolved issues: bounded end-to-end generation was not run in this pass; use
the Docker validation path if broad e2e confidence is required.

Next actions: none for this slice.

Dependencies, blockers, or restart requirements: rebuild required for running
processes to pick up the parser change; the local `build/main` was rebuilt
during validation.

Signature: Codex GPT-5

### 2026-04-27T11:28:16+02:00 - Literal Singleton Dispatch Surface

Objective attempted: replace metadata-flavored value dispatch with a literal
singleton type surface that is compact in annotations and explicit in value
position.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: implemented `^#datum` parser support for
literal singleton annotations, added `^(Literal datum)` as the long-form
annotation, removed public `^(Value datum)` / `^(Val datum)` parsing, registered
`Literal` as a builtin type/primitive, and made `(Literal datum)` return a
singleton type descriptor instead of boxing the datum. Updated compiler
primitive hashing, printing, tests, `memory/changelog_parts/changelog_part_38.md`,
and the relevant language/type/syntax docs.

Commands run:
- `jj status`
- `git diff --check`
- C3 LSP diagnostics for parser annotation helpers, parser type annotations,
  advanced type-parametric tests, and advanced type-dispatch tests
- `c3c build`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

Key results: build passed; focused advanced dispatch/type-parser slice passed
with `245 passed, 0 failed`; full Lisp suite passed with `173 passed, 0 failed`.
Direct smokes confirmed `^#true`, `^#false`, `^#nil`, and `^#'open` dispatch,
and old `^(Value 0)` now reports the removal error.

Invalidated assumptions or failed approaches: `[INVALIDATED]` Public
metadata-style singleton dispatch (`^(Value datum)` / `^(Val datum)`) is no
longer authoritative. The canonical public surface is `^#datum` shorthand,
`^(Literal datum)` long form, and `(Literal datum)` in value position for the
type descriptor.

Current best recommendation: keep the public name `Literal` for singleton types
and reserve the internal `ValueLiteralKey` machinery as representation detail.
Do not reintroduce value boxing semantics for `(Literal datum)`.

Unresolved issues: bounded end-to-end generation was not run in this pass; host
execution of broad e2e gates is avoided by repo policy unless using the Docker
validation path.

Next actions: if broader compiler/e2e confidence is required, run the bounded
container validation path for the e2e suite rather than `scripts/run_e2e.sh`
directly on the host.

Dependencies, blockers, or restart requirements: rebuild required for the
runtime/compiler binary to pick up the parser and primitive table changes.

Signature: Codex GPT-5

### 2026-04-27T10:18:04+02:00 - LLM Language Digest

Objective attempted: create a compact LLM-friendly version of the language
surface so future agents do not invent syntax around value types, dispatch, or
reader tags.

Relevant workspace: `/home/christos/Omni`.

Code or configuration changes made: no code changes. Added
`docs/LLM_LANGUAGE_DIGEST.md` as a non-normative digest and linked it from
`docs/README.md` and `docs/OMNI_REFERENCE.md`.

Commands run:
- `jj status`
- `rg` targeted scans for reader tags, type definitions, value dispatch, and
  metadata wording
- `sed` reads of canonical docs and reference chapters

Key results: the digest records the live syntax: product values use
`(define [type] ...)` / `(define [struct] ...)` plus callable constructors;
literal singleton dispatch uses `^#datum` / `^(Literal datum)`; reader tags are `#tag form`
lowering to `(tag form)`. It explicitly rejects invented syntax such as
`value-type`, `defvalue`, and `#Type{...}`.

Invalidated assumptions or failed approaches: none from execution. Existing
memory already invalidates treating `define [...]` as an Array argument.

Current best recommendation: use `docs/LLM_LANGUAGE_DIGEST.md` as the first
compact prompt context after loading canonical authority docs. Do not treat it
as a normative replacement for `LANGUAGE_SPEC` or `SYNTAX_SPEC`.

Unresolved issues: no implementation of new value-type or reader-tag syntax was
attempted in this docs pass.

Next actions: if the owner wants new value-type ergonomics beyond current
`[type]` constructors, create a concrete language-surface decision note under
`docs/plans/` before implementation.

Dependencies, blockers, or restart requirements: none.

Signature: Codex GPT-5
