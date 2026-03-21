# Runtime/Backend Simplification Backlog (2026-03-19)

Status: `complete`
As of: 2026-03-19
Owner: Codex workflow

## Purpose

Track the next highest-value work after:

- backlog closure in
  `docs/plans/codebase-improvement-backlog-2026-03-19.md`
- compiler/parser split-lane closure in
  `docs/plans/compiler-parser-refactor-plan.md`

This backlog is explicitly not a generic file-splitting queue. Structural work
belongs here only when it reduces real runtime complexity, correctness risk, or
iteration cost.

## Current Rationale

- The compiler/parser lane is no longer the right target; remaining files there
  are small enough that queue-driven splitting is not justified.
- The largest and most consequential remaining monoliths are now runtime-facing
  files such as:
  - `src/lisp/primitives_iter_coroutine.c3`
  - `src/lisp/jit_jit_compiler.c3`
  - `src/lisp/eval_dispatch_match.c3`
  - `src/lisp/schema.c3`
- Recent seam work already pushed runtime/backend ownership into
  `src/lisp/runtime_backend_hooks.c3`, which makes a runtime-focused cleanup
  pass technically coherent.

## Ranking Rules

1. Prefer correctness and ownership clarity over cosmetic structure.
2. Prefer changes that reduce coupling at the runtime/backend boundary.
3. Structural changes must be behavior-preserving unless the item explicitly
   calls for a semantic fix.
4. Every landed item must include:
   - `c3c build`
   - one bounded validation command matching the touched area
   - `memory/CHANGELOG.md` update
5. Do not reopen compiler/parser queue-driven splitting under this backlog.

## P1: Runtime/Backend Structure

### 1. Reduce `jit_jit_compiler.c3` ownership surface

Why:
- It is still a real monolith.
- It sits on the runtime/backend seam.
- It controls cache, attachment, lifecycle, and compiled-state policy that now
  has partial seam coverage but still lives in one heavy module family.

Primary targets:
- `src/lisp/jit_jit_compiler.c3`
- `src/lisp/jit_jit_compiler_lifecycle.c3`
- `src/lisp/jit_jit_compiler_compile.c3`
- `src/lisp/runtime_backend_hooks.c3`

Acceptance:
- backend-facing ownership boundaries are explicit for:
  - compiled-state lifecycle,
  - cache policy,
  - attachment/exec bookkeeping,
  - compile/retire policy.
- normal runtime code does not need new direct JIT-global storage access.
- the remaining `jit_jit_compiler.c3` surface is smaller and easier to reason
  about than the current compatibility-heavy state.

Validation:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`

Current state (2026-03-19):
- complete
- the retired-code tombstone registry was split out of
  `src/lisp/jit_jit_compiler.c3` into
  `src/lisp/jit_jit_compiler_retired_code.c3`
- the cache layer was also split out of `src/lisp/jit_jit_compiler.c3` into
  `src/lisp/jit_jit_compiler_cache.c3`
- the attachment table was also split out of
  `src/lisp/jit_jit_compiler.c3` into
  `src/lisp/jit_jit_compiler_attach_table.c3`
- the tracked-state pool and spill-state storage were also split out of
  `src/lisp/jit_jit_compiler.c3` into
  `src/lisp/jit_jit_compiler_state_pool.c3`
- the runtime/thread identity and attach/exec ownership helpers were also split
  out of `src/lisp/jit_jit_compiler.c3` into
  `src/lisp/jit_jit_compiler_runtime_identity.c3`
- `src/lisp/jit_jit_compiler.c3` now no longer owns:
  - `JitRetiredCode`
  - retired-code table storage
  - retired-code insert/reset/forget/prune helpers
  - `JitCacheEntry`
  - cache table storage
  - cache slot access/count helpers
  - `jit_cache_lookup(...)`
  - `jit_cache_find_slot(...)`
  - `jit_cache_commit_slot(...)`
  - `jit_cache_store(...)`
  - `JitAttachedInterp`
  - attachment table storage
  - attachment table access/depth helpers
  - `runtime_backend_next_attach_serial()`
  - `jit_register_attached_interp(...)`
  - `jit_unregister_attached_interp(...)`
  - `JitTrackedState`
  - tracked-state pool storage
  - tracked-state slot access helpers
  - `JitStateSpillNode`
  - spill-state list storage
  - spill-state head/node access helpers
  - thread-affinity checks
  - attached-serial queries
  - interpreter refcount helpers
  - exec-depth helpers
  - suspended-guard helpers
  - initialized/owner-token helpers
- `src/lisp/jit_jit_compiler.c3` is now down to the compiled-function type,
  compatibility wrappers, and backend-global flag definitions (`76` lines)
- bounded validation is green:
  - `OMNI_LISP_TEST_SLICE=jit-policy` (`28 passed, 0 failed`)

### 2. Simplify iterator/coroutine runtime state

Why:
- iterator fixes recently landed, but the implementation surface is still large
  and runtime-sensitive.
- this is a correctness-sensitive area where structure and invariants matter.

Primary targets:
- `src/lisp/primitives_iter_coroutine.c3`
- `src/lisp/eval_boundary_commit_flow.c3`
- `src/lisp/eval_boundary_commit_escape_builders.c3`
- iterator-focused tests in `src/lisp/tests_advanced_*`

Acceptance:
- primitive-backed iterator state stays the default path for lazy combinators.
- closure-backed iterator fallback rules are explicit and regression-tested.
- remaining iterator runtime paths are easier to audit for boundary promotion
  and scope/lifetime correctness.

Validation:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`

Current state (2026-03-19):
- complete
- split the match-analysis/scoring layer out of `src/lisp/eval_dispatch_match.c3`
  into `src/lisp/eval_dispatch_match_breakdown.c3`
- split the dispatch error/reporting layer out of
  `src/lisp/eval_dispatch_match.c3` into
  `src/lisp/eval_dispatch_match_errors.c3`
- `src/lisp/eval_dispatch_match_breakdown.c3` now owns:
  - `DispatchMatchFailureReason`
  - `DispatchMatchBreakdown`
  - value-literal dispatch matching
  - method constraint unification/satisfaction
  - dispatch scoring and per-argument breakdown
- `src/lisp/eval_dispatch_match_errors.c3` now owns:
  - failure-reason symbol mapping
  - lambda-call type-error payload construction
  - ambiguous-dispatch payload construction
  - expected/literal rendering helpers
- `src/lisp/eval_dispatch_match.c3` now stays focused on public boundary flow:
  - lambda-call boundary checking
  - best-method selection
- resulting file sizes:
  - `src/lisp/eval_dispatch_match.c3`: `98` lines
  - `src/lisp/eval_dispatch_match_breakdown.c3`: `214` lines
  - `src/lisp/eval_dispatch_match_errors.c3`: `215` lines
- bounded validation is green:
  - `OMNI_LISP_TEST_SLICE=advanced` (`1086 passed, 0 failed`)

Current state (2026-03-19):
- in progress
- split the match-analysis/scoring layer out of `src/lisp/eval_dispatch_match.c3`
  into `src/lisp/eval_dispatch_match_breakdown.c3`
- split the dispatch error/reporting layer out of
  `src/lisp/eval_dispatch_match.c3` into
  `src/lisp/eval_dispatch_match_errors.c3`
- `src/lisp/eval_dispatch_match_breakdown.c3` now owns:
  - `DispatchMatchFailureReason`
  - `DispatchMatchBreakdown`
  - value-literal dispatch matching
  - method constraint unification/satisfaction
  - dispatch scoring and per-argument breakdown
- `src/lisp/eval_dispatch_match_errors.c3` now owns:
  - failure-reason symbol mapping
  - lambda-call type-error payload construction
  - ambiguous-dispatch payload construction
  - expected/literal rendering helpers
- `src/lisp/eval_dispatch_match.c3` now stays focused on public boundary flow:
  - lambda-call boundary checking
  - best-method selection
- file sizes after the split:
  - `src/lisp/eval_dispatch_match.c3`: `98` lines
  - `src/lisp/eval_dispatch_match_breakdown.c3`: `214` lines
  - `src/lisp/eval_dispatch_match_errors.c3`: `215` lines
- bounded validation is green:
  - `OMNI_LISP_TEST_SLICE=advanced` (`1086 passed, 0 failed`)

Current state (2026-03-19):
- in progress
- split the coroutine half out of `src/lisp/primitives_iter_coroutine.c3`
  into `src/lisp/primitives_coroutine.c3`
- split the iterator thunk/state helper layer out of
  `src/lisp/primitives_iter_coroutine.c3` into
  `src/lisp/primitives_iter_state.c3`
- split the iterator terminal/collection-consumption layer out of
  `src/lisp/primitives_iter_coroutine.c3` into
  `src/lisp/primitives_iter_terminal.c3`
- split the iterator source / infinite-source layer out of
  `src/lisp/primitives_iter_coroutine.c3` into
  `src/lisp/primitives_iter_sources.c3`
- `src/lisp/primitives_iter_coroutine.c3` now stays focused on iterator
  transform/combinator primitives
- `src/lisp/primitives_coroutine.c3` now owns:
  - `CoroutineThunkState`
  - coroutine thunk/bootstrap helpers
  - `Coroutine` bootstrap / context-creation support
- `src/lisp/primitives_iter_state.c3` now owns:
  - iterator partial-thunk builders
  - iterator source-state constructors
  - iterator combinator state constructors
  - shared iterator next/predicate helpers
- `src/lisp/primitives_iter_terminal.c3` now owns:
  - `iterator?` and `Iterator`
  - `next`
  - iterator argument/consume helpers
  - `collect` and `to-array`
- `src/lisp/primitives_iter_sources.c3` now owns:
  - collection-backed iterator constructors
  - `range-from`
  - `repeat`
  - `cycle`
- `src/lisp/primitives_coroutine_resume.c3` now owns:
  - resume validation and context-switch helpers
  - yielded-value copyout
  - `resume`
  - `yield`
- bounded validation is green:
  - `OMNI_LISP_TEST_SLICE=advanced` (`1086 passed, 0 failed`)

### 3. Reduce match/dispatch runtime complexity

Why:
- `eval_dispatch_match.c3` is still large enough to matter.
- match/dispatch correctness affects language behavior directly and is a common
  debugging surface.

Primary targets:
- `src/lisp/eval_dispatch_match.c3`
- relevant dispatch and type/method tests

Acceptance:
- dispatch-specific helper boundaries are explicit.
- debugging and failure surfaces are narrower than the current monolith.
- no regression in type-dispatch or match semantics.

Validation:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`

## P2: Build and Iteration

### 4. Turn the fast dev path into a maintained runtime target

Why:
- the fast path is useful now, but still partly script-shaped.
- the next real speed gains require stable target boundaries, not more ad hoc
  exclusions.

Primary targets:
- `scripts/build_fast_dev.sh`
- `tools/fast-dev/*`
- runtime/backend module boundaries that affect fast-target composition

Acceptance:
- fast dev target remains below the current clean-build baseline.
- unsupported surfaces remain explicit and documented.
- target composition is easier to maintain than the current patched-source
  approach.

Validation:
- `scripts/build_fast_dev.sh`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`

Current state (2026-03-19):
- complete
- moved fast-target project generation out of `scripts/build_fast_dev.sh` into
  `tools/fast-dev/generate_fast_dev_project.py`
- replaced the generated fast-dev `schema_validation.c3` patch with a
  maintained source file at `tools/fast-dev/lisp/schema_validation.c3`
- replaced the generated fast-dev `eval_init_primitives.c3` patch with a
  maintained source file at `tools/fast-dev/lisp/eval_init_primitives.c3`
- the fast target still supports:
  - generated project composition
  - profile reporting
  - up-to-date / no-op checks
- the fast target no longer rewrites runtime source files during generation;
  project generation now composes maintained fast-dev sources directly
- fixed the no-op freshness check so it depends on the generator helper too,
  not just `build_fast_dev.sh`
- public script validation is green:
  - `scripts/build_fast_dev.sh --profile`
  - `scripts/build_fast_dev.sh`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`
  - `scripts/build_fast_nodeduce_dev.sh --profile`
  - `scripts/build_fast_nodeduce_dev.sh`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'`
  - repeat runs now report `fast-dev[default] build: up to date` and
    `fast-dev[nodeduce] build: up to date`

## P3: Observability and Guardrails

### 5. Add one machine-readable validation summary path

Why:
- repo status is cleaner now, but still too dependent on narrative docs.
- a structured summary should answer “what is green/yellow/red right now”
  without reading multiple files.

Primary targets:
- validation scripts under `scripts/`
- docs that point operators to the status artifact

Acceptance:
- one checked-in script emits a machine-readable summary for the main bounded
  gates.
- the summary is stable enough to use in CI or local status checks.
- area/docs point to it as an operator entrypoint.

Validation:
- `c3c build`
- one bounded validation run plus the summary script itself

Current state (2026-03-19):
- complete
- `scripts/run_validation_status_summary.sh` is now the checked-in
  machine-readable operator entrypoint
- the summary artifact now covers:
  - integration build status
  - status/docs consistency policy
  - e2e baseline policy
  - bounded `jit-policy`, `scheduler`, `deduce`, `compiler`,
    `memory-lifetime-smoke`, and `advanced` slices
- the JSON artifact is written to
  `build/validation_status_summary.json` by default, with per-run command/log
  files under `build/validation_status_logs/`
- area docs now point operators at this artifact before narrower reruns
- validation is green:
  - `c3c build`
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
  - artifact result: `9/9` runs passed

## Starting Order

1. `jit_jit_compiler.c3` ownership/coupling reduction
2. iterator/coroutine runtime simplification
3. build-fast target stabilization
4. machine-readable validation summary
5. match/dispatch runtime cleanup

## Closeout

All ranked items in this backlog are now materially complete on the checked-in
tree. Further runtime work should start from a fresh backlog instead of
reopening this queue.
