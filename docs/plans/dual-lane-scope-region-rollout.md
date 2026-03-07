# Dual-Lane Scope Region Rollout (Corrected Execution Plan)

Status: Active
Owner: Codex
Source of truth: `memory/DESTINATION_ARENA_PLAN.md` Section 20 (Clean Consolidated Systems Spec)

## Objectives
- Eliminate `scope_adopt`-driven temporary retention in hot return paths.
- Preserve deterministic RC/scope lifetimes and no-stop-the-world behavior.
- Keep ASAN clean through staged, reversible changes.

## Non-Negotiable Invariants
1. No stop-the-world GC.
2. `TEMP` pointer must never be stored into `ESCAPE` container.
3. Hot-path lane checks must be O(1) (stamp/metadata), not chunk scans.
4. `Unsplittable && RC > 1` keeps both lanes alive (captured island).
5. Fallback promotion (`copy_to_parent`) must remain available during migration.

## Explicit Corrections Applied
1. Do not treat `NIL` as an immortal immediate by default; it is a `Value*` with scope lifetime in current runtime.
2. Barrier checks use per-scope stamps (`generation` and `escape_generation`), not abstract constants.
3. `Env.define` is a mutator and must eventually participate in write-barrier safety.
4. TCO migration order must preserve source validity: copy/promote first, then reset/free temp lane.
5. `scope_adopt` removal is gated by telemetry + ASAN + stress parity, not immediate deletion.

## Phase 0: Baseline & Instrumentation Gates
Files:
- `src/lisp/eval.c3`
- `src/lisp/jit_jit_helper_functions.c3`
- `src/scope_region.c3`

Tasks:
1. Add counters for: `copy_to_parent` invocations (by tag + callsite bucket), `scope_adopt` bytes moved, and fallback activations.
2. Add optional end-of-process summary behind debug flag.
3. Capture baseline on current suite + loop stress + coroutine stress.

Exit criteria:
- Build/test/ASAN clean.
- Baseline metrics recorded in `memory/CHANGELOG.md`.

## Phase 1: Structural Dual-Lane ScopeRegion (No semantic switch)
Files:
- `src/scope_region.c3`

Tasks:
1. Add fields to `ScopeRegion`:
   - `escape_bump`, `escape_limit`, `escape_chunks`, `escape_dtors`, `escape_generation`.
2. Initialize in `scope_create` (unique `escape_generation` per scope).
3. Extend `scope_destroy` and `scope_reset` to clean both lanes.
4. Add APIs:
   - `ScopeRegion.alloc_escape(usz)`
   - `ScopeRegion.alloc_escape_slow(usz)`
   - `scope_register_dtor_escape(...)`
5. Add/extend scope-region unit tests for lane allocation and lifecycle.

Rules:
- Existing `alloc()` call sites remain TEMP behavior.
- No JIT/eval/value-path semantic changes in this phase.

Exit criteria:
- Existing tests pass.
- New lane unit tests pass.
- ASAN clean.

## Phase 2: Safe Promotion Primitive (Intra-scope TEMP -> ESCAPE)
Files:
- `src/lisp/eval.c3`
- `src/lisp/value.c3`

Tasks:
1. Introduce `promote_to_escape(Value*, Interp*)` using existing copy semantics but targeting current scope ESCAPE lane.
2. Keep cycle-safe/iterative behavior for list spines (no recursion regressions).
3. Keep fallback compatibility with `copy_to_parent` where dynamic boundaries require cross-scope promotion.

Exit criteria:
- Deterministic behavior parity on existing tests.
- No stack-overflow regressions on long list promotion tests.

## Phase 3: Write-Barrier Coverage (Constructor + Mutator)
Files:
- `src/lisp/value.c3`
- `src/lisp/prim_collection.c3`
- `src/lisp/jit_jit_helper_functions.c3`

Tasks:
1. Update `make_cons` to enforce TEMP->ESCAPE barrier before storing fields when allocating ESCAPE conses.
2. Add barrier-aware env mutation path (`Env.define` when env is ESCAPE-lane allocated).
3. Audit and patch pointer-write mutators that can target ESCAPE containers.

Rules:
- Promotion must occur before write commit.
- No chunk-scan in hot mutator path.

Exit criteria:
- Barrier invariant property tests pass.
- ASAN clean on stress loops + coroutine resume/yield loops.

## Phase 4: Scope Exit Fast Path (O(1) ESCAPE splice)
Files:
- `src/scope_region.c3`
- `src/lisp/jit_jit_helper_functions.c3`

Tasks:
1. Implement `scope_splice_escapes(parent, child)`.
2. In `jit_eval_in_single_scope` / `jit_eval_in_call_scope`:
   - Use splice when return result is ESCAPE-lane and scope is splittable condition by construction.
   - Keep `copy_to_parent` fallback for non-escape results and dynamic boundaries.
3. Preserve `RC > 1` captured-scope behavior.

Exit criteria:
- No semantic regressions.
- Reduced `copy_to_parent` volume in accumulator-heavy loops.

## Phase 5: TCO Temp-Lane Reuse
Files:
- `src/scope_region.c3`
- `src/lisp/jit_jit_helper_functions.c3`

Tasks:
1. Add `scope_reset_temp_lane` (escape lane untouched).
2. Update TCO bounce flow with safe order:
   - Copy/promote env dependencies first.
   - Then reset/free temp lane.
3. Keep fallback path for uncertain provenance.

Exit criteria:
- No UAF under ASAN in TCO/coroutine stress.
- Throughput parity or better on named-let loop benchmark.

## Phase 6: Controlled Deletion / Simplification
Files:
- `src/lisp/value.c3`
- `src/lisp/jit_jit_helper_functions.c3`
- `src/lisp/eval.c3`

Tasks:
1. Remove/retire `escape_scope` plumbing once lane architecture fully replaces it.
2. Remove `scope_adopt` from normal runtime paths; keep temporary compatibility wrapper only if needed.
3. Reduce defensive fallbacks only after metric proof.

Exit criteria:
- Test suite + ASAN + stress all green.
- Memory growth bounded in long-loop benchmark.
- Changelog updates complete.

## Verification Matrix (Run every phase)
1. `c3c build`
2. `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
3. `c3c build --sanitize=address`
4. `LD_LIBRARY_PATH=/usr/local/lib ./build/main` (ASAN build)
5. Loop stress: `(let loop ((n 0) (acc '())) (if (= n 100000) acc (loop (+ n 1) (cons n acc))))`
6. Coroutine stress: repeated `yield`/`resume` cycles

## Rollback Strategy
- Keep each phase in isolated commits.
- If ASAN or stress fails, revert only the last phase commit and keep prior validated phases.
