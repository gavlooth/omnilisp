# fiber-temp-session-plan Part 02

Source: `docs/plans/fiber-temp-session-plan.md`

## Fiber TEMP Phase 6h Progress (2026-03-05)

Completed:
- Added a one-command boundary-hardening runner:
  - `scripts/run_boundary_hardening.sh`
  - runs normal + ASAN matrix with Fiber TEMP and stack-affinity harness enabled by default.
- Added profile docs in `docs/PROJECT_TOOLING.md` with toggle env vars.

Validation:
- `scripts/run_boundary_hardening.sh` passed end-to-end.
- Normal stage: `stack_engine 21/0`, `scope_region 51/0`, `unified 1182/0`, `compiler 73/0`.
- ASAN stage: `stack_engine 20/0`, `scope_region 51/0`, `unified 1181/0`, `compiler 73/0`.
- Harness summary present and passing in both stages.

Next:
- Wire this script (or equivalent env profile) into external CI where available.

## Fiber TEMP Phase 6i Progress (2026-03-05)

Completed:
- Added machine-checkable summary assertions to `scripts/run_boundary_hardening.sh`.
- The runner now captures normal/ASAN logs and enforces:
  - `stack_engine/scope_region/unified/compiler fail=0`
  - `stack_affinity_harness fail=0` when harness is enabled
  - `fiber_temp_pool enabled=1` when Fiber TEMP is enabled
- Added assertion toggle:
  - `OMNI_BOUNDARY_ASSERT_SUMMARY=0` to disable assertion checks.

Validation:
- `scripts/run_boundary_hardening.sh` passed with Stage 5 assertions.
- Produced log artifacts:
  - `build/boundary_hardening_normal.log`
  - `build/boundary_hardening_asan.log`

Next:
- Reuse these logs in external CI to publish concise summary artifacts.

## Fiber TEMP Phase 6j Progress (2026-03-05)

Completed:
- Added machine-readable boundary summary emission:
  - new parser script `scripts/parse_boundary_summary.sh`,
  - runner Stage 6 emits `build/boundary_hardening_summary.json`.
- Added runner controls:
  - `OMNI_BOUNDARY_EMIT_JSON`
  - `OMNI_BOUNDARY_SUMMARY_JSON`

Validation:
- `scripts/run_boundary_hardening.sh` passed:
  - Stage 5 assertions pass,
  - Stage 6 JSON artifact generated and verified for both normal/ASAN sections.

Next:
- Hook `build/boundary_hardening_summary.json` as a CI artifact in external workflow.

## Fiber TEMP Phase 6k Progress (2026-03-05)

Completed:
- Added external CI wiring for boundary profile:
  - `.github/workflows/boundary-hardening.yml`
  - manual trigger (`workflow_dispatch`)
  - runs `scripts/run_boundary_hardening.sh`
  - uploads normal/ASAN logs + JSON summary artifact

Validation:
- Local boundary profile remains green and emits expected artifacts.
- Workflow is non-disruptive (manual trigger only).

Next:
- Optionally add a PR/comment bot step to surface key summary fields from `boundary_hardening_summary.json`.

## Fiber TEMP Phase 6l Progress (2026-03-05)

Completed:
- Added CI job-summary renderer:
  - `scripts/emit_boundary_job_summary.sh`
  - workflow now publishes compact boundary result table to `$GITHUB_STEP_SUMMARY`.
- This complements artifact upload with immediate, in-page diagnostics.

Validation:
- Local summary-renderer dry-run showed expected PASS rows and fail-field values.
- `scripts/run_boundary_hardening.sh` remains green end-to-end.

Next:
- Optional: add a lightweight PR comment bridge consuming the same renderer output.

## Fiber TEMP Phase 6m Progress (2026-03-05)

Completed:
- Added optional PR-comment bridge in boundary workflow:
  - new `workflow_dispatch` input `pr_number`,
  - workflow posts boundary summary markdown to the specified PR when set.
- Added workflow-level permissions required for PR comments and included markdown summary artifact upload.

Validation:
- Shell scripts syntax-check clean (`bash -n` across boundary tooling scripts).
- Summary renderer output remains correct on existing normal/ASAN logs.

Next:
- Optional: deduplicate/replace prior boundary bot comments (update-last-comment policy) to reduce PR noise.

## Fiber TEMP Phase 6n Progress (2026-03-05)

Completed:
- Implemented boundary PR comment upsert policy in workflow:
  - marker-based detection,
  - update existing bot comment when present,
  - create only when absent.

Validation:
- Workflow script path remains deterministic and non-invasive (manual dispatch + optional input).

Next:
- Optional: switch from list-first-page to paginated lookup if comment volume grows beyond 100 comments.

## Fiber TEMP Phase 6o Progress (2026-03-05)

Completed:
- Implemented paginated PR comment lookup in boundary workflow upsert path (up to 10 pages).
- Marker-based create-or-update behavior preserved.

Validation:
- CI workflow script remains valid and non-invasive (manual dispatch + optional `pr_number`).

Next:
- Optional: tighten target identity from generic `Bot` type to GitHub Actions bot account if needed.

## Fiber TEMP Phase 6p Progress (2026-03-05)

Completed:
- Tightened PR comment upsert match criteria:
  - requires `user.type == Bot` and `user.login == github-actions[bot]`
  - marker + pagination behavior preserved.

Validation:
- CI/workflow-only change; no runtime impact.

Next:
- Optional: make expected bot login configurable via workflow input/env if alternate auth token is used.

## Fiber TEMP Phase 6q Progress (2026-03-05)

Completed:
- Added configurable bot-login input for boundary PR comment upsert:
  - workflow input `pr_comment_bot_login` (default `github-actions[bot]`),
  - matcher uses provided bot login + marker/pagination safeguards.

Validation:
- Workflow-only change; no runtime impact.

Next:
- Optional: add workflow guard to skip PR comment step when `pr_number` references a non-PR issue.

## Fiber TEMP Phase 5b Progress (2026-03-05)

Completed:
- Added scheduler thread-boundary coverage for Fiber TEMP context metrics:
  - repeated `thread-spawn`/`thread-join` cycles under flag,
  - verifies `ctx_take_hits`/`ctx_return_count` remain unchanged when no stack context is active.
- Preserved mixed scheduler boundary stress semantics without introducing invalid invariants for stack-context-involving paths.

Validation:
- Normal: `Stack engine 19/0`, `Scope region 51/0`, `Unified 1179/0`, `Compiler 73/0`.
- ASAN strict: `Stack engine 18/0`, `Scope region 51/0`, `Unified 1178/0`, `Compiler 73/0`.

Next:
- Add targeted scheduler wakeup/offload interleaving stress with Fiber TEMP enabled to widen boundary-race coverage.

## Session Rules

Global rule for every session:

```bash
c3c build
LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

Memory/lifetime touching sessions additionally require:

```bash
c3c build --sanitize=address
ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

Commit discipline:
- One session can produce 1-3 commits.
- Keep commits phase-local and revertable.
- Update `memory/CHANGELOG.md` for behavior-impacting lifetime changes.

Hard stop conditions:
1. Stack engine needs direct `ScopeRegion` operations to proceed.
2. Any per-type RC lifetime appears for language graph values.
3. New ASAN UAF/double-free/leak appears after integration.
4. Clone/discard behavior becomes non-deterministic.

---

## Wave A: Baseline + Defer Substrate (Sessions 1-6)

### Session 1: Baseline Freeze + Acceptance Contract

Goal:
- Lock baseline behavior and invariant contract.

Work:
- Record current teardown/clone failure surfaces and expected invariants.
- Update:
  - `docs/plans/fiber-temp-teardown-revision-summary.md`
  - `docs/plans/session-34-44-boundary-hardening.md`
  - `memory/CHANGELOG.md` (baseline snapshot)

Commits:
1. `docs: freeze teardown+clone invariants and baseline gates`

Validation:
- Global rule only.

### Session 2: Stack Engine API Skeleton

Goal:
- Introduce generic defer API surface without runtime wiring.

Likely files:
- `src/stack_engine.c3`

Work:
- Add `stack_ctx_defer` and `stack_ctx_undefer` API.
- Add generic `DeferOps` callback contract (`destroy`, `clone`).
- Add basic stack-engine unit tests for API shape.

Commits:
1. `stack: add generic defer API and DeferOps contract`

Validation:
- Global rule + ASAN.

### Session 3: Destroy-Path Defer Execution

Goal:
- Ensure stack destroy path runs remaining defers deterministically.

Likely files:
- `src/stack_engine.c3`

Work:
- Implement LIFO defer execution on destroy.
- Add tests for:
  - normal return (no destroy callback)
  - destroy with multiple defers (ordering)

Commits:
1. `stack: execute defer callbacks on destroy in deterministic order`

Validation:
- Global rule + ASAN.

### Session 4: Clone-Path Defer Replay

Goal:
- Ensure clone path re-establishes semantic ownership via `clone` callbacks.

Likely files:
- `src/stack_engine.c3`

Work:
- Implement clone-time defer metadata replay.
- Invoke `ops.clone(arg)` during stack clone flow.
- Add clone-specific tests:
  - clone without destroy
  - clone then destroy both original and clone

Commits:
1. `stack: add defer clone replay hooks for semantic ownership duplication`

Validation:
- Global rule + ASAN.

### Session 5: Runtime Boundary Wiring (JIT/Boundary Core)

Goal:
- Use defer substrate for scope teardown safety in core boundary paths.

Likely files:
- `src/lisp/jit_jit_eval_scopes.c3`
- `src/lisp/eval_boundary_api.c3`

Work:
- Register cleanup at boundary entry.
- Remove assumptions that epilogue always executes naturally.

Commits:
1. `jit/boundary: register scope cleanup via generic defer substrate`

Validation:
- Global rule + ASAN + targeted:
  - `--filter continuation`

### Session 6: Runtime Boundary Wiring (Env/Promotion Context)

Goal:
- Extend defer-backed cleanup to env/promotion-sensitive paths.

Likely files:
- `src/lisp/eval_env_copy.c3`
- `src/lisp/eval_promotion_context.c3`

Work:
- Wire boundary cleanup for env/promotion transitions that can cross suspend/clone paths.

Commits:
1. `lifetime: harden env/promotion boundaries with defer-backed cleanup`

Validation:
- Global rule + ASAN + targeted:
  - `--filter continuation`
  - `--filter scheduler`

---
