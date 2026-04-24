## Developer Test Profiles

## Developer Build Loop

For normal edit/build iteration, use the dedicated fast dev build:

```bash
scripts/build_fast_dev.sh
OMNI_HOST_TOOLCHAIN_LIB_PATH="${OMNI_HOST_TOOLCHAIN_LIB_PATH:-/usr/local/lib}"
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/dev-fast/main-dev --eval '(+ 1 2)'
```

This path:
- builds into its own object/output trees,
- excludes embedded test sources and test-only entry wiring,
- keeps `--eval`, `--repl`, `--check`, and script-mode workflows.

Use the full project build when you need the repo-local full binary:

```bash
OMNI_HOST_TOOLCHAIN_LIB_PATH="${OMNI_HOST_TOOLCHAIN_LIB_PATH:-/usr/local/lib}"
LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LIBRARY_PATH:+:$LIBRARY_PATH}" c3c build
# run the locally built artifact directly
# installed/user-facing CLI examples elsewhere in the docs use `omni`
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/main
```

`project.json` searches `build`, `/usr/local/lib`, and `deps/lib` for native
link dependencies such as `lightning`, `replxx`, `omni_chelpers`, and
`omni_ftxui`. If those dependencies are installed in a local toolchain prefix,
set `OMNI_HOST_TOOLCHAIN_LIB_PATH` to that prefix's `lib` directory before
building and running, for example `$HOME/.local/lib`.

Optional narrower profile for non-deduce work:

```bash
scripts/build_fast_nodeduce_dev.sh
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'
```

Use the full binary for:
- `--test-suite`
- `--gen-e2e`
- `--stack-affinity-probe`
- `--language-ref`
- `--lang-ref`
- `--manual`
- `--init`
- `--bind`
- `--build`
- `--compile`
- integration validation before finishing substantial changes

Runtime library path overrides:
- `scripts/run_e2e.sh` uses `OMNI_RUNTIME_TOOLCHAIN_LIB_PATH` when the host
  runtime libraries are not under `/usr/local/lib`; Docker-bound validation
  still prefers the mounted `/opt/omni-host-toolchain/lib` path.
- AOT backend builds use `OMNI_AOT_LINK_LIBRARY_PATH` for the host library
  search path when linking generated binaries. If unset, they fall back to
  `/usr/local/lib`.

Operational rule:
- do not run multiple `c3c build` commands concurrently against the same `build/` tree
- the fast dev path is safe to run alongside the full build because it uses separate build/output directories

Current local baseline on this repo:
- `c3c build`: about 15s
- `scripts/build_fast_dev.sh` clean build: about 2.0s
- `scripts/build_fast_nodeduce_dev.sh` clean build: about 2.0s
- `scripts/build_fast_dev.sh` unchanged no-op: about 0.06s
- `scripts/build_fast_nodeduce_dev.sh` unchanged no-op: about 0.06s

To inspect what still dominates `main-dev` without compiling:

```bash
scripts/build_fast_dev.sh --profile
```

This prints:
- included source count,
- total included C3 size,
- largest source groups,
- the largest source files still linked into the lean target.

To inspect the deduce-free variant:

```bash
OMNI_FAST_DEV_PROFILE=nodeduce scripts/build_fast_dev.sh --profile
```

Current profiling data shows the dominant remaining groups are `eval` and `jit`.
That is why optional-surface cuts such as `deduce` only modestly affect clean
build time.

Fast-path caveat:
- `main-dev` excludes the `pika` module to keep build time down, so regex-backed
  schema validation is not a parity guarantee on the lean dev binary.
- `main-dev` also excludes the compiler/AOT/bindgen source families, so the
  lean binary is explicitly not an integration-equivalent build.
- `main-dev-nodeduce` additionally excludes `deduce` / `deduce/*` bindings, so
  those names are intentionally unavailable there.

### Boundary-Hardening Profile

```bash
scripts/run_boundary_hardening.sh
```

Runs a full hardening matrix:

- boundary-facade guard (`scripts/check_boundary_facade_usage.sh`) to block direct boundary calls outside sanctioned files,
- normal build + test run,
- ASAN build + test run,
- `OMNI_FIBER_TEMP=1` enabled,
- `OMNI_STACK_AFFINITY_HARNESS=1` enabled (subprocess misuse probe),
- summary output via `OMNI_TEST_SUMMARY=1`.
- summary assertions (required suites must report `fail=0`).
- boundary-change policy gate (`scripts/check_boundary_change_policy.sh`) requiring normal+ASAN evidence when boundary-sensitive files change.

Toggles:

- `OMNI_BOUNDARY_ENABLE_FIBER_TEMP=0` to disable Fiber TEMP exercise.
- `OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS=0` to disable misuse probe.
- `OMNI_BOUNDARY_QUIET=0` for verbose suite output.
- `OMNI_BOUNDARY_SUMMARY=0` to disable summary lines.
- `OMNI_BOUNDARY_ASSERT_SUMMARY=0` to skip summary verification.
- `OMNI_BOUNDARY_EMIT_JSON=0` to skip JSON artifact emission.
- `OMNI_BOUNDARY_SUMMARY_JSON=/path/to/file.json` to customize JSON artifact path.
- `OMNI_BOUNDARY_POLICY_RANGE=origin/main...HEAD` to control diff range used by boundary-change policy detection.

Artifacts:

- `build/boundary_hardening_normal.log`
- `build/boundary_hardening_asan.log`
- `build/boundary_hardening_summary.json`
- `build/effects_contract_lint_summary.json`
- `build/validation_status_summary.json`
- `build/effects_contract_policy.log`
- `build/libuv_surface_policy.log`
- `build/primitive_docs_parity.log`

Validation status summary:

```bash
scripts/run_validation_status_summary.sh
```

This emits one JSON artifact at `build/validation_status_summary.json`. Regenerate
it before treating that path as current broad validation output covering
the bounded:

- `scheduler`
- `deduce`
- `memory-lifetime-smoke`

Each run records:

- command
- subsystem
- pass/fail state
- exit code
- known-blocker classification
- start/finish timestamps
- parsed `OMNI_TEST_SUMMARY` rows

### Container-Capped Validation (Docker)

The shared cap wrapper (`scripts/c3c_limits.sh`) supports real Docker
containment for validation runs:

```bash
scripts/build_validation_image.sh

scripts/run_global_gates.sh
```

This mode runs each capped command in a constrained container with:
- memory hard cap (`--memory`, `--memory-swap`),
- CPU cap (`--cpus`),
- PID cap (`--pids-limit`),
- optional wall-time cap (`OMNI_DOCKER_TIMEOUT_SEC`),
- resource telemetry log (`build/docker_resource_stats.log` by default).

Default policy:
- Docker-bound execution for gate scripts when run outside validation containers.
- Max resource envelope of 30% host memory + 30% host CPUs per capped command (CPU and memory overrides are clamped to this maximum).
- Host-side sliced Lisp runs are intentionally restricted by subsystem ownership:
  - boundary/lifetime ownership changes must use `scripts/run_validation_container.sh` for the minimum required ownership lane (`memory-lifetime-smoke`) and for any broader ownership lane (`memory-lifetime-policy`, `memory-lifetime-bench`, `memory-lifetime-soak`, `memory-stress`).
  - AST allocator ownership changes use allocator lanes, not boundary/lifetime lanes: run `allocator-validation` for non-benchmark correctness and add `allocator-bench` only when parser/compiler/macro allocation benchmarks or throughput claims changed.
  - syntax/compiler-only work that does not touch boundary/lifetime or allocator ownership paths should stay on explicit non-memory lanes (for example `advanced`, `compiler`, `list-closure`, `json`) and does not require `memory-lifetime*` or allocator-lane coverage for contributor parity.
- Deprecated `OMNI_LISP_TEST_SLICE` aliases (`memory-soak`, `syntax`) are rejected by `src/lisp/tests_slice_policy.c3`; use the explicit slice names above instead.
- Contributor rule: choose the narrowest lane that owns the changed subsystem; do not bundle memory/allocator ownership lanes into syntax/compiler-only work for convenience.

Slice-aware run profiles:

- `memory-lifetime` / `memory-lifetime-smoke` / `memory-lifetime-policy`:
  - boundary/scoping/coroutine ownership correctness and boundary-policy contract coverage.
  - this is the minimum container path for boundary/lifetime changes.
  - run via `scripts/run_validation_container.sh`.
- `memory-lifetime-bench` / `memory-lifetime-soak` / `memory-stress`:
  - broader ownership perf/stress lanes for boundary/lifetime work.
  - opt in only when the touched change owns that risk surface.
  - run via `scripts/run_validation_container.sh`.
- `allocator-validation`:
  - non-benchmark `AstArena` correctness checks, separate from boundary/lifetime ownership coverage.
  - run via `scripts/run_validation_container.sh`.
- `allocator-bench`:
  - AST parser/compiler/macro throughput + allocation benchmarks.
  - separate from boundary/lifetime correctness lanes; add it only when benchmark-sensitive allocator behavior changed.
  - run via `scripts/run_validation_container.sh`.
  - requires benchmark env flags (`OMNI_AST_ARENA_BENCH` and friends).
- `basic`:
  - broad bounded integration smoke that may exercise ownership-sensitive paths.
  - run via `scripts/run_validation_container.sh` when you intentionally want that broader integration lane.
  - do not treat it as required for syntax/compiler-only changes.
- non-memory syntax/runtime lanes (for example `advanced`, `compiler`, `list-closure`, etc.):
  - safe to run independently on the host when no boundary/lifetime or allocator ownership path changed.
  - do not add `memory-lifetime*` or allocator lanes unless the change actually owns those subsystems.

To include benchmark-adjacent lanes in global gates, opt in explicitly:

```bash
OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK=1 \
OMNI_GLOBAL_GATES_INCLUDE_ALLOCATOR_BENCH=1 \
scripts/run_validation_container.sh scripts/run_global_gates.sh
```

Useful knobs:
- `OMNI_DOCKER_IMAGE` (default `omni-validation:2026-03-10`)
- `OMNI_DOCKER_CPUS` (default: auto 30% host CPU quota, clamped to max policy)
- `OMNI_DOCKER_PIDS_LIMIT` (default `512`)
- `OMNI_DOCKER_TIMEOUT_SEC` (default `0`, disabled)
- `OMNI_DOCKER_NETWORK` (default `none`)
- `OMNI_DOCKER_MONITOR` (default `1`)
- `OMNI_DOCKER_MONITOR_LOG` (default `build/docker_resource_stats.log`)
- `OMNI_DOCKER_REQUIRE_LOCAL_IMAGE` (default `1`, disables implicit pull noise)
- `OMNI_DOCKER_EXTRA_ARGS` (extra raw `docker run` args)
- `OMNI_DOCKER_TOOLCHAIN_ROOT` (optional host toolchain root mounted at `/opt/omni-host-toolchain`)

When `OMNI_DOCKER_TOOLCHAIN_ROOT` is set, gate scripts automatically switch
runtime `LD_LIBRARY_PATH` to `/opt/omni-host-toolchain/lib` for container runs.

Pinned validation image build knobs:
- `OMNI_VALIDATION_IMAGE` (default `omni-validation:2026-03-10`)
- `OMNI_VALIDATION_DOCKERFILE` (default `docker/validation.Dockerfile`)
- `OMNI_VALIDATION_BUILD_PULL` (default `1`)
- `OMNI_VALIDATION_BUILD_ARGS` (raw extra `docker build` args)

### Single-Container Validation Session

For a full validation session in one constrained container (instead of
per-command container launches), use:

```bash
scripts/run_validation_container.sh
```

By default this runs `scripts/run_global_gates.sh` inside one Docker container
with hard caps + telemetry. You can pass a custom command:

```bash
scripts/run_validation_container.sh scripts/run_boundary_hardening.sh
```

Session knobs:
- `OMNI_VALIDATION_IMAGE` (default `omni-validation:2026-03-10`)
- `OMNI_VALIDATION_MEM_MB` (defaults to detected 30% hard cap; value is clamped to cap policy)
- `OMNI_VALIDATION_CPUS` (defaults to auto 30% host CPU quota; value is clamped to cap policy)
- `OMNI_VALIDATION_PIDS_LIMIT` (default `512`)
- `OMNI_VALIDATION_TIMEOUT_SEC` (default `0`, disabled)
- `OMNI_VALIDATION_NETWORK` (default `none`)
- `OMNI_VALIDATION_REQUIRE_LOCAL_IMAGE` (default `1`, disables implicit pull noise)
- `OMNI_VALIDATION_TOOLCHAIN_ROOT` (default empty, optional host toolchain mount)
- `OMNI_VALIDATION_MONITOR_LOG` (default `build/docker_validation_container_stats.log`)

### Effects-Contract Lint Profile

```bash
scripts/run_effects_contract_lint.sh
```

Runs contract drift guards for the effects-first error model:

- `scripts/check_effects_contract_policy.sh`
  - forbids `raise_error(...)` and `make_error(...)` in newly added public primitives.
  - forbids newly-added direct `(signal raise ...)` forms in `stdlib/stdlib.lisp`.
- `scripts/check_libuv_surface_policy.sh`
  - requires newly introduced `io/*` surface additions to keep canonical raw/effect/wrapper mapping.
  - forbids introducing new `io/*` wrappers as lambda aliases; wrapper additions must use function-style `define`.
- `scripts/check_primitive_docs_parity.sh`
  - fails when public primitives registered in `src/lisp/eval_init_primitives.c3`
    are not documented in reference/spec docs.
  - fails when newly-added `__raw-*` io primitives or wrappers are undocumented or missing wrapper/effect mapping.

Artifact:

- `build/effects_contract_lint_summary.json`

### CI Integration

Repository workflow:

- `.github/workflows/boundary-hardening.yml`

The workflow supports:
- `pull_request` runs when boundary-sensitive runtime/policy files change,
- `workflow_dispatch` for manual runs and optional PR comment publication.

It expects a self-hosted Linux runner with `c3c` and runtime dependencies preinstalled. It:

1. runs `scripts/run_boundary_hardening.sh`,
2. publishes a compact Markdown summary to the GitHub job summary via `scripts/emit_boundary_job_summary.sh`,
3. uploads:
   - `build/boundary_hardening_normal.log`
   - `build/boundary_hardening_asan.log`
   - `build/boundary_hardening_summary.json`
   - `build/effects_contract_lint_summary.json`
   - `build/effects_contract_policy.log`
   - `build/libuv_surface_policy.log`
   - `build/primitive_docs_parity.log`

### Lint Rules (Effects Error Contract)

These are enforced by `scripts/run_effects_contract_lint.sh` in local and CI
flows:

1. New public primitives must not introduce old failure style (`raise_error`
   / `make_error`); they must use canonical payload-aware raise helpers.
2. New stdlib wrapper code must not add direct `(signal raise ...)` usage.
3. Public primitive registration must stay doc-complete (reference/spec must
   include every registered public primitive).

### Backlog Freshness Rule

`TODO.md` is now the single live backlog and should be updated directly when
unfinished work changes.

Tunable knobs:

- `OMNI_POST_COMPLETE_BACKLOG_RELEASE_CYCLES` (default: `1`)
- `OMNI_POST_COMPLETE_BACKLOG_FALLBACK_DAYS` (default: `30`)

Optional workflow input:

- `pr_number` — when provided, the workflow posts the same boundary summary as a PR comment.
  Re-runs update the existing boundary bot comment (upsert), rather than creating duplicates.
  Lookup is paginated to handle long PR comment threads.
  If the number is not a PR in the repository, the comment step is skipped.
- `pr_comment_bot_login` — bot login used for upsert matching (default: `github-actions[bot]`).
  Upsert still requires the boundary marker and paginated lookup.
- `policy_range` — optional git diff range used by boundary policy checks (for example `origin/main...HEAD`).
  If omitted, the runner uses a local fallback (`HEAD~1..HEAD` when available).
  On `pull_request` runs, the workflow auto-sets this to `base_sha...head_sha`.

---
