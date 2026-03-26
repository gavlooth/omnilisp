# Omni Lisp — Project Tooling

**Last updated:** 2026-03-26

Omni provides CLI commands for creating projects and auto-generating FFI bindings from C headers.

---

## Install CLI + Man Page

Install `omni` under a prefix (default `~/.local`) and install both manual pages:
- `omni(1)` CLI reference
- `omni-language(7)` language quick reference

```bash
scripts/install_omni.sh install
```

Uninstall from the same prefix:

```bash
scripts/install_omni.sh uninstall
```

Useful overrides:

```bash
scripts/install_omni.sh install --prefix /usr/local
scripts/install_omni.sh install --binary /path/to/omni-build
```

Verify:

```bash
omni --version
man -M "$HOME/.local/share/man" 1 omni
man -M "$HOME/.local/share/man" 7 omni-language
```

---

## Editor Tooling

First-party editor tooling scaffolds are kept in `tooling/` so they can track
the current Omni syntax and CLI behavior:

- `tooling/tree-sitter-omni/` for Tree-sitter grammar and highlight queries
- `tooling/omni-lsp/` for diagnostics, completion, hover, and document symbols over stdio LSP
- `tooling/omni-nvim/` for structured REPL-driven Neovim workflows

These packages are intentionally thin. They reuse the installed `omni` binary
where possible instead of carrying a second parser/runtime stack inside editor
plugins.

### Editor Tooling Bootstrap (Roadmap Slice)

Minimal setup from a repo checkout:

```bash
# Omni CLI availability
omni --version
```

Tree-sitter grammar sanity check:

```bash
cd tooling/tree-sitter-omni
tree-sitter generate
tree-sitter parse examples/sample.omni
```

Run the first-party LSP server over stdio:

```bash
cd /home/heefoo/Documents/code/Omni
python3 tooling/omni-lsp/omni_lsp.py
```

Neovim plugin wiring:

```lua
{
  dir = "/home/heefoo/Documents/code/Omni/tooling/omni-nvim",
  ft = { "omni" },
  config = function()
    require("omni").setup({
      cmd = { "omni", "--repl", "--json" },
      repl = { mode = "json" },
      eval = { cmd = { "omni", "--eval", "--json" } },
      lsp = { auto_setup = true },
    })
  end,
}
```

Related package docs:
- `tooling/tree-sitter-omni/README.md`
- `tooling/omni-lsp/README.md`
- `tooling/omni-nvim/README.md`

---

## Formatting and Indentation Policy

Current policy is intentionally phased:

- phase 1: indentation-first tooling (`omni-lsp` document/range/on-type
  indentation behavior),
- phase 2: canonical full formatter behind a dedicated CLI entrypoint
  (`omni --fmt` or equivalent) once rule coverage and compatibility are stable.

Canonical formatting rules to preserve across editor tooling:

- special forms:
  - keep head symbol first (`if`, `let`, `block`, `handle`, `match`, etc.),
  - single-line forms may stay single-line when concise,
  - multiline forms use one argument/form per line with aligned indentation.
- vector/dict literals:
  - keep inline when short and legible,
  - for multiline literals, one element/entry per line,
  - align closing delimiter with the opening form indentation.
- `handle` clauses:
  - one clause per line/block,
  - clause body forms are indented under the clause head,
  - preserve explicit `(resolve ...)` placement instead of collapsing handler
    control-flow forms into one line.
- type/method declarations:
  - keep declaration head and name grouped for readability,
  - place parameter/type annotations consistently with one parameter per line in
    multiline declarations,
  - avoid reflow that obscures dispatch-relevant annotation structure.
- macro/template blocks:
  - keep `syntax-match` and `template` structures vertically explicit,
  - keep each pattern arm visually isolated,
  - preserve `(insert ...)` and `(splice ...)` boundaries without inline
    collapsing that harms readability.

Implementation note:
- the current `omni --fmt` path is intentionally conservative:
  normalize leading indentation from structural depth, align wrapped
  `export`/`export-from` payloads, align wrapped `let` binding lists, preserve
  repo-style `if` branch indentation including nested `if` bodies that start
  later in a clause line, keep block-style `when` / `unless` / `raise` /
  `checkpoint` bodies out of generic continuation alignment, keep inline block
  forms inside `let` binding lists aligned to their binding context, keep
  higher-order collection-call lambda bodies (`map` / `foldl` / `foldr` /
  `filter` / `find` / `partition` / `sort-by` / `for-each`) aligned from the
  lambda's own opening column, keep multiline `Coroutine (lambda ...)` bodies
  on the current in-tree wrapper-lambda layout, keep multiline clause bodies
  and inline dict/vector payload entries aligned from their opening delimiter
  in current `match` arms and data literals, align wrapped generic call and
  pipeline continuations under their first argument, trim trailing whitespace,
  preserve blank lines, preserve original line-ending style, and avoid
  aggressive intra-line rewrites.

---

## Validation Lanes

Keep validation scope aligned with the change family:

- `memory-lifetime-smoke`, `memory-lifetime-bench`, `memory-lifetime-soak`,
  and `memory-stress` are the container-bound lanes for boundary/lifetime
  changes.
- `allocator-validation` and `allocator-bench` are separate lanes for AST
  allocator correctness and throughput work.
- Syntax/compiler-only changes should stay on their own non-memory lane and do
  not implicitly require memory-ownership coverage.

---

## CLI Commands

### `--check` — Parse/Check Source Without Executing It

```bash
omni --check script.omni
omni --check --json script.omni
```

Runs a non-executing parse/check pass over an Omni source file.

Current behavior:

- does not execute user code,
- returns exit code `0` for clean input,
- returns non-zero for syntax, lowering, and read failures,
- `--json` emits structured diagnostics with 0-based ranges.

### `--fmt` — Apply the Conservative Formatter

```bash
omni --fmt script.omni
omni --fmt --write script.omni
omni --fmt --check script.omni
```

Runs Omni's current first-party formatter contract over one source file.

Current behavior:

- default mode prints formatted source to stdout,
- `--write` rewrites the file in place,
- `--check` prints nothing and returns `0` when the file is already formatted,
  `1` when formatting changes would be applied,
- formatting is intentionally conservative:
  - normalize leading indentation from structural depth,
  - align wrapped `export` / `export-from` payload lines,
  - align wrapped `let` binding-list continuation lines,
  - preserve repo-style `if` branch indentation, including nested `if` bodies
    that begin later in a clause line,
  - keep `when` / `unless` / `raise` / `checkpoint` bodies on block-style
    indentation instead of treating them as generic continuations,
  - keep inline block forms inside `let` binding lists aligned to their
    binding context,
  - keep multiline higher-order lambda bodies aligned from the lambda's own
    opening column for current in-tree collection calls such as `map`,
    `foldl`, `foldr`, `filter`, `find`, `partition`, `sort-by`, and
    `for-each`,
  - keep multiline `Coroutine (lambda ...)` bodies on the current in-tree
    wrapper-lambda layout instead of flattening them toward generic lambda
    indentation,
  - keep multiline clause bodies and inline dict/vector payload entries aligned
    from their opening delimiter in current `match` arms and data literals,
  - align wrapped generic call and pipeline continuation lines under their
    first argument,
  - trim trailing whitespace,
  - preserve blank lines,
  - preserve the source file's existing newline style (`LF` vs `CRLF`),
  - avoid aggressive intra-line rewrites.

### `--eval` — Evaluate One Expression

```bash
omni --eval '(+ 1 2)'
omni --eval --json '(+ 1 2)'
```

Runs one Omni expression through the normal runtime path.

Current behavior:

- evaluates exactly one CLI-supplied expression string,
- returns rendered value output in text mode,
- returns structured success/error payloads in `--json` mode,
- returns non-zero on evaluation failure.

### `--describe` — Symbol Help Surface

```bash
omni --describe define
omni --describe --json define
```

Returns first-party help for core special forms and selected builtin symbols.

Current behavior:

- text mode prints symbol, kind, and documentation,
- `--json` emits machine-readable payload:
  - `ok`,
  - `symbol`,
  - `kind`,
  - `documentation`,
  - `error` for unknown symbols.
- returns exit code `0` for known symbols and non-zero for unknown/usage errors.

### `--test-suite` — Structured Test Execution

```bash
omni --test-suite all
omni --test-suite all --json
```

`--json` emits one machine-readable payload that includes per-suite pass/fail and exit
code:

- `ok` (`true`/`false`)
- `requested_suite`
- `suites` array with:
  - `name`
  - `pass`
  - `fail`
  - `code` (`0` success, `1` suite failure)
- `totals` object with aggregate counters and final status code

Current behavior:

- JSON mode runs the same suite selection semantics as text mode:
  - `all` (default when omitted), `stack`, `scope`, `lisp`
- non-JSON mode keeps the existing human output
- returns non-zero on suite failures, same as text mode
- invalid suite selection still returns exit code `2`

### `--repl --json` — Structured Session Transport

```bash
printf '%s\n' '{"id":"1","input":"(+ 1 2)","mode":"expr"}' | omni --repl --json
```

Runs a persistent newline-delimited JSON request/response loop for tools.

Current behavior:

- accepts one JSON request per input line,
- supports `mode: "expr"` and `mode: "program"`,
- returns one JSON response per request line,
- exits cleanly on stdin EOF.

Current first-party usage:

- `tooling/omni-nvim/` uses this transport for persistent editor requests,
- current-line and current-form eval still prefer `omni --eval --json` for
  one-shot calls,
- whole-buffer sends and REPL fallback calls now go through the structured
  session instead of scraping text REPL output.

---

## Exit Status Map

The CLI mostly follows a simple `0` success / `1` failure convention.
`--test-suite` is the main exception because it distinguishes invalid suite
selection from normal suite failure.

- `--check`
  - `0` on clean input
  - `1` on usage, read, parse, or lowering/check failure
- `--eval`
  - `0` on successful evaluation
  - `1` on usage or evaluation failure
- `--describe`
  - `0` on known symbol
  - `1` on unknown symbol or usage failure
- `--test-suite`
  - `0` on success
  - `1` on suite failure or stack-affinity harness failure
  - `2` on invalid suite name
  - `--json` returns the same exit status semantics plus the JSON envelope:
    - top-level `code` inside `totals`
- `--build`, `--bind`, `--compile`, `--init`, `--repl`, `--gen-e2e`,
  `--language-ref`, `--version`, and `--help`
  - `0` on success
  - `1` on failure or missing required inputs

### `--language-ref` — Print Built-In Full Language Reference

```bash
omni --language-ref
```

Alias flags: `--lang-ref`, `--manual`.

Prints the embedded full language reference directly from the executable
without requiring external docs on disk.

### `--init` — Scaffold a New Project

```bash
omni --init myproject
```

Creates a complete project directory:

```
myproject/
  omni.toml             # Project config (the only file you edit)
  src/
    main.omni           # Entry point
  lib/
    ffi/                # Auto-generated FFI bindings go here
  include/              # Drop C headers here for local use
  build/                # Build artifacts
    project.json        # C3 build config (generated — do not edit)
```

The generated `src/main.omni` contains a hello-world program:

```lisp
(println "Hello from myproject!")
```

Recommended editor/tooling setup after `--init`:

```bash
cd myproject
omni --check src/main.omni
```

If developing from this repo checkout, then wire first-party tooling from
`tooling/`:

- Tree-sitter grammar: `tooling/tree-sitter-omni/`
- LSP server: `tooling/omni-lsp/omni_lsp.py`
- Neovim plugin: `tooling/omni-nvim/`

For exact setup snippets, use:
- `docs/PROJECT_TOOLING.md` (`Editor Tooling Bootstrap (Roadmap Slice)`)
- `tooling/omni-lsp/README.md`
- `tooling/omni-nvim/README.md`

### `--bind` — Generate FFI Bindings

```bash
omni --bind myproject/
omni --bind                 # uses current directory
```

Reads `omni.toml`, parses C headers using libclang, and writes Omni FFI modules to `lib/ffi/`.

**Requires:** libclang installed on the system (see [Dependencies](#dependencies) below).

---

## Developer Test Profiles

## Developer Build Loop

For normal edit/build iteration, use the dedicated fast dev build:

```bash
scripts/build_fast_dev.sh
LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 1 2)'
```

This path:
- builds into its own object/output trees,
- excludes embedded test sources and test-only entry wiring,
- keeps `--eval`, `--repl`, `--check`, and script-mode workflows.

Use the full project build when you need the full binary:

```bash
c3c build
LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

Optional narrower profile for non-deduce work:

```bash
scripts/build_fast_nodeduce_dev.sh
LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'
```

Use the full binary for:
- `--test-suite`
- `--gen-e2e`
- `--stack-affinity-probe`
- `--language-ref`
- `--manual`
- `--init`
- `--bind`
- `--build`
- `--compile`
- integration validation before finishing substantial changes

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

- boundary-facade guard (`scripts/check_boundary_facade_usage.sh`) to block direct legacy boundary calls outside sanctioned files,
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

This emits one current JSON artifact at `build/validation_status_summary.json`
covering the bounded:

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

1. New public primitives must not introduce legacy failure style (`raise_error`
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

## omni.toml Format

The project configuration uses a minimal TOML subset.

### Project Metadata

```toml
[project]
name = "myproject"
version = "0.1.0"
```

### Build Configuration

The `[build]` section controls compilation settings. These map to C3 compiler flags in the generated `build/project.json`.

```toml
[build]
output-dir = "build"        # Where build artifacts go
safety = "safe"             # "safe", "fast", or "none"
opt = "O0"                  # "O0", "O1", "O2", "O3", "Os", "Oz"
debug-info = "full"         # "full", "line-tables", "none"
# sanitize = "none"         # "none", "address", "memory", "thread"
# single-module = false
```

| Field | Default | Values | Description |
|-------|---------|--------|-------------|
| `output-dir` | `"build"` | Any path | Build output directory |
| `safety` | `"safe"` | `"safe"`, `"fast"`, `"none"` | Runtime bounds/null checks, contracts |
| `opt` | `"O0"` | `"O0"`–`"O3"`, `"Os"`, `"Oz"` | Optimization level |
| `debug-info` | `"full"` | `"full"`, `"line-tables"`, `"none"` | Debug symbol level |
| `sanitize` | `"none"` | `"none"`, `"address"`, `"memory"`, `"thread"` | Sanitizer |
| `single-module` | `false` | `true`, `false` | Compile all modules together (more inlining) |

**Safety levels explained:**

- **`"safe"`** (default) — Bounds checking, null pointer checks, contract enforcement. Use during development.
- **`"fast"`** — Disables safety checks for maximum performance. Use for release/production.
- **`"none"`** — No safety, no contracts. Only for benchmarking or when you know exactly what you're doing.

### FFI Dependencies

Each C library dependency gets a `[dependencies.ffi.NAME]` section:

```toml
[dependencies.ffi.math]
library = "m"                           # Shared lib name → dlopen("libm.so")
headers = ["/usr/include/math.h"]       # C headers to parse
functions = ["sin", "cos", "sqrt"]      # Optional: only bind these functions
```

| Field | Required | Description |
|-------|----------|-------------|
| `library` | Yes | Shared library name (without `lib` prefix and `.so` suffix) |
| `headers` | Yes | Array of C header file paths to parse |
| `functions` | No | Array of function names to bind. If omitted, all exported functions are bound. |

### Multiple Dependencies

```toml
[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "sqrt", "pow", "floor", "ceil"]

[dependencies.ffi.curl]
library = "curl"
headers = ["/usr/include/curl/curl.h"]
# No functions list = bind ALL exported function declarations
```

---

## Generated Bindings

Running `--bind` produces one `.omni` file per dependency in `lib/ffi/`.

### Example: `lib/ffi/math.omni`

```lisp
;; Auto-generated FFI bindings for libm
;; Regenerate with: omni --bind

(module ffi-math (export cos sin sqrt)

  (define _lib (ffi-open "libm.so"))

  (define (cos (^Double arg0))
    (ffi-call _lib "cos" 'double arg0 'double))

  (define (sin (^Double arg0))
    (ffi-call _lib "sin" 'double arg0 'double))

  (define (sqrt (^Double arg0))
    (ffi-call _lib "sqrt" 'double arg0 'double))

)
```

### What Gets Generated

- A `module` with an `export` list of all bound functions
- A `_lib` handle opened via `ffi-open`
- Each function gets typed parameters (`^Integer`, `^Double`, `^String`, `^Pointer`) and a body calling `ffi-call` with canonical type annotations
- C `snake_case` names are converted to Omni `kebab-case` (e.g., `string_length` becomes `string-length`)
- Variadic C functions are skipped with a comment (Omni's FFI doesn't support variadic calls)

### Using Generated Bindings

```lisp
;; Import the generated module
(import "lib/ffi/math.omni")

;; Use via qualified access
(ffi-math.sin 1.0)    ;; => 0.8414709848...
(ffi-math.sqrt 2.0)   ;; => 1.4142135623...

;; Or import specific functions
(import ffi-math (sin cos sqrt))
(sin 3.14159)          ;; => ~0.0
```

---

## C-to-Omni Type Mapping

The binding generator uses libclang to resolve types (including typedefs) and maps them:

| C Type | Omni FFI Symbol | Omni Type Annotation | Notes |
|--------|----------------|---------------------|-------|
| `int`, `long`, `unsigned int`, `unsigned long` | `'int` | `^Integer` | All integer-width types |
| `size_t`, `ssize_t` | `'int` | `^Integer` | Resolved via typedef |
| `enum` types | `'int` | `^Integer` | Enums are integers |
| `float`, `double` | `'double` | `^Double` | All floating-point types |
| `char *`, `const char *` | `'string` | `^String` | Detected by type spelling |
| `void *`, other pointers | `'ptr` | `^Pointer` | Opaque handle as pointer-sized value (`^Ptr` shorthand supported) |
| `void` (return only) | `'void` | `^Void` | Returns the runtime `Void` singleton value |

### Limitations

- **Struct parameters/returns**: C functions that pass or return structs by value cannot be bound (Omni's `ffi-call` only handles scalars and pointers)
- **Variadic functions**: Skipped automatically (e.g., `printf`)
- **Function pointers as parameters**: Mapped as `'ptr`/`^Pointer` (callback registration requires manual wrappers)
- **Macros**: `#define` constants and macro-functions are not parsed (libclang only sees declarations)

---

## Dependencies

### libclang (optional — only needed for `--bind`)

libclang is loaded at runtime via `dlopen` only when `--bind` is invoked. It is **not** required for building, running, or compiling Omni programs.

**Install:**

| Distribution | Command |
|-------------|---------|
| Arch Linux | `pacman -S clang` |
| Debian/Ubuntu | `apt install libclang-dev` |
| Fedora | `dnf install clang-devel` |

The tool searches for libclang in this order:
1. `libclang.so`
2. `libclang.so.1`
3. `/usr/lib/libclang.so`
4. `/usr/lib/llvm/lib/libclang.so`

If libclang is not found, `--bind` prints install instructions and exits with an error.

---

## Complete Workflow Example

```bash
# 1. Create a new project
omni --init calculator

# 2. Edit omni.toml to add math library
cat > calculator/omni.toml << 'EOF'
[project]
name = "calculator"
version = "0.1.0"

[build]
output-dir = "build"
safety = "safe"
opt = "O0"
debug-info = "full"

[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "tan", "sqrt", "pow", "log", "exp", "floor", "ceil"]
EOF

# 3. Generate FFI bindings
omni --bind calculator/

# 4. Write your program
cat > calculator/src/main.omni << 'EOF'
(import "lib/ffi/math.omni")

(println "sin(pi/2) =" (ffi-math.sin 1.5707963))
(println "sqrt(2)   =" (ffi-math.sqrt 2.0))
(println "2^10      =" (ffi-math.pow 2.0 10.0))
EOF

# 5. Run it
cd calculator
LD_LIBRARY_PATH=/usr/local/lib ../build/main src/main.omni
```

---

## Implementation Details

### Architecture

```
omni.toml ──► TOML Parser ──► TomlConfig
                                    │
C Headers ──► libclang (dlopen) ──► ParsedFunc[]
                                    │
                                    ▼
                            Binding Generator ──► lib/ffi/*.omni
```

### Source Files

| File | Description |
|------|-------------|
| `src/lisp/toml.c3` | Minimal TOML parser (~260 lines) |
| `src/lisp/libclang_bind.c3` | libclang dlopen wrapper + C header visitor (~300 lines) |
| `src/lisp/bindgen.c3` | Omni FFI module code generator (~150 lines) |
| `src/entry.c3` | `run_init()` and `run_bind()` CLI handlers |

### Design Decisions

- **`omni.toml` + `build/project.json`**: Omni's config lives at project root; C3's `project.json` is generated inside `build/` to keep the root clean and make the ownership clear. C3 is invoked with `--path build/` to find it.
- **libclang via C3-level dlopen** (not Omni FFI): libclang returns structs by value (`CXString`, `CXCursor`, `CXType`) which Omni's `ffi-call` can't handle. C3 function pointer aliases with proper struct types handle the x86-64 hidden-pointer ABI correctly.
- **Optional runtime dependency**: libclang is only loaded when `--bind` is invoked. Projects that don't use `--bind` have zero additional dependencies.
- **Canonical type resolution**: The type mapper calls `clang_getCanonicalType` to resolve typedefs before mapping, so `size_t` correctly maps to `'int` regardless of the typedef chain.
