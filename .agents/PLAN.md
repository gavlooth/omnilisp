# Agent Plan Index

`.agents/PLAN.md` remains the operational plan entrypoint; read the indexed part files for the full plan state.

The historical content was split mechanically to keep individual files below the 700-line repository limit. Content order is preserved in the part files.

## Parts

- Part 01: [.agents/plan_parts/plan_part_01.md](plan_parts/plan_part_01.md) (341 lines)
- Part 02: [.agents/plan_parts/plan_part_02.md](plan_parts/plan_part_02.md) (624 lines)
- Part 03: [.agents/plan_parts/plan_part_03.md](plan_parts/plan_part_03.md) (353 lines)
- Part 04: [.agents/plan_parts/plan_part_04.md](plan_parts/plan_part_04.md) (395 lines)
- Part 05: [.agents/plan_parts/plan_part_05.md](plan_parts/plan_part_05.md) (55 lines)

## Current Checkpoint

Date: 2026-04-19

- All jj-tracked text files above 700 LOC have been split into indexed part
  files, include-only wrappers, or ordered data shards. The root entrypoints
  remain under the cap.
- The current raw jj-tracked text inventory reports zero files above 700 LOC.
  This includes vendored and generated text that earlier checkpoints treated
  as exclusions.
- Audit fixes landed during this continuation:
  - corrected index links so part links resolve relative to each stub file;
  - restored the LSP smoke extended-formatting assertion loop after a split had
    reduced it to `pass`;
  - preserved the Tensor `realize` evaluated-error propagation fix from the
    prior source audit.
- Validation path for this checkpoint:
  - raw jj-tracked text LOC inventory with no vendor/generated exclusions;
  - generated index-link existence check;
  - `git diff --check`;
  - targeted build/smoke gates for the earlier source changes.

## Follow-Up Audit Checkpoint

Date: 2026-04-19

- Re-ran the split gates after the all-file documentation split.
- Fixed mechanical Markdown split hygiene: stripped trailing whitespace and
  extra blank EOFs from generated part files so `git diff --check` is clean.
- Fast subagent audit results:
  - earlier docs/tooling audit found vendor/generated residuals; those have
    since been split rather than kept as LOC exceptions;
  - source/build-wiring audit found no missing helper build entries, PTX
    include mismatches, or duplicate moved Tensor ownership definitions.
- Latest validation:
  - raw tracked-text LOC gate prints no rows above 700;
  - generated Markdown index-link audit passed;
  - LSP smoke and Python syntax checks passed;
  - `scripts/build_omni_chelpers.sh` passed;
  - `c3c build --obj-out obj` linked `build/main`.

## Enforced File Size Gate Checkpoint

Date: 2026-04-19

- Added `scripts/check_file_size_gate.sh` and wired it into
  `scripts/run_global_gates.sh` as Stage 0.
- Gate contract: tracked text files must be at or below 700 LOC. The gate no
  longer exempts vendored/dependency text, generated SPIR-V/PTX text, or
  generated tree-sitter text. Only repo-state artifacts, backup/disabled
  snapshots, bytecode caches, and non-text binaries remain outside the text
  LOC contract.
- Artifact cleanup from audit:
  - untracked generated `.codegraph/logs/*` files and `.codegraph/daemon.pid`;
  - untracked generated Python bytecode under `scripts/__pycache__` and
    `tooling/omni-lsp/**/__pycache__`;
  - added `*.pyc` and `__pycache__/` ignore rules.
- `plan.jsonl` was split into a one-line JSONL index plus ordered
  `plan_jsonl_parts/*.jsonl` shards. Shards are valid JSONL, under 700 LOC,
  and below the jj new-file snapshot-size cap.
- `test_aot` was untracked and ignored as a top-level ELF test binary.
- `scripts/check_file_size_gate.sh` now uses `jj file list` when available so
  newly added jj-tracked files are checked before Git export.
- Generated Vulkan SPIR-V C embeddings that were still above 700 lines were
  split into wrapper-plus-`*_spv_part_*.inc` chunks.
- The repeated owner request extended the split policy to vendor/generated text
  too. Remaining oversized vendor C/C++/header files and the generated
  tree-sitter parser were split into include-only wrappers plus under-700
  `.inc` chunks. The oversized FTXUI doc/config files were split into stubs
  plus under-700 part files.
- `scripts/check_file_size_gate.sh` no longer exempts third-party, generated
  SPIR-V, generated PTX, or generated tree-sitter text files. Final raw
  `jj file list` text inventory reports zero files above 700 LOC; the largest
  tracked text file is exactly 700 lines.

## Active Vulkan ML Suite Planning Checkpoint

Date: 2026-04-19

- Owner direction: build toward a complete ML suite on Vulkan, not just
  individual math kernels.
- Added active roadmap:
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- Added live TODO lanes:
  - `ML-VK-001` capability matrix and backend-neutral contract;
  - `ML-VK-010` batched linear algebra foundation;
  - `ML-VK-020` neural elementwise/reduction/softmax/loss kernels;
  - `ML-VK-030` convolution, pooling, and image tensor kernels;
  - `ML-VK-040` normalization and attention primitives;
  - `ML-VK-050` reverse-mode Tensor autograd with explicit Vulkan semantics;
  - `ML-VK-060` Vulkan-capable optimizer suite;
  - `ML-VK-070` model/layer library and serialization;
  - `ML-VK-080` graph capture, fusion, and memory planning;
  - `ML-VK-090` ML validation and benchmark suite.
- Active hypothesis:
  - complete ML support should preserve the current Tensor/Vulkan contract:
    backend-neutral public names, explicit placement/copyback, capability
    reporting, and no hidden CPU fallback for Vulkan operands.
- Next checkpoint:
  - start with `ML-VK-001` and freeze the public operation names plus
    `tensor-backends` capability keys before adding kernels.

## Active Vulkan ML Contract Audit Checkpoint

Date: 2026-04-19

- Audit findings from the ML-VK-001 pass:
  - `tensor-backends` had no backend-neutral ML operation-family keys, so the
    Vulkan ML roadmap had no runtime-discoverable contract.
  - Vulkan Float64 `stats/normal-*` support was documented and tested, but
    discoverability was blurred by the coarse `scientific-map-float64` key.
- Implemented checkpoint:
  - `tensor-backends` now exposes explicit false ML capability keys for every
    backend entry: `ml-linear`, `ml-convolution`, `ml-neural-map`,
    `ml-normalization`, `ml-attention`, `ml-autograd`, `ml-optimizer`, and
    `ml-graph-execution`.
  - `tensor-backends` now exposes `stats-normal-float64` and
    `stats-normal-float32` separately from broad `scientific-map-*` coverage.
    Vulkan keeps `scientific-map-float64` false while reporting
    `stats-normal-float64` from Vulkan Float64 availability.
- Validation:
  - `git diff --check`
  - `scripts/check_file_size_gate.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Next checkpoint:
  - continue `ML-VK-001` by freezing the first public ML operation names and
    adding fail-closed tests for unsupported Vulkan ML operations before
    implementing kernels.

## Active Vulkan ML First Operation Checkpoint

Date: 2026-04-19

- Audit finding:
  - `ML-VK-001` could not honestly claim fail-closed coverage while no public
    ML operation name was frozen.
- Implemented checkpoint:
  - froze `ml/linear` as the first backend-neutral public ML operation;
  - implemented CPU dense `Float64`/`Float32` affine projection with optional
    bias;
  - registered runtime and AOT primitive lookup for `ml/linear`;
  - kept GPU `ml-linear` capability false and added Vulkan fail-closed tests
    that reject concrete and lazy Vulkan operands before CPU realization.
- Validation:
  - `c3c build`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - focused advanced collections suite: `pass=1606 fail=0`
- Next checkpoint:
  - start `ML-VK-010` by deciding whether Vulkan `ml/linear` should lower
    through existing `contract` kernels or get a dedicated batched GEMM path.
