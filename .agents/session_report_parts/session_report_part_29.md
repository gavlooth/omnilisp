# Session Report Part 29

## 2026-04-20 02:45 CEST - ML Stable Reduction And Softmax Checkpoint

- Objective attempted:
  - Continue the strict TODO audit/implementation loop after CPU ML axis
    reductions, using fast subagents for audit support and then implementing
    the next coherent TODO slice.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_reduction.c3`
  - `src/lisp/prim_ml_stable_reduction.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- Code or configuration changes made:
  - Added CPU `ml/max` as the canonical axis maximum reducer for Float64 and
    Float32 tensors.
  - Changed Float32 reduction output narrowing failures from
    `tensor/backend-unsupported` to `tensor/numeric-overflow`.
  - Added CPU `ml/logsumexp(input axes)` with max-shifted multi-axis reduction
    semantics.
  - Added CPU `ml/softmax(input axis)` with max-shifted single-axis
    normalization and input-shape preservation.
  - Split stable reduction/softmax code into
    `src/lisp/prim_ml_stable_reduction.c3` so all tracked files remain under
    the 700-line repository limit.
  - Registered the new primitives in runtime and AOT lookup tables.
  - Updated TODO, roadmap, language spec, primitive appendix, and collections
    reference docs. Cross-entropy and mean-squared-error are now separate open
    TODO items rather than hidden under a completed softmax/logsumexp slice.
- Commands run:
  - Fast subagent audits for reducer diagnostics/coverage, Vulkan reduction
    feasibility, and `ML-VK-020-007` surface/edge cases.
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Key results:
  - Focused advanced collections suite passed with `pass=1681 fail=0`.
  - File-size gate passed with no tracked text file over 700 lines.
- Invalidated assumptions or failed approaches worth preserving:
  - A stable softmax/logsumexp implementation cannot be built on sum-only
    reducers without a per-slice max path.
  - Cross-entropy remains underdefined until the target contract is chosen;
    do not expose a primitive that accepts both probability/one-hot tensors and
    class-index tensors by implication.
- Current best recommendation / checkpoint:
  - Continue with `ML-VK-020-007-B` only after deciding the cross-entropy target
    contract, or implement `ML-VK-020-007-C` mean-squared-error first.
  - Implement `ML-VK-020-006-VK` before claiming any Vulkan reduction,
    logsumexp, or softmax capability.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - CUDA/Vulkan reductions, logsumexp, and softmax intentionally fail closed.
  - Cross-entropy and mean-squared-error remain open TODO items.
- Signature: Codex GPT-5.4
