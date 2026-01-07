# OmniLisp TODO

## Review Directive

**All newly implemented features must be marked with `[R]` (Review Needed) until explicitly approved by the user.**

- When an agent completes implementing a feature, mark it `[R]` (not `[DONE]`)
- `[R]` means: code is written and working, but awaits user review/approval
- After user approval, change `[R]` to `[DONE]`
- Workflow: `[TODO]` → implement → `[R]` → user approves → `[DONE]`

---

## STANDBY Notice
Phases marked with **[STANDBY]** are implementation-complete or architecturally stable but are currently deprioritized to focus on **Phase 17: Runtime Bridge & Feature Wiring**. This ensures the language is usable and "wired" before further optimizing the memory substrate.

---

## Phase 13: Region-Based Reference Counting (RC-G) Refactor

Replace hybrid memory management with a unified Region-RC architecture.

- [DONE] Label: T-rcg-arena
  Objective: Integrate `tsoding/arena` as the physical allocator backend.
- [DONE] Label: T-rcg-region
  Objective: Implement the Logical `Region` Control Block.
- [DONE] Label: T-rcg-ref
  Objective: Implement `RegionRef` fat pointer and atomic ops.
- [DONE] Label: T-rcg-transmigrate
  Objective: Implement Adaptive Transmigration (Deep Copy + Promotion).
- [DONE] Label: T-rcg-constructors
  Objective: Implement Region-Aware Value Constructors.
- [DONE] Label: T-rcg-cleanup
  Objective: Remove obsolete runtime components.

---

## Phase 14: ASAP Region Management (Static Lifetimes) [STANDBY]

**Objective:** Implement static liveness analysis to drive region deallocation, with RC as a fallback only.
**Reference:** `docs/STATIC_REGION_LIFETIME_ARCHITECTURE.md`

- [TODO] Label: T-asap-region-liveness
...
- [TODO] Label: T-asap-region-main
...

---

## Phase 15: Branch-Level Region Narrowing

**Objective:** Reduce RC overhead by keeping branch-local data out of RC-managed regions.
**Reference:** `docs/BRANCH_LEVEL_REGION_NARROWING.md`

- [DONE] Label: T1-analysis-scoped-escape
  Objective: Implement hierarchical, branch-level escape analysis to support "Region Narrowing", allowing temporary variables in non-escaping branches to be allocated on the stack or scratchpad instead of the parent region.
  Reference: docs/BRANCH_LEVEL_REGION_NARROWING.md
  Where: csrc/analysis/analysis.h, csrc/analysis/analysis.c

  Implementation completed:
    - Added `EscapeTarget` enum (NONE, PARENT, RETURN, GLOBAL)
    - Added `ScopedVarInfo` struct to track variables per scope
    - Added `ScopeInfo` struct to build scope tree
    - Implemented `omni_scope_enter()`, `omni_scope_exit()`, `omni_scope_add_var()`
    - Implemented `omni_scope_mark_escape()`, `omni_scope_get_escape_target()`
    - Implemented `omni_scope_find_var()`, `omni_scope_get_cleanup_vars()`
    - Implemented `omni_analyze_scoped_escape()` entry point
    - Updated `analyze_if_scoped()` to track branch scopes
    - Updated `analyze_let_scoped()` to track let scopes
    - Updated `analyze_lambda_scoped()` for lambda scopes
    - Added debug printing: `omni_scope_print_tree()`

  Acceptance:
    - `AnalysisContext` tracks scope depth via `current_scope` and `root_scope`
    - Variables correctly identify their defining scope
    - Variables that do not leave a branch are flagged as non-escaping *for that branch*
    - Variables that leave a branch (e.g. via return or assignment to outer var) are flagged as escaping

- [DONE] Label: T2-codegen-narrowing
  Objective: Update code generation to utilize scoped escape analysis, emitting stack/scratch allocations for non-escaping variables and inserting precise cleanup at branch exits.
  Reference: docs/BRANCH_LEVEL_REGION_NARROWING.md
  Where: csrc/codegen/codegen.c, csrc/codegen/codegen.h

  Implementation completed:
    - Added `omni_codegen_emit_narrowed_alloc()` for allocation routing
    - Added `omni_codegen_emit_scope_cleanup()` for branch cleanup
    - Added `omni_codegen_if_narrowed()` for narrowed if codegen
    - Added `omni_codegen_let_narrowed()` for narrowed let codegen
    - Allocation routing: ESCAPE_TARGET_NONE → stack, others → region_alloc
    - Scope cleanup emits free_tree/free_obj for non-escaping variables
    - Shape-aware cleanup based on `ScopedVarInfo->shape`

  Acceptance:
    - Code generator queries escape analysis via `omni_scope_get_escape_target()`
    - Non-escaping variables result in stack-allocated C code (e.g., `Obj _stack_x = {0};`)
    - Escaping variables use region_alloc
    - Scope exit points contain specific cleanup for locals

  Test file created: tests/test_phase15_narrowing.omni

---

## Phase 15b: Adaptive Region Sizing [STANDBY]

**Objective:** Scale region overhead with actual data size to reduce penalty for small lifecycle groups.
**Reference:** `docs/BRANCH_LEVEL_REGION_NARROWING.md` (Section 12)

- [TODO] Label: T-adaptive-size-classes
...
- [TODO] Label: T-adaptive-non-atomic-tiny
...

---

## Phase 16: Advanced Region Optimization [STANDBY]

**Objective:** Implement high-performance transmigration and tethering algorithms.
**Reference:** `docs/ADVANCED_REGION_ALGORITHMS.md`

- [DONE] Label: T-opt-bitmap-cycle
...
- [DONE] Label: T-opt-tether-cache
...

---

## Phase 17: Runtime Bridge & Feature Wiring

**Objective:** Complete the wiring of core language features into the compiler codegen and modern runtime.
**Note:** Legacy evaluator `src/runtime` has been removed. Features must be implemented via compiler intrinsics (`csrc/codegen`) or runtime library functions (`runtime/src`).

- [TODO] Label: T-wire-multiple-dispatch
  Objective: Implement robust Multiple Dispatch selection logic.
  Where: `csrc/codegen/codegen.c`, `runtime/src/runtime.c` (method tables)
  What to change:
    - [ ] Update `define` codegen to automatically convert `TAG_CLOSURE` to `TAG_GENERIC` on redefinition.
    - [ ] Implement Julia-style specificity sorting for methods (more specific Kinds before more general ones) in runtime.
  How to verify: Run `tests/unwired_features.omni`; verify correct method selected for different Kinds.
  Acceptance:
    - Redefining a function with different types adds to the method table instead of overwriting.
    - Dispatch selects the most specific match at runtime.

- [TODO] Label: T-wire-parametric-subtyping
  Objective: Implement subtyping logic for parametric Kinds.
  Where: `runtime/src/runtime.c` (`is_subtype` implementation)
  What to change:
    - [ ] Implement recursive comparison of Kind parameters in `is_subtype` runtime function.
    - [ ] Support covariance for immutable structures (e.g., `{List Int} <: {List Any}`).
  How to verify: `(type? {List Int} {List Any})` returns `true`.
  Acceptance:
    - Parametric types correctly identified in the subtype hierarchy.

- [TODO] Label: T-wire-pika-api
  Objective: Bridge Pika Grammar engine to Lisp environment.
  Where: `runtime/src/runtime.c`
  What to change:
    - [ ] Port Pika parser C implementation to modern runtime.
    - [ ] Implement `prim_pika_match` primitive (takes grammar, rule-name, string).
    - [ ] Implement `prim_pika_find_all` primitive.
  How to verify: Run `tests/unwired_features.omni`; verify grammar definition can be used to parse.
  Acceptance:
    - Pika engine functions accessible from Lisp via runtime primitives.

- [TODO] Label: T-wire-deep-put
  Objective: Support recursive path mutation in `put!`.
  Where: `csrc/codegen/codegen.c` (`codegen_put_bang`)
  What to change:
    - [ ] Rewrite `codegen_put_bang` to traverse deep paths (`a.b.c`) and perform mutation at the leaf.
  How to verify: Mutate a nested dict field and verify the update reflects in the parent.
  Acceptance:
    - Deep path mutation works for both Arrays and Dicts.

- [TODO] Label: T-wire-iter-ext
  Objective: Implement `iterate` and `take` for infinite sequence support.
  Where: `runtime/src/runtime.c` (or new `runtime/src/iter.c`)
  What to change:
    - [ ] Implement `prim_iterate` (lazy generator from function + seed).
    - [ ] Implement `prim_take` (lazy limiter for iterators).
  How to verify: `(collect-list (take 5 (iterate inc 0)))` returns `(0 1 2 3 4)`.
  Acceptance:
    - System supports infinite lazy sequences via runtime primitives.

- [TODO] Label: T-wire-reader-macros
  Objective: Implement Sign `#` dispatch for literals.
  Where: `csrc/parser/parser.c` (Reader macros handled at compile time)
  What to change:
    - [ ] Add support for named characters: `#\newline`, `#\space`, `#\tab`.
    - [ ] Add support for `#fmt` interpolated strings parsing.
  How to verify: Read `#\newline` and verify it produces character code 10.
  Acceptance:
    - Authoritative reader support for all Sign-prefixed literals.

- [TODO] Label: T-wire-modules
  Objective: Implement basic Module isolation.
  Where: `csrc/compiler/compiler.c`, `csrc/codegen/codegen.c`
  What to change:
    - [ ] Implement module namespacing in symbol table/codegen mangling.
    - [ ] Implement `import` to selectively map symbols between namespaces.
  How to verify: Define a module, export a function, and import it into another scope.
  Acceptance:
    - Functional module system with namespace isolation at compile time.