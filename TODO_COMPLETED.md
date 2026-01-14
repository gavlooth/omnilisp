# OmniLisp Completed Tasks Archive

This file contains completed and review-pending tasks from the TODO list.
Tasks here are either `[DONE]` or `[DONE] (Review Needed)`.

**Last Updated:** 2026-01-14

---

## Blocking Issues (Resolved)

### Build Errors (All Resolved)

- **[DONE] Block: Dict struct not found (HashMap type not visible)**
  - RESOLVED (2026-01-10): Fixed Dict struct in internal_types.h
  - Added `#include "util/hashmap.h"` and changed `struct HashMap map;` to `HashMap map;`
  - Location: `runtime/src/internal_types.h` line 29-32

- **[DONE] (Review Needed) Block: runtime ASAN target fails to link (toolchain path assumption)**
  - Fix (2026-01-12): `runtime/tests/Makefile` now defaults ASAN builds to gcc via `ASAN_CC ?= gcc`.
  - Repro: `make -C runtime/tests asan`

- **[DONE] Block: language linkage mismatch in arena.h/omni.h**
  - RESOLVED (2026-01-10): Removed duplicate arena_alloc and arena_reset declarations from omni.h
  - Location: `runtime/include/omni.h` line 876, 879 (now removed)

- **[DONE] Block: omni_store_repair() declaration needed but not implemented**
  - RESOLVED (2026-01-10): Function omni_store_repair() is implemented in runtime/src/runtime.c
  - Location: `runtime/src/runtime.c` line 762

---

## Issue 2: Pool/arena practice + region accounting + auto-repair threshold tuning [DONE] (Review Needed)

**Objective:** Implement region accounting counters and store barrier enforcement for CTRR memory model.

### P0: Region accounting doc + required counters [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-region-accounting-doc (P0)
  - Created `runtime/docs/REGION_ACCOUNTING.md` documenting counter types and semantics
  - Defined: alloc_count, live_count, external_ref_count, region_lifetime_rank

### P1: Retention diagnostics plan [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-retention-diagnostics-plan (P1)
  - Documented diagnostic hooks for pool retention analysis
  - Created test harness for retention validation

### P3: Implement region accounting counters [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-impl-region-accounting-counters (P3)
  - Added counters to Region struct in runtime/src/region.h
  - Implemented increment/decrement on alloc/free paths

### P4: Store Barrier Implementation [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-store-barrier-choke-point (P4)
  - Identified store barrier insertion points in codegen
  - Created omni_store_repair() function

- [DONE] (Review Needed) Label: I2-p4-rank-add-field-and-reset (P4.1)
  - Added lifetime_rank field to Region struct
  - Reset rank on region creation

- [DONE] (Review Needed) Label: I2-p4-rank-codegen-assignment (P4.2)
  - Emitting lifetime_rank assignment in generated C code
  - Child regions get parent_rank + 1

- [DONE] (Review Needed) Label: I2-p4-rank-enforce-store-repair (P4.3)
  - Store barrier uses rank comparison for repair decision
  - Implemented omni_store_repair with proper semantics

- [DONE] (Review Needed) Label: I2-p4-outlives-ancestry-metadata (P4.3b)
  - Added parent pointer for ancestry tracking
  - Used by omni_store_repair() for correct repair decisions

- [DONE] (Review Needed) Label: I2-p4-integrate-store-barrier-boundaries (P4.4)
  - Integrated barriers in dict_set, array_set, box_set paths
  - New entry insertion properly barrier-mediated

- [DONE] (Review Needed) Label: I2-p4-dict-new-entry-store-barrier
  - Dict new-entry insertion uses store barrier (not only update path)

- [DONE] (Review Needed) Label: I2-p4-doc-rank-policy (P4.5)
  - Documented rank assignment policy in CTRR.md

### P5: Region Merge Policy [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-region-merge-policy (P5)
  - Documented when region merging is safe
  - Defined merge criteria based on lifetime analysis

### RF: Regression Fixes

- [DONE] (Review Needed) Label: I2-wire-and-strengthen-region-accounting-tests (RF-I2-1)
  - Added accounting tests to test_main.c
  - Tests verify counter invariants

- [DONE] (Review Needed) Label: I2-warning-clean-build-gate (RF-I2-2)
  - Build passes with -Wall -Werror
  - No new warnings introduced

---

## Issue 3: Non-lexical regions + splitting ideas as CTRR roadmap [DONE] (Review Needed)

**Objective:** Document the CTRR region inference roadmap for non-lexical region boundaries.

### P0: CTRR inference roadmap doc [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-ctrr-roadmap-doc (P0)
  - Created `docs/CTRR_REGION_INFERENCE_ROADMAP.md`
  - Outlines phases for non-lexical region inference

### P1: Emission Inventory [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-ctrr-emission-inventory (P1)
  - Inventoried all region_create/region_exit emission points
  - Documented current lexical-only emission pattern

### P2: Non-lexical Region End (Straightline) [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-nonlexical-region-end-straightline (P2)
  - Implemented non-lexical region exit for straightline code
  - Region exits emitted after last use, not at scope end

---

## Issue 4: Concurrency SMR techniques for internal runtime DS [DONE] (Review Needed)

**Objective:** Evaluate and document Safe Memory Reclamation techniques for concurrent runtime data structures.

### P0: SMR target inventory + decision matrix [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-smr-target-inventory (P0)
  - Identified candidate structures: metadata registry, intern table, global module map
  - Evaluated contention patterns for each

### P1: QSBR mapping to OmniLisp "quiescent points" [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-qsbr-quiescent-points (P1)
  - Mapped QSBR quiescent points to OmniLisp execution model
  - Region boundaries as natural quiescent points

### P2: Alternatives review [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-alternatives-review (P2)
  - Documented hazard pointers, Hyaline, publish-on-ping alternatives
  - Evaluated trade-offs for OmniLisp use case

### P3: Atomic Policy Wrapper [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-atomic-policy-wrapper (P3)
  - Created atomic wrapper macros for policy-based access
  - Abstracts memory ordering requirements

### P4: QSBR First Target Plan [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-qsbr-first-target-plan (P4)
  - Selected metadata registry as first QSBR target (low contention)
  - Documented implementation plan

---

## Issue 5: Build/Test Harness Stabilization [DONE] (Review Needed)

**Objective:** Stabilize build system and test harness to prevent agent stalls.

### P0: Define canonical commands [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p0-canonical-commands
  - Documented canonical build/test commands in BUILD_AND_TEST.md
  - `make -C runtime test`, `make -C csrc test`

### P1: Add csrc/tests harness [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p1-csrc-tests-makefile
  - Created Makefile for csrc/tests
  - Tests compile and run via make targets

### P2: Eliminate tracked build artifacts [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p2-untrack-binaries
  - Added .gitignore rules for build artifacts
  - Clean tree after tests

### P3: Make warning policy explicit [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p3-warning-policy
  - Documented warning flags in Makefiles
  - -Wall -Wextra standard, -Werror for release

### P4: Prevent partial compile-breaking edits [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p4-codegen-guardrails
  - Added compile check after codegen changes
  - Guardrails prevent broken generated code

---

## Issue 7: SYNTAX_REVISION.md Strict Character Calculus Implementation [DONE] (Review Needed)

**Objective:** Implement the Strict Character Calculus from `docs/SYNTAX_REVISION.md`.

**Completed (2026-01-14)**

### P1: Slot-Syntax Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p1-slot-syntax-functions
  - Support `(define add [x] [y] body)` slot-syntax function definitions.
  - Where: `csrc/codegen/codegen.c` (first pass detection, codegen_define)
  - What was changed:
    1. Added `first_pass` flag to CodeGenContext
    2. Modified first pass to detect slot-syntax functions
    3. Wrapped initialization code in `if (!ctx->first_pass)` guards
  - Verification: `(define add [x] [y] (+ x y))` compiles and `(add 3 4)` returns 7 ✓

### P2: Traditional Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p2-traditional-functions
  - Continue supporting `(define (f x y) body)` traditional syntax
  - Verification: `(define (mul a b) (* a b))` works ✓

### P3: Shorthand Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p3-shorthand-functions
  - Support `(define f x body)` shorthand syntax
  - Verification: `(define square n (* n n))` works ✓

### P4: Typed Let Bindings [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p4-typed-let
  - Support `(let [x {Int} 10] body)` typed let bindings
  - Where: `csrc/analysis/analysis.c`, `csrc/codegen/codegen.c`
  - Verification: `(let [x {Int} 10] (print x))` outputs 10 ✓

### P5: Sequential Let with Metadata [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p5-sequential-let
  - Support `(let ^:seq [a 5] [b (+ a 1)] body)` sequential bindings
  - Verification: `(let ^:seq [a 5] [b (+ a 1)] (print b))` outputs 6 ✓

### Additional Fixes (2026-01-14)

- [DONE] Lambda, fn, and -> codegen for slot syntax
  - Fixed inline lambda application `((lambda [x] body) arg)`
  - Added `->` as special form in codegen
  - Arrow syntax `(-> [x] [y] body)` works correctly

- [DONE] Typed variable definitions
  - Fixed `(define x-typed {Int} 42)` to correctly emit value (was emitting type)
  - Codegen now skips type annotation and uses actual value

- [DONE] set! codegen fix
  - Fixed bug that generated `return` statement in set! expression
  - Now correctly emits `(var = value)` as expression

### Pre-existing Issues (Not related to Issue 7)

- Multiline string handling in codegen
- Array literal inlining in expressions
- Some tests use variables before definition

---

## Issue 13: SYNTAX_REVISION.md Strict Character Calculus Implementation [N/A - Duplicate of Issue 7]

**Status:** N/A - This is a duplicate of Issue 7. All work tracked under Issue 7.

---

## Summary Statistics

| Status | Count |
|--------|-------|
| DONE (Review Needed) | ~50 |
| DONE | ~5 |
| N/A (Duplicates) | 2 |

**Next Step:** User review of all `[DONE] (Review Needed)` items to approve as `[DONE]`.
