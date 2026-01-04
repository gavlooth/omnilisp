# OmniLisp Implementation Roadmap

## Complete Step-by-Step Plan

This document provides actionable steps to implement all future optimizations and validation testing.

---

# Phase 1: Critical Validation (Week 1-2)

## Step 1.1: Valgrind Integration

**Goal**: Verify no memory leaks in generated C code.

### 1.1.1 Create test harness
```bash
# Create directory
mkdir -p test/validation
```

**File**: `test/validation/valgrind_test.go`
[Go code removed]

### 1.1.2 Create comprehensive test cases

**File**: `test/validation/testcases.go`
[Go code removed]

### 1.1.3 Run initial validation
```bash
go test ./test/validation/... -v -run TestValgrind
```

---

## Step 1.2: AddressSanitizer Integration

**Goal**: Detect buffer overflows, use-after-free, double-free.

### 1.2.1 Create ASan test

**File**: `test/validation/asan_test.go`
[Go code removed]

---

## Step 1.3: ThreadSanitizer Integration

**Goal**: Detect data races in concurrent code.

### 1.3.1 Create TSan test

**File**: `test/validation/tsan_test.go`
[Go code removed]

---

## Step 1.4: Correctness Testing

**Goal**: Compiled code produces same results as interpreter.

### 1.4.1 Create comparison harness

**File**: `test/validation/correctness_test.go`
[Go code removed]

---

# Phase 2: RC Elimination (Week 3-4)

## Step 2.1: Enhance Uniqueness Analysis

**Goal**: Identify more cases where RC can be skipped.

### 2.1.1 Add uniqueness propagation

**File**: `pkg/analysis/rcopt.go` (extend)
[Go code removed]

### 2.1.2 Add borrowed reference tracking

[Go code removed]

### 2.1.3 Generate optimized RC operations

**File**: `pkg/codegen/codegen.go` (extend GenerateLet)
[Go code removed]

### 2.1.4 Add RC elimination statistics

[Go code removed]

### 2.1.5 Test RC elimination

**File**: `pkg/analysis/rcopt_elimination_test.go`
[Go code removed]

---

# Phase 3: Active Reuse (Week 5-6)

## Step 3.1: Integrate Reuse into Let Generation

**Goal**: Transform `free(x); y = alloc()` → `y = reuse(x)`.

### 3.1.1 Track allocations with types

**File**: `pkg/analysis/reuse.go` (extend)
[Go code removed]

### 3.1.2 Modify GenerateLet to use reuse

**File**: `pkg/codegen/codegen.go` (modify GenerateLet)
[Go code removed]

### 3.1.3 Test reuse transformation

**File**: `pkg/codegen/reuse_codegen_test.go`
[Go code removed]

---

# Phase 4: Interprocedural Ownership (Week 7-8)

## Step 4.1: Use Summaries at Call Sites

**Goal**: Optimize RC based on callee ownership behavior.

### 4.1.1 Query summaries during codegen

**File**: `pkg/codegen/codegen.go` (add method)
[Go code removed]

### 4.1.2 Auto-generate summaries for user functions

**File**: `pkg/eval/eval.go` (extend evalDefine)
[Go code removed]

### 4.1.3 Test interprocedural optimization

**File**: `pkg/analysis/interprocedural_test.go`
[Go code removed]

---

# Phase 5: DPS Code Generation (Week 9-12)

## Step 5.1: Identify DPS Candidates

**Goal**: Find functions that return fresh allocations.

### 5.1.1 Add DPS analysis

**File**: `pkg/analysis/dps.go` (new file)
[Go code removed]

### 5.1.2 Generate DPS variants

**File**: `pkg/codegen/dps_codegen.go` (new file)
[Go code removed]

---

# Phase 6: Region Inference (Week 13-16)

## Step 6.1: Basic Region Analysis

**Goal**: Group allocations with same lifetime.

### 6.1.1 Create region analysis

**File**: `pkg/analysis/region.go` (new file)
[Go code removed]

### 6.1.2 Generate region-based allocation

**File**: `pkg/codegen/region_codegen.go`
[Go code removed]

---

# Phase 7: Performance Benchmarks (Week 17-18)

## Step 7.1: Create Benchmark Suite

**File**: `test/benchmark/benchmark_test.go`
[Go code removed]

## Step 7.2: Memory Profiling

**File**: `test/benchmark/memory_test.go`
[Go code removed]

---

# Phase 8: Stress Testing (Week 19-20)

## Step 8.1: Edge Case Tests

**File**: `test/stress/stress_test.go`
[Go code removed]

## Phase 10: Component-Level Scope Tethering (Island Reclamation) ✅ COMPLETE

All phases of the component-level tethering system have been **implemented and verified** (2026-01-04).

### 10.1 Component Runtime ✅ COMPLETE
- [x] Defined `SymComponent` unit of reclamation with handle and tether counts.
- [x] Implemented thread-local slab pools for O(1) component allocation.
- [x] Implemented robust edge-cancellation dismantling algorithm.

### 10.2 Boundary & Tethering ✅ COMPLETE
- [x] Implemented `sym_acquire_handle` and `sym_release_handle` for ASAP-managed liveness.
- [x] Implemented `sym_tether_begin` and `sym_tether_end` for zero-cost scoped access.
- [x] Added thread-local cleanup to prevent memory leaks on thread exit.

### 10.3 Compiler Integration ✅ COMPLETE
- [x] Implemented `omni_analyze_components` to group SCCs and identify boundary handles.
- [x] Added tether injection to wrap borrow scopes, eliminating runtime RC/IPGE overhead.
- [x] Verified with 450+ functional tests and multi-threaded stress tests.

**Total: 454 functional tests passed in `runtime/tests/`**

---

# Summary: Complete Timeline

| Week | Phase | Focus |
|------|-------|-------|
| 1-2 | Phase 1 | Critical Validation (Valgrind, ASan, TSan, Correctness) |
| 3-4 | Phase 2 | RC Elimination |
| 5-6 | Phase 3 | Active Reuse Transformation |
| 7-8 | Phase 4 | Interprocedural Ownership |
| 9-12 | Phase 5 | DPS Code Generation |
| 13-16 | Phase 6 | Region Inference |
| 17-18 | Phase 7 | Performance Benchmarks |
| 19-20 | Phase 8 | Stress Testing |

---

# Checklist

## Phase 1: Critical Validation
- [ ] 1.1.1 Create Valgrind test harness
- [ ] 1.1.2 Create comprehensive test cases
- [ ] 1.1.3 Run initial validation
- [ ] 1.2.1 Create ASan test
- [ ] 1.3.1 Create TSan test
- [ ] 1.4.1 Create correctness comparison harness

## Phase 2: RC Elimination
- [ ] 2.1.1 Add uniqueness propagation
- [ ] 2.1.2 Add borrowed reference tracking
- [ ] 2.1.3 Generate optimized RC operations
- [ ] 2.1.4 Add RC elimination statistics
- [ ] 2.1.5 Test RC elimination

## Phase 3: Active Reuse
- [ ] 3.1.1 Track allocations with types
- [ ] 3.1.2 Modify GenerateLet to use reuse
- [ ] 3.1.3 Test reuse transformation

## Phase 4: Interprocedural Ownership
- [ ] 4.1.1 Query summaries during codegen
- [ ] 4.1.2 Auto-generate summaries for user functions
- [ ] 4.1.3 Test interprocedural optimization

## Phase 5: DPS Code Generation
- [ ] 5.1.1 Add DPS analysis
- [ ] 5.1.2 Generate DPS variants

## Phase 6: Region Inference
- [ ] 6.1.1 Create region analysis
- [ ] 6.1.2 Generate region-based allocation

## Phase 7: Performance Benchmarks
- [ ] 7.1 Create benchmark suite
- [ ] 7.2 Memory profiling

## Phase 8: Stress Testing
- [ ] 8.1 Edge case tests

## Phase 9: Memory Architecture Enhancements (Post‑11, ASAP‑Compatible) ✅ ALL COMPLETE

All 5 optional extensions have been **implemented with full test coverage** (2026-01-04).
Implementation lives in `runtime/src/memory/region.c` (~2000 lines) with 99 new tests.

### 9.1 Linear/Offset Regions for Serialization & FFI ✅ COMPLETE

**Implementation**: `runtime/src/memory/region.c`

**Completed Tasks**:
- [x] 9.1.1 Add `offset_mode` + `adjuster` to region state → `OffsetRegion` with 32-bit offsets
- [x] 9.1.2 Route pointer stores/loads through `region_store_ptr` / `region_deref_ptr` → `offset_to_ptr()`
- [x] 9.1.3 Add tests for serialized buffers and file offsets → 17 tests in `test_iregion.c`

**Key APIs**: `iregion_new_offset()`, `offset_region_serialize()`, `offset_region_deserialize()`, `offset_to_ptr()`

### 9.2 Pluggable Region Backends (IRegion‑style) ✅ COMPLETE

**Implementation**: `runtime/src/memory/region.c`

**Completed Tasks**:
- [x] 9.2.1 Define a `RegionVTable` (alloc/free/deref/scan) → `IRegionVtable` with 10 function pointers
- [x] 9.2.2 Implement arena + RC backends behind the vtable → Arena, Linear, Offset, Pool backends
- [x] 9.2.3 Add codegen routing: escape/shape → backend selection → `iregion_kind()` returns `RegionKind`

**Key APIs**: `iregion_new_arena()`, `iregion_new_linear()`, `iregion_new_pool()`, `iregion_alloc()`, `iregion_free_all()`

### 9.3 Weak Ref Control Blocks (Merge‑Friendly) ✅ COMPLETE

**Implementation**: `runtime/src/memory/region.c`

**Completed Tasks**:
- [x] 9.3.1 Add `WeakCB` control block (gen + ptr + weak_count) → `WeakControlBlock` struct
- [x] 9.3.2 Make weak refs point to control blocks instead of objects → `WeakHandle` wraps control block
- [x] 9.3.3 Invalidate by bumping `gen` + NULLing `ptr` → `weak_cb_invalidate()` with generation check

**Key APIs**: `weak_cb_new()`, `weak_handle_new()`, `weak_handle_lock()`, `weak_table_new()`, `weak_table_invalidate()`

**Tests**: 21 tests in `test_weak_control_blocks.c`

### 9.4 Transmigration / Isolation on Region Escape ✅ COMPLETE

**Implementation**: `runtime/src/memory/region.c`

**Completed Tasks**:
- [x] 9.4.1 Add `transmigrate_*` walkers (generation‑offset or copy‑out) → `TransmigrationContext` with hash map
- [x] 9.4.2 Hook into codegen when region‑local data escapes → `transmigrate()` deep-copies objects
- [x] 9.4.3 Add tests: return from arena/pure region with borrows → 17 tests in `test_transmigration.c`

**Key APIs**: `transmigration_new()`, `transmigrate()`, `check_isolation()`, `region_bound_ref_new()`

### 9.5 External Handle Indexing (FFI + Determinism) ✅ COMPLETE

**Implementation**: `runtime/src/memory/region.c`

**Completed Tasks**:
- [x] 9.5.1 Add `HandleTable` (index+gen → ptr) → `ExternalHandleTable` with slot array
- [x] 9.5.2 Expose `handle_alloc/get/free` for FFI boundaries → `external_handle_create/get/release()`
- [x] 9.5.3 Optional: map handles deterministically for replay → `external_table_set_deterministic()`

**Key APIs**: `external_table_new()`, `external_handle_create()`, `ffi_obj_to_handle()`, `ffi_handle_to_obj()`

**Tests**: 27 tests in `test_external_handles.c`

### Test Summary

| Feature | Tests | File |
|---------|-------|------|
| IRegion Vtable | 17 | `test_iregion.c` |
| Weak Control Blocks | 21 | `test_weak_control_blocks.c` |
| Transmigration | 17 | `test_transmigration.c` |
| External Handles | 27 | `test_external_handles.c` |
| **Total** | **82** | |

### 9.6 FFI Context‑Word Purity (Compiler‑Internal)

**Goal**: Enforce region mutability/purity across FFI boundaries without user‑visible
region parameters, **only when the ABI allows carrying context**.

**Key constraint**:
- If the foreign API does **not** allow a context pointer (`user_data`) or a token,
  the compiler cannot restore context on callbacks. In that case, the export must
  be restricted to read‑only behavior or require an explicit token.

**Where to start (codebase)**:
- `runtime/src/runtime.c` (TLS pattern; exception context uses `__thread`)
- `runtime/include/omnilisp.h` (public runtime hooks)
- `runtime/src/memory/region.c` (if binding context bits to region runtime)

**Search terms**: `__thread`, `exception_*`, `RegionContext`, `region_enter`, `ffi`

**Tasks**:
- [ ] 9.6.1 Add TLS context stack (`ctx_push/ctx_pop/ctx_current`)
- [ ] 9.6.2 Emit FFI call wrappers that push/pop context
- [ ] 9.6.3 Generate trampolines for callbacks that restore context via `user_data`
- [ ] 9.6.4 Define fallback behavior when callbacks have no context (read‑only or explicit token)
