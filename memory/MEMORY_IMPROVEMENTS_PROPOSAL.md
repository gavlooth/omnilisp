# Memory System Improvement Proposal

**Date:** 2026-04-21
**Scope:** Omni Lisp Runtime — dual-lane TEMP/ESCAPE region architecture
**Constraints:** No GC, no stop-the-world, RC-authoritative, maximum user transparency

---

## State of the Architecture

The current system (as of Revision XV, 2026-03-05) is already a well-engineered dual-lane bump allocator with:

- **TEMP/ESCAPE lanes** per `ScopeRegion` (`scope_region.c3:60-91`)
- **O(1) scope splice** via `scope_splice_escapes` (`scope_region_reset_adopt.c3:7-100`)
- **Write barrier** enforced on env insertion (`value_environment_barrier.c3`)
- **Epoch-based promotion** with memoization to avoid DAG explosion
- **Budgeted abort** with tri-state fallback to `copy_to_parent`
- **Fiber-temp pool** for eligible TEMP allocations

This proposal does not redesign the architecture. It closes the gap between the *specified* behavior and the *actual* runtime.

---

## Tier 1: Fix Actual Bugs and Leaks (Do First)

### 1.1 Fix Boundary Promotion Leaks on Failure Paths

Three confirmed leak paths exist in the promotion/escape code:

**Leak A: `copy_closure_to_parent` leaks wrapper on clone failure**
- **Where:** `eval_promotion_copy_wrapper_helpers.c3:196-202`
- **What:** `result = interp.alloc_value()` is allocated, then `copy_parent_clone_closure_payload` is called. If payload cloning fails (param alloc OOM), the function returns `null` without freeing `result` or registering a dtor.
- **Fix:** Use `defer` pattern: allocate wrapper → `defer` cleanup wrapper on failure path → allocate payload → on success, cancel deferred cleanup.

**Leak B: `promote_escape_cons` leaks partially built spine on budget abort**
- **Where:** `eval_promotion_escape_structured.c3:291-351`
- **What:** If `ctx.aborted` becomes true mid-loop (`:323` or `:326`), the function returns the original `v` but leaves already-allocated `head` (and tail cells) in the ESCAPE lane with no reachable reference.
- **Fix:** Track allocated cells in a temp array; on abort, walk that array and free each cell. Since these are freshly allocated bump cells, free is a no-op (they die with the scope) — but **they must be removed from the ESCAPE dtor list** if dtors were registered.

**Leak C: `boundary_build_destination_cons_escape` leaks `head` on tail alloc failure**
- **Where:** `eval_boundary_commit_escape_cons.c3:81-82`
- **What:** When `next_new = boundary_alloc_value_in_current_escape(...)` returns `null`, previously allocated `head` remains in target scope but unreachable.
- **Fix:** On allocation failure after partial construction, free allocated cells and abort the entire operation atomically.

**Leak D: `promote_escape_closure` leaks partially allocated closure**
- **Where:** `eval_promotion_escape_structured.c3:354-447`
- **What:** `cloned` is allocated via `alloc_escape`, then `cloned.params` and `cloned.type_sig` are allocated. If later allocations fail, the earlier `cloned` block is not freed and has no dtor.
- **Fix:** Same defer pattern: allocate closure struct → defer free on failure → allocate params → defer free params on failure → allocate type_sig → success cancels defers.

### 1.2 Fix Stale `scope_gen` Stamps After `scope_splice_escapes`

**Where:** `eval_promotion_copy.c3:251-253` (documented as known limitation)

After `scope_splice_escapes` moves ESCAPE chunks from child to parent, allocations in those chunks retain the child's `escape_generation`. Fast-reuse logic (`copy_to_parent_try_fast_reuse`) falls back to pointer-range checks, which is correct but slower.

**Fix:** After splicing, increment the parent's `escape_generation` and **rewrite stamps** on all spliced values. Options:

- **Option A (preferred):** During splice, walk the spliced ESCAPE chunk list and rewrite `scope_gen` from `child.escape_generation` to `parent.escape_generation`. This is O(N) over the spliced graph — acceptable because it's already amortized against the return cost.
- **Option B:** Add a `scope_splice_epoch` counter to `ScopeRegion`. Stamps encode `(generation, splice_epoch)`. This avoids rewriting but complicates comparison logic.

Recommendation: Option A. Add a `scope_rewrite_escape_stamps(ScopeRegion* child, ScopeRegion* parent)` helper called inside `scope_splice_escapes` after chunk transfer. Use the existing iterative graph walk machinery (same as `copy_to_parent` but write-only).

### 1.3 Add `defer` to Multi-Resource Allocation Functions

**Where:**
- `value_tensor_clone.c3:13-221` — `tensor_clone_payload` mallocs shape, strides, data, axes. ~15 manual error-path cleanup sites.
- `eval_promotion_copy_route_helpers.c3:67-133, 135-220, 222-317` — `copy_array_to_parent`, `copy_hashmap_backed_to_parent`, `copy_method_table_to_parent` each have 3-5 manual free paths.
- `eval_promotion_escape_structured.c3:53-109, 111-181, 183-263` — Escape promotion helpers with manual partial cleanup.

**Fix:** Use a cleanup struct:
```c3
struct TensorCloneCleanup {
    TensorVal* tensor;
    bool owned;
}
fn void tensor_clone_cleanup(TensorCloneCleanup* c) {
    if (c.owned && c.tensor != null) tensor_free_payload(c.tensor);
}
// In tensor_clone_payload:
TensorCloneCleanup cleanup = { .tensor = tensor, .owned = true };
defer tensor_clone_cleanup(&cleanup);
// ... allocate sub-buffers
// On success path:
cleanup.owned = false;
```

---

## Tier 2: Eliminate Remaining Deep Copies from Hot Paths

### 2.1 JIT Should Emit Lane-Specific Allocation Opcodes

**Current state:** The JIT is lane-agnositc. It calls `jit_cons` → `make_cons`, which dynamically checks `escape_env_mode` and compares `scope_gen` stamps at runtime (`value_constructors_core.c3:158`). This costs ~6-10 instructions per cons cell.

**Improvement:** When the JIT can prove a value is being built as a function return (tail position in the AST), emit:
- `jit_cons_escape(car, cdr)` — directly allocates in ESCAPE lane, skips the runtime `scope_gen` check
- `jit_make_array_escape(len)` — directly allocates array in ESCAPE lane
- `jit_make_string_escape(len)` — directly allocates string in ESCAPE lane

This eliminates the dynamic barrier check on the hottest path (list construction in tail position). The barrier still exists in the interpreter and for dynamic boundaries.

**Implementation:**
1. Add `jit_cons_escape`, `jit_make_array_escape`, `jit_make_string_escape` to `jit_apply_helpers.c3`.
2. In `jit_compile_expr_core.c3`, detect tail-position cons/array/string construction.
3. Emit the `_escape` variant instead of the generic variant.
4. Add compiler tests verifying the generated code calls the escape variant.

### 2.2 TCO Recycling Should Use Lane Reset, Not Scope Recreation

**Current state:** `jit_eval` trampoline for TCO creates a fresh `ScopeRegion` per bounce via `scope_create`, copies env chain via `copy_tco_env_chain`, and releases the old scope.

**Cost per bounce:**
- 1 `scope_create` (~15 instructions + possible malloc)
- N `copy_to_parent` calls per binding
- 1 `scope_release` (dtors + chunk free)

**Improvement:** Under dual-lane, TCO recycling becomes:
1. Reset the TEMP lane: `temp_bump` back to chunk start, run `temp_dtors`, free overflow temp chunks.
2. The ESCAPE lane is untouched.
3. Env bindings already in the ESCAPE lane survive without copying.

**Implementation:**
1. Add `scope_reset_temp_lane(ScopeRegion* scope)` to `scope_region_reset_helpers.c3`.
2. Update `jit_eval_in_call_scope` to use lane reset instead of `scope_create` + `scope_release` for TCO bounces.
3. Ensure env bindings allocated during the loop are correctly routed to ESCAPE.

**Expected impact:** Eliminates the dominant per-bounce cost in named-let loops (>50% speedup on tight recursive loops).

### 2.3 Reduce `copy_to_parent` Calls on Env-Copy Paths

**Current state:** `copy_env_to_scope` (closure capture) is a COLD path, but `copy_tco_env_chain` is HOT. Both deep-copy the env chain.

**Improvement:** Detect when the source env is already in the target scope chain (or root scope). If so, reuse directly instead of copying. This is partially implemented (`copy_env_to_scope` already checks target-chain membership), but `copy_tco_env_chain` does not.

**Where:** `eval_env_copy_values.c3` and JIT env-copy helpers.

---

## Tier 3: Structural Hardening

### 3.1 Add `$assert` for Critical Struct Sizes

**Policy violation:** `docs/C3_STYLE.md` says to use `$assert` for struct sizes. None exist for interop-critical structs.

| Struct | File | Why |
|--------|------|-----|
| `Value` | `value_core_types.c3:189` | Tagged union; size changes break FFI |
| `ScopeRegion` | `scope_region.c3:60` | Core allocator state |
| `Closure` | `value_runtime_types.c3:36` | Env pointer layout |
| `TensorVal` | `value_runtime_types.c3:227` | Vulkan/CPU interop |
| `HashMap` | `value_runtime_types.c3:111` | Serialization layout |
| `Array` | `value_runtime_types.c3:180` | Dynamic array backing |
| `Env` | `value_environment_storage.c3:89` | Binding storage |
| `Interp` | `value_interp_state.c3:39` | Interpreter state |

**Example:**
```c3
$assert(Value.sizeof == 24, "Value size changed — update FFI bindings");
$assert(ScopeRegion.sizeof <= 128, "ScopeRegion fits in two cache lines");
```

### 3.2 Add Memory Pressure Telemetry for Adaptive Behavior

The runtime currently has no way to observe memory pressure. Add lightweight counters:

```c3
struct MemoryTelemetry {
    usz total_temp_bytes_allocated;      // cumulative TEMP allocations
    usz total_escape_bytes_allocated;    // cumulative ESCAPE allocations
    usz temp_bytes_freed;                // freed via scope_release
    usz escape_bytes_spliced;            // transferred via splice
    usz copy_to_parent_bytes;            // deep-copy cost
    usz promotion_bytes;                 // promotion cost
    usz scope_create_count;              // scope creation rate
    usz scope_recycle_count;             // freelist hit rate
    usz fiber_temp_hits;                 // fiber-temp pool usage
    usz fiber_temp_misses;               // fiber-temp bypasses
}
```

Expose via:
- `runtime-memory-stats` primitive for user inspection
- Optional JSON dump at process exit (`OMNI_MEM_TELEMETRY=1`)

This enables:
1. Identifying which primitives allocate the most
2. Measuring whether dual-lane is actually reducing copies
3. Tuning chunk sizes and fiber-temp eligibility

### 3.3 Validate `scope_splice_escapes` with a Debug Reachability Scan

**Where:** `scope_region_reset_adopt.c3:57-68`

After splicing ESCAPE chunks to the parent and before freeing TEMP chunks, add a `$if DEBUG_BUILD:` scan that verifies no ESCAPE root retains a TEMP edge. This is the debug invariant from DESTINATION_ARENA_PLAN:

> "Boundary debug invariant: any committed ESCAPE root must not retain reachable Omni-owned edges into TEMP."

**Implementation:**
```c3
$if DEBUG_BUILD:
    // After splice, before temp teardown
    foreach (root : escape_roots) {
        assert_no_reachable_temp(root, child);
    }
```

This catches classification bugs early (similar to ASAN but for lane semantics).

---

## Tier 4: Long-Term Architectural Evolution

### 4.1 Typed Arrays and Inline Capacity for Small Collections

For small collections (arrays ≤ 4 elements, dicts ≤ 4 entries), store payloads inline in the `Value` struct instead of a separate heap allocation. This eliminates:
- Chunk allocation overhead
- Cache miss on first element access
- Destructor registration

**Design sketch:**
```c3
union ValuePayload {
    // Existing fields...
    struct {
        Value*[4] inline_items;
        usz len;
        Value** heap_items;  // null if len ≤ 4
    } small_array;
}
```

This is a major change (affects `Array` struct, all array accessors, JIT array opcodes). Defer until after the simpler fixes above land.

### 4.2 Scope Paging / Size Classes for TEMP Lane

Currently, TEMP chunks grow via doubling up to 64KB. For workloads with many small allocations (e.g., cons-heavy programs), this causes fragmentation. Consider:

- Small chunk class: 4KB (for scopes with < 100 allocations)
- Medium chunk class: 16KB
- Large chunk class: 64KB

Select class based on `scope_alloc_count` heuristic. This is a tuning change, not an architectural change.

### 4.3 Per-Thread Scope Pools

The current `g_scope_freelist` is global. Under high concurrency, this becomes a contention point. Replace with per-thread scope pools:

```c3
tlocal ScopeRegion* g_thread_scope_pool;
```

`scope_create` draws from the thread pool; `scope_release` returns to it. Cross-thread scope transfer (for scheduler offload) uses a dedicated transfer pool. This requires NUMA-aware design for the scheduler.

---

## Implementation Order

Status note, 2026-04-21: this table is historical. Proposal-scoped
`MEMORY-*` items have been backfilled and closed in
`docs/todo_parts/todo_part_15.md`, and the remaining Tier 2/Tier 4 ideas are
design notes requiring measurement or explicit new backlog items before direct
implementation. Do not treat this table as the current live queue.

| Priority | Task | Files Touched | Validation |
|----------|------|---------------|------------|
| P0 | Fix Leak A-D | `eval_promotion_copy_wrapper_helpers.c3`, `eval_promotion_escape_structured.c3`, `eval_boundary_commit_escape_cons.c3` | ASAN clean, boundary tests |
| P0 | Fix stale `scope_gen` after splice | `scope_region_reset_adopt.c3`, `eval_promotion_copy.c3` | Copy-site tests, reuse tests |
| P0 | Add `defer` to tensor_clone + promotion helpers | `value_tensor_clone.c3`, `eval_promotion_copy_route_helpers.c3`, `eval_promotion_escape_structured.c3` | ASAN clean |
| P1 | Add `$assert` for struct sizes | `value_core_types.c3`, `scope_region.c3`, `value_runtime_types.c3`, `value_environment_storage.c3`, `value_interp_state.c3` | Build passes |
| P1 | Add debug reachability scan in `scope_splice_escapes` | `scope_region_reset_adopt.c3` | Debug build passes |
| P1 | Add memory telemetry | New file + `value_interp_state.c3` | Telemetry output test |
| P2 | JIT escape allocation opcodes | `jit_apply_helpers.c3`, `jit_compile_expr_core.c3`, JIT tests | Compiler tests pass |
| P2 | TCO lane reset | `jit_eval_scopes.c3`, `scope_region_reset_helpers.c3` | TCO benchmark |
| P3 | Typed arrays / inline capacity | `value_runtime_types.c3`, array primitives, JIT | Array tests |
| P3 | Per-thread scope pools | `scope_region_temp_pool.c3`, scheduler | Concurrency stress tests |

---

## Confidence Assessment

| Improvement | Confidence | Risk |
|-------------|------------|------|
| Fix Leak A-D | 99% | Low — bounded scope, known patterns |
| Fix stale scope_gen | 95% | Low — proven approach, well-tested walk infra |
| JIT escape opcodes | 90% | Medium — requires JIT analysis pass |
| TCO lane reset | 85% | Medium — sensitive to env semantics |
| Inline capacity | 70% | High — pervasive struct change |
| Per-thread pools | 75% | High — threading complexity |

---

## Notes

- The dual-lane architecture is correct. Do not redesign. Refine.
- All changes must preserve: no GC, RC-authoritative, deterministic cleanup, ASAN clean.
- The "maximum flexibility to user" constraint means: no user-visible allocation hints, no manual memory management in Lisp surface, no `free`/`malloc` primitives. The runtime must be fully automatic.
- Performance measurement before and after each change is mandatory. Use `OMNI_MEM_TELEMETRY` (once added) to quantify copy reduction.
