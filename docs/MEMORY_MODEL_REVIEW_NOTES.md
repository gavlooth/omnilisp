# Memory Model Review Notes (2026-01-05)

<!-- Internal review notes so we can resume later without re-scanning. -->

## Scope (files read)
- README.md
- docs/MEMORY_MODEL_TECHNICAL.md
- docs/MEMORY_OPTIMIZATION_OPPORTUNITIES.md
- runtime/src/runtime.c
- runtime/src/memory/component.c
- runtime/src/memory/component.h
- runtime/src/memory/symmetric.c
- runtime/src/memory/symmetric.h
- runtime/src/memory/region.c
- runtime/src/memory/region.h
- runtime/src/memory/handle.c
- runtime/src/memory/handle.h
- runtime/src/memory/slot_pool.h
- csrc/analysis/analysis.c
- csrc/analysis/analysis.h
- csrc/analysis/component.c
- csrc/analysis/static_sym.c
- csrc/analysis/tether_elision.c
- csrc/codegen/codegen.c

## Findings (architectural mismatches and risks)

1) Codegen emits component/tether APIs that do not exist in the Obj runtime layout.
   Comment: Generated frees/tethers assume `Obj*` has a `comp` field, but `Obj` has no such field; only `SymObj` has `comp`. This is a compile/link break or a semantic no-op depending on build mode.
   Refs: csrc/codegen/codegen.c:1405-1441, runtime/src/runtime.c:219-232, runtime/src/memory/symmetric.h:31-45

2) `free_scc_static()` is emitted by codegen but is not implemented anywhere.
   Comment: The compiler can output calls that the runtime does not provide.
   Refs: csrc/codegen/codegen.c:1405-1408 (no definition found in runtime/src or csrc)

3) Component system is effectively inert in the runtime path.
   Comment: `SymObj->comp` is zeroed in `sym_obj_new` and never set; `sym_component_add_member` is never called; `sym_ctx_link` only merges components if `comp` is already set, which never happens.
   Refs: runtime/src/memory/symmetric.c:71-79, runtime/src/memory/symmetric.c:316-324, runtime/src/memory/component.c:140-152 (definition only)

4) Handle/slot-pool safety is not integrated with allocation/free paths.
   Comment: Constructors (`mk_int`, `mk_pair`, etc.) use `malloc`, and free paths call `free` directly; pool/handle helpers exist but are only used in tests. If a pool-allocated object ever enters these paths, handles will remain valid after free (unsound).
   Refs: runtime/src/runtime.c:416-492, runtime/src/runtime.c:800-914, runtime/src/memory/handle.c:122-191, runtime/tests/test_slot_pool.c (only caller of alloc/free pool)

5) Region system conflicts with stated C99-only requirement and has stubbed exit semantics.
   Comment: `region.c` uses `<stdatomic.h>` (C11), and `region_exit` only marks closed without freeing objects/refs; bulk free only happens on `region_context_free`.
   Refs: runtime/src/memory/region.c:6-13, runtime/src/memory/region.c:152-170

6) Region-RC fusion is mostly placeholder logic.
   Comment: `region_inc_ref/region_dec_ref` contain no real accounting; comments acknowledge missing region membership for standard Obj. This means fusion optimization cannot work as described yet.
   Refs: runtime/src/memory/region.c:335-357

7) Tether elision uses a heuristic that can be unsound.
   Comment: Elision assumes component match based solely on `shape == SHAPE_CYCLIC`, not SCC identity; this can drop tethers for unrelated cyclic components.
   Refs: csrc/analysis/tether_elision.c:39-48

8) Concurrency safety is undefined for core memory structures.
   Comment: Global freelist, weak-ref list, and stack pool are mutable without locks; refcount operations are non-atomic; pthreads are in use elsewhere. This is a correctness risk if objects cross threads.
   Refs: runtime/src/runtime.c:308-316, runtime/src/runtime.c:352-374, runtime/src/runtime.c:876-914

## Constructive criticism (design pressure points)
- The memory model has multiple partially implemented strategies (SCC Tarjan, component symmetric RC, region/arena). Without a single canonical path that the compiler targets, improvements will keep landing in dead code paths and correctness gaps will persist.
- “No new programmer restrictions” means the compiler must be conservative and the runtime must be robust; that increases the need for sound defaults (pool allocation for borrowable objects, conservative RC when analysis is unsure).

## Plan options (not chosen yet)

Plan A — Align codegen/runtime APIs (components + SCC)
- Step 1: Inventory all emitted memory APIs and map to runtime symbols; list missing/unused paths.
- Step 2: Choose canonical cycle strategy (component vs SCC) and remove/guard unused emission.
- Step 3: Implement missing runtime hooks or adjust codegen output accordingly.
- Step 4: Add tests first (compiler output + runtime behavior), then implement, document, commit.

Plan B — Slot-pool & borrow safety integration
- Step 1: Trace all borrowable paths and allocations; identify where pool alloc must be used.
- Step 2: Route allocations to slot-pool when borrowable; ensure free paths use `handle_free_obj`.
- Step 3: Eliminate unsafe IPGE fallback when pool is available.
- Step 4: Add tests first for UAF detection and handle invalidation, then implement, document, commit.

Plan C — Region exit + Region-RC fusion hardening
- Step 1: Define concrete region lifecycle semantics (what exit guarantees).
- Step 2: Implement bulk free on `region_exit` and consistent component cleanup.
- Step 3: Wire real ref accounting or drop stubs until ready.
- Step 4: Add tests first (isolation + bulk free), then implement, document, commit.

## Open questions for later
- Which cycle strategy is the canonical runtime path the compiler should target (component or SCC)?
- Should pool allocation be the default for all heap objects (soundness-first) or only for proven-borrowable objects (performance-first)?
- Is cross-thread sharing of `Obj*` allowed without explicit ownership transfer? If yes, RC and global lists need synchronization.
