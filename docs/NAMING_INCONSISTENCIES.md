# Naming Inconsistencies & Standardization Plan

This document tracks identified naming inconsistencies between the compiler (`csrc/`) and runtime (`runtime/`) and outlines a plan for standardization.

## 1. Naming Prefixes

| Component | Current Style | Examples | Proposed Standard |
|-----------|---------------|----------|-------------------|
| **Compiler** | `omni_` prefix | `omni_analyze`, `omni_get_escape_class` | Keep `omni_` (Public API) |
| **Runtime (Core)** | No prefix / Inconsistent | `inc_ref`, `free_obj`, `mk_int` | `omni_rt_` or `rt_`? |
| **Runtime (Arena)** | `arena_` prefix | `arena_create`, `arena_alloc` | `arena_` (Keep) |
| **Runtime (Region)** | `region_` / `iregion_` | `region_enter`, `iregion_alloc` | `region_` / `iregion_` (Keep) |
| **Runtime (Internal)** | `_` prefix | `_invalidate_weak` | `_omni_` or `static`? |

**Observation:** The compiler is well-namespaced. The runtime core (`runtime.c`) pollutes the global namespace with common names like `inc_ref` and `mk_int`.

## 2. Struct & Type Naming

| Component | Current Style | Examples | Status |
|-----------|---------------|----------|--------|
| **Compiler** | PascalCase | `VarUsage`, `AnalysisContext` | Good |
| **Runtime** | PascalCase | `Obj`, `Closure`, `Arena` | Good |
| **Internal** | `_` PascalCase | `_InternalWeakRef` | Replace with `WeakRefInternal`? |

**Issue:** `runtime.c` redefines typedefs (`Obj`, `SymComponent`) instead of including headers properly, leading to compiler warnings.

## 3. Function Naming Patterns

*   **Allocation:** `mk_int` (runtime) vs `arena_mk_int` (tests) vs `region_alloc` (region).
    *   *Proposal:* Standardize on `make_` or `alloc_`? E.g., `omni_make_int`.
*   **Deallocation:** `free_obj` vs `release_user_obj` vs `free_channel_obj`.
    *   *Proposal:* Use `free_` for deterministic freeing and `release_` for refcounting?
*   **Predicates:** `is_int`, `is_nil` (Good).

## 4. Leaky Abstractions

*   `should_use_arena` in `runtime/src/memory/arena.h`: This looks like compiler analysis logic (`find_arena_scopes`) residing in a runtime header. Runtime should just providing the mechanism, not the policy.

## 5. Action Plan

1.  **Namespace Runtime Core:** Rename core runtime functions to avoid collisions (e.g., `omni_inc_ref`, `omni_mk_int`).
2.  **Fix Header Hygiene:** Remove typedef redefinitions in `runtime.c`. Create a shared `types.h` or proper forward declaration header.
3.  **Standardize Allocation:** consistently use `alloc` for raw memory and `make` for objects?
4.  **Move Analysis Logic:** Move `find_arena_scopes` and `should_use_arena` out of runtime headers if they rely on AST/Compiler types.

## 6. Detailed Inventory

### Runtime Core (`runtime.c`)
*   `inc_ref` -> `omni_inc_ref`
*   `dec_ref` -> `omni_dec_ref`
*   `free_obj` -> `omni_free_obj`
*   `mk_int` -> `omni_make_int`
*   `is_int` -> `omni_is_int`

### Memory Modules
*   `arena.h`: Mostly consistent (`arena_`).
*   `region.h`: mostly consistent (`region_`, `iregion_`).

### Implemented Language in C
*   `prim_add_ints` -> `omni_prim_add`?
