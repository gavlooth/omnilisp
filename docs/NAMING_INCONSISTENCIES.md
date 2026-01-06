# Naming Inconsistencies & Standardization Plan

This document tracks identified naming inconsistencies between the compiler (`csrc/`) and runtime (`runtime/`) and outlines a plan for standardization.

## 1. Naming Prefixes

| Component | Current Style | Examples | Recommendation |
|-----------|---------------|----------|----------------|
| **Compiler** | `omni_` prefix | `omni_analyze`, `omni_get_escape_class` | Keep `omni_` (Public API) |
| **Runtime (Core)** | No prefix / Short | `inc_ref`, `free_obj`, `mk_int` | **Keep as-is** (see below) |
| **Runtime (Arena)** | `arena_` prefix | `arena_create`, `arena_alloc` | Keep `arena_` |
| **Runtime (Region)** | `region_` / `iregion_` | `region_enter`, `iregion_alloc` | Keep `region_` / `iregion_` |
| **Runtime (Internal)** | `_` prefix | `_invalidate_weak` | Use `static` instead |

### Revised Analysis

**Original concern:** The runtime core "pollutes the global namespace" with names like `inc_ref` and `mk_int`.

**Counter-argument:** This concern is overstated for several reasons:

1. **OmniLisp compiles to standalone binaries.** The runtime is statically linked with generated code. There is no third-party library to collide with - the runtime IS the world.

2. **Short names are idiomatic C.** Functions like `mk_int`, `is_nil`, `free_obj` follow established C conventions. Adding `omni_` everywhere makes code verbose without real benefit:
   ```c
   // Current (readable)
   Value* x = mk_int(42);
   if (is_nil(x)) free_obj(x);

   // With omni_ prefix (verbose)
   Value* x = omni_make_int(42);
   if (omni_is_nil(x)) omni_free_obj(x);
   ```

3. **Prefixes already provide implicit namespacing:**
   - `mk_*` = make/construct
   - `is_*` = type predicate
   - `free_*` = deallocate
   - `arena_*` = arena operations
   - `region_*` = region operations

4. **Static linkage solves internal collisions.** Internal functions should use `static`, not `_omni_` prefixes.

## 2. Struct & Type Naming

| Component | Current Style | Examples | Status |
|-----------|---------------|----------|--------|
| **Compiler** | PascalCase | `VarUsage`, `AnalysisContext` | Good |
| **Runtime** | PascalCase | `Obj`, `Closure`, `Arena` | Good |
| **Internal** | `_` PascalCase | `_InternalWeakRef` | Use `static` or rename to `WeakRefInternal` |

**Real Issue:** `runtime.c` redefines typedefs (`Obj`, `SymComponent`) instead of including headers properly, leading to compiler warnings. **This is a bug to fix.**

## 3. Function Naming Patterns

*   **Allocation:** `mk_int` (runtime) vs `arena_mk_int` (tests) vs `region_alloc` (region).
    *   *Recommendation:* Keep `mk_*` for value construction, `alloc` for raw memory. Context (arena/region) provides the namespace.
*   **Deallocation:** `free_obj` vs `release_user_obj` vs `free_channel_obj`.
    *   *Recommendation:* Use `free_*` for deterministic freeing, `release_*` for refcount decrement that may or may not free.
*   **Predicates:** `is_int`, `is_nil` - Good as-is.

## 4. Leaky Abstractions

*   `should_use_arena` in `runtime/src/memory/arena.h`: This looks like compiler analysis logic (`find_arena_scopes`) residing in a runtime header.
    *   *Recommendation:* Move to compiler if it depends on AST types. Runtime provides mechanism, compiler provides policy.

## 5. Revised Action Plan

### DO:
1. **Fix Header Hygiene:** Remove typedef redefinitions in `runtime.c`. Create proper forward declaration header or include paths.
2. **Use `static` for internal functions:** Replace `_` prefix with `static` linkage.
3. **Document naming conventions:** Add a `CONVENTIONS.md` explaining the pattern (mk_, is_, free_, etc.).

### DON'T:
1. **Don't mass-rename to `omni_*`:** Too verbose, no real collision risk.
2. **Don't add prefixes to fix imaginary problems:** Only rename on actual collisions.

## 6. Real Fixes Needed

### Priority 1: Typedef Redefinitions (Bug)
```c
// runtime.c currently has:
typedef struct Obj Obj;  // DUPLICATE - already in types.h

// Fix: Remove duplicate, ensure proper includes
```

### Priority 2: Static for Internal Functions
```c
// Current:
void _invalidate_weak(WeakRef* ref);

// Better:
static void invalidate_weak(WeakRef* ref);  // file-scope only
```

### Priority 3: Move Analysis Out of Runtime
Move `find_arena_scopes` and `should_use_arena` to compiler if they use AST types.

## 7. Summary

| Issue | Severity | Action |
|-------|----------|--------|
| Typedef redefinitions | **High** | Fix includes, remove duplicates |
| Internal function visibility | Medium | Use `static` instead of `_` prefix |
| Analysis in runtime headers | Medium | Move to compiler module |
| Runtime function names | **Low** | Keep as-is, document conventions |
| `omni_*` prefix everywhere | N/A | **Don't do this** |
