# Complete Integration Plan: Compiler & Runtime Wiring

## 1. Executive Summary
The **Region-Based RC (RC-G)** runtime is built and tested. The **Legacy Runtime** (heap/malloc) is still active. This plan details the **"Hard Switch"**: modifying the compiler to emit Region-aware code, effectively turning OmniLisp into a Region-managed language.

## 2. Phase A: Compiler Analysis Integration
**Goal:** The compiler must know *where* to create regions.

### 2.1 Integrate `region_inference.c`
*   **File:** `src/runtime/compiler/omni_compile.c`
*   **Action:** In `omni_compile_function`, call `infer_regions(ctx)`.
*   **Result:** The `AnalysisContext` will be populated with `RegionInfo` (start/end positions for regions).

### 2.2 Update `CFG` for Region Scopes
*   **File:** `csrc/analysis/cfg.c` (or wherever CFG is built)
*   **Action:** Ensure that Region boundaries align with Basic Blocks.
*   **Why:** We can't insert `region_create` in the middle of a block if it's conditional. (Our simplified inference places them at Dominators, which is safe).

## 3. Phase B: Code Generation Update (The Big Swap)
**Goal:** Stop emitting `malloc`. Start emitting `region_alloc`.

### 3.1 Global/Root Region Injection
*   **File:** `csrc/codegen/codegen.c` -> `omni_codegen_emit_function_start`
*   **Action:**
    *   Change function signature: `Obj* my_func(Obj* args)` -> `Obj* my_func(Region* _caller, Obj* args)`
    *   Emit entry: `Region* _local = region_create();`

### 3.2 Constructor Swap
*   **File:** `csrc/codegen/codegen.c` -> `omni_codegen_emit_constructor`
*   **Action:**
    *   Replace `mk_int(x)` with `mk_int_region(_local, x)`
    *   Replace `mk_pair(a, b)` with `mk_pair_region(_local, a, b)`
    *   *Crucial:* Ensure `_local` is available in the scope (guaranteed by 3.1).

### 3.3 Return Path (Escape & Transmigrate)
*   **File:** `csrc/codegen/codegen.c` -> `omni_codegen_emit_return`
*   **Action:**
    *   Identify return value `val`.
    *   Emit: `Obj* _ret = transmigrate(val, _local, _caller);`
    *   Emit: `region_exit(_local);`
    *   Emit: `return _ret;`

## 4. Phase C: Entry Point & Main
**Goal:** The world starts with a Region.

### 4.1 Update `main.c`
*   **File:** `src/runtime/main.c`
*   **Action:**
    *   Create `Region* root = region_create();`
    *   Call entry function with `root`.
    *   Clean up `root` at exit.

## 5. Verification Strategy (Progressive)

### Step 1: "The Hybrid Test"
*   Create a test file `test_wired.omni`.
*   Manually compile it with the *modified* compiler.
*   Link against the *new* runtime.
*   **Expectation:** It runs without crashing. Valgrind shows 0 leaks (Regions reclaimed).

### Step 2: "The Cycle Test"
*   Write a Lisp program that creates a cyclic list and returns a *copy* of it (or part of it).
*   **Expectation:** The local region dies. The cycle is preserved in the caller via Transmigration.

### Step 3: "The Benchmark"
*   Run `examples/scicomp_demo.omni`.
*   Compare performance. Expectation: Allocation is faster (Bump ptr), GC is faster (Region drop).

## 6. Risks & Mitigation

| Risk | Mitigation |
| :--- | :--- |
| **Stack Overflow** | Transmigration is recursive. **Mitigation:** Iterative deep copy (future work) or increase stack size. |
| **Missing Type** | If we forgot a `mk_*_region` constructor. **Mitigation:** Compiler error will catch this. |
| **Global State** | Globals need a "Static Region" that never dies. **Mitigation:** Create a global `Region* _static_region` in `runtime.c`. |

## 7. Action Plan (Next Steps)

1.  **[TODO] T-rcg-inference-hook:** Call inference pass in compiler.
2.  **[TODO] T-rcg-codegen-wiring:** Implement Phase B (Signatures, Constructors, Returns).
3.  **[TODO] T-rcg-entry:** Update `main.c`.
