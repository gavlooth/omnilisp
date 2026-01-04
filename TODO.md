# OmniLisp TODO

## Priority: Eliminate Weak Reference Exposure to Users

**Goal**: Users should never need to use `WeakRef` directly. The compiler should automatically detect back-edges in cyclic data structures and handle them internally.

### Current State

The Go implementation has partial infrastructure:
- `TypeRegistry` with `OwnershipEdge` and `IsBackEdge` field ✅
- `AnalyzeBackEdges()` using DFS cycle detection ✅
- `markFieldWeak()` to mark back-edge fields ✅
- `WeakRef` runtime support ✅

**Gap**: This infrastructure isn't connected to actual user-defined types or codegen. The type registry only has hardcoded defaults (`Pair`, `List`, `Tree`).

---

### Phase 1: Type Definition Syntax & Registry Population

**Files**: `pkg/parser/parser.go`, `pkg/eval/eval.go`, `pkg/codegen/types.go`

1. Add `deftype` form to the language:
   ```scheme
   (deftype Node
     (value int)
     (next Node)
     (prev Node))   ; compiler will infer this is a back-edge
   ```

2. During parsing/eval, populate `TypeRegistry` from `deftype` forms

3. Wire up `BuildOwnershipGraph()` and `AnalyzeBackEdges()` after all types are registered

**Acceptance**: User-defined types appear in registry with correct field info

---

### Phase 2: Back-Edge Heuristics

**Files**: `pkg/codegen/types.go`

Enhance `AnalyzeBackEdges()` with naming heuristics:

| Field Pattern | Inference |
|---------------|-----------|
| `parent`, `owner`, `container` | Likely back-edge (points to ancestor) |
| `prev`, `back`, `up` | Likely back-edge (reverse direction) |
| Second field of same type | Candidate for back-edge |

Algorithm:
```
for each type T:
  for each field F where F.Type could form cycle with T:
    if F.name in BACK_EDGE_HINTS:
      mark F as weak (high confidence)
    else if already have owning field of same type:
      mark F as weak (medium confidence)
    else:
      use DFS cycle detection (current approach)
```

**Acceptance**: `Node.prev` auto-detected as weak without DFS

---

### Phase 3: Codegen Integration

**Files**: `pkg/codegen/codegen.go`, `pkg/codegen/runtime.go`

1. Generate field accessors that respect strength:
   ```c
   // Strong field - normal access
   Obj* get_next(Node* n) { return n->next; }

   // Weak field - no inc_ref, just read
   Obj* get_prev(Node* n) { return n->prev; }  // no ownership
   ```

2. Generate release functions that skip weak fields:
   ```c
   void release_Node(Node* n) {
       dec_ref(n->next);   // strong - decrement
       // n->prev skipped  // weak - don't decrement
       free(n);
   }
   ```

3. Remove `mk_weak_ref` from public API (keep internal)

**Acceptance**: Doubly-linked list works without user-visible WeakRef

---

### Phase 4: Shape-Aware Back-Edge Routing

**Files**: `pkg/analysis/shape.go`, `pkg/codegen/codegen.go`

Integrate shape analysis with back-edge detection:

```
Shape Analysis Result → Memory Strategy
─────────────────────────────────────────
TREE                 → free_tree (no back-edges possible)
DAG                  → dec_ref (no cycles)
CYCLIC + frozen      → SCC-based RC
CYCLIC + mutable     → auto-weak back-edges + dec_ref
CYCLIC + unknown     → arena allocation (fallback)
```

**Acceptance**: `letrec` cycles handled without user annotation

---

### Phase 5: Constructor-Level Ownership Tracking

**Files**: `pkg/analysis/ownership.go` (new), `pkg/codegen/codegen.go`

Track ownership at construction sites, not just types:

```scheme
(let ((a (mk-node 1 nil nil)))      ; a owns node
  (let ((b (mk-node 2 a nil)))      ; b.next owns a? No - a already owned
    (set! (node-prev a) b)          ; back-edge: a.prev -> b (non-owning)
    ...))
```

The `set!` to `prev` is automatically non-owning because:
1. `prev` field detected as back-edge pattern
2. `b` is already owned by enclosing scope

**Acceptance**: Circular references via `set!` handled automatically

---

### Phase 6: Testing & Validation

**Files**: `pkg/codegen/backedge_test.go` (new)

Test cases:
1. Doubly-linked list (prev is weak)
2. Tree with parent pointers (parent is weak)
3. Graph with arbitrary edges (arena fallback)
4. Closure capturing cyclic data
5. `letrec` mutual recursion

Validation:
- No memory leaks (valgrind)
- No use-after-free
- No double-free
- Deterministic behavior

---

## Backlog: Memory Architecture Enhancements (Post‑11)

These are ASAP‑compatible enhancements (no language restrictions, no STW GC).
See `ROADMAP.md` Phase 9 and `docs/UNIFIED_OPTIMIZATION_PLAN.md` for sketches.

1) Linear/offset regions for serialization & FFI  
   - Search terms: `RegionContext`, `region_enter`, `region_alloc`, `region_ref_deref`
2) Pluggable region backends (IRegion‑style vtable)  
   - Search terms: `region_alloc`, `arena_alloc`, `free_tree`, `dec_ref`
3) Weak ref control blocks (merge‑friendly)  
   - Search terms: `Weak`, `weak`, `invalidate_weak`, `BorrowRef`
4) Transmigration / isolation on region escape  
   - Search terms: `ShapeInfo`, `ESCAPE_`, `scan_`, `release_children`
5) External handle indexing (FFI + determinism)  
   - Search terms: `BorrowRef`, `ipge_`, `generation`, `Handle`, `tag`
6) FFI context‑word purity (compiler‑internal only)  
   - Requires TLS context stack + trampolines; no user‑visible params unless FFI lacks `user_data`  
   - Search terms: `__thread`, `exception_*` (TLS pattern), `RegionContext`, `ffi`

References (Vale docs):
- `Vale/docs/LinearRegion.md`
- `Vale/docs/IRegion.md`
- `Vale/docs/WeakRef.md`
- `Vale/docs/regions/Transmigration.md`
- `Vale/docs/PerfectReplayability.md`

---

## Secondary: Port Missing Features from omnilisp_c_scratch

### Destination-Passing Style (DPS)

**Reference**: omnilisp_c_scratch/src/analysis/dps.c

Enables stack allocation of return values:
```scheme
;; Current: heap allocates result
(define (map f xs) ...)

;; DPS: caller provides destination
(define (map-into! dest f xs) ...)
```

**Files to create**: `pkg/analysis/dps.go`, `pkg/codegen/dps.go`

---

### Exception Handling (Landing Pads)

**Reference**: omnilisp_c_scratch/src/memory/exception.h

Generate cleanup metadata for stack unwinding:
```c
// At each try point, track live allocations
// Generate landing pads that free live objects on unwind
```

**Files to create**: `pkg/codegen/exception.go`

**Depends on**: `try`/`catch` forms (already implemented)

---

### Concurrency (Ownership Transfer)

**Reference**: omnilisp_c_scratch/src/memory/concurrent.h

Ownership classes:
```go
const (
    OwnLocal       // Thread-local, pure ASAP
    OwnTransferred // Ownership moves via channel
    OwnShared      // Atomic RC required
    OwnImmutable   // No sync needed
)
```

**Files to create**: `pkg/analysis/concurrent.go`, `pkg/codegen/concurrent.go`

**Depends on**: Adding channel/thread primitives to language

---

### Interprocedural Analysis (Function Summaries)

Summarize each function's memory behavior:
```
process : (xs: List @borrowed) -> List @fresh
  consumes: none
  escapes: return value
  allocates: O(n)
```

**Files to create**: `pkg/analysis/summary.go`

**Benefits**:
- Cross-function ownership tracking
- Better escape analysis
- Enables more ASAP optimizations

---

### Perceus Reuse Analysis

**Reference**: Reinking et al., PLDI 2021

Pair `free` with subsequent `alloc` of same size:
```c
// Before
free_obj(x);
y = mk_int(42);

// After (reuse x's memory)
y = reuse_as_int(x, 42);
```

**Files to create**: `pkg/analysis/reuse.go`, `pkg/codegen/reuse.go`

---

## Comparison: omnilisp_c_scratch vs omnilisp

| Feature | omnilisp_c_scratch | omnilisp | Priority |
|---------|------------------|-----------|----------|
| Type registry | Full ownership graph | Partial (hardcoded) | **HIGH** |
| Back-edge inference | Auto from types | Infrastructure only | **HIGH** |
| WeakRef exposure | Internal only | User-visible | **HIGH** |
| DPS | Implemented | Missing | Medium |
| Exception cleanup | Implemented | Missing | Medium |
| Concurrency | Ownership transfer | Missing | Medium |
| Interprocedural | Partial | Missing | Medium |
| Perceus reuse | Planned | Missing | Low |
| Shape analysis | Full | Full | ✅ Done |
| Escape analysis | Full | Full | ✅ Done |
| Liveness analysis | Full | Full | ✅ Done |
| SCC-based RC | Full | Full | ✅ Done |
| Deferred RC | Full | Full | ✅ Done |
| Arena allocation | Full | Full | ✅ Done |

---

## Implementation Order

```
Phase 1: deftype + registry population     [Week 1]
    ↓
Phase 2: Back-edge heuristics              [Week 1]
    ↓
Phase 3: Codegen integration               [Week 2]
    ↓
Phase 4: Shape-aware routing               [Week 2]
    ↓
Phase 5: Constructor ownership             [Week 3]
    ↓
Phase 6: Testing                           [Week 3]
    ↓
DPS / Exception / Concurrency              [Future]
```

---

# Missing Features from Original OmniLisp (HVM4)

Comparison between our Go implementation and the original OmniLisp at `/home/heefoo/Documents/code/omnilisp`.

## Legend
- ✅ Implemented in omnilisp
- ❌ Missing from omnilisp
- 🔶 Partially implemented

---

## Core Language Features

### Data Types
| Feature | Status | Notes |
|---------|--------|-------|
| Numbers (integers) | ✅ | `TInt` |
| Symbols | ✅ | `TSym` |
| Pairs/Cons cells | ✅ | `TCell` |
| Nil | ✅ | `TNil` |
| Characters (`#\a`, `#\newline`) | ✅ | `TChar` with named chars |
| Strings (as char lists) | ✅ | Quoted lists of `TChar` |
| Closures | ✅ | `TLambda` |
| Code values | ✅ | `TCode` |
| Error values | ✅ | `TError` with message |

### Special Forms
| Feature | Status | Notes |
|---------|--------|-------|
| `lambda` | ✅ | Basic lambda |
| `lambda self` (recursive) | ✅ | `(lambda self (x) body)` for self-reference |
| `let` | ✅ | Single binding |
| `letrec` | ✅ | Recursive bindings |
| `if` | ✅ | Conditional |
| `quote` | ✅ | Quote expression |
| `and` / `or` | ✅ | Short-circuit logic |
| `do` (sequencing) | ✅ | `(do e1 e2 ... en)` returns last |
| `match` | ✅ | Full pattern matching |

### Pattern Matching (✅ Implemented)
| Feature | Description |
|---------|-------------|
| Wildcard `_` | Matches anything, binds nothing |
| Variable patterns | `x` matches and binds |
| Literal patterns | `(42)` matches specific value |
| Constructor patterns | `(CON a b)` destructures |
| Nested patterns | `(CON (CON a b) c)` |
| Or-patterns | `(or pat1 pat2)` alternatives |
| As-patterns | `(x @ pat)` bind whole and parts |
| List patterns | `(list a b . rest)` |
| Guards | `:when condition` |

---

## Stage-Polymorphic Evaluation

| Feature | Status | Notes |
|---------|--------|-------|
| `lift` | ✅ | Quote value as code |
| `run` | ✅ | `(run code)` execute code at base level |
| `code` / `quote` | ✅ | Quote as AST |
| Compile mode | ✅ | Full 9-handler tower support |

---

## Meta-Level / Reflective Features (✅ Implemented)

| Feature | Status | Description |
|---------|--------|-------------|
| `EM` | ✅ | Execute at parent meta-level |
| `shift` | ✅ | `(shift n expr)` Go up n levels and evaluate |
| `clambda` | ✅ | Compile lambda under current semantics |
| `meta-level` | ✅ | `(meta-level)` Get current tower level |
| `get-meta` | ✅ | `(get-meta 'name)` Fetch handler by name |
| `set-meta!` | ✅ | `(set-meta! 'name fn)` Install custom handler |
| `with-menv` | ✅ | `(with-menv menv body)` Evaluate with custom menv |
| `with-handlers` | ✅ | `(with-handlers ((name fn) ...) body)` |
| `default-handler` | ✅ | `(default-handler 'name arg)` Delegate to default |

### Handler Customization (✅ 9-Handler Table)
- `lit` handler - numeric literal evaluation
- `var` handler - variable lookup
- `lam` handler - lambda creation
- `app` handler - function application
- `if` handler - conditional
- `lft` handler - lift operation
- `run` handler - code execution
- `em` handler - meta-level jump
- `clam` handler - compiled lambda

---

## Primitives

### Arithmetic
| Feature | Status |
|---------|--------|
| `+`, `-`, `*`, `/`, `%` | ✅ |

### Comparison
| Feature | Status |
|---------|--------|
| `=`, `<`, `>`, `<=`, `>=` | ✅ |
| `not` | ✅ |

### List Operations
| Feature | Status | Notes |
|---------|--------|-------|
| `cons` | ✅ | |
| `car` / `fst` | ✅ | |
| `cdr` / `snd` | ✅ | |
| `null?` | ✅ | |
| `map` | ✅ | Higher-order |
| `filter` | ✅ | Higher-order |
| `fold` / `foldr` | ✅ | Right fold |
| `foldl` | ✅ | Left fold |
| `length` | ✅ | |
| `append` | ✅ | |
| `reverse` | ✅ | |
| `apply` | ✅ | Apply fn to arg list |

### Function Combinators (✅ Implemented)
| Feature | Description |
|---------|-------------|
| `compose` | `(compose f g)` → f ∘ g |
| `flip` | Swap argument order |

---

## Introspection

| Feature | Status | Description |
|---------|--------|-------------|
| `ctr-tag` | ❌ | Extract constructor name |
| `ctr-arg` | ❌ | Extract constructor argument by index |
| `reify-env` | ❌ | Return current environment as value |
| `gensym` | ✅ | Generate unique symbol |
| `eval` | ✅ | Evaluate code at runtime |
| `sym-eq?` | ✅ | Symbol equality check |
| `trace` | ✅ | Trace value during evaluation |

---

## Error Handling (✅ Implemented)

| Feature | Status | Description |
|---------|--------|-------------|
| `error` | ✅ | Raise error with message |
| `try` | ✅ | Catch errors with handler |
| `assert` | ✅ | Conditional error |
| `default-handler` | ❌ | Delegate to default |

---

## Type Hierarchy & Dispatch (Testing)

- [TODO] Label: T-dispatch-test-matrix
  Objective: Ensure core operators select the most specific implementation for built-in numeric types.
  Where: `csrc/tests/test_dispatch.c` (new), `csrc/Makefile` (test target)
  What to change:
    - Add a small test harness that evaluates OmniLisp expressions via the CLI or compiler API.
    - Add test cases for `(Int, Int)`, `(Int, Float)`, `(Float, Float)` and verify the result type/value.
    - Wire the new test into `make test` with a clear PASS/FAIL message.
  How to verify: run `make test` in `csrc/` and confirm all dispatch tests pass.
  Acceptance:
    - Each numeric pair hits the expected, most-specific branch.
    - Failing dispatch shows a clear assertion message with the expression.

- [TODO] Label: T-dispatch-test-fallback
  Objective: Verify hierarchical fallback to supertypes when no exact method exists.
  Where: `csrc/tests/test_dispatch.c`, `docs/LANGUAGE_REFERENCE.md` (test examples section if needed)
  What to change:
    - Add a method definition for a supertype (e.g., `Number`) and omit a subtype method.
    - Assert that a subtype value dispatches to the supertype method.
    - Add a negative test for “no method found” with expected error text.
  How to verify: run `make test` and confirm fallback and error cases match expected output.
  Acceptance:
    - Subtype values resolve to the nearest supertype method when no exact match exists.
    - “No method found” errors are deterministic and informative.

- [TODO] Label: T-dispatch-test-precedence
  Objective: Ensure ambiguity resolution prefers the most specific method.
  Where: `csrc/tests/test_dispatch.c`
  What to change:
    - Create overlapping methods (e.g., `Number`, `Real`, `Int`) and call with `Int`.
    - Assert that `Int` method is selected over broader ones.
  How to verify: run `make test` and confirm the chosen method matches expectations.
  Acceptance:
    - The most specific method is always chosen when multiple matches exist.

---

## I/O and FFI

| Feature | Status | Description |
|---------|--------|-------------|
| `(ffi "func" args)` | ✅ | Call external C function |
| `(ffi-declare ...)` | ✅ | Declare external function |
| `ffi "puts"` | ✅ | Write string to stdout |
| `ffi "putchar"` | ✅ | Write single character |
| `ffi "getchar"` | ✅ | Read single character |
| `ffi "exit"` | ✅ | Exit with code |
| `trace` | ✅ | Evaluate and trace value |
| SciComp.BLAS (OpenBLAS/CBLAS) | ⚠️ Partial | Bindings exist; loader/tests/docs pending |
| SciComp.Torch (LibTorch) | ⚠️ Partial | Bindings exist; error plumbing/tests/docs pending |

### SciComp: BLAS/LAPACK + Torch (FFI)

- [TODO] Label: T-sci-blas-loader
  Objective: BLAS bindings load OpenBLAS/CBLAS across Linux/macOS with clear errors.
  Where: `lib/scicomp/blas.omni`, `runtime/src/ffi` (if loader helpers are needed), `docs/QUICK_REFERENCE.md`
  What to change:
    - Add a library name fallback list (openblas, blas) and optional env override (e.g., `OMNILISP_BLAS_LIB`).
    - Add an import-time check that raises a descriptive error when the shared library is missing.
    - Add a minimal smoke example in docs or `examples/`.
  How to verify: run `./omnilisp -e '(import SciComp.BLAS) (ddot (vector 1.0 2.0) (vector 3.0 4.0))'` and confirm a numeric result.
  Acceptance:
    - Missing library produces a clear, actionable error.
    - Linux/macOS loader resolves at least one BLAS shared library name.
    - Smoke example runs successfully.

- [TODO] Label: T-sci-torch-errors
  Objective: Torch bindings surface LibTorch error messages instead of generic failures.
  Where: `lib/scicomp/torch.omni`, `runtime/src/ffi/libtorch_c_api.*`, `docs/QUICK_REFERENCE.md`
  What to change:
    - Implement error extraction using `torch_get_last_error` and clear it after reporting.
    - Update `check-torch-error!` to include the native error message in OmniLisp errors.
    - Add a minimal smoke example (tensor add + shape query) to docs or `examples/`.
  How to verify: run `./omnilisp -e '(import SciComp.Torch) (torch-add (tensor [1 2]) (tensor [3 4]))'` and confirm a tensor result; trigger a deliberate invalid call and confirm the error message.
  Acceptance:
    - Native LibTorch error message appears in OmniLisp error output.
    - Smoke example runs successfully when LibTorch is available.

### FFI Autowrap (C-first, cl-autowrap-class ergonomics)

- [TODO] Label: T-ffiwrap-abi-model
  Objective: Define a C ABI model that can represent functions, types, and ownership hints for autowrap.
  Where: `csrc/ffiwrap/abi.h` (new), `csrc/ffiwrap/abi.c` (new)
  What to change:
    - Add `CType`, `CFunction`, `CStruct`, `CEnum`, and `COpaque` structs.
    - Add helpers for allocating/freeing the model and for string interning.
    - Document how ownership and error hooks are represented in the model.
  How to verify: build the new module and run a small unit test that constructs and frees a sample ABI graph without leaks.
  Acceptance:
    - ABI model compiles cleanly in C99.
    - No memory leaks in the model test.

- [TODO] Label: T-ffiwrap-parser
  Objective: Parse C headers into the ABI model using a C-based parser.
  Where: `csrc/ffiwrap/parser.c` (new), `csrc/ffiwrap/parser.h` (new), `csrc/ffiwrap/Makefile` (new or integrated)
  What to change:
    - Implement a minimal parser path using libclang or castxml (selectable at build time).
    - Extract function signatures, typedefs, enums, and opaque structs.
    - Store parsing diagnostics with file/line info for actionable errors.
  How to verify: run `omnilisp-ffi wrap --headers runtime/src/ffi/libtorch_c_api.h` and confirm it emits a populated ABI model summary.
  Acceptance:
    - Functions + typedefs parsed from a real header.
    - Parsing errors include file/line/col.

- [TODO] Label: T-ffiwrap-codegen-omni
  Objective: Generate OmniLisp bindings (`define {extern ...}` + wrappers) from the ABI model.
  Where: `csrc/ffiwrap/codegen_omni.c` (new), `csrc/ffiwrap/codegen_omni.h` (new)
  What to change:
    - Emit extern declarations for all parsed functions.
    - Emit wrapper functions that handle type conversions and validate args.
    - Emit opaque type declarations with destructors when configured.
  How to verify: generate bindings for BLAS and Torch headers and load them in OmniLisp without manual edits.
  Acceptance:
    - Generated `.omni` file loads without errors.
    - Wrapper functions call the correct externs.

- [TODO] Label: T-ffiwrap-loader
  Objective: Auto-generate platform-aware FFI loader blocks with fallback library names.
  Where: `csrc/ffiwrap/codegen_omni.c`, `lib/ffi/` (new bindings output)
  What to change:
    - Add `:linux`/`:darwin` loader stanzas with fallback lists.
    - Add optional env var override (e.g., `OMNILISP_FFI_LIB_<NAME>`).
    - Emit a descriptive error when no library can be loaded.
  How to verify: simulate a missing library and confirm the error is actionable; load a present library and confirm import succeeds.
  Acceptance:
    - Missing library yields a clear, actionable error.
    - Present library loads without changing user code.

- [TODO] Label: T-ffiwrap-errors
  Objective: Auto-generate module-level error plumbing (e.g., `check‑<module>-error!`) when error hooks exist.
  Where: `csrc/ffiwrap/codegen_omni.c`, `lib/ffi/` bindings
  What to change:
    - Detect `<module>_get_last_error` / `<module>_ok` patterns in the ABI model.
    - Generate `check‑<module>-error!` that includes native error messages.
    - Ensure error state is cleared after reporting.
  How to verify: call a failing Torch API and confirm OmniLisp error includes native message.
  Acceptance:
    - Native error messages are surfaced consistently.
    - Error state is cleared after reporting.

- [TODO] Label: T-ffiwrap-cli
  Objective: Provide a C CLI tool to run autowrap from headers to bindings.
  Where: `csrc/ffiwrap/main.c` (new), `csrc/Makefile`
  What to change:
    - Add `omnilisp-ffi` CLI with `wrap` subcommand and flags `--name`, `--headers`, `--libs`, `--out`.
    - Wire build target (e.g., `make ffiwrap`) alongside the main toolchain.
    - Print a summary of generated functions and types.
  How to verify: run `./omnilisp-ffi wrap --name SciComp.BLAS --headers /usr/include/cblas.h --libs openblas,blas --out lib/ffi/scicomp_blas.omni`.
  Acceptance:
    - CLI runs end-to-end without manual edits.
    - Output bindings file is generated at the requested path.

---

## Conditions, Restarts, and Debugging Integration

- [TODO] Label: T-cond-core-types
  Objective: Define structured condition objects and a base condition hierarchy.
  Where: `runtime/src/condition.c` (new), `runtime/src/condition.h` (new), `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Add C structs for `Condition`, `ConditionType`, and a registry for condition types.
    - Define base types (e.g., `:error`, `:type-error`, `:arithmetic-error`, `:ffi-error`, `:memory-error`).
    - Expose constructors for conditions in the runtime API.
  How to verify: run a small C test that constructs a condition, sets fields, and renders it.
  Acceptance:
    - Condition objects can be created and queried from runtime code.
    - Base condition types are registered at init.

- [TODO] Label: T-cond-runtime-throw
  Objective: Route all runtime errors through condition objects instead of raw strings.
  Where: `runtime/src/runtime.c`, `runtime/src/condition.c`
  What to change:
    - Update `error`, `assert`, and FFI error hooks to produce `Condition` values.
    - Preserve existing `try` semantics while passing structured conditions.
    - Ensure uncaught conditions print full condition data (type + message).
  How to verify: run `./csrc/omnilisp -e '(error \"boom\")'` and confirm structured condition output.
  Acceptance:
    - All runtime errors emit `Condition` objects.
    - Uncaught errors include condition type and message.

- [TODO] Label: T-restart-core
  Objective: Implement restart stacks and named restart invocation.
  Where: `runtime/src/restart.c` (new), `runtime/src/restart.h` (new), `runtime/src/runtime.c`
  What to change:
    - Add a thread-local restart stack with named restart entries.
    - Expose runtime APIs: `restart_push`, `restart_pop`, `restart_invoke`.
    - Add minimal restart selection by name.
  How to verify: call a C test that registers two restarts and invokes one by name.
  Acceptance:
    - Restart stack supports push/pop/invoke.
    - Invoking a restart resumes control flow deterministically.

- [TODO] Label: T-cond-restart-syntax
  Objective: Provide OmniLisp syntax for conditions and restarts.
  Where: `csrc/parser/parser.c`, `csrc/codegen/codegen.c`, `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Add forms like `handler-case`, `handler-bind`, and `restart-case`.
    - Generate runtime calls that register restarts and handlers.
    - Document syntax and examples.
  How to verify: run a small OmniLisp program that triggers an error and selects a restart.
  Acceptance:
    - `handler-case` captures conditions and executes its handler.
    - `restart-case` makes restarts visible and invocable.

- [TODO] Label: T-debug-logical-stack
  Objective: Add logical OmniLisp stack tracing for errors and condition reports.
  Where: `runtime/src/debug.c` (new), `runtime/src/debug.h` (new), `csrc/codegen/codegen.c`
  What to change:
    - Implement `dbg_push`, `dbg_pop`, `dbg_set_loc`, and `dbg_print_stack`.
    - Inject stack push/pop in generated functions and on error boundaries.
    - Print logical stack for uncaught conditions.
  How to verify: run a nested function call with a forced error and confirm the OmniLisp stack prints.
  Acceptance:
    - Logical stack shows function names + source locations.
    - Stack trace appears on uncaught conditions.

- [TODO] Label: T-debug-macro-trace
  Objective: Emit macro expansion traces in diagnostics.
  Where: macro expansion path (preprocessor), `runtime/src/debug.c`, `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Track expansion parents and store expansion spans.
    - Print expansion chain in error reports.
  How to verify: trigger an error inside a macro expansion and confirm the expansion trace prints.
  Acceptance:
    - Error output includes macro expansion chain.

- [TODO] Label: T-debug-asap-why-freed
  Objective: Surface compile-time ASAP free reasons in runtime errors.
  Where: `csrc/analysis/analysis.c`, `csrc/codegen/codegen.c`, `runtime/src/runtime.c`
  What to change:
    - Emit a debug table mapping free sites to reason metadata (last use, escape class, capture).
    - On UAF/double-free errors, print the free reason.
  How to verify: trigger a controlled UAF in debug mode and confirm the “why freed” message.
  Acceptance:
    - Runtime errors explain the compile-time free decision.

- [TODO] Label: T-debug-rc-tracing
  Objective: Add debug-only RC tracing with source spans for inc/dec/free.
  Where: `runtime/src/runtime.c`, `csrc/codegen/codegen.c`
  What to change:
    - Add debug hooks in `inc_ref`/`dec_ref`/`free_*` that emit events.
    - Record source span IDs from codegen for each RC operation.
  How to verify: run with debug enabled and confirm RC events are logged.
  Acceptance:
    - RC logs show source spans and operation type.

- [TODO] Label: T-debug-continuations
  Objective: Make continuation/prompt frames visible in logical stack traces.
  Where: `runtime/src/memory/continuation.c`, `runtime/src/debug.c`
  What to change:
    - Add debug metadata to continuation frames (span + function).
    - Merge continuation frames into stack output on error.
  How to verify: trigger an error inside a continuation and confirm the stack includes continuation frames.
  Acceptance:
    - Continuation frames appear in debug stack traces.

---

## Diagnostics, Introspection, and Developer UX (Missing Pieces)

- [TODO] Label: T-diag-source-spans
  Objective: Ensure all AST nodes carry source spans to enable snippet+caret diagnostics everywhere.
  Where: `csrc/ast/ast.h`, `csrc/parser/parser.c`, `csrc/util/span.c` (new)
  What to change:
    - Add span fields to `OmniValue` and propagate in parser constructors.
    - Build a file table with line offsets for fast line/col mapping.
    - Expose a span resolver in the compiler/runtime for diagnostics.
  How to verify: force a parse error and confirm it prints correct line/col and snippet/caret.
  Acceptance:
    - All AST nodes have spans.
    - Diagnostics include accurate snippet+caret.

- [TODO] Label: T-diag-structured-output
  Objective: Produce structured diagnostics (text + JSON) for editor integration.
  Where: `csrc/util/diagnostic.c` (new), `csrc/compiler/compiler.c`, `csrc/cli/main.c`
  What to change:
    - Implement a diagnostic object model (severity, spans, notes, help).
    - Add `--diag=json` CLI flag to output machine-readable diagnostics.
    - Keep human-readable output as default.
  How to verify: run `./csrc/omnilisp --diag=json -e '(+ 1 )'` and confirm valid JSON diagnostics.
  Acceptance:
    - JSON diagnostics emitted on error.
    - Text diagnostics unchanged by default.

- [TODO] Label: T-diag-line-mapping
  Objective: Emit `#line` directives in generated C for native debugger source mapping.
  Where: `csrc/codegen/codegen.c`
  What to change:
    - Emit `#line <n> "<file>"` before function bodies and key expressions.
    - Ensure paths are stable and relative to project root.
  How to verify: compile and inspect generated C for `#line` directives and confirm gdb/llb backtrace points to source.
  Acceptance:
    - Generated C includes `#line` mapping.
    - Native debugger shows OmniLisp source locations.

- [TODO] Label: T-debug-breakpoints
  Objective: Add `break` / `debug` forms to pause evaluation and inspect locals in REPL.
  Where: `csrc/parser/parser.c`, `csrc/codegen/codegen.c`, `runtime/src/runtime.c`
  What to change:
    - Parse new forms `(break)` and `(debug expr)`.
    - When triggered, print locals and allow interactive continuation.
    - Provide REPL commands: `step`, `next`, `continue`.
  How to verify: run a program with `(break)` and confirm REPL pause + continue behavior.
  Acceptance:
    - Breakpoint pauses evaluation.
    - User can continue execution.

- [TODO] Label: T-debug-trace-enhance
  Objective: Improve `trace`/`untrace` to show call depth, args, and return values with filtering.
  Where: `runtime/src/runtime.c`, `csrc/codegen/codegen.c`, `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Implement per-function tracing with depth and arg/return logging.
    - Add optional filters by symbol name or namespace.
  How to verify: enable trace on a function and confirm call/return logs include args and values.
  Acceptance:
    - Trace output includes call depth and return values.

- [TODO] Label: T-introspect-types
  Objective: Add type/dispatch introspection helpers (`type-of`, `methods-of`, `subtype?`).
  Where: `runtime/src/runtime.c`, `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Implement primitives: `type-of`, `subtype?`, `methods-of`, `describe`.
    - Connect to the method/dispatch registry if present.
  How to verify: run REPL and check introspection results for core types.
  Acceptance:
    - Introspection helpers return stable, documented results.

- [TODO] Label: T-debug-weak-edge-explain
  Objective: Explain why a field was auto-marked weak (pattern + cycle reasoning).
  Where: `csrc/analysis/analysis.c`, `csrc/codegen/codegen.c`, `runtime/src/runtime.c`
  What to change:
    - Emit weak-edge metadata (pattern match vs cycle proof).
    - Provide a runtime query helper or diagnostic note.
  How to verify: define a doubly-linked type and confirm diagnostics explain the weak edge.
  Acceptance:
    - Weak-edge reasoning is visible in debug output.

- [TODO] Label: T-debug-region-explain
  Objective: Provide region mismatch explanations with source spans.
  Where: `runtime/src/memory/region.c`, `runtime/src/runtime.c`
  What to change:
    - Track source spans for region creation and cross-region references.
    - On mismatch, print region chain and source locations.
  How to verify: create a cross-scope ref and confirm the diagnostic explains the mismatch.
  Acceptance:
    - Region errors show source spans and suggested fixes.

- [TODO] Label: T-debug-borrow-explain
  Objective: Report borrow/tether misuse with origin and invalid access spans.
  Where: `runtime/src/runtime.c`, `csrc/codegen/codegen.c`
  What to change:
    - Record borrow origin spans in debug metadata.
    - Emit a diagnostic when invalid access occurs.
  How to verify: trigger a borrow misuse and confirm diagnostic includes origin + access spans.
  Acceptance:
    - Borrow misuse diagnostics are precise and actionable.

- [TODO] Label: T-debug-cycle-logs
  Objective: Log SCC/symmetric RC lifecycle events for cyclic structures in debug builds.
  Where: `runtime/src/memory/scc.c`, `runtime/src/memory/symmetric.c`
  What to change:
    - Assign cycle IDs and emit creation/orphan/free events.
    - Include object IDs and source spans when available.
  How to verify: create a cyclic structure and confirm cycle logs appear.
  Acceptance:
    - Cycle lifecycle events are logged in debug mode.

- [TODO] Label: T-debug-event-ring
  Objective: Add a bounded event ring buffer for alloc/free/rc/borrow events.
  Where: `runtime/src/debug.c`, `runtime/src/debug.h`, `runtime/src/runtime.c`
  What to change:
    - Implement a fixed-size ring buffer with event types and span IDs.
    - Add REPL command `events N` to display recent events.
  How to verify: trigger allocations and confirm `events` prints recent history.
  Acceptance:
    - Event ring captures and prints recent actions.

- [TODO] Label: T-replay-deterministic
  Objective: Add deterministic replay for debugging (scheduler + RNG).
  Where: `runtime/src/runtime.c`, `runtime/src/memory/continuation.c`
  What to change:
    - Record scheduling decisions and random seeds.
    - Provide a replay mode flag and ensure runs are deterministic.
  How to verify: run the same program twice with replay mode and confirm identical output and event order.
  Acceptance:
    - Deterministic replay reproduces behavior reliably.

- [TODO] Label: T-diag-error-ids
  Objective: Assign stable error IDs for documentation and test matching.
  Where: `runtime/src/condition.c`, `csrc/util/diagnostic.c`, `docs/LANGUAGE_REFERENCE.md`
  What to change:
    - Add numeric or symbolic error IDs to each condition type.
    - Include IDs in diagnostic output.
  How to verify: trigger an error and confirm the ID appears in output.
  Acceptance:
    - All conditions emit stable IDs.

- [TODO] Label: T-diag-symbol-suggestions
  Objective: Provide “did you mean?” suggestions for unbound symbols.
  Where: `runtime/src/runtime.c`
  What to change:
    - Implement symbol similarity search (edit distance or prefix match).
    - Include top suggestions in unbound symbol errors.
  How to verify: reference a misspelled symbol and confirm suggested alternatives.
  Acceptance:
    - Unbound symbol errors include relevant suggestions.

- [TODO] Label: T-debug-tests-golden
  Objective: Add golden tests for diagnostics, conditions, and stack traces.
  Where: `csrc/tests/test_diagnostics.c` (new), `runtime/tests/`
  What to change:
    - Add tests for error formatting, macro traces, and stack traces.
    - Store expected output in golden files.
  How to verify: run test suite and confirm golden comparisons pass.
  Acceptance:
    - Golden diagnostics tests pass.

- [TODO] Label: T-debug-tests-memory
  Objective: Add tests for UAF, double-free, borrow misuse, and region errors.
  Where: `runtime/tests/`
  What to change:
    - Create targeted tests that trigger each memory error.
    - Assert the diagnostic output contains required context.
  How to verify: run runtime tests and confirm all memory diagnostics tests pass.
  Acceptance:
    - Memory error diagnostics are exercised and validated.

---

## Macro System

| Feature | Status | Description |
|---------|--------|-------------|
| Quasiquote `` ` `` | ✅ | Quote with evaluation |
| Unquote `,` | ✅ | Evaluate in quasiquote |
| Unquote-splicing `,@` | ✅ | Splice list |
| `defmacro` | ✅ | Define macro with transformer |
| `mcall` | ✅ | Call macro by name |
| `macroexpand` | ✅ | Expand without eval |

---

## Implementation Details

### Variable Representation
| Feature | Status | Notes |
|---------|--------|-------|
| Named variables | ✅ | Current approach |
| De Bruijn indices | ❌ | Original uses indices |

### Memory Management
| Feature | Status | Notes |
|---------|--------|-------|
| ASAP free insertion | ✅ | Compile-time |
| Shape analysis | ✅ | TREE/DAG/CYCLIC |
| Arena allocation | ✅ | For cyclic data |
| Weak edges | ✅ | Break ownership cycles |
| HVM4 interaction nets | ❌ | Original uses HVM4 |

---

## Priority Order for Implementation

### High Priority (Core Language) - ✅ COMPLETE
1. ✅ **Pattern matching** - fundamental for idiomatic code
2. ✅ **Recursive lambda** - `(lambda self ...)` for cleaner recursion
3. ✅ **Error handling** - `error`, `try`, `assert`
4. ✅ **List operations** - `map`, `filter`, `fold`, etc.

### Medium Priority (Staging) - ✅ COMPLETE
5. ✅ **`run` form** - execute code at base level
6. ✅ **Meta-level operations** - EM, shift, clambda, meta-level
7. ✅ **Handler customization** - 9-handler table with get/set-meta!, with-handlers

### Lower Priority (Convenience) - ✅ COMPLETE
8. ✅ **Quasiquote** - template syntax
9. ✅ **Macro system** - syntactic abstraction (defmacro, mcall, macroexpand)
10. ✅ **FFI/I/O** - practical programs
11. ✅ **Characters/strings** - text handling
12. ✅ **Introspection** - gensym, eval, sym-eq?, trace
13. ✅ **JIT execution** - Runtime C code execution via GCC

---

## References

- Original OmniLisp: `/home/heefoo/Documents/code/omnilisp`
- "Collapsing Towers of Interpreters" (Amin & Rompf, POPL 2018)
- HVM4: Higher Order Virtual Machine
