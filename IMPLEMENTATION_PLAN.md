# Purple Go - Gap Elimination Implementation Plan

## Current Status (Updated)

**Completed - Language Features:**
- Mutable state (`box`, `set!`, `define`) ✅
- I/O (`display`, `newline`, `read`) ✅
- Continuations (`call/cc`, `prompt`/`control`) ✅
- CSP (`go`, `make-chan`, `chan-send!`, `chan-recv!`, `select`) ✅
- deftype with constructors/accessors/setters/predicates ✅
- User-defined types with :weak annotation support ✅
- Introspection primitives (`ctr-tag`, `ctr-arg`, `reify-env`) ✅
- All 100+ tests passing ✅

**Completed - Memory Infrastructure (ported from C):**
- `pkg/memory/`: ASAP, SCC, arena, deferred, symmetric, genref, region, constraint ✅
- `pkg/analysis/`: escape, liveness, ownership, rcopt, shape ✅
- `pkg/codegen/types.go`: TypeRegistry with back-edge detection ✅
- `pkg/codegen/runtime.go`: C runtime generation (1,900+ lines) ✅

**Completed - Gap Elimination Phases:**
- Phase 1: deftype → TypeRegistry wiring ✅
- Phase 2: Back-edge heuristics enhancement ✅
- Phase 3: Codegen weak field integration ✅
- Phase 8: Minor primitives (ctr-tag, ctr-arg, reify-env) ✅

**Remaining Gaps:** Phases 4-7 (analysis and optimization)

---

## Dependency Graph

```
Phase 1 (deftype wiring)
    │
    ▼
Phase 2 (back-edge heuristics) ────► Phase 3 (codegen weak fields)
    │                                         │
    ▼                                         ▼
Phase 4 (exception landing pads) ◄──── Phase 5 (interprocedural analysis)
    │                                         │
    ▼                                         ▼
Phase 6 (concurrency ownership) ────► Phase 7 (shape routing & Perceus)
    │
    ▼
Phase 8 (minor gaps)
```

---

## Phase 1: deftype → TypeRegistry Wiring

**Problem**: Infrastructure exists but user-defined types don't populate registry properly.

### Current State
- `evalDeftype` in `pkg/eval/eval.go:2126-2201` exists
- Calls `codegen.GlobalRegistry().RegisterType()` with fields
- Calls `BuildOwnershipGraph()` and `AnalyzeBackEdges()` after registration
- But: No constructor primitives, no C struct generation

### Tasks

| Task | File | Description |
|------|------|-------------|
| 1.1 | `pkg/eval/primitives.go` | Add dynamic constructor primitives (`mk-Node`, `Node-value`, `Node?`) |
| 1.2 | `pkg/eval/eval.go` | Track `DefinedTypes` slice for compilation phase |
| 1.3 | `pkg/codegen/runtime.go` | Generate C struct forward declarations for mutual recursion |

### Acceptance Criteria
- [x] `(deftype Node (value int) (next Node))` creates callable `mk-Node`
- [x] `(mk-Node 42 nil)` returns value with correct fields
- [x] `(Node-value n)` accesses field
- [x] Generated C compiles for complex type hierarchies

---

## Phase 2: Back-Edge Heuristics Enhancement

**Problem**: Existing heuristics need expansion and optimization.

### Current State
- `BackEdgeHints` in `pkg/codegen/types.go:122-126`: `parent`, `owner`, `container`, `prev`, `previous`, `back`, `up`, `outer`
- Second pointer of same type marked weak
- DFS-based cycle detection as fallback

### Tasks

| Task | File | Description |
|------|------|-------------|
| 2.1 | `pkg/codegen/types.go` | Expand patterns: `predecessor`, `ancestor`, `enclosing`, `*_back`, `backref` |
| 2.2 | `pkg/codegen/types.go` | Improve second-pointer detection with confidence scoring |
| 2.3 | `pkg/codegen/types.go` | Cache DFS results, minimize cycle breaks |
| 2.4 | `pkg/eval/eval.go` | Support `(deftype Node (prev Node :weak))` explicit annotation |

### Acceptance Criteria
- [x] All common back-edge patterns auto-detected
- [x] User can override with `:weak` annotation
- [x] Analysis scales to 50+ types in <100ms

---

## Phase 3: Codegen Weak Field Integration ✅

**Status**: COMPLETED

### Completed Tasks
- Release functions skip weak fields in all paths ✅
- `GetCycleStatusForType()` and `ShouldUseArenaForType()` added to CodeGenerator ✅
- `GenerateUserTypeScanners()` generates type-specific scanners that skip weak fields ✅
- set! for weak fields generates no refcount changes ✅

### Acceptance Criteria
- [x] Doubly-linked list compiles without memory leaks
- [x] Valgrind clean on cyclic structure teardown
- [x] Generated code documents weak field handling

---

## Phase 4: Exception Landing Pads

**Problem**: `try`/`catch` works but no cleanup during stack unwinding.

### Current State
- `(try expr handler)` catches errors via Go panic/recover in `pkg/eval/eval.go:717-728`
- No metadata about live allocations at potential throw points
- No generated landing pad code in C output

### Tasks

| Task | File | Description |
|------|------|-------------|
| 4.1 | `pkg/codegen/exception.go` (new) | Define `CleanupPoint`, `LandingPad` structures |
| 4.2 | `pkg/codegen/codegen.go` | Track live allocations at potential throw points |
| 4.3 | `pkg/codegen/runtime.go` | Generate setjmp/longjmp macros: `TRY_BEGIN`, `THROW` |
| 4.4 | `pkg/codegen/exception.go` | Generate landing pad cleanup code (reverse order) |
| 4.5 | Integration | Use liveness info from `pkg/analysis/liveness.go` |

### Acceptance Criteria
- [ ] `(try (let ((x (mk-int 10))) (error "fail")) handler)` frees x
- [ ] Nested try blocks clean up properly
- [ ] No leaks when exceptions traverse multiple frames

---

## Phase 5: Interprocedural Analysis

**Problem**: Only intraprocedural analysis exists.

### Current State
- `pkg/analysis/escape.go` analyzes within single function
- `pkg/analysis/ownership.go` tracks within scope
- No summaries for cross-function ownership transfer

### Tasks

| Task | File | Description |
|------|------|-------------|
| 5.1 | `pkg/analysis/summary.go` (new) | Define `FunctionSummary`: params, return, side effects |
| 5.2 | `pkg/analysis/summary.go` | Compute summaries for user-defined functions |
| 5.3 | `pkg/analysis/escape.go` | Use summaries at call sites for ownership propagation |
| 5.4 | `pkg/analysis/summary.go` | Hardcode summaries for primitives (`cons`, `car`, `map`) |
| 5.5 | `pkg/analysis/summary.go` | Incremental updates on function changes |

### Function Summary Format
```
process : (xs: List @borrowed) -> List @fresh
  consumes: none
  escapes: return value
  allocates: O(n)
```

### Acceptance Criteria
- [ ] `(define (f x) x)` → summary: x=borrowed, return=borrowed
- [ ] `(define (g x) (cons x nil))` → summary: x=stored, return=fresh
- [ ] Call sites use callee summaries for ownership

---

## Phase 6: Concurrency Ownership Transfer

**Problem**: CSP primitives work but no ownership analysis for compiled code.

### Current State
- `pkg/eval/scheduler.go` - channels, processes
- `pkg/eval/eval.go` - `evalGo`, `evalSelect`
- No tracking of ownership transfer across channel send/receive in codegen

### Tasks

| Task | File | Description |
|------|------|-------------|
| 6.1 | `pkg/analysis/ownership.go` | Add: `OwnerThreadLocal`, `OwnerTransferring`, `OwnerReceived`, `OwnerSharedConcurrent` |
| 6.2 | `pkg/analysis/concurrent.go` (new) | Analyze send: mark val as transferring, dead in sender |
| 6.3 | `pkg/analysis/concurrent.go` | Analyze recv: result is owned, normal ASAP applies |
| 6.4 | `pkg/analysis/concurrent.go` | Analyze go: captured vars need atomic RC if shared |
| 6.5 | `pkg/codegen/concurrent.go` (new) | Generate atomic inc_ref/dec_ref for shared values |

### Ownership Transfer Model
```
Process A              Channel              Process B
─────────              ───────              ─────────
(let ((x (alloc)))     ┌─────┐
  (chan-send! ch x) ──▶│  x  │ ──▶  (let ((y (chan-recv! ch)))
  ;; x is DEAD here    └─────┘       (use y)
  )                                   (free y))

Sender loses ownership → Receiver gains ownership
```

### Acceptance Criteria
- [ ] `(let ((x 1)) (go (print x)))` identifies x as shared
- [ ] `(chan-send! ch x)` followed by use of x is rejected
- [ ] `(let ((y (chan-recv! ch))) ...)` treats y as locally owned
- [ ] No data races in generated code

---

## Phase 7: Shape-Aware Routing & Perceus Reuse

**Problem**: Shape analysis not integrated with reuse optimization.

### Current State
- `pkg/analysis/shape.go` computes TREE/DAG/CYCLIC
- `pkg/codegen/runtime.go:567-609` has Perceus stubs (`try_reuse`, `reuse_as_int`, `reuse_as_pair`)
- No analysis to pair free with subsequent alloc

### Tasks

| Task | File | Description |
|------|------|-------------|
| 7.1 | `pkg/analysis/shape.go` | Consult `TypeRegistry.GetCycleStatus()` in shape analysis |
| 7.2 | `pkg/analysis/reuse.go` (new) | Define `ReusePair`, scan for adjacent free-alloc patterns |
| 7.3 | `pkg/codegen/codegen.go` | Transform: `free(x); y=mk_int(42)` → `y=reuse_as_int(x,42)` |
| 7.4 | `pkg/analysis/dps.go` (new) | Identify functions returning fresh allocations |
| 7.5 | `pkg/codegen/dps.go` (new) | Generate `_dps` variants for stack allocation |

### Shape → Strategy Mapping
| Shape | Cycle Status | Strategy |
|-------|--------------|----------|
| TREE | N/A | free_tree (no back-edges) |
| DAG | N/A | dec_ref (no cycles) |
| CYCLIC | frozen | SCC-based RC |
| CYCLIC | mutable | auto-weak + dec_ref |
| CYCLIC | unknown | arena allocation |

### Acceptance Criteria
- [ ] Shape analysis identifies cycles broken by weak edges
- [ ] Adjacent free-alloc pairs transformed to reuse
- [ ] DPS reduces heap allocations in benchmarks
- [ ] All generated code passes valgrind

---

## Phase 8: Minor Gaps ✅

**Status**: COMPLETED

### Completed Tasks
- `PrimCtrTag`: Returns constructor name as symbol ✅
- `PrimCtrArg`: Returns nth constructor argument ✅
- `PrimReifyEnv`: Returns current environment as assoc list ✅

### Acceptance Criteria
- [x] `(ctr-tag (cons 1 2))` → `'cell`
- [x] `(ctr-arg (cons 1 2) 0)` → `1`
- [x] `(let ((x 1)) (reify-env))` includes `(x . 1)`

---

## New Files to Create

| File | Phase | Purpose |
|------|-------|---------|
| `pkg/codegen/exception.go` | 4 | Landing pad generation |
| `pkg/analysis/summary.go` | 5 | Function summaries |
| `pkg/analysis/concurrent.go` | 6 | Concurrency ownership |
| `pkg/codegen/concurrent.go` | 6 | Thread-safe codegen |
| `pkg/analysis/reuse.go` | 7 | Perceus reuse analysis |
| `pkg/analysis/dps.go` | 7 | Destination-passing analysis |
| `pkg/codegen/dps.go` | 7 | DPS code generation |

## Files to Modify

| File | Phases | Changes |
|------|--------|---------|
| `pkg/eval/eval.go` | 1, 2 | Constructor primitives, `:weak` syntax |
| `pkg/eval/primitives.go` | 1, 8 | Dynamic type constructors, introspection |
| `pkg/codegen/types.go` | 2 | Enhanced heuristics |
| `pkg/codegen/runtime.go` | 1, 3, 4 | Structs, release functions, exception macros |
| `pkg/codegen/codegen.go` | 3, 7 | Weak field handling, reuse transforms |
| `pkg/analysis/escape.go` | 5 | Use function summaries |
| `pkg/analysis/ownership.go` | 6 | Concurrent ownership classes |
| `pkg/analysis/shape.go` | 7 | TypeRegistry integration |

---

## Testing Strategy

### Unit Tests per Phase
- Phase 1: `test/deftype_test.go` (extend)
- Phase 2: `pkg/codegen/types_test.go` (new)
- Phase 3: `pkg/codegen/codegen_test.go` (extend)
- Phase 4: `pkg/codegen/exception_test.go` (new)
- Phase 5: `pkg/analysis/summary_test.go` (new)
- Phase 6: `pkg/analysis/concurrent_test.go` (new)
- Phase 7: `pkg/analysis/reuse_test.go` (new)

### Integration Tests
- `test/backedge_integration_test.go` (extend)
- `test/memory_integration_test.go` (new)

### Validation
- All generated C must pass `valgrind --leak-check=full`
- Benchmark suite before/after each phase

---

## Summary

| Phase | Focus | Dependencies |
|-------|-------|-------------|
| 1 | deftype wiring | None |
| 2 | Back-edge heuristics | Phase 1 |
| 3 | Codegen weak fields | Phase 2 |
| 4 | Exception landing pads | Phase 5 (liveness) |
| 5 | Interprocedural analysis | Phase 1 |
| 6 | Concurrency ownership | Phase 5 |
| 7 | Shape routing & Perceus | All previous |
| 8 | Minor gaps | None |

**Critical Path**: Phase 1 → 2 → 3 → 7 (memory safety)
**Parallel Path**: Phase 5 → 6 (can proceed after Phase 1)

---

## NOT Included (Deferred)

- Tensor/libtorch integration (see LIBTORCH_PLAN.md)
- HVM4 interaction nets
- De Bruijn indices

---

## Future: Pika Parser with Lisp Semantics

**Goal**: Add a user-friendly surface syntax while preserving homoiconicity and tower compatibility.

### Concept

Traditional Lisp uses S-expressions where lists ARE code. Pika + Lisp semantics uses AST nodes as first-class data:

```
┌─────────────────────────────────────────────────┐
│  S-expr Lisp:  (+ 1 2) is a list AND addition   │
├─────────────────────────────────────────────────┤
│  Pika + Lisp:  1 + 2 produces AST node data     │
│                BinOpExpr('+, IntExpr(1), ...)   │
└─────────────────────────────────────────────────┘
```

### Why This Works with the Tower

- Code is still data (AST nodes instead of lists)
- `quote` returns AST nodes, not strings
- `unquote` (~) splices values into AST
- Pattern matching on code structure works naturally
- `lift`/`run`/`EM` operate on AST nodes

### Surface Syntax Examples

```
-- Function definition
def fact(n) =
  if n == 0 then 1
  else n * fact(n - 1)

-- Desugars to: (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

-- Type definition with weak annotation
type DList {
  value: int
  next: DList
  prev: DList :weak
}

-- Desugars to: (deftype DList (value int) (next DList) (prev DList :weak))

-- Quoting produces AST data
let code = quote(x => x + 1)
-- code = LamExpr(['x], BinOpExpr('+, SymExpr('x), IntExpr(1)))

-- Pattern matching on code
def simplify(expr) =
  match expr {
    BinOpExpr('+, IntExpr(0), x) => x      -- 0 + x → x
    BinOpExpr('+, IntExpr(n), IntExpr(m)) => IntExpr(n + m)  -- constant fold
    _ => expr
  }

-- Staging with unquote
def staged_power(n) =
  quote(x => ~(fold n (fn acc => quote(~acc * x)) quote(1)))
```

### Implementation Tasks

| Task | File | Description |
|------|------|-------------|
| P.1 | `pkg/ast/expr.go` | Define AST node types using deftype |
| P.2 | `pkg/parser/pika.go` | Implement Pika parsing algorithm |
| P.3 | `pkg/parser/grammar.go` | Define Purple surface grammar |
| P.4 | `pkg/parser/desugar.go` | Transform Pika AST → Purple AST nodes |
| P.5 | `pkg/eval/quote.go` | Implement quote/unquote for AST construction |
| P.6 | `pkg/eval/match.go` | Pattern matching on AST nodes |
| P.7 | `pkg/eval/eval.go` | Extend eval to interpret AST nodes |

### AST Node Types

```scheme
;; Core expression types (defined using deftype)
(deftype Expr (tag sym) (data Obj))

(deftype IntExpr (value int))
(deftype FloatExpr (value float))
(deftype SymExpr (name sym))
(deftype BinOpExpr (op sym) (left Expr) (right Expr))
(deftype UnaryExpr (op sym) (arg Expr))
(deftype AppExpr (fn Expr) (args List))
(deftype LamExpr (params List) (body Expr))
(deftype IfExpr (cond Expr) (then Expr) (else Expr))
(deftype LetExpr (bindings List) (body Expr))
(deftype QuoteExpr (expr Expr))
(deftype UnquoteExpr (expr Expr))
(deftype MatchExpr (scrutinee Expr) (cases List))
```

### Pika Algorithm Key Properties

1. **Right-to-left parsing** - Natural bottom-up construction
2. **Left recursion handling** - `a + b + c` parses correctly
3. **O(n) complexity** - Linear time for unambiguous grammars
4. **Error recovery** - Better error messages than recursive descent

### Architecture

```
User Input ──► Pika Parser ──► AST Nodes ──► Tower of Interpreters
    │              │              │                   │
"1 + 2"      PikaNode(...)   BinOpExpr(...)    eval/lift/run
                                  │
                                  └── First-class data, quotable
```

### Acceptance Criteria

- [ ] `def f(x) = x + 1` parses and evaluates correctly
- [ ] `quote(1 + 2)` returns `BinOpExpr('+, IntExpr(1), IntExpr(2))`
- [ ] `match expr { BinOpExpr(...) => ... }` works
- [ ] `~x` (unquote) splices values into quoted code
- [ ] Tower operations (lift/run/EM) work with AST nodes
- [ ] Error messages include line/column information

### References

- [Pika Parsing Paper](https://arxiv.org/abs/2005.06444) - Campagnola, 2020
- [Sweet Expressions (SRFI-110)](https://srfi.schemers.org/srfi-110/) - Readable Lisp
- [Julia Metaprogramming](https://docs.julialang.org/en/v1/manual/metaprogramming/) - AST as data
- [Elixir Macros](https://elixir-lang.org/getting-started/meta/macros.html) - Quote/unquote model
