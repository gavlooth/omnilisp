# Native Compiler Memory Management Integration Plan

## Goal
Wire shape analysis, Symmetric RC, weak edges, and ASAP strategies into `pkg/compiler/compiler.go` to match the design in CLAUDE.md.

## Current State
- Native compiler uses simple RC everywhere (~95%)
- Shape analysis exists in `pkg/analysis/shape.go` but NOT wired
- Symmetric RC exists in `pkg/memory/symmetric.go` but NOT wired
- Weak edge detection exists in `pkg/analysis/backedge.go` but NOT wired

## Target State
| Technique | Target % | Data Structures |
|-----------|----------|-----------------|
| Pure ASAP | 60% | Trees, lists, records |
| Reference Counting | 20% | DAG-shaped shared data |
| Symmetric RC | 5-10% | Cyclic structures |
| Stack Allocation | 5% | Non-escaping temps |
| Arena | 3% | Complex temp cycles |
| Weak Edges | 2% | Back-pointers |

---

## Phase 1: Infrastructure Setup
- [ ] P1.1 Add ShapeContext to Compiler struct
- [ ] P1.2 Add BackEdgeAnalyzer to Compiler struct
- [ ] P1.3 Add CyclicStrategy field to track per-allocation strategy
- [ ] P1.4 Create VarMemInfo struct (shape, strategy, escape class)
- [ ] P1.5 Update VarInfo to include VarMemInfo

## Phase 2: Shape Analysis Integration
- [ ] P2.1 Call `shapeCtx.AnalyzeShapes()` for each expression
- [ ] P2.2 Store shape results in scope (Tree/DAG/Cyclic/Unknown)
- [ ] P2.3 Propagate shape through let bindings
- [ ] P2.4 Propagate shape through function returns
- [ ] P2.5 Add shape info to function summaries

## Phase 3: Free Strategy Selection
- [ ] P3.1 Implement `selectFreeStrategy(shape, escapeClass)` function
- [ ] P3.2 Tree shape → `free_tree()` (Pure ASAP)
- [ ] P3.3 DAG shape → `dec_ref()` (Reference counting)
- [ ] P3.4 Cyclic + non-escaping → `arena_free()` or `sym_exit_scope()`
- [ ] P3.5 Cyclic + escaping → Symmetric RC
- [ ] P3.6 Unknown → Conservative (dec_ref)

## Phase 4: Symmetric RC Integration
- [ ] P4.1 Add `sym_scope_enter()` calls at function/let entry
- [ ] P4.2 Add `sym_scope_own(obj)` for cyclic allocations
- [ ] P4.3 Add `sym_inc_ref(from, to)` for internal references
- [ ] P4.4 Add `sym_scope_exit()` at scope boundaries
- [ ] P4.5 Track symmetric-managed vars separately from RC vars

## Phase 5: Weak Edge Integration
- [ ] P5.1 Run BackEdgeAnalyzer on deftype definitions
- [ ] P5.2 Mark back-edge fields as weak in TypeRegistry
- [ ] P5.3 Generate `weak_ref()` instead of `inc_ref()` for weak fields
- [ ] P5.4 Skip weak fields in release functions
- [ ] P5.5 Add weak ref invalidation on free

## Phase 6: Stack Allocation (ESCAPE_NONE)
- [ ] P6.1 Identify non-escaping allocations via escape analysis
- [ ] P6.2 Use `mk_int_stack()` for non-escaping integers
- [ ] P6.3 Add stack pool management per function
- [ ] P6.4 Auto-free stack objects at scope exit (no RC needed)

## Phase 7: Arena Allocation
- [ ] P7.1 Detect temp cyclic structures (don't escape function)
- [ ] P7.2 Add `arena_create()` at function entry when needed
- [ ] P7.3 Route allocations to `arena_alloc()` instead of `malloc()`
- [ ] P7.4 Add `arena_destroy()` at function exit
- [ ] P7.5 No individual frees needed for arena objects

## Phase 8: Code Generation Updates
- [ ] P8.1 Update `compileLet` to emit shape-aware frees
- [ ] P8.2 Update `compileApply` to track result shapes
- [ ] P8.3 Update `genLambdaFunc` for symmetric scope handling
- [ ] P8.4 Update `compileIf` to merge shapes from branches
- [ ] P8.5 Add debug comments showing chosen strategy

## Phase 9: Runtime Code Updates
- [ ] P9.1 Ensure `GenerateSymmetricRuntime()` is called
- [ ] P9.2 Add missing Symmetric RC C functions if needed
- [ ] P9.3 Add arena runtime functions if not present
- [ ] P9.4 Add weak reference runtime support
- [ ] P9.5 Verify all free strategies have C implementations

## Phase 10: Testing & Validation
- [ ] P10.1 Fix pkg/codegen build error
- [ ] P10.2 Fix BackEdge integration tests
- [ ] P10.3 Run AddressSanitizer tests - expect pass
- [ ] P10.4 Run Valgrind tests - expect no leaks
- [ ] P10.5 Add native compiler unit tests
- [ ] P10.6 Add shape-specific test cases
- [ ] P10.7 Benchmark: compare RC-only vs shape-aware

---

## Implementation Order

```
Week 1: Phase 1-2 (Infrastructure + Shape Analysis)
        ↓
Week 2: Phase 3-4 (Free Strategy + Symmetric RC)
        ↓
Week 3: Phase 5-6 (Weak Edges + Stack Alloc)
        ↓
Week 4: Phase 7-8 (Arena + Codegen Updates)
        ↓
Week 5: Phase 9-10 (Runtime + Testing)
```

## Files to Modify

| File | Changes |
|------|---------|
| `pkg/compiler/compiler.go` | Add shape ctx, strategy selection, codegen |
| `pkg/codegen/runtime.go` | Ensure all strategies have C code |
| `pkg/analysis/shape.go` | May need minor API adjustments |
| `pkg/analysis/backedge.go` | May need compiler-friendly API |
| `test/native_memory_test.go` | New test file |

## Success Criteria

1. All existing tests pass
2. AddressSanitizer shows no memory errors
3. Valgrind shows no leaks
4. Shape analysis correctly identifies 80%+ of allocations
5. Cyclic structures handled by Symmetric RC (no leaks)
6. Performance: no regression vs current RC-only approach

## Rollback Plan

If issues arise, the shape analysis can be disabled by:
```go
// In compiler.go
const USE_SHAPE_ANALYSIS = false // Toggle off
```

This falls back to simple RC for everything (current behavior).
