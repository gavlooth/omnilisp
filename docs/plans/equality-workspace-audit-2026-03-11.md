# Equality Workspace Allocation Audit (2026-03-11)

Scope: `src/lisp/eval_pattern_support.c3` deep equality path in
`values_equal(...)` under nested list/array comparisons.

## Audit Findings

### 1) Per-call heap workspace allocation is unconditional for recursive path

- `values_equal(...)` allocates two heap buffers on entry to recursive mode:
  - `stack` (`EqualPair*`) at initial capacity `256`
  - `seen` (`EqualPair*`) at initial capacity `256`
- Source:
  - `EQUAL_STACK_INITIAL_CAPACITY`, `EQUAL_SEEN_INITIAL_CAPACITY`
  - `mem::malloc` calls in `values_equal(...)`

Impact:
- Every non-trivial `CONS`/`ARRAY` equality check incurs heap allocation and
  free churn, even when structure size is small.

### 2) Growth strategy is geometric, but copies existing workspace repeatedly

- When capacity is exceeded, both `stack` and `seen`:
  - allocate a new buffer (`malloc`)
  - copy existing entries in a loop
  - free old buffer
- Source:
  - `CONS` branch growth (`seen` + `stack`)
  - `ARRAY` branch growth (`seen` + `stack`)

Impact:
- Nested comparisons with large breadth/depth amplify allocation + copy cost.
- Array-heavy comparisons can trigger larger `stack` bursts (`needed = stack_count + len`).

### 3) Null-allocation guards are missing in workspace growth path

- Workspace allocations do not guard `malloc` returning null before write/use.
- Source:
  - initial `stack`/`seen` allocations
  - all `new_seen` / `new_stack` growth allocations

Impact:
- Under memory pressure, this can produce unsafe behavior instead of a safe
  deterministic failure outcome.

### 4) Cycle tracking uses linear scan

- `seen` lookup is linear (`for i in seen_count`) for each candidate pair.
- Source:
  - `already_seen` checks in both `CONS` and `ARRAY` branches.

Impact:
- Degenerates toward quadratic behavior on large cyclic/shared graphs.
- Combined with allocation churn, this is the primary pressure point for O3.

## Summary for O3

- O3.1 audit is complete.
- O3.2 implementation follow-up is complete:
  - inline-first bounded workspace for common cases,
  - safe allocation failure handling during growth,
  - heap buffers only when inline capacity is exceeded.
- O3.3 benchmark/regression slice is documented in:
  - `docs/plans/equality-nested-benchmark-baseline-2026-03-11.md`
