# C3 Style Guide (Omni)

This guide defines how we use C3 in Omni for production-grade code:

- Deterministic memory behavior
- No stop-the-world GC assumptions
- Scope/lifetime ownership that is explicit and reviewable
- Error handling that is typed and local
- Compile-time checks to prevent runtime surprises

This document is based on the official C3 language docs and adapted to this codebase.

## 1. Core Principles

1. Prefer explicit lifetimes over implicit behavior.
2. Prefer typed failures (`fault` + optionals) over stringly-typed errors.
3. Prefer compile-time validation (`$assert`, contracts) over runtime debugging.
4. Prefer small, predictable control flow over clever abstractions.
5. Keep language features orthogonal: use the feature that matches the job.

## 2. Error Model: Optionals + Faults

C3 optionals are a first-class error model. Use them as the default for fallible operations.

### Rules

- New fallible APIs should return optionals (`T!` or `T?`) instead of `null` sentinels.
- Define fault categories with `faultdef` and propagate them across layers.
- Use `!` only when intentionally propagating to the caller.
- Use `if (catch err = ...)` to branch on failure close to the boundary.
- Use `??` only when a safe default is semantically valid.
- Avoid `!!` in production paths. Reserve it for tests/bootstrap where abort-on-error is intended.

### Example

```c3
faultdef IO_OPEN_FAILED, IO_READ_FAILED;

fn char[]! read_config(char[] path) {
    if (catch err = io::file::open((String)path, "r")) {
        return IO_OPEN_FAILED?;
    } else if (try f = io::file::open((String)path, "r")) {
        defer (void)f.close();
        if (try s = f.read_all()) return s;
        return IO_READ_FAILED?;
    }
}
```

## 3. `defer` Discipline

`defer` is mandatory for cleanup symmetry. Acquire, immediately register cleanup, then proceed.

### Rules

- Cleanup must be declared immediately after acquisition.
- Cleanup order should be obvious from lexical order (LIFO).
- For temporary state changes (scope swaps, flags), always restore via `defer`.
- Use `defer (catch err) { ... }` for error-only cleanup logic.

### Example

```c3
main::ScopeRegion* saved = interp.current_scope;
interp.current_scope = child_scope;
defer interp.current_scope = saved;

MdbTxn* txn = null;
if (mdb_txn_begin(env, null, 0, &txn) != MDB_SUCCESS) return DB_TXN_FAILED?;
defer mdb_txn_abort(txn);
```

## 4. Contracts (`@require`, `@ensure`, `@param`, `@return`)

Use contracts to encode invariants that reviewers otherwise have to infer.

### Rules

- Add `@require` for non-obvious preconditions:
  - expected tags
  - non-null pointers
  - valid scope state
  - ownership expectations
- Add `@ensure` for critical postconditions (constructed object shape, restored state, etc.).
- Use `@param` when pointer/reference mutation semantics are not obvious.
- Keep contracts truthful and cheap. Do not add decorative contracts.

### Example

```c3
<*
@require v != null
@require v.tag == CONS : "car requires CONS"
@ensure return != null
*>
fn Value* car(Value* v) @inline {
    return v.cons_val.car;
}
```

## 5. Attributes: Use Intentionally

C3 has rich attributes. Use them to make intent machine-checkable.

### Rules

- `@inline`: tiny hot-path functions only (pure accessors, tag checks).
- `@noinline`: cold/error paths or large functions to keep hot code small.
- `@nodiscard`: use on APIs where ignoring the return value is almost always a bug.
- `@maydiscard`: only for APIs intentionally called for side effects.
- `@private` / `@local`: default internal visibility; expose only stable API points.
- `@if`: use for top-level conditional compilation.
- `@test` / `@benchmark`: use for self-contained language-level unit/perf tests when practical.
- Sanitizer-disabling attributes: exceptional use only; require comment with rationale and issue reference.

## 6. Compile-Time Features

Treat compile-time checks as guardrails, not garnish.

### Rules

- Use `$assert` for:
  - enum/table sync
  - struct size/alignment assumptions
  - ABI/layout expectations
  - array length invariants
- Use `$if`/`$switch` inside expressions and macro code.
- Use `@if` for top-level declaration-level conditional compilation.
- Use compile-time loops (`$for`, `$foreach`) to remove boilerplate when structure is regular.
- Keep macro metaprogramming deterministic and readable.

### Example

```c3
$assert(ValueTag.values.len == 18, "ValueTag changed: update dispatch table");
$assert(ScopeChunk.sizeof % 16 == 0, "ScopeChunk alignment changed");
```

## 7. Macros vs Functions vs Generics

Do not default to macros.

### Rules

- Use normal functions first.
- Use generics for type-parameterized behavior.
- Use macros only when you need:
  - compile-time code generation
  - expression-parameter behavior
  - syntax-level ergonomics impossible with functions/generics
- Macro bodies must be transparent and small enough to review without expansion tools.
- Avoid macro tricks that hide control flow or ownership.

## 8. Memory and Lifetime Rules (Omni-Specific)

Omni uses explicit lifetimes and scope-based memory management. Respect boundaries.

### Rules

- Every allocation strategy must have one clear owner:
  - scope/region allocation
  - heap allocation (`mem::malloc`/`mem::free`)
  - allocator/pool-backed temporary memory
- Never free scope-owned pointers with `mem::free`.
- Never assume heap pointers are scope-managed unless explicitly registered with a scope destructor.
- Register destructors only for heap-backed payloads that outlive stack scopes.
- Keep cross-scope promotion explicit and centralized.
- If ownership crosses API boundaries, document who releases and when.

### For this codebase

- `Value*`/`Env*` in interpreter paths are scope-managed unless explicitly allocated otherwise.
- C interop/resource handles must have explicit teardown policy.
- Coroutine/continuation resources must be reclaimed via lifecycle APIs, not only at process exit.

## 9. Iteration and Data Access

### Rules

- Prefer `foreach` over index loops when no index math is required.
- Use `foreach (&x : items)` for mutation.
- Use slices over raw pointer+len pairs when possible.
- Avoid manual bounds arithmetic when slice operations express intent directly.

## 10. Switches and Tagged Unions

### Rules

- Prefer exhaustive `switch` on enums/tags.
- Avoid catch-all `default` in tag dispatch unless there is a deliberate forward-compatible fallback.
- Keep tag-to-union-field handling local and obvious.

## 11. API Shape and Visibility

### Rules

- Keep module public surface small and explicit.
- Internal helper functions default to internal visibility.
- Public APIs should have stable names, contracts, and failure semantics.
- If a function mutates global/interpreter state, name and document that side effect clearly.

## 12. Testing and Build Modes

### Rules

- Validate both correctness and lifetime behavior.
- Run normal builds and sanitizer builds for memory-sensitive changes.
- Add regression tests for lifecycle invariants, not only result values.
- Prefer tests that prove ownership/promotion/release behavior under repeated execution.

## 13. Review Checklist

Before merging C3 changes, verify:

1. Fallible paths use optionals/faults (not silent nulls).
2. Every acquired resource has an adjacent `defer`.
3. Contracts cover non-obvious invariants.
4. Compile-time invariants are asserted where layout/size matters.
5. Ownership across scopes is explicit and test-covered.
6. No macro added where function/generic is sufficient.

## 14. Boundary Hardening Rules (Omni Runtime)

These rules are mandatory for `boundary_*` runtime work and related eval/JIT paths.

1. Macro vs function:
   Use macros only for instrumentation scaffolding (counters/trace/benchmark wrappers). Keep ownership transitions, session/txn lifecycle, and control flow in normal functions.
2. Hot-path helper policy:
   Only tiny predicates/accessors and state checks belong in `@inline` hot paths. Move verbose reporting, graph-audit traversal, and telemetry dumps to `@noinline`.
3. Compile-time instrumentation gates:
   All boundary telemetry/trace/benchmark behavior must be compile-time gated so default builds have zero or minimal overhead.
4. Legal ownership-transition entrypoints:
   Cross-scope ownership transitions must flow through boundary facade helpers only (`boundary_finalize_scoped_result`, `boundary_commit_escape`, `boundary_copy_from_releasing_scope`, `boundary_copy_to_scope_site`, and env-copy boundary helpers). Do not introduce ad-hoc direct promotion/copy in eval/JIT callsites.
5. Debug graph-audit invariant:
   For any committed ESCAPE root, no reachable Omni-owned edge may point into TEMP. Traversed edge sets must be explicit, and opaque foreign payload wrappers must stay excluded unless they gain explicit Omni-owned edges.
6. No hidden global state mutation without clear naming/contracts.
7. Tests include at least one failure-path or lifecycle assertion for the touched area.

## 15. Migration Guidance (Underused Features)

When touching older code, incrementally modernize:

1. Convert null-return fallible helpers to optionals.
2. Replace manual cleanup ladders with `defer`.
3. Add `@require` at unsafe tag/ptr boundaries.
4. Add `$assert` for table/enum and layout assumptions.
5. Add `@nodiscard` to critical result-returning APIs.

Do this opportunistically in touched code; avoid giant style-only refactors unless requested.
