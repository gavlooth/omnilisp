# Eliminate Capped Recursion Anti-Pattern — Plan

**Date:** 2026-05-01
**Status:** Open plan, awaiting TODO-backed queue activation.
**Owner:** Agent-driven, human-reviewed before implementation.

## 1. Anti-Pattern Definition

A **capped recursion** is any C3 function that:

1. Recursively calls itself to walk an unbounded data structure (AST, Value graph, env chain, parse tree).
2. Carries a `depth` parameter or uses a static depth counter.
3. Fails with an artificial limit (`depth > N`) to prevent C stack overflow.

This is an anti-pattern because:
- The cap is a **band-aid for a design flaw**: recursion depth should not be bounded by C stack limits.
- The cap values are **arbitrary** (64, 128, 256, 1024, 4096, 10000) with no relation to actual language semantics.
- The caps **violate user expectations**: a deeply nested quasiquote, a long cons list, or a deep type alias chain is valid Omni Lisp code that should work.
- The caps **hide real bugs**: if a user hits the cap, they get a misleading error instead of a correct result.

## 2. Audit Findings — All Capped Recursion Sites

| # | File | Function | Cap | Data Structure | Risk | Strategy |
|---|------|----------|-----|----------------|------|----------|
| 1 | `src/lisp/jit_quasiquote_macros.c3` | `jit_qq_impl` | 64 | Expr AST (quasiquote) | **High** — JIT runtime hits user templates | Explicit worklist |
| 2 | `src/lisp/jit_quasiquote_macros.c3` | `jit_qq_expand_elements` | 64 (items) | Spliced cons list | **Medium** — Fixed array `Value*[64]` | Dynamic alloc |
| 3 | `src/lisp/jit_quasiquote_macros.c3` | `jit_qq_expand_call` | 64 (items) | Spliced cons list | **Medium** — Fixed array `Value*[64]` | Dynamic alloc |
| 4 | `src/lisp/compiler_quasiquote_flat.c3` | `compile_qq_flat` | 64 | Expr AST (quasiquote) | **High** — AOT compile-time | Two-pass dep graph |
| 5 | `src/lisp/parser_expr_atoms.c3` | `parse_expr` | 1024 | Expr AST (nesting) | **Medium** — Parser hits user code | Iterative descent or raise |
| 6 | `src/lisp/compiler_expr_serialize_values.c3` | `serialize_value_to_buf_depth` | 128 | Value graph (cons/array/hashmap/set) | **Medium** — AOT serialization | Explicit stack |
| 7 | `src/lisp/stable_escape_store.c3` | `stable_escape_prepare_graph_node` | 128 | Value graph (all tags) | **High** — Boundary runtime | Explicit worklist |
| 8 | `src/lisp/eval_env_copy_helpers.c3` | `copy_env_to_scope_inner_checked` | 256 | Env chain | **Medium** — Boundary runtime | Iterative while loop |
| 9 | `src/lisp/eval_env_copy_frame_helpers.c3` | `copy_env_is_terminal_frame` | 256 | Env chain | **Medium** — Boundary runtime | Iterative while loop |
| 10 | `src/lisp/macros_template_expansion.c3` | `append_values` | 10000 | Cons spine (splice) | **Medium** — Macro expansion | Trivial tail-rec → loop |
| 11 | `src/lisp/macros_template_validation.c3` | `macro_validate_emitted_form` | 4096 | Value tree (emitted form) | **Low** — Validation only | Explicit stack |
| 12 | `src/lisp/jit_eval_scope_chain_helpers.c3` | `jit_binding_graph_needs_copy` | 128 | Value graph (cons car/cdr) | **High** — JIT boundary runtime | Explicit stack |
| 13 | `src/lisp/primitives_meta_types_ctor_helpers.c3` | `ctor_match_type_form` | 8 | Value tree (ctor form) | **Low** — Type checking | Explicit stack |
| 14 | `src/lisp/eval_boundary_provenance.c3` | `boundary_alias_push_visit` family | 128 | Value graph / env graph | **High** — Boundary runtime | Already uses explicit stack; review recursive helpers |
| 15 | `src/pika/traverse.c3` | `print_tree` | None | Parse tree (submatches) | **Low** — Debug only | Add cap or make iterative |
| 16 | `src/entry_fmt_stack.c3` | `fmt_push_frame` | 512 | Format stack | **Low** — Formatter | Already explicit stack; cap is fine |

### Notable Non-Issues (Loops, Not Recursion)

| File | Function | Cap | Why It's Fine |
|------|----------|-----|---------------|
| `src/lisp/value_type_registry.c3` | `resolve_alias` | 16 | `for` loop, not recursion |
| `src/lisp/value_type_registry.c3` | `is_subtype` | 16 | `for` loop, not recursion |
| `src/lisp/parser_lexer_whitespace.c3` | (comment depth) | N/A | `while` loop tracking paren depth |

## 3. Risk Prioritization

### Tier 1 — Eliminate First (User-Visible, Runtime, High Blast Radius)

1. **`jit_qq_impl`** (JIT quasiquote evaluation) — Users write macro templates. A 64-depth cap is absurdly low for generated code.
2. **`compile_qq_flat`** (AOT quasiquote lowering) — Same data structure, same user impact.
3. **`stable_escape_prepare_graph_node`** (Boundary graph preparation) — Any deeply nested value (e.g., nested JSON, nested list) hits this at runtime.
4. **`jit_binding_graph_needs_copy`** (JIT boundary copy detection) — Same as above.
5. **`copy_env_to_scope_inner_checked`** (Env chain copy) — Long closure capture chains are valid.

### Tier 2 — Eliminate Second (Compile-Time or Lower Frequency)

6. **`serialize_value_to_buf_depth`** (AOT value serialization) — Only hits deeply nested literals in compiled output.
7. **`parse_expr`** (Parser nesting) — 1024 is generous but still arbitrary. A file with 1025 nested parens is valid.
8. **`append_values`** (Macro splice append) — 10000 is high but still a cap. Trivial to remove.

### Tier 3 — Accept or Defer (Low Impact, Debug-Only, Already Explicit Stack)

9. **`macro_validate_emitted_form`** — Validation only; 4096 is deep enough for any real macro.
10. **`ctor_match_type_form`** — 8 is shallow but constructor nesting >8 is pathological in practice.
11. **`print_tree`** — Debug-only; add a small cap and document it.
12. **`fmt_push_frame`** — Already uses an explicit `FmtFrame[512]` array; the cap is a bounded-stack invariant, not a recursion workaround.

## 4. Refactoring Strategies by Pattern

### 4.1 Tree Walker → Explicit Worklist (jit_qq_impl, stable_escape_prepare_graph_node, jit_binding_graph_needs_copy)

Replace recursive descent with a heap-allocated stack of `(node, depth, phase)` tuples.

```c3
struct QqWorkItem {
    Expr* expr;
    usz depth;
    QqPhase phase;  // ENTER, PROCESS_CAR, PROCESS_CDR, FINALIZE
}
```

The driver loop:
```c3
while (stack.len > 0) {
    QqWorkItem item = stack.pop();
    switch (item.phase) {
        case ENTER: /* push children with PROCESS/ENTER phases */;
        case PROCESS_CAR: /* process car result */;
        case PROCESS_CDR: /* process cdr result */;
        case FINALIZE: /* combine results */;
    }
}
```

**Benefits:** No C stack growth; depth is bounded only by heap memory.
**Costs:** More code; need to manage result staging.
**Applies to:** #1, #7, #12.

### 4.2 Two-Pass Dependency Graph → Topological Emit (compile_qq_flat)

The compiler emits linear C3 text with temp registers (`_r0`, `_r1`, ...). Recursion naturally produces the right dependency order.

Two-pass approach:
1. **Build pass**: Walk the AST iteratively, build a DAG of `QqNode` structs with edges (dependencies).
2. **Emit pass**: Topologically sort the DAG (it's already a tree, so reverse post-order), emit nodes in order.

```c3
struct QqCompileNode {
    usz id;           // temp register number
    ExprTag tag;      // what kind of node
    usz[4] deps;      // dependency node ids
    usz dep_count;
}
```

**Benefits:** Eliminates recursion; preserves register ordering.
**Costs:** Significant code restructuring; need to manage node allocation.
**Applies to:** #4.

### 4.3 Tail Recursion → Loop (append_values)

`append_values` is tail-recursive on the cons spine:

```c3
fn Value* append_values(Value* a, Value* b, Interp* interp) {
    // Build reversed list of cars
    Value* reversed = make_nil(interp);
    Value* cur = a;
    while (!is_nil(cur)) {
        if (!is_cons(cur)) return make_error(...);
        reversed = make_cons(interp, cur.cons_val.car, reversed);
        cur = cur.cons_val.cdr;
    }
    // Reverse onto b
    while (!is_nil(reversed)) {
        b = make_cons(interp, reversed.cons_val.car, b);
        reversed = reversed.cons_val.cdr;
    }
    return b;
}
```

**Benefits:** Trivial; no depth limit.
**Costs:** One extra pass (reversal).
**Applies to:** #10.

### 4.4 Linked List / Chain → Iterative While Loop (copy_env_to_scope_inner_checked)

Env chains are singly-linked lists. Convert recursive parent traversal to a loop that walks `env.parent` until `global_env` or a terminal condition.

```c3
Env* cur = env;
while (cur != null && cur != interp.global_env && !copy_env_is_terminal_frame(cur, ...)) {
    // process frame
    cur = cur.parent;
}
```

**Benefits:** Trivial; no depth limit.
**Costs:** Need to handle the "materialize frame" logic iteratively (may need a two-pass: first collect frames, then materialize in reverse).
**Applies to:** #8, #9.

### 4.5 Fixed Array → Dynamic Allocation (jit_qq_expand_elements, jit_qq_expand_call)

The `Value*[64] items` arrays for splicing can be replaced with a `List<Value*>` or `mem::malloc`ed array.

```c3
List{Value*} items;
items.init(mem);
while (is_cons(tmp)) {
    items.push(tmp.cons_val.car);
    tmp = tmp.cons_val.cdr;
}
if (!is_nil(tmp)) return eval_error("...@ value is not a proper list");
for (usz j = items.len(); j > 0; j--) {
    // ...
}
```

**Benefits:** No item count limit.
**Costs:** Slightly more allocation; need `defer items.free()`.
**Applies to:** #2, #3.

### 4.6 Parser Nesting → Raise Cap or Iterative (parse_expr)

The parser's `depth` counter tracks expression nesting via `self.depth++` / `defer self.depth--`. The 1024 limit is generous (each frame is small), but it's still arbitrary.

Options:
- **Option A**: Raise `PARSER_MAX_NESTING` to 8192 or 16384. The C stack frame for `parse_expr` is tiny (~32 bytes). 8192 frames = ~256 KB, well within an 8 MB stack.
- **Option B**: Make parsing fully iterative using an explicit parse stack. This is a major refactor.

**Recommendation**: Option A for now. The parser is not a hot path for deep nesting, and 1024→8192 is a safe, one-line change.
**Applies to:** #5.

## 5. Validation Strategy

Every refactor must include:

1. **Regression test** that exercises the old cap + 1 (e.g., 65-deep quasiquote, 129-deep value serialization).
2. **Performance test** to ensure the iterative version is not slower than recursive for shallow inputs.
3. **ASAN/Valgrind pass** for any new heap allocations.
4. **Build + full test suite** pass.

### Specific Test Additions

- `tests_runtime_feature_jit_groups_failures.c3`: Add `run_jit_policy_quasiquote_deep_nesting_test` with 128-deep and 256-deep templates.
- `tests_compiler_codegen_groups.c3`: Add AOT quasiquote 128-depth compilation test.
- `tests_memory_lifetime_boundary_groups.c3`: Add deeply nested value boundary tests.
- `tests_advanced_macro_hygiene_groups.c3`: Add macro splice with >64 items.

## 6. Implementation Order

Recommended order, smallest to largest blast radius:

1. **Phase 0: Trivial wins** — `append_values` (#10), `jit_qq_expand_elements/call` (#2, #3), parser cap raise (#5).
2. **Phase 1: JIT quasiquote** — `jit_qq_impl` (#1). High user impact, well-contained.
3. **Phase 2: AOT quasiquote** — `compile_qq_flat` (#4). Highest complexity.
4. **Phase 3: Boundary graph** — `stable_escape_prepare_graph_node` (#7), `jit_binding_graph_needs_copy` (#12).
5. **Phase 4: Env copy** — `copy_env_to_scope_inner_checked` (#8).
6. **Phase 5: Serializer** — `serialize_value_to_buf_depth` (#6).

Each phase must be a separate commit with tests.

## 7. Policy — No New Capped Recursion

Add to `docs/C3_STYLE.md` or `CLAUDE.md`:

> **No new capped recursion.** If a function recurses on user data (AST, Value graph, env chain, parse tree), it must use an explicit heap-allocated stack or worklist. Depth caps are acceptable only for:
> 1. Explicit bounded stacks that are part of the data structure (e.g., `FmtFrame[512]`).
> 2. Debug-only output where correctness is not affected (e.g., `print_tree`).
> 3. Loops with a counter, not recursive calls.

## 8. References

- `src/lisp/jit_quasiquote_macros.c3:31-91` — JIT quasiquote recursion
- `src/lisp/compiler_quasiquote_flat.c3:86-138` — AOT quasiquote recursion
- `src/lisp/parser_expr_atoms.c3:74-78` — Parser depth cap
- `src/lisp/compiler_expr_serialize_values.c3:10-85` — Value serializer recursion
- `src/lisp/stable_escape_store.c3:162-250` — Stable escape graph recursion
- `src/lisp/eval_env_copy_helpers.c3:72-95` — Env copy recursion
- `src/lisp/macros_template_expansion.c3:179-186` — Macro splice tail recursion
- `src/lisp/jit_eval_scope_chain_helpers.c3:465-500` — JIT binding graph recursion
- `src/lisp/eval_boundary_provenance.c3:1-200` — Boundary alias graph (mixed)
- `docs/C3_STYLE.md` — C3 coding standard (add anti-pattern rule)
