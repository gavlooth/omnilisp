# OmniLisp TODO (Active Tasks)

This file contains only active tasks: `[TODO]`, `[IN_PROGRESS]`, and `[BLOCKED]`.

**Completed tasks:** See `TODO_COMPLETED.md`

**Last Updated:** 2026-01-17

---

## Active Session: Completion Plan Phases 5.2-5.3 [DONE]

### Phase 5.2: Pattern Matching Completeness [DONE]

- [DONE] Label: CP-5.2-pattern-bindings-runtime
  Location: `runtime/src/runtime.c:504-722`
  What was done:
  - Added `PatternBindings` struct (max 64 bindings)
  - Added `is_pattern_match_with_bindings()` - matches and extracts variables
  - Added `pattern_bindings_to_dict()` - converts bindings to Obj dict
  - Added `prim_pattern_match()` - returns bindings dict or NULL
  Code snippet:
  ```c
  typedef struct { const char* name; Obj* value; } PatternBinding;
  typedef struct { PatternBinding bindings[64]; int count; } PatternBindings;

  int is_pattern_match_with_bindings(Obj* pattern, Obj* value, PatternBindings* bindings) {
      // Symbol patterns bind to value
      if (ptag == TAG_SYM) {
          pattern_bindings_add(bindings, sym, value);
          return 1;
      }
      // 'as' pattern: [inner_pattern as name]
      if (ptag == TAG_ARRAY && arr->len == 3 && strcmp(middle_str, "as") == 0) {
          is_pattern_match_with_bindings(inner_pattern, value, bindings);
          pattern_bindings_add(bindings, name, value);
      }
  }
  ```

- [DONE] Label: CP-5.2-pattern-var-extraction-codegen
  Location: `csrc/codegen/codegen.c:2673-2743`
  What was done:
  - Added `PatternVarList` struct for compile-time variable extraction
  - Added `extract_pattern_vars()` - recursively collects variable names
  - Skips reserved keywords: `_`, `nil`, `true`, `false`, `as`
  - Handles `as` patterns correctly
  Code snippet:
  ```c
  static void extract_pattern_vars(OmniValue* pattern, PatternVarList* vars) {
      if (omni_is_sym(pattern)) {
          pattern_var_list_add(vars, pattern->str_val);
      }
      if (omni_is_array(pattern) && pattern->array.len == 3) {
          // Check for 'as' pattern
          if (strcmp(middle->str_val, "as") == 0) {
              extract_pattern_vars(pattern->array.data[0], vars);
              pattern_var_list_add(vars, name->str_val);
          }
      }
  }
  ```

- [DONE] Label: CP-5.2-match-codegen-bindings
  Location: `csrc/codegen/codegen.c:2965-3042`
  What was done:
  - Updated `codegen_match` to use `prim_pattern_match` for patterns with variables
  - Emits code to extract bindings into local Obj* variables
  - Falls back to simple `is_pattern_match` when no variables present
  Code snippet (updated to use dict_get_by_name):
  ```c
  if (has_bindings) {
      omni_codegen_emit(ctx, "Obj* _bindings = prim_pattern_match(pattern, _match_value);\n");
      omni_codegen_emit(ctx, "if (_bindings) {\n");
      for (int i = 0; i < vars.count; i++) {
          omni_codegen_emit(ctx, "Obj* %s = dict_get_by_name(_bindings, \"%s\");\n",
                            vars.names[i], vars.names[i]);
      }
      // ... result expression with variables in scope
  }
  ```

- [DONE] Label: CP-5.2-test-pattern-bindings
  Location: `runtime/tests/test_pattern_match.c`
  What was done:
  - 12 comprehensive tests for pattern matching with bindings
  - Tests simple variable binding, nested patterns, as patterns
  - Tests rest patterns, wildcards, literal matching
  - Tests edge cases: length mismatch, insufficient elements, reserved keywords
  All tests pass.

- [DONE] Label: CP-5.2-rest-patterns
  Location: `runtime/src/runtime.c:573-612`
  What was done:
  - Added `find_rest_position()` to detect `&` in pattern arrays
  - Modified `is_pattern_match_with_bindings()` to handle rest patterns
  - Pattern `[x y & rest]` matching `[1 2 3 4 5]` gives x=1, y=2, rest=[3 4 5]
  - Empty rest is allowed: `[x y & rest]` matching `[1 2]` gives rest=[]

- [DONE] Label: CP-5.2-dict-get-by-name
  Location: `runtime/src/runtime.c:1312-1336`
  What was done (bug fix):
  - Added `dict_get_by_name()` function for symbol key lookup by string content
  - Hashmap uses pointer identity for keys, but symbols aren't interned
  - `dict_get_by_name(dict, "x")` iterates buckets and compares symbol names
  - Declared in `omni.h:1110`

### Phase 5.3: Module System [DONE]

- [DONE] Label: CP-5.3-module-struct
  Location: `runtime/src/modules.c:31-94`
  What exists:
  - Module struct with name, exports (linked list), imports (linked list)
  - ModuleExport, ModuleImport helper structs
  - Global module registry (g_module_registry)
  - `get_or_create_module()`, `find_module()`

- [DONE] Label: CP-5.3-module-registry
  Location: `runtime/src/modules.c:53-94`
  What exists:
  - `g_module_registry` - global linked list of modules
  - `prim_module_begin()`, `prim_module_end()` for module definition
  - `prim_module_get()`, `prim_module_list()`
  - `prim_resolve()` for qualified symbol lookup (Module.symbol)

- [DONE] Label: CP-5.3-defmodule-codegen
  Location: `csrc/codegen/codegen.c:3140-3243`
  What was done:
  - Added `codegen_defmodule()` for `(defmodule name (:export ...) (:import ...) body...)`
  - Two-pass approach: first collects exports/imports, then generates body
  - Emits `prim_module_begin()`, body defs, `prim_export()` calls, `prim_module_end()`
  - Added dispatch in `codegen_list` for `defmodule` and `module` forms

- [DONE] Label: CP-5.3-qualified-symbols
  Location: `csrc/codegen/codegen.c:1314-1321`
  What was done:
  - Modified `codegen_sym()` to detect `/` or `.` in symbol names
  - Qualified symbols like `math/add` or `math.add` emit `prim_resolve(mk_string("..."))`
  - Runtime resolution via `prim_resolve()` in modules.c

- [DONE] Label: CP-5.3-import-export-codegen
  Location: `csrc/codegen/codegen.c:3245-3339`
  What was done:
  - Added `codegen_import()` - emits `prim_import(mk_sym("module"), NULL)`
  - Added `codegen_export()` - emits `prim_export(mk_sym("sym"), value)`
  - Added `codegen_require()` - emits `prim_require(mk_sym("module"))`
  - Added dispatch for `import`, `export`, `require` forms

- [DONE] (Review Needed) Label: CP-5.3-import-forms-advanced
  Objective: Support advanced import variations.
  What was done:
  1. Runtime: Added `ImportMode` enum (IMPORT_ALL, IMPORT_ONLY, IMPORT_EXCEPT)
  2. Runtime: Added `prim_import_only()`, `prim_import_as()`, `prim_import_except()`
  3. Runtime: Updated `prim_resolve()` to handle aliased lookups and import restrictions
  4. Codegen: Added `is_quoted_symbol()` helper (`:key` is sugar for `'key`)
  5. Codegen: Added `codegen_symbol_array()` for generating symbol list literals
  6. Codegen: Updated `codegen_import()` to parse `:only`, `:as`, `:except` options
  7. Codegen: Updated `codegen_defmodule()` to handle advanced imports in `:import` clause
  Supported syntax:
    - `(import mod :only [a b])` → `prim_import_only(mk_sym("mod"), ...)`
    - `(import mod :as m)` → `prim_import_as(mk_sym("mod"), mk_sym("m"))`
    - `(import mod :except [x])` → `prim_import_except(mk_sym("mod"), ...)`
    - `(import (mod :only [a b]))` → nested form (same semantics)
    - `(:import (mod :as m))` → inside defmodule

---

## Review Directive

**All newly implemented features must be marked with `[DONE] (Review Needed)` until explicitly approved by the user.**
- When an agent completes implementing a feature, mark it `[DONE] (Review Needed)` (not `[DONE]`)
- If a feature is blocked, mark it `[BLOCKED]` with a reason
- `[DONE] (Review Needed)` means: code is written and working, but awaits user review/approval
- After user approval, move completed tasks to `TODO_COMPLETED.md`
- Workflow: `[TODO]` → implement → `[DONE] (Review Needed)` → user approves → move to archive

---

## Global Directives (Read First)

### Repo Root + Path Discipline

**Do not assume an absolute clone path.** All paths are **repo-relative**.

When writing or running verification commands:
- Prefer: `REPO_ROOT="$(git rev-parse --show-toplevel)"` then use `"$REPO_ROOT/..."`
- Prefer `rg -n "pattern" path/` over `ls ... | grep ...`
- Prefer `find path -name 'pattern'` over `ls | grep` for existence checks

### Transmigration Directive (Non-Negotiable)

**Correctness invariant:** For every in-region heap object `src` reached during transmigration, `remap(src)` yields exactly one stable destination `dst`, and all pointer discovery/rewrites happen only via metadata-driven `clone/trace`.

**Do not bypass the metadata-driven transmigration machinery for "fast paths".**

### Issue Authoring Directive

- **Append-only numbering:** Never renumber existing issues
- **No duplicates:** One header per issue number
- **Status required:** Every task line must be `[TODO]`, `[IN_PROGRESS]`, `[DONE] (Review Needed)`, `[BLOCKED]`, or `[N/A]`

### Jujutsu Commit Directive

**Use Jujutsu (jj) for ALL version control operations.**

Before beginning ANY implementation subtask:
1. Run `jj describe -m "Issue N: task description"`
2. Run `jj log` to see current state
3. If mismatch: Either `jj squash` to consolidate or `jj new` to start fresh

---

## Design Decisions

### Data Structure Simplification (2026-01-15)

**Decision:** OmniLisp uses exactly **3 core collection types**:

| Type | Syntax | Use Case |
|------|--------|----------|
| **List** | `(1 2 3)` | Cons cells, code representation, recursive processing |
| **Array** | `[1 2 3]` | Mutable, indexed access, general sequences |
| **Dict** | `#{:a 1 :b 2}` | Key-value storage, structured data |

**Deprecated types** (runtime support retained for backward compatibility):
- **Tuple** → Use arrays instead (same semantics, simpler model)
- **Named Tuple** → Use dicts instead (`:key value` pairs)
- **Alist/Plist** → Use dicts instead (unified key-value abstraction)

**Rationale:**
1. Tuples provide no semantic benefit over arrays in a Region-RC model
2. Named tuples are just dicts with ordered iteration (dicts suffice)
3. Alists/Plists are legacy Lisp patterns; dicts are more intuitive
4. Fewer types = simpler mental model, consistent destructuring syntax
5. No immutability notation needed: Region-RC handles mutation safety at region boundaries

**Documentation updated:**
- `docs/SYNTAX.md` - Collection syntax
- `language_reference.md` - Destructuring patterns, type examples
- `docs/QUICK_REFERENCE.md` - Let destructuring
- `docs/SYNTAX_REVISION.md` - Character Calculus table, variance examples
- `docs/LANGUAGE_COMPLETENESS_REPORT.md` - Data types status

**Runtime deprecation:**
- `mk_tuple()`, `mk_tuple_region()` - marked deprecated
- `mk_named_tuple()`, `mk_named_tuple_region()` - marked deprecated
- `print_tuple()`, `print_named_tuple()` - marked deprecated

---

## Issue 19: FFI Implementation (C Library Interop) [DONE] (Review Needed)

**Objective:** Enable calling C library functions directly from OmniLisp via `ccall` syntax.

**Priority:** HIGH - Enables delegation of primitives to existing C libraries.

**Completed:** 2026-01-17

**Reference (read first):**
- Plan file: `~/.claude/plans/jiggly-doodling-sprout.md` (Phase 0)
- `csrc/codegen/codegen.c`
- `runtime/include/omni.h`

### Syntax

```lisp
(ccall "libm.so.6" "sin" [x {CDouble}] {CDouble})
(ccall "libc.so.6" "strlen" [s {CString}] {CSize})
(ccall "libc.so.6" "puts" [s {CString}] {Nothing})
```

### Type Mapping

| OmniLisp | C Type | To-C | From-C |
|----------|--------|------|--------|
| `{CInt}` | `int` | `obj_to_cint()` | `mk_int()` |
| `{CDouble}` | `double` | `obj_to_cdouble()` | `mk_float()` |
| `{CString}` | `char*` | `obj_to_cstring()` | `mk_string()` |
| `{CPtr}` | `void*` | `obj_to_cptr()` | `mk_cptr()` |
| `{CSize}` | `size_t` | `obj_to_csize()` | `mk_int()` |
| `{Nothing}` | `void` | N/A | `NOTHING` |

### P0: Type Marshaling Helpers [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I19-p0-marshaling-helpers
  Objective: Add C type conversion helpers to omni.h.
  Where: `runtime/include/omni.h:575-626`
  What was done:
    1. `obj_to_cint(Obj*)` - extract int from Obj (line 583)
    2. `obj_to_cdouble(Obj*)` - extract double from Obj (line 588)
    3. `obj_to_cstring(Obj*)` - extract char* from Obj (line 593)
    4. `obj_to_cptr(Obj*)` - extract void* from Obj (line 601)
    5. `obj_to_csize(Obj*)` - extract size_t from Obj (line 609)
    6. `mk_cptr(void*)` - create Obj from void pointer (line 618)
    7. Added `#include <dlfcn.h>` for dlopen/dlsym (line 17)
  Verification: All helpers compile and convert correctly. ✓

### P1: Codegen FFI Counter [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I19-p1-ffi-counter
  Objective: Add FFI call site counter to CodeGenContext.
  Where: `csrc/codegen/codegen.h:98`
  What was done:
    1. `int ffi_counter` field added for unique FFI call site IDs
  Verification: Counter increments for each ccall site. ✓

### P2: ccall Codegen Handler [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I19-p2-ccall-handler
  Objective: Implement `codegen_ccall()` function.
  Where: `csrc/codegen/codegen.c:3318-3530`
  What was done:
    1. Full ccall code generation with static library/function caching
    2. Argument conversion with type-specific marshaling
    3. Function pointer casting with proper signatures
    4. Return value wrapping to Obj*
    5. Error handling for library/function load failures
  Generated pattern:
    ```c
    ({
        Obj* _ffi_expr_N = NOTHING;
        static void* _ffi_lib_N = NULL;
        static void* _ffi_fn_N = NULL;
        if (!_ffi_lib_N) {
            _ffi_lib_N = dlopen("libm.so.6", RTLD_NOW);
            if (_ffi_lib_N) _ffi_fn_N = dlsym(_ffi_lib_N, "sin");
        }
        if (!_ffi_fn_N) {
            fprintf(stderr, "FFI error: ...");
        } else {
            double _ffi_arg_0_N = obj_to_cdouble(arg);
            double _ffi_result_N = ((double(*)(double))_ffi_fn_N)(_ffi_arg_0_N);
            _ffi_expr_N = mk_float(_ffi_result_N);
        }
        _ffi_expr_N;
    })
    ```
  Verification: Generated C compiles and calls library function. ✓

### P3: ccall Dispatch [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I19-p3-ccall-dispatch
  Objective: Add `ccall` to special form dispatch in `codegen_list()`.
  Where: `csrc/codegen/codegen.c:4910-4914`
  What was done:
    1. Added check for `ccall` symbol name
    2. Calls `codegen_ccall(ctx, expr)`
  Verification: `(ccall ...)` forms are recognized and processed. ✓

### P4: Link Flag [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I19-p4-link-flag
  Objective: Ensure `-ldl` is in link flags.
  Where: `csrc/Makefile:9`, `runtime/Makefile:11`
  What was done:
    1. `-ldl` already present in csrc/Makefile
    2. Added `-ldl` to runtime/Makefile
  Verification: Build succeeds with dlopen/dlsym symbols resolved. ✓

### Verification Tests

Test file: `/tmp/test_ffi_simple.lisp`
```lisp
(println (ccall "libm.so.6" "sin" [0.5 {CDouble}] {CDouble}))   ;; 0.479426 ✓
(println (ccall "libc.so.6" "strlen" ["hello" {CString}] {CSize})) ;; 5 ✓
(println (ccall "libm.so.6" "pow" [2.0 {CDouble} 10.0 {CDouble}] {CDouble})) ;; 1024 ✓
(println (ccall "libc.so.6" "abs" [-42 {CInt}] {CInt}))         ;; 42 ✓
```

---

## Issue 1: Adopt "RC external pointers" semantics as Region-RC spec cross-check [IN_PROGRESS]

**Objective:** Cross-check OmniLisp's per-region reference counting model against the RC dialect's definition of "external pointers".

**Reference (read first):**
- `runtime/docs/MEMORY_TERMINOLOGY.md`
- `docs/CTRR.md`

### P2: External RC Insertion [IN_PROGRESS]

- [IN_PROGRESS] Label: I1-ctrr-external-rc-insertion (P2)
  Objective: Implement external reference counting at region boundaries.
  Where: `csrc/codegen/codegen.c`, `runtime/src/region.c`
  Why: Required for correct cross-region reference management.
  What to change:
    1. Identify external reference sites in codegen
    2. Emit retain/release calls at appropriate boundaries
    3. Integrate with transmigration system

### RF: Regression Fix

- [DONE] (Review Needed) Label: I1-transmigrate-external-root-identity-regression (RF-I1-1)
  - Fixed root identity regression in transmigration

---

## Issue 6: Parser Syntax Completion (Align SYNTAX.md with Implementation) [TODO]

**Objective:** Implement missing parser features documented in SYNTAX.md.

**Reference (read first):**
- `docs/SYNTAX.md` (Implementation Status section)
- `csrc/parser/parser.c`

### P0: Implement Dict Literal Parsing `#{}` [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I6-p0-dict-literals
  Objective: Parse `#{:a 1 :b 2}` as dictionary literal.
  Where: `csrc/parser/parser.c`
  Why: Documented syntax but no parser rule exists.
  What to change:
    1. Add `R_DICT` rule matching `#{` ... `}`
    2. Parse key-value pairs (alternating symbols and expressions)
    3. Return `OMNI_DICT` AST node
  Verification:
    - Add test: `#{:a 1}` parses to dict with key 'a value 1
    - Run: `make -C csrc/tests test`

### P1: Implement Signed Integer Parsing `+456` [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I6-p1-signed-integers
  Objective: Parse `+456` as positive integer literal.
  Where: `csrc/parser/parser.c`
  Why: Currently `+` parsed as symbol, not sign.
  What to change:
    1. Modify `R_INT` to optionally accept leading `+` or `-`
    2. Ensure `-123` and `+456` both parse as integers
  Verification:
    - Add test: `+456` parses to integer 456
    - Add test: `-123` parses to integer -123
    - Run: `make -C csrc/tests test`

### P2: Implement Partial Float Parsing `.5`, `3.` [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I6-p2-partial-floats
  Objective: Parse `.5` and `3.` as float literals.
  Where: `csrc/parser/parser.c`
  Why: Current parser requires `INT.INT` format.
  What to change:
    1. Update `R_FLOAT` to accept `.DIGITS` and `DIGITS.`
    2. Handle edge cases (`.` alone should not be float)
  Verification:
    - Add test: `.5` parses to float 0.5
    - Add test: `3.` parses to float 3.0
    - Run: `make -C csrc/tests test`

### P3: Complete Match Clause Parsing [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I6-p3-match-clauses
  Objective: Parse `[pattern result]` and `[pattern :when guard result]` match clauses.
  Where: `csrc/codegen/codegen.c`, `runtime/include/omni.h`
  What was done:
    1. Updated `codegen_match` to detect array-based clause syntax
    2. Extract pattern from array[0], result from array[1]
    3. Detect `:when` keyword at array[1] for guarded clauses
    4. Added `is_pattern_match` declaration to omni.h
    5. Both new syntax `[pattern result]` and legacy syntax work
  Verification:
    - Test: `(match x [1 "one"])` → works
    - Test: `(match x [n :when (> n 0) "positive"])` → works
    - Test file: `tests/test_match_clauses.lisp` - all 6 tests pass

### P4: Make Signed Numbers Lexical (Not Symbolic) [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I6-p4-lexical-signed-numbers
  Objective: Treat signed numbers as lexical atoms, not applications of `-`/`+` operator.
  Where: `csrc/parser/parser.c`
  What was done (2026-01-17):
    1. Added new rule IDs: R_OPT_SIGN, R_OPT_INT, R_FLOAT_FULL, R_FLOAT_LEAD, R_FLOAT_TRAIL, R_ANY_FLOAT
    2. Implemented R_FLOAT_FULL: SIGN? INT "." INT (e.g., -3.14, +2.5)
    3. Implemented R_FLOAT_LEAD: SIGN? "." INT (e.g., .5, -.25)
    4. Implemented R_FLOAT_TRAIL: SIGN? INT "." (e.g., 3., -5.)
    5. R_ANY_FLOAT combines all float forms in correct precedence order
    6. Updated R_EXPR to use R_ANY_FLOAT instead of R_FLOAT
    7. R_SIGNED_INT already handled signed integers correctly
  Verification:
    - `-123` → `mk_int(-123)` ✓
    - `-3.14` → `mk_float_region(_local_region, -3.140000)` ✓
    - `.5` → `mk_float_region(_local_region, 0.500000)` ✓
    - `3.` → `mk_float_region(_local_region, 3.000000)` ✓
    - `-.25` → `mk_float_region(_local_region, -0.250000)` ✓
    - `(+ -10 5)` → `prim_add(mk_int(-10), mk_int(5))` ✓

### P5: Value Type Literals `{value}` [PARTIAL] (2026-01-18)

- [PARTIAL] Label: I6-p5-value-type-literals
  Objective: Allow literal values in braces `{3}` to be value types (not just types).
  Where: `csrc/parser/parser.c` (lines 397-421), `csrc/codegen/codegen.c` (lines 5344-5356)
  What was done:
    1. Extended `act_type()` parser function to detect literal values inside braces
    2. Implemented pragmatic mapping: `{3}`→{Int}, `{true}`→{Bool}, `{"hello"}`→{String}, etc.
    3. Added special handling for `value->type` primitive in codegen to avoid symbol mangling
    4. Value type literals now compile and run correctly
  Limitations:
    - Current implementation maps value types to their runtime types (pragmatic, not full value types)
    - Type checking doesn't enforce refinement: `(define x {3} 4)` compiles (should fail)
    - No pattern matching support for value types: `[{3} "three"]` not yet implemented
    - Full implementation requires value type system support in compiler (future work)
  Verification:
    - ✓ All literal types compile: `{3}`, `{true}`, `{false}`, `{"hello"}`, `{nil}`, `{#\a}`, `{1.5}`
    - ✓ Type annotations work: `(define x {3} 42)` compiles
    - ✓ Backwards compatible: Existing type annotations `{Int}`, `{String}` continue to work
    - See `ISSUE6_P5_VALUE_TYPE_LITERALS.md` for detailed documentation
  Future Work (requires deeper compiler changes):
    - Implement proper value type representation in AST
    - Add Type.from_value() or equivalent for compile-time type computation
    - Implement refinement type checking to enforce {3} only accepts value 3
    - Support value type patterns in match expressions
    - Integrate with parametric types: `{Option [{3}]}`
  Note: Related to Julia's value types and refinement types.

### P6: Unify Define Forms to Slot Syntax [TODO]

- [TODO] Label: I6-p6-define-slot-syntax
  Objective: Eliminate non-canonical define forms; require all parameters to use slot syntax `[]`.
  Where: `csrc/parser/parser.c`, `csrc/codegen/codegen.c`
  Why:
    - Currently multiple function definition syntaxes exist (Scheme-style, slot, typed)
    - This creates cognitive load and parsing complexity
    - Slot syntax `[]` is uniform and works for all cases (destructuring too)
  What to change:
    1. Make canonical form the only accepted syntax:
       ```lisp
       ;; Canonical (required)
       (define add [x] [y] {Int}
         (+ x y))
       ```
    2. Remove or deprecate Scheme-style: `(define (add x y) ...)`
    3. Allow shorthand sugar that desugars to slots:
       ```lisp
       ;; Sugar (desugars to canonical)
       (define add x y
         (+ x y))
       ;; => (define add [x] [y] (+ x y))
       ```
    4. Update parser to desugar shorthand
    5. Update docs to show canonical as primary
  Verification:
    - `(define add x y (+ x y))` works and desugars correctly
    - `(define (add x y) (+ x y))` either deprecates or errors
    - Documentation reflects canonical form as primary
  Note: Slot syntax enables uniform destructuring: `(define foo [[x y] z] ...)`.
  Where: `csrc/codegen/codegen.c`, `runtime/include/omni.h`
  What was done:
    1. Updated `codegen_match` to detect array-based clause syntax
    2. Extract pattern from array[0], result from array[1]
    3. Detect `:when` keyword at array[1] for guarded clauses
    4. Added `is_pattern_match` declaration to omni.h
    5. Both new syntax `[pattern result]` and legacy syntax work
  Verification:
    - Test: `(match x [1 "one"])` → works
    - Test: `(match x [n :when (> n 0) "positive"])` → works
    - Test file: `tests/test_match_clauses.lisp` - all 6 tests pass

---

## Issue 8: Codebase Connectivity & Static Helper Audit [TODO]

**Objective:** Resolve unresolved edges and audit static functions for codebase integrity.

**Reference (read first):**
- `DISCONNECTED_EDGES_ANALYSIS.md` (Sections 1, 3, 8)
- `runtime/include/omni.h`

### P0: External Library & Macro Edge Recovery [TODO]

- [TODO] Label: I8-t1-resolve-library-edges
  Objective: Create manual bridge index for third-party libraries (SDS, Linenoise, UTHash, stb_ds).
  Where: Add to `docs/EXTERNAL_DEPENDENCIES.md`
  Why: CodeGraph has blind spots for external libraries (40% of unresolved edges).
  Verification: CodeGraph resolution rate improves.

- [TODO] Label: I8-t3-macro-edge-recovery
  Objective: Document edges generated by core macros (IS_BOXED, ATOMIC_INC_REF, etc.).
  Where: `docs/MACRO_ARCHITECTURE.md`
  Why: Macro-generated calls represent 20% of unresolved edges.

### P1: Static Function Audit & Cleanup [TODO]

- [TODO] Label: I8-t2-audit-static-helpers
  Objective: Audit 467 static functions to identify orphans or expose useful utilities.
  Where: `runtime/src/runtime.c`, `runtime/src/generic.c`, `runtime/src/modules.c`
  What to change:
    1. Scan for unused helpers using `-Wunused-function`
    2. Move useful utilities to public headers
  Verification: `make test` passes; static function count reduced.

---

## Issue 9: Feature Completion: Algebraic Effects, Continuations, and Typed Arrays [DONE] (Review Needed)

**Objective:** Implement missing core functionality in the Algebraic Effect system, Fiber/Continuation system, and Typed Arrays.

**Completed:** 2026-01-17

**IMPORTANT Design Decision (2026-01-14):**
- `try`/`catch` is **NOT supported** in OmniLisp
- Error handling uses **algebraic effects** instead (see `SYNTAX_REVISION.md` Section 7)
- Algebraic effects are implemented using **delimited continuations** (shift/reset style)
- `handle` installs a prompt; effect operations capture continuation to handler
- Handlers can resume, discard, or invoke multiple times the captured continuation

**Reference (read first):**
- `docs/SYNTAX_REVISION.md` (Section 7: Algebraic Effects)
- `DISCONNECTED_EDGES_ANALYSIS.md` (Sections 2, 6.1, 6.3, 6.4)
- `runtime/src/effect.c`
- `runtime/src/memory/continuation.c`

**Constraints:**
- `try`/`catch` syntax is explicitly NOT supported
- Must satisfy existing test stubs in `runtime/tests`

### P0: Effect Tracing & Fiber Callbacks [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I9-t4-effect-tracing
  Objective: Implement effect trace printing, recording, and clearing.
  Where: `runtime/src/effect.c`
  What was done:
    1. Added TraceEntry struct and ring buffer (256 entries)
    2. Implemented `effect_trace_print()` - prints trace to stdout
    3. Implemented `effect_trace_to_string()` - returns trace as malloc'd string
    4. Implemented `effect_trace_record()` - records effect with timestamp
    5. Implemented `effect_trace_clear()` - clears trace buffer
    6. Added `effect_trace_mark_handled()` and `effect_trace_last_index()` helpers
  Verification: `runtime/tests/test_effect_tracing.c` passes (10 tests).

- [DONE] (Review Needed) Label: I9-t5-fiber-callbacks
  Objective: Fiber callback execution (on_fulfill, on_reject) and error handling.
  Where: `runtime/src/memory/continuation.c`
  What was done:
    1. `promise_resolve()` calls `on_fulfill` callbacks (line 1310-1316)
    2. `promise_reject()` calls `on_reject` callbacks (line 1341-1354)
    3. `promise_then()` registers callbacks for settled/pending promises
    4. Fiber scheduler with round-robin and work-stealing already operational
  Verification: `runtime/tests/test_effect.c` passes (22 tests).

### P1: Typed Array Functional Primitives [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I9-t6-typed-array-primitives
  Objective: Implement map, filter, and reduce for typed arrays.
  Where: `runtime/src/typed_array.c`
  Why: Core collection types lack functional parity.
  What was done:
    1. Implemented `omni_typed_array_map()` - maps function over elements
    2. Implemented `omni_typed_array_filter()` - filters elements by predicate
    3. Implemented `omni_typed_array_reduce()` - reduces array with accumulator
    4. Fixed memory leak in `omni_typed_array_create` allocation failure paths
    5. Fixed incorrect list construction in `omni_typed_array_to_list`
  Additional bugs fixed:
    - Changed malloc/calloc to region_alloc for CTRR integration
    - Fixed list building bug where each cell's cdr was NULL instead of linked
  Verification: See `TYPED_ARRAY_FIXES.md` for detailed bug analysis

### P2: Algebraic Effects Compiler Support [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I9-p2-effect-declaration
  Objective: Parse `{effect Name}` as effect type declaration.
  Where: `runtime/src/effect.c`
  What was done:
    1. Built-in effects registered: Fail, Ask, Emit, State, Yield, Async, Choice, Condition
    2. `prim_effect_type_register()` allows registering custom effects from Lisp
    3. `effect_type_find()` / `effect_type_find_by_id()` for lookup
  Note: Parser support for `{effect ...}` syntax can be added later as sugar.
  Verification: `runtime/tests/test_effect.c` passes (22 tests).

- [DONE] (Review Needed) Label: I9-p2-handle-form
  Objective: Implement `handle` as special form in compiler.
  Where: `csrc/codegen/codegen.c`, `runtime/src/effect.c`
  What was done (via Issue 14 P2):
    1. Added `handle` to special form detection in codegen
    2. `codegen_handler_closure` generates static handler functions
    3. `codegen_body_thunk` generates closure for handle body
    4. Emits `effect_handle(_body_thunk, _h_clauses, _h_return_clause, NULL)`
    5. Handler closures receive (payload, resume) args and can call `prim_resume`
  Verification: ✓ 7 effect tests pass (see tests/test_effects.lisp).

 - [DONE] (Review Needed) Label: I9-p2-continuation-primitives
   Objective: Expose delimited continuation primitives for effect implementation.
   Where: `runtime/src/memory/continuation.c`
   What was done:
     1. `cont_prompt()` installs delimiter with region boundary
     2. `cont_capture()` captures continuation up to prompt
     3. `cont_invoke()` resumes captured continuation with value
     4. `effect_perform()` bridges effects to continuation system
   Verification: `runtime/tests/test_effect.c` passes (22 tests).

### P3: Effect Type System & Semantics [TODO]

- [TODO] Label: I9-p3-effect-semantics
  Objective: Define effect semantics, type signatures, and resume precisely.
  Where: `docs/SYNTAX_REVISION.md`, `csrc/parser/parser.c`, `csrc/analysis/analysis.c`, `csrc/codegen/codegen.c`
  Why:
    - Current effect docs are good but missing key semantic details
    - Ambiguity around whether `raise` is a function call or special form
    - Type signatures lack effect row annotations
    - `resume` semantics and type are unclear
  What to change:
    A) Clarify effect operation invocation:
       1. State explicitly: "Effect operations are invoked like normal calls; they are intercepted by handle"
       2. `(raise "Division by zero")` is NOT a special form - it's a normal function call
       3. The `handle` form intercepts these calls at runtime
       4. Update SYNTAX_REVISION.md with this clarification

    B) Add effect row type signatures:
       1. Define syntax for effect rows in type signatures (placeholder ok):
          ```lisp
          (define safe-div [x {Int}] [y {Int}] {Int} ^:effects [{Error}]
            ...)
          ```
       2. Or Julia-ish annotation: `(define safe-div [x {Int}] [y {Int}] {Int} <{Error}> ...)`
       3. Add parser/analysis support for effect annotations
       4. Integrate with type checker to track effects

    C) Define `resume` precisely:
       1. Clarify: Is `resume` a normal function only available in handler bodies?
       2. Or is it a special form with restricted scope?
       3. Define its type signature: `(resume :: (Continuation a -> a) ?)`
       4. Document: "resume can only be called from within a handler clause"
       5. Update runtime docs with precise semantics
  Verification:
    - SYNTAX_REVISION.md clarifies effect operation semantics
    - Type signatures with `^:effects [{Error}]` parse and type-check
    - `resume` semantics documented with type signature
  Note: Koka and Eff are good references for effect typing.

---

## Issue 10: Advanced Memory Safety: IPGE Integration & Region-Aware Realloc [IN_PROGRESS]

**Objective:** Strengthen memory safety by integrating generation checking with IPGE and implementing region-aware reallocation.

**Reference (read first):**
- `runtime/src/memory/region_pointer.h`
- `docs/CTRR.md`
- `docs/IPGE_GENERATION_CHECKING.md`
- `DISCONNECTED_EDGES_ANALYSIS.md` (Sections 6.2, 7.3)

### P0: IPGE Generation & Region Realloc [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I10-t7-generation-checking
  Objective: Integrate IPGE (Indexed Pointer Generation Epoch) with region system.
  Where: `runtime/src/memory/region_pointer.h`, `runtime/src/memory/region_value.c`
  What was done:
    1. Fixed object generation initialization in `alloc_obj_typed()` - objects now get region's current generation instead of always 0
    2. Implemented `pointer_mask_safe_access()` with proper generation checking for cross-region access
    3. Updated `pointer_mask_encode_with_generation()` to document IPGE integration approach
    4. Added include guard for omni.h to access `Obj` struct definition in header
  Verification:
    - Test file: `runtime/tests/test_generation_checking.c` - all 3 tests pass
    - All 397 existing runtime tests pass with no regressions
    - See `docs/IPGE_GENERATION_CHECKING.md` for detailed documentation
  Note: Generation checking is now functional and can detect stale pointers when regions are reused.

- [TODO] Label: I10-t8-region-realloc
  Objective: Implement region-aware realloc that preserves region membership.
  Where: `runtime/src/region.c`
  Why: Current realloc may move objects between regions.
  Verification: Reallocated object stays in original region.

---

## Issue 11: Build/Test Consolidation & Debug Instrumentation [TODO]

**Objective:** Consolidate test suites and add debug instrumentation toggles.

**Reference (read first):**
- `DISCONNECTED_EDGES_ANALYSIS.md` (Section 5)
- `runtime/tests/Makefile`

### P0: Test Suite Consolidation & Debug Toggles [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I11-t13-test-consolidation
  Objective: Merge fragmented test files into cohesive suites.
  Where: `runtime/tests/`, `csrc/tests/`, `run_tests.sh` (NEW)
  Why: Current tests are scattered across directories.
  What was done:
    1. Created unified test runner `run_tests.sh` at repo root
    2. Runner executes all test suites (runtime, compiler, CLI) from single command
    3. Provides color-coded output and comprehensive summaries
    4. Supports multiple options: -s (slow), -a (asan), -t (tsan), -v (verbose)
    5. Added `debug`, `release`, and `coverage` targets to all Makefiles
  Verification: `./run_tests.sh` runs all tests and reports results ✓

- [DONE] (Review Needed) Label: I11-t14-debug-integration
  Objective: Add compile-time debug toggles for instrumentation.
  Where: `runtime/Makefile`, `runtime/tests/Makefile`, `csrc/tests/Makefile`
  Why: Provide configurable debug builds for development.
  What was done:
    1. Added `BUILD_TYPE` variable to all relevant Makefiles
    2. Created `make debug` target: builds with `-O0 -g -DOMNI_DEBUG=1`
    3. Created `make release` target: builds with `-O2` (optimized, default)
    4. Created `make coverage` target: builds with `-fprofile-arcs -ftest-coverage`
    5. Existing `omni_debug.h` infrastructure properly utilized by build system
  Verification: Debug builds enable debug output; release builds are clean ✓

---

## Issue 12: Parser Syntax Completion [N/A - Duplicate of Issue 6]

**Status:** N/A - This is a duplicate of Issue 6. All work tracked under Issue 6.

---

## Issue 14: Continuation Infrastructure Integration [TODO]

**Objective:** Wire the existing continuation/effect infrastructure into the compiler and runtime.

**DESIGN PRINCIPLE:** Continuations are the **foundational primitive** for control flow
abstractions. Effects, generators, and async/fibers build on continuations. The correct
integration order is: continuations → regions → effects → higher abstractions.

**ARCHITECTURE NOTE:** Trampolines are **intentionally separate** from the continuation
infrastructure. They are a simple tail-call optimization mechanism that predates the CEK
machine and do not require continuation semantics. The two systems coexist independently.

**Analysis (2026-01-14):** The runtime has comprehensive infrastructure that is NOT connected:
- CEK machine with continuation frames exists but codegen doesn't use it
- Effect system exists but no primitives are registered
- Region system doesn't connect to continuation prompts as designed
- Conditions/restarts now unified with effects (P3 DONE)

**Reference (read first):**
- `runtime/src/memory/continuation.h` (CEK machine, fibers, generators, promises)
- `runtime/src/effect.h` (effect handlers, resumptions)
- `docs/SYNTAX_REVISION.md` (Section 7: Algebraic Effects)
- `docs/CTRR.md` (Region memory model)

### P0: Region-Continuation Boundary Integration [DONE] (Review Needed) (FOUNDATION)

- [DONE] (Review Needed) Label: I14-p0-prompt-region-boundary
  Objective: Connect continuation prompts with region boundaries.
  Where: `runtime/src/memory/continuation.c`, `runtime/src/memory/continuation.h`
  What was done (already implemented in codebase):
    1. `Region* region` field exists in Prompt frame (continuation.h:99)
    2. `cont_prompt()` creates child region on prompt install (continuation.c:487)
    3. `cont_capture()` transmigrates frame contents to parent (continuation.c:363-374)
    4. `cont_prompt_exit()` calls `region_exit()` on prompt's region (continuation.c:574)
    5. `frame_transmigrate_contents()` handles all frame types (continuation.c:248-301)
  Verification:
    - Region-Continuation boundary is fully integrated
    - Continuation system uses CTRR for memory safety

### P1: Wire Effect Primitives [DONE] (Review Needed) (BUILD ON CONTINUATIONS)

- [DONE] (Review Needed) Label: I14-p2-register-effect-primitives
  Objective: Register effect primitives for use from Lisp code.
  Where: `runtime/src/runtime.c`, `runtime/src/effect.c`
  What was done:
    1. `prim_perform` - performs effect by name, captures continuation
    2. `prim_resume` - resumes captured continuation with value
    3. `effect_handle` - runs body thunk with handlers installed
    4. Primitives already registered in runtime
  Verification: ✓ All 7 effect tests pass (see tests/test_effects.lisp)

- [DONE] (Review Needed) Label: I14-p2-codegen-handle-form
  Objective: Emit C code for `handle` special form.
  Where: `csrc/codegen/codegen.c`
  What was done:
    1. Added `handle` to special form detection
    2. Added `codegen_handler_closure` - generates static handler functions with region-aware signature
    3. Added `codegen_body_thunk` - generates 0-arg closure for handle body
    4. Emit `effect_handle(_body_thunk, _h_clauses, _h_return_clause, NULL)`
    5. Handler closures receive (payload, resume) args and can call `prim_resume`
  IMPORTANT: Built-in `Fail` effect is RECOVERY_ABORT mode (can't resume).
             Use custom effect names for resumable effects.
  Verification: ✓ Generated C compiles and runs, 7 tests pass:
    - Test 1: No effect (body returns 3)
    - Test 2: Perform+resume (1+42=43)
    - Test 5: Chained ops (5+10=15)
    - Test 6: Payload use (5*2=10)
    - Test 7: Nested expr (2*(3+5)=16)

### P2: Unify Condition/Restart with Effects [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I14-p2-unify-conditions-effects
  Objective: Reimplement conditions/restarts on top of effect system.
  Where: `runtime/src/condition.c`, `runtime/src/condition.h`, `runtime/src/effect.c`
  What was done:
    1. Added `TAG_CONDITION` to omni.h for Obj-wrapped conditions
    2. Added `EFFECT_CONDITION` built-in effect type (RECOVERY_ONE_SHOT mode)
    3. Implemented `mk_condition_obj()` - wraps Condition in Obj with TAG_CONDITION
    4. Implemented `condition_signal()` - performs EFFECT_CONDITION (resumable)
    5. Implemented `condition_error()` - performs EFFECT_FAIL (non-resumable)
    6. Implemented `condition_signal_with_message()` - convenience wrapper
    7. Implemented `condition_from_obj()` - extracts Condition from Obj
  Design: Adapter approach - preserves condition type hierarchy but uses effects
         for signaling. Handlers using `handle` form can catch condition effects.
  Note: Lisp-level `signal`, `handler-case`, `restart-case` require separate codegen work.

### P3: Iterator-Generator Integration [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I14-p3-iterator-generator
  Objective: Implement iterators using generator continuations.
  Where: `runtime/src/iterator.c`, `runtime/src/memory/continuation.c`
  What was done:
    1. Added `TAG_GENERATOR` to omni.h for Obj-wrapped generators
    2. Implemented `mk_generator_obj()` in continuation.c - wraps Generator in Obj
    3. Added generator-based iterator functions to iterator.c:
       - `prim_make_generator(producer)` - create generator from closure
       - `prim_generator_next(gen)` - get next value using `generator_next`
       - `prim_generator_done(gen)` - check if generator exhausted
       - `prim_yield(value)` - yield value from within generator
       - `is_generator(obj)` - helper to check if Obj is a generator
       - `prim_iter_next_unified(iter)` - supports both pair and generator iterators
       - `prim_take_unified(n, seq)` - supports both pair and generator sequences
  Design: Existing pair-based iterators preserved for backwards compatibility.
          Generator-based iteration uses delimited continuations for suspend/resume.
  Note: Lisp codegen for `make-generator` and `yield` forms needs separate work.

---

## Issue 15: Arena & Memory System Enhancements [TODO]

**Objective:** Implement remaining SOTA arena techniques and complete memory system integration.

**Reference (read first):**
- `runtime/docs/ARCHITECTURE.md` (Arena Allocator Implementation, SOTA Techniques sections)
- `third_party/arena/vmem_arena.h`
- `runtime/src/memory/region_core.h`

**Background (2026-01-14):** Arena implementation review completed. Current system has:
- ✅ Dual arena (malloc-based + vmem commit-on-demand)
- ✅ O(1) chunk splice for transmigration
- ✅ Inline buffer (512B) for small objects
- ✅ Thread-local region pools
- ✅ THP alignment (2MB chunks)
- ⚠️ Partial store barrier / region_of support

### P0: Scratch Arena API [TODO]

- [TODO] Label: I15-p0-scratch-arena-api
  Objective: Implement double-buffered scratch arenas for temporary allocations.
  Where: `runtime/src/memory/scratch_arena.h` (new), `runtime/src/memory/transmigrate.c`
  Why: Transmigration temporaries (forwarding table, worklist) currently pollute destination region.
  What to change:
    1. Create `ScratchArenas` struct with two arenas + current index
    2. Implement `get_scratch(Arena* conflict)` - return non-conflicting arena
    3. Implement `scratch_checkpoint()` / `scratch_rewind()` for scoped usage
    4. Add `__thread ScratchArenas tls_scratch` per-thread pool
    5. Update transmigrate.c to use scratch for forwarding table
  Verification:
    - Transmigration doesn't bloat destination region with temporaries
    - `runtime/tests/test_scratch_arena.c` passes

### P1: Explicit Huge Page Support [TODO]

- [TODO] Label: I15-p1-madv-hugepage
  Objective: Add MADV_HUGEPAGE hint for vmem_arena chunks.
  Where: `third_party/arena/vmem_arena.h`
  Why: Explicit hint guarantees huge pages vs THP's opportunistic promotion.
  What to change:
    1. Add `madvise(base, reserved, MADV_HUGEPAGE)` after mmap in `vmem_chunk_new()`
    2. Guard with `#ifdef MADV_HUGEPAGE` for portability
    3. Add `VMEM_USE_HUGEPAGES` compile flag to enable/disable
  Verification:
    - Check `/proc/meminfo` shows AnonHugePages increasing
    - No regression on systems without huge page support

### P2: Store Barrier Choke-Point [TODO]

- [TODO] Label: I15-p2-store-barrier-impl
  Objective: Implement single choke-point for all pointer stores with lifetime checking.
  Where: `runtime/src/memory/store_barrier.h` (new), codegen integration
  Why: Required to enforce Region Closure Property at mutation time.
  What to change:
    1. Create `omni_store_barrier(Obj* container, int field, Obj* value)` function
    2. Implement lifetime check: `if (!omni_region_outlives(dst_region, src_region))`
    3. Auto-repair via transmigrate or merge based on `get_merge_threshold()`
    4. Update codegen to emit store barrier for all pointer field assignments
    5. Increment `region->escape_repair_count` on auto-repair
  Verification:
    - Younger→older store triggers auto-repair
    - `runtime/tests/test_store_barrier.c` passes

### P3: region_of(obj) Lookup [TODO]

- [TODO] Label: I15-p3-region-of-lookup
  Objective: Implement O(1) lookup from object pointer to owning region.
  Where: `runtime/src/memory/region_pointer.h`, `runtime/src/memory/region_core.c`
  Why: Store barrier needs to determine object's region efficiently.
  What to change:
    1. Option A: Use existing pointer masking (region_id in high bits)
       - Add `region_lookup_by_id(uint16_t id)` global registry
    2. Option B: Store region pointer in object header (space tradeoff)
    3. Implement `Region* region_of(Obj* obj)` using chosen strategy
  Verification:
    - `region_of(arena_allocated_obj)` returns correct region
    - O(1) lookup performance

### P4: Size-Class Segregation [OPTIONAL]

- [TODO] Label: I15-p4-size-class-segregation
  Objective: Separate arenas for different allocation sizes.
  Where: `runtime/src/memory/region_core.h`
  Why: Better cache locality for homogeneous object traversal.
  What to change:
    1. Add `Arena pair_arena` for TYPE_ID_PAIR allocations
    2. Add `Arena container_arena` for arrays/dicts/strings
    3. Route `region_alloc_typed()` to appropriate arena
  Note: May not be needed - inline buffer already handles small objects.
  Verification:
    - List traversal benchmark shows improved cache behavior
    - No regression in allocation throughput

---

## Issue 16: Region-RC Dynamic Closure Integration [DONE] (Review Needed)

**Objective:** Make closures, HOFs, and generic methods region-aware for proper Region-RC lifecycle.

**Completed:** 2026-01-15

### P0: Region-Aware Closure Types [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p0-closure-fn-region
  Objective: Add region-aware closure function pointer type and struct support.
  Where: `runtime/include/omni.h`, `runtime/src/runtime.c`
  What was done:
    1. Added `ClosureFnRegion` typedef: `Obj* (*)(Region*, Obj** captures, Obj** args, int argc)`
    2. Updated `Closure` struct with union for `fn`/`fn_region` + `region_aware` flag
    3. Added `mk_closure_region()` with store barriers for captured values
    4. Added `call_closure_region()` that passes caller region to closure

### P1: Region-Aware HOFs [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p1-region-aware-hofs
  Objective: Create region-aware versions of higher-order functions.
  Where: `runtime/src/runtime.c`
  What was done:
    1. Added `list_map_region`, `list_filter_region`, `list_fold_region`, `list_foldr_region`
    2. HOFs use `call_closure_region` to propagate region context
    3. All allocations within HOFs use caller's region

### P2: HOF Codegen [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p2-hof-codegen
  Objective: Update codegen to emit region-aware HOF calls.
  Where: `csrc/codegen/codegen.c`
  What was done:
    1. Updated `map`, `filter`, `fold`, `foldr` handlers to emit `list_*_region` calls
    2. Pass `_local_region` as first argument to all HOFs

### P3: Inline Lambda Closure Wrappers [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p3-lambda-closure-wrapper
  Objective: Generate closure wrappers for inline lambdas in HOF calls.
  Where: `csrc/codegen/codegen.c`
  What was done:
    1. Added `is_lambda_expr()` to detect lambda forms (lambda, fn, λ)
    2. Added `count_lambda_params()` to count parameters
    3. Added `codegen_lambda_as_closure()` that generates:
       - Static lambda function (`_lambda_N`)
       - Trampoline function (`_lambda_N_tramp`) adapting signature to `ClosureFnRegion`
       - `mk_closure_region()` call to wrap trampoline
    4. HOF handlers now detect inline lambdas and use closure wrapper
    5. Removed arrow syntax (`->`) - superseded by `(fn [params] body)`
  Verification: `(map (fn [x] (* x 2)) '(1 2 3))` → `(2 4 6)` ✓

### P4: Generic Method Region Support [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p4-generic-method-region
  Objective: Add region-aware method support to generic functions.
  Where: `runtime/src/generic.c`
  What was done:
    1. Updated `MethodInfo` struct with `impl_region` union + `region_aware` flag
    2. Added `generic_add_method_region()` for region-aware methods
    3. Added `call_generic_region()` / `omni_generic_invoke_region()`

### P5: prim_deep_put Store Barriers [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I16-p5-deep-put-barriers
  Objective: Implement deep_set with store barriers for nested mutations.
  Where: `runtime/src/runtime.c`
  What was done:
    1. Implemented `deep_set()` with `omni_store_repair()` for all mutations
    2. Supports alist-style structures: `((key1 . val1) (key2 . val2) ...)`
    3. Recursive path traversal with barrier at each level

---

## Issue 17: Remaining Integration Tasks [DONE] (Review Needed)

**Objective:** Complete integration of Region-RC with continuation, effect, and codegen systems.

**Reference (read first):**
- Issue 14 (Continuation Infrastructure)
- Issue 9 (Algebraic Effects)
- `runtime/src/memory/continuation.c`
- `runtime/src/effect.c`

### P0: Array Growth with Region Realloc [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I17-p0-array-growth
  Objective: Implement proper array growth that preserves region membership.
  Where: `runtime/src/runtime.c:733-775`
  What was done (2026-01-17):
    1. Implemented `array_grow()` function that allocates new data buffer in same region
    2. Updated `array_push()` to grow array when full (2x capacity, minimum 8)
    3. Elements copied directly using `region_alloc()` (CTRR model, no explicit free needed)
    4. Old data becomes garbage, automatically reclaimed when region exits
  Bug Fixed:
    - Previously: array_push silently dropped elements beyond capacity
    - Now: Array grows correctly (4→8→16→32) and stores all elements
  Verification:
    - `runtime/tests/test_array_growth_bug.c` passes
    - All 392 runtime tests pass
    - See `ARRAY_GROW_BUG_FIX.md` for detailed analysis

### P1: Store Barrier Merge Path [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I17-p1-store-barrier-merge
  Objective: Add merge path to store barrier (currently only transmigrates).
  Where: `runtime/src/runtime.c:979-1006`
  What was done: (Already implemented in prior work)
    1. `omni_store_repair()` checks `get_merge_threshold()` for large regions
    2. Calls `region_merge_safe()` for regions above threshold
    3. Falls back to transmigrate for smaller regions or merge failure
  Verification: ✓ Tests "store barrier checks merge threshold" and "merge safe basic" pass

### P2: Print Functions Completion [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I17-p2-print-completion
  Objective: Complete print functions for container types.
  Where: `runtime/src/runtime.c:1247-1330`
  What was done:
    1. Implemented `print_array()` - iterates and prints `[elem1 elem2 ...]`
    2. Implemented `print_tuple()` - iterates and prints `{elem1 elem2 ...}`
    3. Implemented `print_dict()` - iterates buckets and prints `#{:key val ...}`
    4. Implemented `print_named_tuple()` - prints `#(:key val ...)`
    5. Fixed `alloc_obj_region()` to preserve original tag for unmapped types (TAG_KEYWORD)
  Verification: ✓ `[1 2 3]`, `{10 20 30}`, `#{:a 100}`, `#(:x 5)` all print correctly

---

## Issue 18: Computed Kinds via Compile-Time Splice [TODO]

**Objective:** Add optional compile-time splice syntax for computing kinds from runtime values.

**Reference (read first):**
- `docs/SYNTAX_REVISION.md` (Type system sections)
- `docs/TYPE_SYSTEM_DESIGN.md`
- `csrc/parser/parser.c`, `csrc/analysis/analysis.c`

### P0: Kind Splice Syntax [TODO]

- [TODO] Label: I18-p0-kind-splice-syntax
  Objective: Add `{#kind expr}` syntax to compute kinds at compile time.
  Where: `csrc/parser/parser.c`, `csrc/analysis/analysis.c`
  Why:
    - Kind forms `{...}` are currently Kind-only and don't evaluate Flow expressions
    - Controlled escape hatch allows computing kinds from compile-time-known values
    - Enables type-level metaprogramming
  What to change:
    1. Add parser rule for `#kind` splice syntax
    2. Inside `{...}`, allow `{#kind expr}` splice forms
    3. Evaluate splice at compile time (analysis time)
    4. Ensure splice MUST evaluate to a runtime type object
    5. Reify resulting type object as a Kind
  Examples:
    ```lisp
    {#kind (typeof 4)}    ; => {Int32}
    {#kind (if (> x 0) {Int} {Nothing})}
    ```
  Verification:
    - `{#kind (typeof 4)}` parses to `{Int32}`
    - Type checker recognizes computed kinds

### P1: Kind Equality Predicates [TODO]

- [TODO] Label: I18-p1-kind-equality
  Objective: Add flow-level and kind-level equality predicates.
  Where: `runtime/src/runtime.c`, `csrc/codegen/codegen.c`
  What to change:
    1. Flow-level type equality (runtime):
       ```lisp
       (eq (typeof 4) Int32) ; true
       ```
    2. Kind-level equality (compile-time):
       ```lisp
       (kind=? {#kind (typeof 4)} {Int32})
       ```
    3. Implement `kind=?` as analysis-time predicate
    4. Ensure Flow equality works with runtime type objects
  Verification:
    - `(eq (typeof 4) Int32)` evaluates to true
    - `(kind=? {#kind (typeof 4)} {Int32})` passes type check

### P2: Type Reflection Integration [TODO]

- [TODO] Label: I18-p2-type-reflection
  Objective: Implement `typeof` operator that returns runtime type objects.
  Where: `runtime/src/runtime.c`, `csrc/codegen/codegen.c`
  What to change:
    1. Implement `typeof` as special form or primitive
    2. Return runtime type object for values
    3. Support: `(typeof 4)` → type object for Int
    4. Support: `(typeof [1 2 3])` → type object for Array
  Verification:
    - `(typeof 4)` returns type object
    - Type object can be spliced with `{#kind ...}`
  Note: This enables type-level metaprogramming and reflection.

---

---

## Issue 20: Phase 1 Type Specialization (Binary Op Codegen) [DONE] (Review Needed)

**Objective:** Generate specialized unboxed code for arithmetic operations when operand types are statically known.

**Priority:** HIGH - Foundation for ~25x speedup on numeric operations.

**Completed:** 2026-01-17

**Reference (read first):**
- Plan file: `~/.claude/plans/jiggly-doodling-sprout.md` (Phase 1)
- `csrc/codegen/codegen.c` (lines 4139-4349)
- `runtime/include/omni.h` (specialization declarations)
- `runtime/src/primitives_specialized.c`

### P0: Box/Unbox Functions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I20-p0-box-unbox
  Objective: Add forward declarations for box/unbox functions.
  Where: `runtime/include/omni.h:629-660`
  What was done:
    1. Declared `unbox_int(Obj*)` - extract int64_t from Obj
    2. Declared `unbox_float(Obj*)` - extract double from Obj
    3. Declared `unbox_char(Obj*)` - extract char from Obj
    4. Declared `unbox_bool(Obj*)` - extract bool from Obj
    5. Declared `box_int(int64_t)` - create Obj from int64_t
    6. Declared `box_float(double)` - create Obj from double
    7. Declared `box_char(char)` - create Obj from char
    8. Declared `box_bool(bool)` - create Obj from bool
  Note: Implementations already existed in `runtime/src/primitives_specialized.c`
  Verification: All functions compile and link correctly. ✓

### P1: Specialized Arithmetic Primitives [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I20-p1-specialized-primitives
  Objective: Add forward declarations for specialized arithmetic primitives.
  Where: `runtime/include/omni.h:662-690`
  What was done:
    1. Declared Int-Int operations: `prim_add_Int_Int`, `prim_sub_Int_Int`, `prim_mul_Int_Int`, etc.
    2. Declared Float-Float operations: `prim_add_Float_Float`, `prim_sub_Float_Float`, etc.
    3. Declared Mixed operations: `prim_add_Int_Float`, `prim_add_Float_Int`, etc.
    4. Declared comparison operations: `prim_lt_Int_Int`, `prim_gt_Int_Int`, etc.
  Note: Implementations already existed in `runtime/src/primitives_specialized.c`
  Verification: All primitives compile and link correctly. ✓

### P2: Operand Type Inference [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I20-p2-type-inference
  Objective: Implement static type inference for arithmetic operands.
  Where: `csrc/codegen/codegen.c:4139-4190`
  What was done:
    1. Added `OperandType` enum: `OPTYPE_UNKNOWN`, `OPTYPE_INT`, `OPTYPE_FLOAT`, `OPTYPE_BOOL`
    2. Implemented `get_operand_type(OmniValue*)` function
    3. Returns `OPTYPE_INT` for integer literals
    4. Returns `OPTYPE_FLOAT` for float literals
    5. Recursively infers types for nested arithmetic expressions
    6. Returns `OPTYPE_UNKNOWN` for variables (conservative fallback)
  Verification: Type inference correctly identifies literal types. ✓

### P3: Specialization Dispatch [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I20-p3-specialization-dispatch
  Objective: Wire specialization into codegen_apply for binary ops.
  Where: `csrc/codegen/codegen.c:4192-4349`
  What was done:
    1. Implemented `get_specialized_binop(op, t1, t2)` - maps op+types to specialized function name
    2. Supports +, -, *, /, %, <, >, <=, >=, = operators
    3. Supports Int_Int, Float_Float, Int_Float, Float_Int combinations
    4. Implemented `try_specialized_binop(ctx, op, a, b)` - emits specialized code
    5. Generated pattern: `boxer(spec_func(unboxer1(a), unboxer2(b)))`
    6. Modified `codegen_apply()` to try specialization before generic fallback
  Generated code example for `(+ 3 4)`:
    ```c
    box_int(prim_add_Int_Int(unbox_int(mk_int(3)), unbox_int(mk_int(4))))
    ```
  Verification: ✓ Generated C compiles and runs correctly.

### Verification Tests

Test file: `csrc/tests/test_type_specialization.c`
All 8 tests pass:
- Integer-Integer add/sub/mul/comparison specialization
- Float-Float add specialization
- Mixed Int-Float specialization
- Nested expression specialization
- Fallback to generic for unknown types

E2E test file: `/tmp/test_spec.lisp`
```lisp
(println "Test 1: 3 + 4 =" (+ 3 4))           ;; uses prim_add_Int_Int ✓
(println "Test 2: 10 - 3 =" (- 10 3))         ;; uses prim_sub_Int_Int ✓
(println "Test 3: 5 * 6 =" (* 5 6))           ;; uses prim_mul_Int_Int ✓
(println "Test 4: 5 < 10 =" (< 5 10))         ;; uses prim_lt_Int_Int ✓
(println "Test 5: (2 + 3) * 4 =" (* (+ 2 3) 4)) ;; both specialized ✓
(println "Test 6: 1.5 + 2.5 =" (+ 1.5 2.5))   ;; uses prim_add_Float_Float ✓
(println "Test 7: 3 + 1.5 =" (+ 3 1.5))       ;; uses prim_add_Int_Float ✓
```

---

## Issue 21: Promise Release Cancellation Fix [DONE] (Review Needed)

**Objective:** Fix improper cancellation of waiting fibers when a promise is released before fulfillment.

**Completed:** 2026-01-18

**Bug Location:** `runtime/src/memory/continuation.c:1443`

**Reference:**
- See `PROMISE_RELEASE_FIX.md` for detailed analysis

### P0: Fix promise_release to properly unpark waiting fibers [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I21-p0-unpark-waiters
  Objective: Properly unpark waiting fibers instead of just marking them FIBER_DONE.
  Where: `runtime/src/memory/continuation.c:1439-1453`
  What was done:
    1. Replaced simple state change with proper `fiber_unpark_error()` calls
    2. Each waiting fiber is unparked with error state (is_error=true, value=NULL)
    3. Cleared waiters list to prevent accessing freed fibers
    4. Added explanatory comments about fix behavior
  Before (buggy code):
    ```c
    /* Cancel waiting tasks */
    Fiber* t = p->waiters;
    while (t) {
        Fiber* next = t->next;
        t->state = FIBER_DONE;  /* TODO: Better cancellation */
        t = next;
    }
    free(p);
    ```
  After (fixed code):
    ```c
    /* Cancel waiting tasks by unparking them with error */
    Fiber* t = p->waiters;
    while (t) {
        Fiber* next = t->next;
        /* Unpark fiber with error indicating promise was destroyed/cancelled */
        /* fiber_unpark_error will enqueue fiber and set state to FIBER_READY */
        fiber_unpark_error(t, NULL, true);
        t = next;
    }
    /* Clear waiters list to prevent accessing freed fibers */
    p->waiters = NULL;
    free(p);
    ```
  Bug Impact:
    - Fibers waiting on a released promise were left stuck in `FIBER_PARKED` state
    - They remained in the promise's waiters list indefinitely
    - Never scheduled to run again → potential deadlocks
    - Memory leaks from fibers never completing
  Fix Behavior:
    - All waiting fibers are properly unparked with error state
    - `fiber_unpark_error()` enqueues fibers (sets state to `FIBER_READY`)
    - Fibers are scheduled to run and can handle the error (promise destroyed)
    - Waiters list is cleared before freeing promise
  Verification: Runtime builds successfully. All 397 tests pass. Fix addresses TODO comment "Better cancellation".
  Test Fix (2026-01-18):
  - Added `scheduler_init()` call to `test_promise_release_unparks_waiters()`
  - Test creates stack-allocated fibers that need scheduler for unparking
  - Without scheduler_init(), `scheduler_enqueue()` returns early without changing fiber state
  - Fix ensures test properly validates to Issue 21 runtime fix
  - Note: Not calling `scheduler_shutdown()` because fibers are stack-allocated

---

## Summary

| Issue | Status | Description |
|-------|--------|-------------|
| 1 | IN_PROGRESS | RC external pointers / Region-RC spec |
| 6 | TODO | Parser syntax completion |
| 8 | TODO | Codebase connectivity audit |
| 9 | DONE (Review) | **Phase 2: Effect System** (effects, fibers, generators - all working) |
| 10 | IN_PROGRESS | **IPGE integration** (P0 DONE, P1 TODO: region realloc) |
| 11 | TODO | Build/test consolidation |
| 14 | DONE (Review) | **Continuation infrastructure** (P0-P3 DONE; trampolines intentionally separate) |
| 15 | TODO | **Arena & memory system enhancements** |
| 16 | DONE (Review) | **Region-RC dynamic closure integration** |
| 17 | DONE (Review) | **Remaining integration tasks (array growth, print functions)** |
| 18 | TODO | **Computed kinds via compile-time splice** |
| 19 | DONE (Review) | **FFI Implementation** (ccall for C library interop) |
| 20 | DONE (Review) | **Phase 1 Type Specialization** (binary op codegen) |
| 21 | DONE (Review) | **Promise Release Cancellation Fix** (unpark waiting fibers) |

**Completed issues:** See `TODO_COMPLETED.md` for Issues 2, 3, 4, 5, 7.

---

## Issue 22: TAG/TypeID Enum Alignment Optimization [DONE] (Review Needed)

**Objective:** Align TAG enum with TypeID enum to eliminate switch statement overhead in type conversion functions.

**Priority:** MEDIUM - Performance and maintainability improvement.

**Completed:** 2026-01-18

**Reference:**
- `TAG_TYPEID_FIX.md` (detailed analysis and documentation)

### P0: Reorder TAG Enum to Match TypeID [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I22-p0-align-tag-typeid
  Objective: Reorder TAG enum to align with TypeID enum (TAG = TypeID + 1).
  Where: `runtime/include/omni.h:190-219`
  What was done:
    1. Reordered TAG enum to match TypeID enum order
    2. Maintained offset relationship (TAG = TypeID + 1)
    3. Kept extended tags (KEYWORD, EFFECT_TYPE, etc.) at end
  Verification: Build succeeds, all tests pass. ✓

### P1: Optimize Conversion Functions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I22-p1-optimize-conversions
  Objective: Replace switch statements with simple arithmetic operations.
  Where: `runtime/src/memory/region_value.c`, `runtime/src/memory/transmigrate.c`
  What was done:
    1. `type_id_to_tag()` - Changed from 18-case switch to `return type_id + 1`
    2. `tag_to_type_id()` - Changed from 19-case switch to `return tag - 1`
    3. `transmigrate_tag_to_type_id()` - Changed from 24-case switch to fast path + special cases
    4. All functions marked `inline` for better optimization
  Verification: Functions compile and work correctly. ✓

### Benefits

1. **Performance:** Eliminates 61 case statements, replaced with O(1) arithmetic
2. **Maintainability:** Single source of truth for enum order
3. **Binary Size:** Reduced by eliminating ~60 case branches
4. **Type Safety:** Compile-time arithmetic instead of runtime switches

### Test Results

All tests pass:
- `runtime/tests/test_main`: All core tests pass
- `runtime/tests/test_effect`: 22 effect tests pass
- `runtime/tests/test_pattern_match`: 12 pattern matching tests pass
- Build: Clean with only warnings (no errors)

---

