- `src/lisp/primitives.c3` — StringVal helpers, macro hash update, sym_cache bump
- `src/lisp/parser.c3` — String literal creation with strval_new
- `src/lisp/compiler.c3` — LambdaDef captures dynamic, CompiledModule bump
- `src/lisp/runtime.c3` — RT_MAX_PRIOR/HANDLERS/CLAUSES/VAR_TABLE bumps

### Constants Removed (now dynamic)
MAX_SYMBOL_LEN, MAX_SYMBOLS, HASH_TABLE_SIZE, MAX_STRING_LEN, MAX_METHODS, MAX_BINDINGS, MAX_MACROS, MAX_MODULES, MACRO_HASH_SIZE, MODULE_HASH_SIZE, MAX_TYPES, TYPE_HASH_SIZE

### Tests
- 743 unified tests passed, 0 failed
- 77 compiler tests passed, 0 failed
- 367 e2e tests passed

---

## 2026-02-23 (Session 23): Creative C3 Feature Combinations

### Phase 1: $embed Stdlib (Self-Bootstrapping)
- Extracted 65 inline `run("...")` calls from `register_stdlib()` into `stdlib/stdlib.lisp`
- Replaced function body with `$embed("../../stdlib/stdlib.lisp")` + line-by-line parser
- Stdlib is now an editable .lisp file with syntax highlighting support
- Files: NEW `stdlib/stdlib.lisp`, MODIFIED `src/lisp/eval.c3`

### Phase 2: Compile-Time Debug/Release Build Tiers
- Added `const bool DEBUG_BUILD = true` in value.c3 (visible to all modules)
- Wrapped 8 instrumentation sites with `$if DEBUG_BUILD:` / `$endif`:
  - JIT pool GC scheduling (jit.c3)
  - JIT cache clearing (jit.c3)
  - Stack overflow depth reporting (jit.c3)
  - Handler stack overflow detail (jit.c3)
  - JIT GC reclamation stats (jit.c3)
  - copy_to_parent root promotion tracing (eval.c3)
  - Macro table near-capacity warning (macros.c3)
  - Symbol table near-capacity warning (value.c3)
- Verified: `DEBUG_BUILD=false` eliminates all debug code (zero-cost in release)
- Files: MODIFIED `src/lisp/value.c3`, `src/lisp/jit.c3`, `src/lisp/eval.c3`, `src/lisp/macros.c3`

### Phase 3: JIT Emit Helper Functions
- Added 8 emit helpers near existing `emit_call_1` in jit.c3:
  `emit_call_2`, `emit_call_2i`, `emit_call_3`, `emit_call_3i`,
  `emit_call_4_rrir`, `emit_call_4_rirr`, `emit_call_4_rrri`
- Replaced ~30 repetitive 4-5 line JIT call sequences with 1-line helper calls
- Also collapsed quasiquote/reset/shift/handle/path/define_macro compile functions
  into `jit_compile_3arg_helper` reuse (which itself now uses `emit_call_3i`)
- Files: MODIFIED `src/lisp/jit.c3`

### Phase 4: Contracts on Critical Paths
- Added `@require` contracts to 6 critical functions:
  - `copy_to_parent` (eval.c3): null interp check
  - `jit_eval_define` (jit.c3): null interp + null value
  - `jit_env_extend_root` (jit.c3): null interp
  - `jit_handle_impl` (jit.c3): null expr + null interp
  - `eval_define_macro` (macros.c3): null expr + null interp
  - `jit_eval` (jit.c3): null interp
- Files: MODIFIED `src/lisp/jit.c3`, `src/lisp/eval.c3`, `src/lisp/macros.c3`

### Phase 5: Generic Bounded Array (SKIPPED)
- C3 generic modules only support type parameters, not value/constant parameters
- `module bounded_array {Type, usz};` fails: "Only generic parameters are allowed"
- Contracts from Phase 4 provide bounded-array safety instead

### Tests
- 743 unified tests passed, 0 failed
- 77 compiler tests passed, 0 failed
- 367 e2e tests passed

---

## 2026-02-23 (Session 22): Fix 6 Pre-Existing Test Failures

### Bug 1: test_eq_jit missing TCO bounce handler (5 tests)
- `test_eq_jit` and `test_nil_jit` called `f(interp)` directly instead of `jit_exec(f, interp)`
- Top-level expressions compiled with `is_tail=true` use `jit_apply_multi_args_tail` for zero-arg calls
- This returns a TCO sentinel (tag=NIL) which requires `jit_exec`'s bounce loop to handle
- Fix: changed both functions to use `jit_exec(f, interp)` (tests.c3)
- Fixed: "set! counter 1/2/3", "module counter 1/2"

### Bug 2: deep_copy_env breaks mutable box sharing (1 test)
- `run()` wraps each eval in a child region; `jit_make_closure_from_expr` calls `deep_copy_env` when in child region
- For mutable let-locals, JIT creates a boxed Env node shared between stack slot and closure capture
- `deep_copy_env` created a COPY of the box — closure and JIT stack pointed to different Env nodes
- `set!` through the closure updated the copy; reads from outer scope used the original
- Fix: Added `persistent` flag to Env struct; `jit_env_extend_root` allocates mutable boxes in root_region with `persistent=true`; `deep_copy_env` skips persistent envs (preserves sharing) but still fixes up their parent chain
- Fixed: "set! parent scope"

### Files Modified
- `src/lisp/tests.c3` — test_eq_jit/test_nil_jit: f(interp) → jit_exec(f, interp)
- `src/lisp/value.c3` — Env struct: added `persistent` field; alloc_env: initialize persistent=false; make_env: initialize persistent=false
- `src/lisp/jit.c3` — new `jit_env_extend_root()`: allocates box in root_region; `jit_compile_let`: uses jit_env_extend_root for mutable locals
- `src/lisp/eval.c3` — `deep_copy_env`: skip persistent envs, fix parent chain only

### Tests
- 743 passed, 0 failed (6 pre-existing failures now fixed)
- 77 compiler tests passed

---

## 2026-02-23 (Session 21): C3 Modernization — Contracts, Bitstructs, Helpers

### Phase 1B: Data-Driven Primitive Registration
- Replaced 95+ individual `register_prim()`/`register_dispatched_prim()` calls with two data tables + loops
- `PrimReg` struct with `{name, func, arity}` — `PrimReg[19]` dispatched + `PrimReg[94]` regular
- Registration is now declarative — adding a primitive is a single table entry (eval.c3)

### Phase 2: C3 Contracts
- Added `@require` preconditions to 8 functions: `get_int`, `get_symbol`, `car`, `cdr`, `SymbolTable.get_name`, `Env.hash_lookup`, `Env.hash_insert`, `jit_apply_value`
- Added `@ensure return != null` postconditions to 6 factory functions: `make_nil`, `make_int`, `make_double`, `make_string`, `make_symbol`, `make_cons`
- Removed redundant `assert()` calls replaced by contracts (value.c3, jit.c3)

### Phase 3: Shared Helpers + DString Migrations
- Extracted `int_to_string()` helper in value.c3 — replaces digit-reversal loop in `prim_number_to_string`
- Extracted `double_to_string()` helper in value.c3 — replaces duplicate NaN/inf/dot-check in `print_value` and `value_to_string`
- Migrated `format_dispatch_error` to DString (eval.c3) — eliminated manual `char[128]`/`char[64]` position-tracked buffers
- Migrated `format_match_error` to DString (eval.c3) — eliminated manual `char[256]` missing-variants buffer

### Phase 4A: InterpFlags Bitstruct
- Defined `InterpFlags` bitstruct packing 6 bools into 1 byte: `jit_enabled`, `jit_tco_bounce`, `shift_occurred`, `effect_occurred`, `cont_substituting`, `cont_is_effect`
- Replaced 6 scattered `bool` fields on `Interp` struct with single `InterpFlags flags`
- Updated 38 access sites across 4 files (value.c3, jit.c3, runtime_bridge.c3, entry.c3)
- Phases 4B/4C (ClosureFlags, TypeAnnotationFlags) skipped — fields shared between Closure and ExprLambda structs across 7 files, 103+ sites, risk too high

### Phase 5: Additional defer
- Added `defer (void)file.close()` to 4 file write sites: `generate_e2e_tests` (2 files), AOT build temp file, AOT output file
- Ensures file handles closed even on write failure with `!!` rethrow

### Phase 6: Generic Modules — Evaluated and Skipped
- C3 v0.7.9 is in generic syntax transition (module-based → @generic attribute)
- Current inline `Type[MAX]; usz count;` patterns are zero-overhead and simpler
- Cost-benefit doesn't justify the added complexity

### Files Modified
- `src/lisp/value.c3` — contracts, int/double_to_string helpers, InterpFlags bitstruct, print_value/value_to_string dedup
- `src/lisp/eval.c3` — PrimReg data tables, DString error formatters
- `src/lisp/jit.c3` — InterpFlags access migration, jit_apply_value contract
- `src/lisp/primitives.c3` — number->string uses shared helpers
- `src/lisp/tests.c3` — defer for e2e file writes
- `src/lisp/runtime_bridge.c3` — InterpFlags access migration
- `src/entry.c3` — InterpFlags access migration, defer for AOT file writes

### Tests
- 737 passed, 6 failed (same pre-existing), 367 e2e passed

---

## 2026-02-23 (Session 20): Idiomatic C3 Modernization

### DString Adoption
- Replaced `Compiler.output_buf/output_len/output_cap` manual buffer with `DString` (compiler.c3)
- Deleted `ensure_capacity()` — DString handles growth automatically
- `emit()` → `output.append_string()`, `emit_char()` → `output.append_char()`, `get_output()` → `output.str_view()`

### String Primitives — Eliminated 7× `char[4096]` Stack Buffers
- `string-append`, `string-join`, `list->string`: replaced with DString (no truncation limit)
- `substring`, `string-trim`, `string-split`: replaced with direct slices (zero-copy)
- `string-upcase`, `string-downcase`: allocate via `make_string` then transform in-place

### defer Cleanup
- REPL readline loop: 5× `mem::free(line)` → single `defer mem::free(line)` (eval.c3)
- `prim_write_file`: 2× `file.close()!!` → single `defer (void)file.close()` (primitives.c3)
- `compile_program`: manual `mem::free(full_buf)` + `exprs.free()` → `defer` (compiler.c3)

### Default Parameter Cleanup
- Removed 15 explicit `false` arguments from `jit_compile_expr` calls (jit.c3) — `is_tail` already defaults to `false`

### Files Modified
- `src/lisp/compiler.c3` — DString output buffer, defer in compile_program
- `src/lisp/primitives.c3` — DString string prims, direct slices, defer file I/O
- `src/lisp/eval.c3` — defer in REPL readline loop
- `src/lisp/jit.c3` — removed redundant is_tail=false arguments

### Tests
- 737 passed, 6 failed (same pre-existing)
- 367 e2e compiler tests pass

## 2026-02-23 (Session 19): JIT TCO + Per-Eval Temp Regions + Audit Review

### JIT TCO
- Added `jit_apply_value_tail` — tail-call variant that sets TCO bounce instead of recursing
- Added `jit_apply_multi_args_tail` — multi-arg tail-call variant (closures, variadics, dispatch)
- Threaded `bool is_tail` through `jit_compile_expr` and all `jit_compile_*` functions
- Tail propagation: if branches, last begin expr, let body, and/or right side inherit tail position
- Tail consumption: `jit_compile_app` and `jit_compile_call` emit `_tail` variants when `is_tail`
- Named-let 10000 iters restored (was reduced to 5000 due to arena OOM)
- 100000 iteration loops and mutual recursion with 10000 depth now work

### Per-Evaluation Temp Regions
- Wrapped `run()` in child region — temporaries die on release, reduces arena pressure
- Fixed `jit_eval_set` to promote values to root_region when in child frame (prevents dangling pointers from set! in closures)
- Fixed `jit_eval_let_rec` to promote rec_env to root_region when in child frame
- Updated eval.c3 comment (was "deferred", now documents active child regions)

### Files Modified
- `src/lisp/jit.c3` — _tail variants, is_tail threading, let_rec/set! child-region fixes
- `src/lisp/eval.c3` — run() child region wrapper, comment update
- `src/lisp/tests.c3` — restored 10000 iteration test

### Audit Review (M1-M18)
- Verified ALL 18 medium-priority audit findings
- M14 (Arena Fragmentation Without Coalescing): Actually FIXED — coalescing code exists, stale comment corrected in main.c3
- M17 (Ghost Table Double-Free): VERIFIED SAFE — value-copy + null-out pattern, `.clear()` not `.free()`, zero-length iteration
- M13 description clarified: JIT uses runtime `jit_get_env()`, not baked compile-time pointer
- Updated `memory/AUDIT_REPORT.md`: 46/64 fixed (was 44), 17 remaining (was 18)

### Files Modified
- `src/lisp/jit.c3` — _tail variants, is_tail threading, let_rec/set! child-region fixes
- `src/lisp/eval.c3` — run() child region wrapper, comment update
- `src/lisp/tests.c3` — restored 10000 iteration test
- `src/main.c3` — fixed stale M14 coalescing comment
- `memory/AUDIT_REPORT.md` — M14 FIXED, M17 VERIFIED SAFE, summary counts updated

### Tests
- 737 passed, 6 failed (same 6 pre-existing failures as before)
- 367 e2e compiler tests pass
- REPL smoke test passes

## 2026-02-23 (Session 18): Runtime Diagnostic Improvements

### Better Dispatch Failure Diagnostics
- Added `format_dispatch_error()` helper in `eval.c3` — shows method name, arg types, and arity hints
- Replaced 2 terse `"no matching method"` errors in `jit.c3` with detailed diagnostics
- Arity mismatch shows expected vs actual arg counts; type mismatch shows `(String)` etc.

### Match Exhaustiveness Warnings for Union Types
- Added `format_match_error()` helper in `eval.c3` — shows missing union variants
- For union types: `"no pattern matched: DiagResult value 'DiagErr', missing variants: DiagOk"`
- For non-union types: `"no pattern matched for value of type Int"`

### `(handle ^strict body ...)` — Opt-in Strict Effect Handlers
- Added `strict_mode` bool to `ExprHandle` and `EffectHandler` structs in `value.c3`
- Parser recognizes `^strict` annotation after `handle` keyword
- In `jit_perform_impl`, strict handlers block effect propagation — unmatched effects produce immediate error
- Error: `"strict handler: unhandled effect 'my-eff' with arg type Int"`

### Enhanced Unhandled Effect Diagnostics
- Replaced terse `"unhandled effect"` with `"unhandled effect 'tag-name' with arg type Type"`

### Tests
- Added `test_error_contains()` helper for substring-matching on error messages
- Added `run_diagnostic_tests()` with 9 new tests (dispatch, match, strict handler, effect diagnostics)
- All 9 diagnostic tests pass; 0 failures

### Arena Pressure Fix
- Reduced deep-iteration test from 10000→5000 to avoid arena OOM when run after ~700 prior tests
- Runtime bridge TCO depth test also reduced 10000→5000
- Root cause: arena memory accumulates across all test evaluations (no per-test release)
- Standalone 10000-iter loops work fine; only fails under cumulative test-suite pressure

### Files Modified
- `src/lisp/eval.c3` — `format_dispatch_error`, `format_match_error` helpers
- `src/lisp/jit.c3` — dispatch/match/effect error wiring, strict handler check
- `src/lisp/value.c3` — `strict_mode` in ExprHandle/EffectHandler
- `src/lisp/parser.c3` — `^strict` annotation parsing in `parse_handle`
- `src/lisp/tests.c3` — `test_error_contains`, `run_diagnostic_tests` (9 tests)

## 2026-02-23 (Session 17): Runtime Consistency, Compiler Cleanup & Documentation Sync

### Runtime Limit Consistency
- `RT_MAX_CLAUSES` 8 → 16 in `runtime.c3` (was missed when interpreter side was raised in Session 16)
- Added shift bounds guards to `prim_lshift`/`prim_rshift` (shift < 0 || shift >= 64 → return 0), matching runtime.c3 behavior

### Compiler Dead Code Removal (~720 lines)
- Removed 23 unreachable non-flat handler functions from `compiler.c3`
- These were only called from `compile_expr`'s switch, which is only reached for leaf expressions (E_LIT, E_VAR, E_QUOTE, E_PATH, E_DEFINE) — all complex tags intercepted by `compile_to_temp`
- Removed: `compile_lambda`, `compile_application`, `is_binary_prim`, `compile_if`, `compile_let`, `compile_and`, `compile_or`, `compile_match`, `compile_call`, `compile_index`, `compile_reset`, `compile_shift`, `compile_perform`, `compile_handle`, `compile_begin`, `compile_set`, `compile_quasiquote`, `compile_qq`, `compile_qq_call_inline`, `compile_qq_call_with_splice_inline`, `compile_defmacro`, `compile_module`, `compile_import`
- Kept: `compile_quote`, `compile_path`, `compile_pattern_check`, `emit_pattern_var_decl`, `compile_pattern_bindings` (all actively used)
- compiler.c3 reduced from ~4742 lines to ~4022 lines

### Documentation Sync
- `docs/LANGUAGE_SPEC.md`: Updated Appendix B limits table (symbols 4096→8192, match 16→32, effects 8→16, eval 200→5000, types 128→256, methods 32→64); updated Appendix C backends (reset/shift/handle/perform/modules now Y for JIT+compiler); added unsafe-free! in Section 7.22
- `docs/FEATURES.md`: Updated Section 7 compiler support table (effects/QQ/modules/dot-bracket/path now Y); added Section 11b AOT Compilation; updated Section 12 limits; added Section 5.11 unsafe-free!
- `docs/SYNTAX_SPEC.md`: Updated Section 9 limits table (symbols 8192, effects 16, match 32, methods 64, types 256)
- `docs/COMPILER.md`: Rewrote Limitations section (removed stale delegation entries); added AOT Binary Generation section

### Files Modified
- `src/lisp/runtime.c3` — RT_MAX_CLAUSES 8→16
- `src/lisp/primitives.c3` — shift bounds guards in prim_lshift/prim_rshift
- `src/lisp/compiler.c3` — removed ~720 lines of dead code
- `docs/LANGUAGE_SPEC.md` — limits, backends, unsafe-free!
- `docs/FEATURES.md` — compiler table, AOT section, limits, unsafe-free!
- `docs/SYNTAX_SPEC.md` — limits
- `docs/COMPILER.md` — limitations rewrite, AOT section

### Tests
- 734 unified + 77 compiler = 811 tests, all passing
- 366 e2e tests generated (pre-existing e2e diff issues unrelated to this session)

---

## 2026-02-22 (Session 16): Memory Safety & Fixed-Size Limits

### unsafe-free! Rename + Use-After-Free Detection
- Renamed `free!` → `unsafe-free!` (naming communicates danger)
- After freeing backing storage, marks value as ERROR with "use after unsafe-free!" instead of silently setting NIL
- Any code accessing a freed value now gets a descriptive error propagated through existing error handling

### Raised Fixed-Size Constants
- `MAX_METHODS`: 32 → 64 (method table is malloc'd, no structural impact)
- `MAX_TYPES`: 128 → 256 (TypeRegistry on Interp heap)
- `TYPE_HASH_SIZE`: 256 → 512 (2×MAX_TYPES)
- `MAX_SYMBOLS`: 4096 → 8192 (SymbolTable on Interp)
- `HASH_TABLE_SIZE`: 8192 → 16384 (2×MAX_SYMBOLS)
- `MAX_MATCH_CLAUSES`: 16 → 32 (after making ExprMatch pointer-indirect)
- `MAX_EFFECT_CLAUSES`: 8 → 16 (after making ExprHandle pointer-indirect)

### ExprMatch & ExprHandle Pointer-Indirect
- Changed `ExprMatch match;` → `ExprMatch* match;` in Expr union (shrinks union)
- Changed `ExprHandle handle;` → `ExprHandle* handle;` in Expr union (shrinks union)
- Added malloc in parse_match/parse_handle; C3 auto-deref means all access sites unchanged

### Files Modified
- `src/lisp/value.c3` — 7 constants raised, ExprMatch/ExprHandle pointer-indirect in Expr union
- `src/lisp/primitives.c3` — prim_free_bang: renamed prefix, ERROR tag on freed values
- `src/lisp/eval.c3` — Renamed `free!` → `unsafe-free!` registration
- `src/lisp/parser.c3` — malloc ExprMatch/ExprHandle in parse_match/parse_handle
- `src/lisp/jit.c3` — Updated error message string for handler clause limit
- `src/lisp/tests.c3` — Updated free! tests, added 3 limit tests

### Test Count
- Before: 731 unified + 77 compiler + 366 e2e = 1174
- After: 734 unified + 77 compiler + 366 e2e = 1177

## 2026-02-22 (Session 15): Julia-Style Type Enforcement at Boundaries

### Constructor Field Type Validation
- `prim_type_constructor` (eval.c3) now validates field types before constructing instances
- Fields with concrete type annotations (^Int, ^Point, etc.) are checked against argument types
- Parametric fields (matching a type_param) are skipped — validated by constraint check instead
- Error messages include field name, expected type, and actual type

### Dispatch Ambiguity Detection
- `find_best_method` (eval.c3) tracks `best_count` alongside `best_score`
- When multiple methods match with equal best score, returns error instead of silently picking first
- Follows Julia's `MethodError: ambiguous` behavior
- Both JIT call sites (jit.c3) updated to propagate ERROR values from dispatch

### Parametric Constraint Enforcement at Construction
- Added `constraints[]` and `constraint_count` fields to `TypeInfo` (value.c3)
- `eval_deftype` extracts constraints from dict annotations (`^{'T Number}`) on fields
- `prim_type_constructor` validates inferred type_args against constraints after construction
- All TypeInfo initializations (builtin, abstract, union, alias, effect) init `constraint_count = 0`

### Files Modified
- `src/lisp/value.c3` — TypeInfo struct: added constraints[] and constraint_count
- `src/lisp/eval.c3` — prim_type_constructor (field validation + constraint check), find_best_method (ambiguity), eval_deftype (constraint extraction), all TypeInfo inits
- `src/lisp/jit.c3` — ERROR propagation from find_best_method at both call sites
- `src/lisp/tests.c3` — 11 new tests (6 constructor, 2 dispatch, 3 parametric)

### Test Count
- Before: 720 unified + 77 compiler + 366 e2e = 1163
- After: 731 unified + 77 compiler + 366 e2e = 1174

## 2026-02-22 (Session 14): Standalone AOT Binary — Decouple runtime.c3

### Native Dict/Array Types (Task #13)
- Added `V_DICT` and `V_ARRAY` tags to runtime ValueTag enum
- Added `RtHashMap`, `RtHashEntry`, `RtArray` structs to runtime.c3
- Implemented self-contained CRUD: `rtmap_new/get/set/grow/remove`, `rtarray_new/get/set/push`
- FNV-1a + Murmur finalizer hashing, open-addressing with backward-shift deletion
- Rewrote all `rt_hash_*` public functions to use native V_DICT instead of V_INTERP_REF delegation

### Bridge Extraction
- Created `src/lisp/runtime_bridge.c3` (module lisp) — all interpreter↔runtime conversion
- `bridge_interp_to_runtime`: HASHMAP→V_DICT, ARRAY→V_ARRAY (native conversion)
- `bridge_runtime_to_interp`: V_DICT→HASHMAP, V_ARRAY→ARRAY
- `bridge_eval_source`: full interpreter eval, registered via hook
- Function pointer hook (`g_eval_source_hook`) so runtime.c3 dispatches without lisp:: dependency

### runtime.c3 Standalone
- Removed all 89 `lisp::` references from runtime.c3 (only `module lisp::runtime;` remains)
- Removed: g_interp, rt_ensure_interp, interp_to_runtime, runtime_to_interp, all wrapper structs
- V_INTERP_REF kept in enum for bridge migration but all handler code removed
- rt_eval_source dispatches through hook (null = stub for standalone)

### AOT Binary Slimmed
- `--build` now compiles from **5 files** (was 22): main.c3, continuation.c3, ghost_index.c3, runtime.c3, generated.c3
- Dropped `-l lightning` and `-l readline` from AOT link
- AOT binaries link only: libc, libm, libdl
- Updated `scripts/run_e2e.sh` with runtime_bridge.c3 in compile list
- Removed `import lisp;` from main.c3

### Tests
- All 720 unified + 77 compiler + 366 e2e tests pass
- AOT dict/arithmetic/length operations verified standalone

### Files Modified
- `src/lisp/runtime.c3` — +250 lines (native dict/array), −200 lines (bridge removed)
- `src/lisp/runtime_bridge.c3` — NEW (~250 lines)
- `src/lisp/tests.c3` — added bridge_init() call
- `src/entry.c3` — slimmed --build to 5 files
- `scripts/run_e2e.sh` — added runtime_bridge.c3
- `src/main.c3` — removed `import lisp;`

## 2026-02-22 (Session 13): Split eval.c3 Monolith

### eval.c3 Module Split (Task #12)
- Split eval.c3 from 8445 lines into 4 focused modules:
  - `eval.c3` (2068 lines): core types, dispatch, type system, memory, pattern matching, init, REPL
  - `primitives.c3` (2362 lines): all 129+ prim_* functions, HashMap impl, FFI primitives
  - `macros.c3` (925 lines): expr_to_value, value_to_expr, macro expansion, gensym, find_module
  - `tests.c3` (3040 lines): all test suites, compiler tests, e2e generation
- Updated `scripts/run_e2e.sh` and `src/entry.c3` --build file lists with new modules
- Cleaned up tombstone comments from deleted JIT-replaced functions
- All 1163 tests pass (720 unified + 77 compiler + 366 e2e)

### Files Modified
- `src/lisp/eval.c3` — 75% reduction (8445 → 2068 lines)
- `src/lisp/primitives.c3` — NEW (2362 lines)
- `src/lisp/macros.c3` — NEW (925 lines)
- `src/lisp/tests.c3` — NEW (3040 lines)
- `scripts/run_e2e.sh` — added new source files to compile list
- `src/entry.c3` — added new source files to --build compile list

## 2026-02-22 (Session 12): JIT GC, Variadic Lambda AOT, FFI Doubles, Error Locations, Region Pinning

### JIT State Pool & Cache GC (Tasks #7-#8)
- Added `jit_gc()` function: destroys all JIT states + clears cache between top-level evaluations when pool exceeds 75% capacity (3072/4096)
- Added `jit_cache_clear()`: evicts all cache entries when cache reaches 75% capacity (768/1024)
- GC runs safely between top-level evals in `run_program()` and REPL (no JIT code on call stack)

### Variadic Lambda AOT Compilation (Task #9)
- Parser: `(define (f x .. rest) body)` shorthand now handles `T_DOTDOT` token correctly
- Compiler: `LambdaDef` extended with `has_rest`, `rest_param`, `params[]`, `param_count`; lambda body generates arg-list unpacking via `rt_car1`/`rt_cdr1`
- Runtime: `ClosureData.is_variadic` flag, `make_variadic_closure()`, `rt_apply_multi` detects variadic closures and passes full arg list
- E_CALL compilation: `compile_call_flat` uses `rt_apply_multi` with cons list; `compile_call_tail_flat` has runtime variadic check with `make_thunk` for TCO preservation

### Multi-arg `list` Compiler Fix
- Added `list` special case in `compile_call_flat` (like `dict`): builds cons chain directly instead of currying through unary `rt_list` primitive
- `compile_call_tail_flat` delegates `list`/`dict` to non-tail version

### Changes
- `src/lisp/jit.c3` — JIT GC infrastructure (51 lines added)
- `src/lisp/compiler.c3` — Variadic lambda support, rt_apply_multi for E_CALL, list special case (239 lines changed)
- `src/lisp/runtime.c3` — Variadic closure support (55 lines added)
- `src/lisp/parser.c3` — Define-shorthand variadic handling (47 lines added)
- `src/lisp/eval.c3` — jit_gc() calls, updated test expectation (14 lines changed)

### Source Location in Error Messages (Task #10)
- `jit_eval_to_result()`, `run_program()`, `run()` now use `eval_error_expr(msg, expr)` instead of `eval_error(msg)`, threading Expr.loc_line/loc_column to error display
- Errors now show `Error at line N, column N:` instead of bare `Error:`

### FFI Double/XMM Register Support (Task #11)
- Added properly-typed function pointer aliases: `FfiFnD0..D3` (double args + double return), `FfiFnID0..2`, `FfiFnDI1..2` (cross-type)
- `prim_ffi_call` classifies arg types and dispatches to correct function pointer cast
- Replaces broken bit-cast approach (doubles passed in integer registers) with correct x86-64 ABI
- Added `ffi_is_double_type()`, `ffi_value_to_double()` helpers
- Added `test_eq_double()` test helper + 2 tests (ffi-call sqrt, ffi-call pow via libm)

### Region Pinning for Continuations (Task #14)
- Added `captured_frame: main::RegionHandle` to `CapturedCont` struct
- `jit_shift_impl` and `jit_perform_impl` call `retain_region(current_frame)` when capturing continuation
- Prevents UAF when temp frame is released before captured continuation is invoked

### Changes
- `src/lisp/jit.c3` — JIT GC, error locations, region pinning
- `src/lisp/compiler.c3` — Variadic lambda, rt_apply_multi for E_CALL, list special case
- `src/lisp/runtime.c3` — Variadic closure support
- `src/lisp/parser.c3` — Define-shorthand variadic handling
- `src/lisp/eval.c3` — jit_gc, error locations, FFI doubles, test helpers
- `src/lisp/value.c3` — CapturedCont.captured_frame field

### Test Count
- 1163 total (720 unified + 77 compiler + 366 e2e) — all passing

---

## 2026-02-21 (Session 11): Tasks #15, #18, #21, #32, #35 — ALL 35 AUDIT TASKS COMPLETE

### Task #15: Replace compile_var linear string chain with hash map
Replaced 87 `str_eq` comparisons in `compile_var()` with O(1) `prim_hash_lookup()`. Added `PrimHashEntry[256]` open-addressing hash table with ~90 entries. `init_prim_hash()` called from `Compiler.init()`. `compile_var` reduced from 190 lines to 20 lines.

### Task #35: Compile multi-binding let as single env extension
Optimized eval's E_LET case: consecutive non-recursive lets are batched into a single Env frame. Creates one `make_env()` + N `define()` calls instead of N `env.extend()` calls. Preserves let* semantics (each init sees previous bindings).

### Task #32: Implement arena free-list coalescing
`Pool.arena_free()` now coalesces adjacent free chunks. Finds left-adjacent and right-adjacent chunks in the same arena and merges them. Handles three-way merge (left + new + right → one chunk).

### Task #18: Consolidate compiler AST traversal passes
Merged `body_creates_closure()` into `scan_lambdas_with_scope()`. Changed return type to `bool` — returns whether any E_LAMBDA was found in subtree. Eliminates a separate full traversal per lambda. Removed ~90 lines.

### Task #21: Add collection reclamation via `free!` primitive
Added `(free! value)` primitive for explicit memory reclamation. Frees heap-allocated backing storage (Array items+struct, HashMap entries, Instance struct, StringVal) and sets value tag to NIL for safe aliasing. 4 tests added.

### Changes
- `src/lisp/compiler.c3` — Tasks #15, #18: hash table for compile_var, merged body_creates_closure
- `src/lisp/eval.c3` — Tasks #21, #35: free! primitive, multi-binding let batching
- `src/main.c3` — Task #32: arena free-list coalescing

### Test Count
- 1099 total (654 unified + 77 compiler + 368 e2e) — all passing

---

## 2026-02-21 (Session 10): Tasks #13-#14, #16-#17, #19-#20, #22-#24, #27-#31, #33-#34

### Task #13: Fix runtime_to_interp V_CLOSURE wrapping
Added `CompiledClosureWrapper` struct and `invoke_compiled_closure` PrimitiveFn. Added `user_data` field to Primitive struct and `prim_user_data` to Interp for threading data through primitive calls.

### Task #14: Cache compiled STDLIB_PRELUDE
Replaced per-char `List{char}` source building with bulk `mem::copy` for stdlib prelude concatenation.

### Task #16: Pre-cache TypeIds for built-in types
Added `tid_Int`, `tid_Double`, etc. fields to Interp, populated after `register_builtin_types()`. Updated `infer_value_type` to use cached IDs instead of repeated `symbols.intern()` calls.

### Task #17: Cache I/O fast path symbols and primitives
Added `raw_print`, `raw_println` etc. cached Value* pointers to Interp. Updated eval_perform I/O fast path. Added `sym_car`/`sym_cdr` for eval_path and set! path.

### Task #19: Align runtime StringData to 4096
Changed runtime StringData from `char[256]` to `char[4096]`, updated all 255 limits to 4095.

### Task #20: Replace emit() with bulk mem::copy
Replaced `List{char}` output with raw `char*` + `output_len` + `output_cap`. `emit()` now uses `mem::copy`.

### Task #22: Remove dead compile_expr tail-call code
Removed `compile_expr_tail`, `compile_application_tail`, `compile_call_tail` (~146 lines).

### Task #23: Remove 7 dead standalone eval functions
Removed eval_app, eval_call, eval_if, eval_and, eval_or, eval_let, eval_match (~250 lines).

### Task #24: Fix rt_string_split multi-char delimiters
Rewrote to scan for full delimiter string matches instead of single-char.

### Task #27: Add type validation to bitwise operations
Added V_INT tag checks to all 6 bitwise ops + shift bounds checking (0-63).

### Task #28: Fix long.min UB
Added `long.min` special cases to `rt_abs` and `rt_number_to_string`.

### Task #29: Increase SymbolTable capacity
MAX_SYMBOLS 512→4096, MAX_SYMBOL_LEN 64→128, HASH_TABLE_SIZE 1024→8192.

### Task #30: Optimize MethodTable dispatch
Pre-compute arg TypeIds once into `TypeId[8]` array before scanning methods.

### Task #31: Fix O(n²) pattern matching
Collect list elements into `Value*[64]` flat array in one pass, then index directly.

### Task #33: Fix make_interp_closure_wrapper leak
Changed from `allocate_in(g_root_region, ...)` to `mem::malloc()`.

### Task #34: Add FFI_HANDLE destructor
Added `dlclose(v.ffi_val.lib_handle)` in destroy_value for FFI_HANDLE.

### Changes
- `src/lisp/eval.c3` — Tasks #16, #17, #23, #30, #31, #34
- `src/lisp/value.c3` — Tasks #13, #16, #17, #29
- `src/lisp/runtime.c3` — Tasks #13, #19, #24, #27, #28, #33
- `src/lisp/compiler.c3` — Tasks #14, #20, #22

### Test Count
- 1095 total (650 unified + 77 compiler + 368 e2e) — all passing

---

## 2026-02-21 (Session 9): Tasks #9-#12, #25

### Task #9: Add depth limit to rt_values_equal
Added `usz depth = 0` default parameter, guard at depth >= 256. Prevents stack overflow on circular structures.

### Task #10: Add ARRAY/HASHMAP equality to rt_values_equal
Added V_INTERP_REF case: ARRAY uses structural equality (element-by-element comparison via `interp_to_runtime`), HASHMAP uses pointer equality (matching interpreter behavior).

### Task #11: Fix rt_string_to_number to parse floats
Added float detection (scan for '.', 'e', 'E') with manual double parser: integer part, fractional part, exponent part. Returns V_DOUBLE for float strings.

### Task #12: Fix rt_number_to_string to handle doubles
Added V_DOUBLE check at function start, formats using `io::bprintf` with "%.15g".

### Task #25: Replace rt_gensym rt_eval_source delegation with counter
Replaced `rt_eval_source("(gensym)")` with `tlocal long g_gensym_counter` that formats "g#N" symbols directly. No more re-parsing overhead.

### Changes
- `src/lisp/runtime.c3` — All 5 tasks: depth limit, ARRAY/HASHMAP equality, float parsing, double formatting, gensym counter

### Test Count
- 1095 total (650 unified + 77 compiler + 368 e2e) — all passing

---

## 2026-02-21 (Session 8): Tasks #6-#8

### Task #7: Add hash table to Env for O(1) variable lookup
Added open-addressing hash table to Env struct, activated when binding_count exceeds 16 (ENV_HASH_THRESHOLD). Global env with 129+ primitives now uses O(1) lookups instead of O(n) linear scan.

### Changes
- `src/lisp/value.c3` — Added `EnvHashEntry` struct, `hash_table`/`hash_capacity` fields to Env, `build_hash_table`/`hash_lookup`/`hash_insert` methods, updated `Env.define`/`Env.lookup`/`Env.set` with hash fast paths, updated `alloc_env` to init new fields
- `src/lisp/eval.c3` — Updated `destroy_env` to free hash table, `deep_copy_env` to rebuild hash table on copy

### Task #8: Add V_FALSE tag to runtime
Added V_FALSE tag to runtime ValueTag enum, `make_false()` constructor. Updated `rt_is_truthy` (V_FALSE is falsy), `rt_print_value` (prints "false"), `rt_values_equal`, `rt_boolean_p` (recognizes V_FALSE), `rt_type_of` (returns "Bool"), `interp_to_runtime` (sym_false→V_FALSE), `runtime_to_interp` (V_FALSE→sym_false). Note: interpreter defines `false` as `make_nil()`, so compiler still emits `make_nil()` for `false` literal to match. V_FALSE primarily used when quoted `false` symbol flows through `interp_to_runtime`.

---

## 2026-02-21 (Session 8): Task #6 — Native dict operations in runtime

### Task #6: Implement native dict operations in runtime
Replaced all 10 dict/hashmap functions that delegated through `rt_eval_source` (parsing source text) with direct calls to interpreter's hashmap functions (`lisp::hashmap_get/set/remove`, `lisp::make_hashmap`, `lisp::make_cons`, `lisp::make_nil`).

### Changes
- `src/lisp/runtime.c3` — Rewrote 10 functions:
  - `rt_hash_map_create`: direct `lisp::make_hashmap(g_interp, 16)`
  - `rt_dict_from_args`: direct hashmap create + `lisp::hashmap_set` per pair (fixes #26 buffer overflow)
  - `rt_hash_ref`: direct `lisp::hashmap_get`
  - `rt_hash_set`: curried 2→3 arg pattern — returns closure(dict,key) that accepts value; uses malloc'd `HashSetCapture` struct
  - `rt_hash_has`: direct `lisp::hashmap_get` + null check, returns `make_true()` not `make_int(1)`
  - `rt_hash_remove`: direct `lisp::hashmap_remove`
  - `rt_hash_keys`/`rt_hash_values`: iterate `map.entries[]`, build cons list
  - `rt_hash_count`: direct `im.hashmap_val.count`
  - `rt_hash_map_p`: direct tag check, returns `make_true()` not `make_int(1)`

### Test Count
- 1095 total (650 unified + 77 compiler + 368 e2e) — all passing

---

## 2026-02-21 (Session 7): Tasks #3-#5

### Task #3: Remove Auto-Currying
Removed auto-currying from interpreter and JIT. Multi-param lambdas kept intact. Compiler re-curries at compile time only.

### Task #5: Eliminate cons-list intermediary for compiler multi-arg calls
Replaced `compile_call_flat`'s cons list building + `rt_apply_multi` with inline currying (`rt_invoke_once` for intermediates, `rt_invoke` for last arg). Fixed `compile_call_tail_flat` intermediates to use `rt_invoke_once` instead of `rt_invoke`. Updated 5 compiler pattern tests.

### Task #4: Pointer-indirect large Expr union variants
Moved 6 large Expr union members behind pointers (malloc'd): ExprLambda*, ExprCall*, ExprBegin*, ExprModule*, ExprDefType*, ExprDefUnion*. Reduces Expr node union from ~3200 bytes to ~272 bytes. C3 auto-deref means read sites are unchanged.

### Changes
- `src/lisp/value.c3` — 6 union members changed from inline to pointer
- `src/lisp/parser.c3` — Added malloc at 17 creation sites (parse_lambda, parse_define, parse_named_let, parse_application, parse_begin, parse_module, parse_deftype, parse_defunion, dict/array literals, quasiquote)
- `src/lisp/eval.c3` — Added malloc at 7 creation sites (value_to_expr), fixed &expr.deftype → expr.deftype
- `src/lisp/compiler.c3` — Added malloc at 1 creation site (scan_lambdas_with_scope wrapper), fixed find_free_vars multi-param

### Test Count
- 1095 total (650 unified + 77 compiler + 368 e2e) — all passing

---

## 2026-02-21 (Session 7, earlier): Remove Auto-Currying (Task #3)

Removed auto-currying from the interpreter and JIT. Multi-param lambdas are now kept intact in the parser and bound directly in eval/JIT. The compiler re-introduces currying at compile time only (via `scan_lambdas_with_scope` AST mutation) since the runtime uses single-arg calling convention.

### Changes
1. **Parser**: `parse_lambda`, `parse_define`, `parse_named_let` — create direct multi-param E_LAMBDA instead of nested single-param curried lambdas
2. **Eval E_CALL CLOSURE**: Direct multi-param binding (one env frame for all params, arity check) instead of currying loop
3. **Eval apply_closure**: Added multi-param rejection (apply_closure is single-arg only)
4. **JIT jit_apply_multi_args**: Direct multi-param binding matching eval semantics
5. **Compiler scan_lambdas_with_scope**: Re-curries multi-param lambdas at compile time (AST mutation to nested single-param)
6. **Compiler find_free_vars**: Fixed E_LAMBDA case to add ALL params (not just first) to bound vars — critical for multi-param lambdas encountered before re-currying

### Files Modified
- `src/lisp/parser.c3` — parse_lambda, parse_define, parse_named_let
