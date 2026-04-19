
## 2026-02-12: Multi-Line REPL Input
- **Feature**: REPL now supports multi-line input for incomplete expressions
  - When an expression has unmatched opening parentheses, REPL prompts with `...   ` for continuation lines
  - Paren counting skips characters inside `"..."` strings and `;` comments
  - Handles escape sequences in strings (`\"` does not close a string)
  - Empty line on continuation prompt cancels the incomplete expression ("Input cancelled.")
  - Ctrl-D on continuation prompt also cancels (without exiting REPL)
  - The full accumulated expression is added to readline history (not individual lines)
  - `quit`/`exit` commands only checked on primary prompt (not during continuation)
  - Uses an 8192-byte buffer for line accumulation with space separators between lines
- **Implementation**: Added `count_paren_depth()` helper function and rewrote `repl()` with line accumulation loop
- **Files modified**: `src/lisp/eval.c3`
- **Tests**: 435 passed, 0 failed (no change -- tests use `run()` not `repl()`)

## 2026-02-12: Macro Hygiene — Definition-Time Binding Capture
- **Feature**: Pattern-based macros now capture definition-time bindings for template hygiene
  - Added `CapturedBinding` struct to `MacroDef` — stores a snapshot of (symbol, value) pairs
  - At macro definition time (`eval_define_macro`), template symbols are scanned:
    - Pattern variables and auto-gensym symbols (ending with `#`) are skipped
    - Special form keywords (`if`, `begin`, `let`, `lambda`, etc.) are skipped
    - All other symbols that resolve in the current global env are captured as snapshots
  - During template expansion (`expand_template`), captured bindings are checked after pattern-var and gensym substitution, embedding the definition-time value directly
  - This prevents expansion-site shadowing from capturing macro-internal references
- **Implementation details**:
  - `collect_pattern_vars()`: Recursively extracts pattern variable names from Pattern* trees (PAT_VAR, PAT_CONS, PAT_SEQ with rest bindings)
  - `capture_template_bindings()`: Walks template Value* tree and snapshots non-special-form, non-pattern-var, non-gensym symbols from global env
  - `is_special_form_symbol()`: Checks 25 special form SymbolIds (if, begin, let, lambda, define, quote, set!, and/or, reset/shift, perform/handle, match, quasiquote, true/false, module/import/export, etc.)
  - Captured bindings stored as `CapturedBinding[32]` per MacroDef (32 max per macro)
  - Template expansion signature updated: `expand_template()` and `expand_template_list()` accept captured bindings array + count instead of Env pointer
- **Files modified**: `src/lisp/value.c3` (CapturedBinding struct, MacroDef fields), `src/lisp/eval.c3` (all template expansion functions, eval_define_macro, new helpers, 12 hygiene tests)
- **Tests**: 423 -> 435 (added 12 hygiene tests)

## 2026-02-12: Fix Production Limits (Module Size, Arg Count, String Length)
- **Bug fix**: Removed 64KB cap on module/script file loading
  - `load_module_from_file()` and `prim_load()` no longer truncate files at 65535 bytes
  - Files are now passed directly to the parser without any artificial size limit
- **Bug fix**: Increased argument/expression limits from 16 to 64
  - `ExprCall.args`, `ExprBegin.exprs`, `ExprLambda.params`, `Closure.params` arrays: 16 -> 64
  - All parser limit checks updated: lambda params, let bindings, named-let bindings, shorthand define params, begin exprs, call args, quasiquote elements
- **Bug fix**: Increased string value capacity from 64 to 4096 bytes
  - Added `MAX_STRING_LEN = 4096` constant, separate from `MAX_SYMBOL_LEN = 64` (symbols stay small)
  - `StringVal` now uses `char[MAX_STRING_LEN]` instead of `char[MAX_SYMBOL_LEN]`
  - `make_string()`, `make_error()`, and all string operation functions updated to use `MAX_STRING_LEN`
  - `prim_read_file()` now passes full file content to `make_string()` (capped at 4095 chars by make_string)
  - `prim_read_lines()` line truncation raised from 63 to 4095 chars
  - All string primitives updated: string-append, string-join, substring, string-split, list->string, string-upcase, string-downcase, string-trim
- **Files modified**: `src/lisp/value.c3`, `src/lisp/eval.c3`, `src/lisp/parser.c3`
- **Tests**: 423 passed, 0 failed (no change)

## 2026-02-12: JIT Lambda/Closure and Recursive Let Support
- **Feature**: JIT compiler now handles E_LAMBDA (closure creation) natively
  - Added `jit_make_closure_from_expr` helper: creates closures from Expr* lambda nodes at runtime
  - Supports single-param, multi-param (curried), zero-arg, and variadic lambdas
  - Closures allocated in root_region for pointer stability (mirrors eval_lambda)
  - New `jit_compile_lambda` emits a call to the helper with interp, expr, and env
- **Feature**: JIT compiler now handles recursive let bindings (let ^rec)
  - Added `jit_eval_let_rec` helper: full recursive let setup (placeholder, extend env, eval init, patch closure env, eval body)
  - Supports recursive functions like factorial and fibonacci via JIT
- **Feature**: JIT E_CALL general case now compiles function/args inline instead of falling back to interpreter
  - Single-arg calls: compile func + arg, call jit_apply_value (like E_APP)
  - Multi-arg calls: curried application f(a0)(a1)...(aN) via stack spilling
  - Zero-arg calls: compile func, apply with nil
  - This enables JIT locals (let-bound lambdas) to be called correctly
- **Bug fix**: Fixed `expand_template_list` in eval.c3 missing `def_env` parameter (arity mismatch with caller)
- **Files modified**: `src/lisp/jit.c3`, `src/lisp/eval.c3`
- **Tests**: 21 JIT tests -> 30 JIT tests (added 9: lambda creation, zero-arg lambda, multi-param lambda, lambda-in-let, let ^rec factorial/fibonacci/tail-recursive-sum)
- **Total tests**: 424 passed, 0 failed (was 415)

## 2026-02-12: GNU Readline REPL Integration
- **Feature**: REPL now uses GNU readline for line editing and command history
  - Arrow keys for cursor movement and history navigation (up/down)
  - Emacs-style editing keybindings (Ctrl-A, Ctrl-E, Ctrl-K, etc.)
  - Persistent in-session command history via `add_history()`
  - Ctrl-D (EOF) gracefully exits the REPL
  - Prompt changed from `> ` to `pika> `
- **Implementation**: Added `readline()` and `add_history()` FFI extern declarations in `eval.c3`
  - Follows same pattern as GNU Lightning FFI in `jit.c3`
  - readline-allocated strings freed with `mem::free()` after each iteration
  - Empty lines skip history but don't error
- **Files modified**: `src/lisp/eval.c3`, `project.json`
- **Tests**: 414 passed, 0 failed (no change)

## 2026-02-12: Script Execution, Stdlib Macros, Load Primitive
- **Feature**: Script file execution mode -- `./main script.pik` reads and evaluates a Pika script file
  - Detects non-flag arguments as script file paths
  - Uses `run_program()` to evaluate multiple top-level expressions
  - Prints last non-nil result; exits with code 0 on success, 1 on error
  - Proper error reporting with line/column info
- **Feature**: Standard macros (`when`, `unless`, `cond`) added to `register_stdlib()`
  - Defined before HOFs so macros are available everywhere, including REPL and scripts
  - `when`: `(when test body...)` -- evaluate body if test is truthy
  - `unless`: `(unless test body...)` -- evaluate body if test is falsy
  - `cond`: `(cond test1 body1 test2 body2 ...)` -- multi-clause conditional
- **Feature**: `load` primitive -- `(load "path/to/file.pik")` reads and evaluates a file in the current environment
  - Takes one string argument (file path)
  - Evaluates all expressions via `run_program()`, returns last result
  - Returns nil on file read error
- **Files modified**: `src/main.c3`, `src/lisp/eval.c3`
- **Tests**: 414 passed, 0 failed (no change)

## 2026-02-12: FEATURES.md Fixes + Graceful Error Handling
- **Documentation**: Fixed multiple inaccuracies in `docs/FEATURES.md`:
  - Removed false claim of "no tail-call optimization" -- Pika has TCO via eval loop
  - Updated memory section from "bump-allocated pools" to region-based allocation
  - Fixed symbol count from 256 to 512, updated all limits to reflect region allocation
  - Added missing features: multi-param lambdas, variadic lambdas, begin, named let, set!, quasiquote, defmacro, hash maps, modules, shorthand define
- **Bug fix**: Replaced `assert()` crashes with graceful error handling:
  - Symbol table exhaustion (`value.c3`): prints error and returns fallback symbol instead of crashing
  - Macro table exhaustion (`eval.c3`): returns `eval_error()` instead of assert crash
  - Module table exhaustion (`eval.c3`, 2 locations): returns `eval_error()` instead of assert crash
- **Files modified**: `docs/FEATURES.md`, `src/lisp/value.c3`, `src/lisp/eval.c3`
- **Tests**: 414 passed, 0 failed (no change)

## 2026-02-12: JIT Nested Direct Primitives
- **Feature**: JIT compiler now supports nested direct primitive calls (e.g. `(+ (* 3 4) (- 10 5))`)
- Previously, direct primitives (+, -, *, <, >, =) only worked when both arguments were simple (E_LIT or E_VAR); nested calls fell back to the interpreter
- Added stack frame spilling via `_jit_allocai` + `stxi_l` / `ldxi_l` to preserve intermediate results when compiling complex arguments
- Fast path (no spilling) retained for simple-arg cases
- **Files modified**: `src/lisp/jit.c3`
- **Tests**: 14 JIT tests -> 17 JIT tests (added 3 nested direct prim tests)
- **Total tests**: 408 passed, 0 failed
