### New Files
- `csrc/ffi_helpers.c` — thin C wrapper around libffi: `omni_ffi_call` (fixed arity) and `omni_ffi_call_var` (variadic)

### Modified Files
- `project.json` — Added `"ffi"` to linked-libraries, added `csrc/ffi_helpers.c` to c-sources
- `src/lisp/value.c3` — New ExprTag E_FFI_LIB/E_FFI_FN, ExprFfiLib/ExprFfiFn/FfiBoundFn structs, FfiTypeTag enum, type_ann_to_ffi_tag helper, sym_ffi/sym_lib/sym_Ptr/sym_Void interned symbols
- `src/lisp/parser.c3` — Multi-word attribute parsing `[ffi lib]`/`[ffi λ libname]`, parse_ffi_lib, parse_ffi_fn, λ UTF-8 detection
- `src/lisp/eval.c3` — eval_ffi_lib (dlopen + bind), eval_ffi_fn (dlsym + build bound primitive), prim_ffi_bound_call (libffi dispatch), omni_ffi_call extern
- `src/lisp/jit.c3` — E_FFI_LIB/E_FFI_FN compile cases via jit_do_ffi_lib/jit_do_ffi_fn, prim_user_data set in multi-arg primitive path
- `src/lisp/bindgen.c3` — Updated --bind output to emit `define [ffi lib]`/`define [ffi lambda]` syntax
- `src/lisp/tests.c3` — 14 new FFI redesign tests

### Tests
- 892 unified + 77 compiler + 9 stack engine = 978 (up from 964: +14 new FFI tests)
- 0 failures

## 2026-02-26 (Session 46): FPU save/restore (D1) + stack overflow detection (D2)

### Summary
Implemented two deferred items from the fiber-continuation plan. D1: FPU state (MXCSR + x87 control word) is now saved/restored across all coroutine context switches, preventing FPU rounding mode leaks from FFI code. D2: Stack overflows in coroutines are caught via SIGSEGV handler on sigaltstack and recovered gracefully as Omni errors instead of crashing.

### New Files
- `csrc/stack_helpers.c` — C helper providing fpu_save/fpu_restore (GCC inline asm), SIGSEGV handler with sigaltstack, guard page registry, and `stack_guard_protected_switch` (sigsetjmp-wrapped context switch)

### Modified Files
- `project.json` — Added `c-sources: ["csrc/stack_helpers.c"]`
- `src/stack_engine.c3` — Extern declarations for C helpers; `stack_context_switch` exported as `omni_context_switch`; `coro_switch_to`/`coro_resume` use `fpu_save`/`fpu_restore` + `stack_guard_protected_switch` for overflow recovery; `coro_suspend` uses `fpu_save`/`fpu_restore`; guard page registration in `coro_create`/`coro_destroy`; `stack_guard_init`/`shutdown` in pool lifecycle; 2 new tests (FPU preservation, stack overflow recovery)
- `src/lisp/jit.c3` — CORO_DEAD checks at all 4 coro switch sites (reset, apply-continuation, handle, resolve) returning "stack overflow" errors
- `src/lisp/primitives.c3` — CORO_DEAD check in `prim_resume` returning "stack overflow in fiber" error
- `.claude/plans/fiber-continuation-unification.md` — D1, D2 marked COMPLETE; D3, D4 marked NOT IMPLEMENTING

### Tests
- 878 unified + 77 compiler + 9 stack engine = 964 (up from 962: +2 new stack engine tests)
- 0 failures

## 2026-02-26 (Session 45): Remove replay mechanism (Phase 4)

### Summary
Removed the entire replay-based continuation system. Fiber-based continuations are now the only path — no more `use_fiber_continuations` flag, no more CapturedCont, no more context_capture/context_restore setjmp/longjmp, no more replay state machine.

### Deleted Files (~110KB of removed code)
- `src/context.c3` — RegisterContext, context_capture/context_restore (x86_64 asm setjmp/longjmp)
- `src/continuation.c3` — PromptTag, SavedContext, PromptStack (unused scaffolding)
- `src/delimited.c3` — Low-level reset/shift/resume using context_capture (unused by interpreter)

### Modified Files
- `src/lisp/jit.c3` — Removed `jit_reset_impl`, `jit_shift_impl`, `jit_handle_impl`, `jit_perform_impl`, replay path in `jit_apply_continuation`, CapturedCont path in `jit_exec_resolve`. Simplified dispatch functions (no more if/else on flag).
- `src/lisp/value.c3` — Removed `CapturedCont` struct, `data` field from `Continuation`, InterpFlags (`shift_occurred`, `effect_occurred`, `cont_substituting`, `cont_is_effect`), ~20 Interp fields (replay state, handle_jmp, active_effect_cc, reset body/env stacks, etc.), `ensure_effect_prior`, `ensure_shift_prior`, `grow_reset_stacks`.
- `src/entry.c3` — Removed `run_context_tests()` and `run_delimited_tests()` calls
- `src/main.c3` — Removed g_prompt_stack and prompt_stack_init/shutdown references

### Tests
- 878 unified + 77 compiler + 7 stack engine = 962 (down from 994 due to removed context/delimited test suites)
- 0 failures

## 2026-02-26 (Session 44): User-facing fibers (Phase 3)

### Summary
Implemented `fiber`, `resume`, `yield`, `fiber?` primitives backed by the stack engine. Added FIBER ValueTag. Renamed stdlib `yield` macro to `stream-yield` to avoid conflict.

### Changes
- `src/lisp/value.c3` — FIBER ValueTag, `fiber_val` in Value union, `make_fiber`, `yield_value` on Interp
- `src/lisp/primitives.c3` — `prim_fiber`, `prim_resume`, `prim_yield`, `prim_fiber_p`, `FiberThunkState`, `fiber_thunk_entry`
- `src/lisp/eval.c3` — Register fiber/resume/yield/fiber? primitives, FIBER in value_type_name
- `src/lisp/tests.c3` — 8 new fiber tests (basic, yield, complete, resume value, nested, deep, multi-yield, generator)
- `src/stack_engine.c3` — Added `user_data` field to Coro struct
- `stdlib/stdlib.lisp` — Renamed `yield` macro to `stream-yield`
- `tests/test_iterators_extended.lisp` — Updated to use `stream-yield`

### Tests
- 994 total (up from 986), 0 failures
- test_yield.lisp and test_nested_fiber.lisp (Tests 1, 3) pass externally

## 2026-02-26 (Session 44): Fiber-based algebraic effects (Phase 2)

### Summary
Wired stack engine into handle/signal/resolve. Effect handlers now run body on a coro — signal suspends the coro, handler clause evaluates in parent context, resolve resumes the coro (single-shot). Both fiber and replay paths coexist via `use_fiber_continuations` flag.

### Key Implementation Details
- `HandleFiberState`: shared state between handle loop and signal, stores signal tag/arg/handler copy
- `jit_signal_impl_fiber`: suspends coro on signal, saves/restores interp state (same pattern as shift)
- `jit_handle_impl_fiber`: creates coro for body, loop dispatches signals to matching clauses
- `jit_exec_resolve` fiber path: single-shot resume, reinstalls handler for multi-signal patterns
- `raise_pending` flag preservation: captured before interp state restore to prevent loss
- I/O fast path handled inline for unhandled signals

### Files Modified
- `src/lisp/jit.c3` — HandleFiberState, handle_fiber_entry, jit_signal_impl_fiber, jit_handle_impl_fiber, jit_exec_handle/perform/resolve dispatch
- `src/lisp/value.c3` — Added `handle_state` to Continuation, `fiber_state` to EffectHandler
- `.claude/plans/fiber-continuation-unification.md` — Phase 2 marked complete

### Tests
- 986 total (unchanged), 0 failures
- All existing effect tests pass: signal/resolve, abort, multi-signal, raise/try-catch, I/O fast path, nested handles

## 2026-02-26 (Session 42-43): Fiber-based delimited continuations (Phase 0+1)

### Summary
Implemented stack engine (Phase 0) and wired it into reset/shift (Phase 1) for the fiber-continuation unification plan. Fiber-based continuations use real separate stacks with assembly context switching instead of the replay-based mechanism. Both paths coexist, selected by `interp.use_fiber_continuations` flag (defaults to true).

### Key Implementation Details
- **Stack engine** (`src/stack_engine.c3`): mmap'd stacks with guard pages, x86_64 assembly context switching (callee-saved regs + stack/frame pointer), cooperative coroutines (Coro), coro_clone with RBP chain fixup for multi-shot continuations, StackPool for reuse
- **Fiber reset/shift** (`src/lisp/jit.c3`): `jit_reset_impl_fiber` runs body on separate coro stack; `jit_shift_impl_fiber` suspends coro and captures it as continuation; `jit_apply_fiber_continuation` always clones for multi-shot safety
- **Hidden return pointer fix**: `jit_shift_impl_fiber` returns `Value*` (not `EvalResult`) to avoid System V ABI hidden stack pointer that becomes stale after coro_clone
- **Interp state isolation**: Save/restore `eval_depth`, `jit_env`, `match_env`, `flags`, `jit_tco_expr/env`, `reset_depth`, `handler_count` at all three boundaries (reset→coro, shift suspend/resume, apply→clone)
- **Value changes** (`src/lisp/value.c3`): `Continuation` struct gains `coro` and `is_fiber_based` fields; `Interp` gains `coro_pool`, `use_fiber_continuations`, `resume_value`, `current_reset_state`

### Files Modified
- `src/stack_engine.c3` — New file: stack engine primitives (Phase 0)
- `src/lisp/jit.c3` — Fiber-based reset/shift/apply functions, dispatch in jit_exec_reset/shift
- `src/lisp/value.c3` — Continuation and Interp struct additions
- `.claude/plans/fiber-continuation-unification.md` — Master plan file

### Tests
- 986 total (up from 927), 0 failures
- Stack engine: 7 unit tests (basic coro, suspend/resume, clone, lifecycle, pool reuse, clone isolation, result passing)
- Fiber reset/shift: existing continuation tests pass via new fiber path

## 2026-02-26 (Session 41): omni-torch refactor + module expression limit increase

### Summary
Refactored omni-torch XOR example to use resolved effect values (no more workarounds). Added sub-scalar-i dispatch. Increased module/begin expression limit from 64 to 256.

### Changes
- **omni-torch/examples/xor_nn.omni** — Removed `train-chunk` workaround; direct per-epoch training with checkpoint signals every 500 epochs; resolved value for early stopping; clean handle return without `set!`
- **omni-torch/lib/torch.omni** — Added `- (Tensor, Int)` dispatch via `omni-torch-sub-scalar-i`
- **omni-torch/csrc/torch_shim.{h,cpp}** — Added `omni_torch_sub_scalar_i`
- **omni-torch/lib/ffi/torch.omni** — Added `omni-torch-sub-scalar-i` FFI binding + export
- **src/lisp/jit.c3** — Increased module expression limit from 64 to 256
- **src/lisp/parser.c3** — Increased begin expression limit from 64 to 256

### Tests
- 947 total (unchanged), 0 failures
- omni-torch: `make test` and `make xor` pass

## 2026-02-26 (Session 39+40): Effect handler bug fixes — all 4 known bugs resolved

### Summary
Fixed all 4 known effect handler bugs: resolve-with-value hang, multi-tag crash, abort hang, and memory explosion with many signals. Root cause was body continuing after handler fired (no non-local exit). Solution: longjmp-based termination via context_capture/context_restore.

### Bug Fixes
1. **Resolve-with-value hang** — body continued executing after handler fired. Fix: handler clause does `context_restore` to handler's save point, terminating the body.
2. **Multi-tag crash** — replay check only matched same-tag signals. Fix: removed tag check from replay condition (tag-agnostic multi-tag replay).
3. **Abort hang** — handler that doesn't call resolve left body running. Fix: all handler clauses now terminate body via `context_restore` to handler save point.
4. **Memory explosion (>~20 signals)** — each signal allocated ~2KB CapturedCont (embeds EffectHandler with 64-element arrays) in root_region, never freed.
   - Fix: switch CapturedCont + Continuation to `malloc`/`free`, recycled per handler level via `active_effect_cc[64]` array on Interp.
   - Root cause of crash at ~256 signals: region system's inline storage invalidated by packed_slots reallocation (Pool growth invalidated `dereference_as` pointers). Fixed by using malloc for Continuation too.
   - 50,000 signals with resolved values now works correctly.

### Direct Resume Optimization (single-shot resolve)
- `jit_exec_perform`: `context_capture` for signal save point
- `jit_exec_resolve`: single-shot path longjmps directly to signal save point (O(1) per signal instead of O(N²) replay)
- Multi-shot fallback: uses existing replay via `jit_apply_continuation`

### Files Modified
- **src/lisp/value.c3** — Added `handle_jmp[64]`, `active_effect_cc[64]` to Interp; added `signal_save`, `saved_jit_env`, `resolved`, `handler_idx`, `owned_continuation` to CapturedCont
- **src/lisp/jit.c3** — Rewrote `jit_handle_impl` (save point + prev_jmp + prev_active_cc management), `jit_perform_impl` (malloc + handler_count unwinding + context_restore), `jit_exec_perform` (signal save point), `jit_exec_resolve` (single-shot direct resume)

### Tests
- 870 unified + 77 compiler = 947 total, 0 failures
- Stress: 50,000 signals with resolved values works correctly

## 2026-02-26 (Session 38): match bug fix, omni-torch syntax + XOR NN demo

### Summary
Fixed critical match bug (TCO bounce not handled when match nested inside define/let/args). Updated omni-torch repo to current syntax. Created XOR neural network example demonstrating effects, match, cond, dispatch, named let, format strings, and module system.

### Bug fix
- **src/lisp/jit.c3** `jit_do_match` — match used TCO bounce to evaluate clause bodies, but this only works at top-level `jit_eval`. When nested inside `define`/`let`/args, outer compiled code received the TCO sentinel instead of the actual value. Fix: evaluate clause body directly via `jit_eval()` instead of TCO bouncing. This also fixed `cond` in handler bodies (same root cause).

### Changes
- **src/lisp/jit.c3** — fixed `jit_do_match` TCO bounce bug (match now works from files, nested in any context)
- **omni-torch/lib/torch.omni** — converted 6 old-style let expressions to flat-pair syntax
- **omni-torch/examples/xor_nn.omni** — new: 2-layer sigmoid network, manual backprop, chunked training with effect-based monitoring, two demo handlers (verbose + annotated with match/cond)
- **omni-torch/Makefile** — added `xor` target

### Effect handler findings
- Single-tag signals with resolve nil: works reliably
- Using resolved value (not nil) hangs the process
- Multi-tag signals (nested/multi-clause handles) crash
- Abort pattern (no resolve) hangs
- Many signals per handle scope causes memory explosion
- Workaround: train in pure chunks, signal between chunks, resolve nil, use set! to capture results

## 2026-02-26 (Session 37): Replxx REPL with syntax highlighting, completion, colored output

### Summary
Replaced GNU readline with replxx for the REPL. Adds real-time syntax highlighting (rainbow parens, keyword/constant coloring, string/comment/number highlighting), tab completion of defined symbols, colored output (green for results, red for errors, blue prompt), and persistent history (~/.omni_history).

### Features
- **Syntax highlighting**: keywords (bold cyan), builtins (magenta), strings (green), comments (gray), numbers (magenta), rainbow parens (6-color cycle), brackets (yellow), braces (brown)
- **Tab completion**: completes any symbol defined in the global environment
- **Colored output**: results in green, errors in red, prompt in bold blue
- **History**: persistent to `.omni_history`, unique entries, max 1000

### Files Modified
- `src/lisp/eval.c3` — replaced readline FFI with replxx FFI, added `lisp_highlighter`, `flush_symbol`, `lisp_completion` callbacks, rewrote `repl()` function
- `project.json` — `readline` → `replxx` + `stdc++` in linked-libraries
- `src/entry.c3` — updated comment

### Test Count
- 870 unified + 77 compiler = 947 total, 0 failures

## 2026-02-26 (Session 36): Printf-style format, cl-format backward compat

### Summary
Added new printf-style `format` primitive with specifiers `%s`, `%d`, `%f`, `%e`, `%x`, `%X`, `%o`, `%b`, `%%`, plus width/precision/left-align support. Renamed old CL-style format (using `~a`, `~s`, `~~`) to `cl-format` for historical support.

### Files Modified
- `src/lisp/primitives.c3` — renamed `prim_format` to `prim_cl_format`, added new `prim_format` with printf-style parsing, added helpers `strval_append`, `strval_append_padded`, `strval_append_num_padded`, `format_append_display`
- `src/lisp/eval.c3` — registered both `"format"` (new) and `"cl-format"` (old), bumped `PrimReg` array size from 88 to 89
- `src/lisp/tests.c3` — replaced 1 old format test with 18 new tests (3 cl-format backward compat + 15 printf-style format)

### Test Count
- 870 unified + 77 compiler = 947 total, 0 failures

## 2026-02-26 (Session 35): Rename respond → resolve

### Summary
Renamed `respond` keyword to `resolve` throughout the codebase. `resolve` better conveys "settling an open question" — signal raises something unresolved, the handler resolves it. Works for all patterns (request/response, logging, errors) without implying directionality.

### Files Modified
- `src/lisp/value.c3` — `E_RESPOND` → `E_RESOLVE`, `ExprRespond` → `ExprResolve`, `sym_respond` → `sym_resolve`
- `src/lisp/parser.c3` — `parse_respond` → `parse_resolve`, all references updated
- `src/lisp/jit.c3` — `jit_exec_respond` → `jit_exec_resolve`, `jit_compile_respond` → `jit_compile_resolve`
- `src/lisp/macros.c3` — `E_RESPOND` → `E_RESOLVE`
- `src/lisp/compiler.c3` — serializer, stdlib prelude, all references
- `src/lisp/tests.c3` — all test expressions: `(respond ...)` → `(resolve ...)`
- `stdlib/stdlib.lisp` — `with-trampoline` handler
- `docs/EFFECTS_GUIDE.md` — full update including section headers
- `docs/LANGUAGE_SPEC.md` — full update, also fixed stale let syntax and removed backward compat section

### Test Count
- 850 unified + 77 compiler = 927 total, 0 failures

## 2026-02-26 (Session 34): Syntax cleanup — flat-pair let, signal/resolve, implicit begin

### Summary
Major syntax simplification pass. Replaced Scheme-style `(let ((x 10)) body)` with flat-pair `(let (x 10) body)`. Replaced `perform`/`k` effect syntax with `signal`/`resolve`. Added implicit begin for lambda and define bodies. No support layer remained (version < 1.0).

### Let Syntax — Flat Pairs
- **Regular let**: `(let (x 10) body)` and `(let (x 1 y 2) body)` — no double parens
- **Named let**: `(let loop (n 0 acc 0) body)` — flat pairs in loop bindings
- **Recursive let**: `(let ^rec (f expr) body)` — same flat-pair pattern
- Updated parser (`parse_let`, `parse_named_let`), macro expander (`value_to_expr`), and compiler serializer
- All bodies now support implicit begin (multiple expressions)

### Effect Syntax — signal/resolve
- `perform` removed from parser — `signal` is the only keyword
- Old handler clause `((tag k arg) (k expr))` replaced with `(tag arg (resolve expr))`
- Old `((tag k arg) expr)` abort style replaced with `(tag arg expr)`
- Multi-shot tests keep old `((tag k x) (+ (k 10) (k 20)))` syntax (needs explicit `k`)
- `with-trampoline` stdlib updated: `bounce` handler uses `resolve`

### Implicit Begin
- Added `parse_implicit_begin()` helper in parser
- Lambda (all variants), shorthand define, let, named let — all support multiple body expressions
- `(lambda () (println "hi") 42)` now works without explicit `(begin ...)`

### Files Modified
- `src/lisp/parser.c3` — `parse_implicit_begin`, flat-pair let/named-let, removed `perform` keyword
- `src/lisp/macros.c3` — `value_to_expr` updated for flat-pair let, `sym_perform` → `sym_signal`
- `src/lisp/compiler.c3` — serializer updated, embedded stdlib updated
- `src/lisp/tests.c3` — all 170+ test expressions updated
- `stdlib/stdlib.lisp` — all let/signal/resolve syntax updated
- `docs/EFFECTS_GUIDE.md` — already used signal/resolve from previous session

### Test Count
- Before: 850 unified + 77 compiler = 927 total
- After: 850 unified + 77 compiler = 927 total, 0 failures

## 2026-02-25 (Session 33): Error messages, value display, module loader, omni-torch

### Summary
Improved error messages, rewrote value display for all types, enhanced module loader to run top-level expressions after module forms, and converted omni-torch from `load` to proper `module`/`import`.

### Error Messages
- **Nil call site**: `'tensor/zeros' is not defined` with hint about load/import
- **Non-function call site**: `'x' is Int, not a function` (shows actual type)
- Added `last_call_name` field to `Interp` struct for call-site context
- ERROR tag propagation in all 4 apply functions prevents "is Any" for unbound vars

### Value Display
- Closures: `#<closure>` → `#<λ say (s)>` / `#<λ (x y)>` / `#<λ (x .. rest)>`
- Added `name` field to Closure struct, set by `define`
- Dispatch: `#<method-table [bytes]:N>` → `#<dispatch map 4 methods>` / `#<dispatch + +default>`
- Dict: `#<hashmap:2>` → `{a 1 b 2}` (shows contents)
- Symbols: fixed `[byte array]` → proper name (ZString cast fix)
- Partial: `#<partial>` → `#<partial N more>`
- Module/Type: fixed garbled names (ZString cast fix)

### Load Error Propagation
- `prim_load` now raises proper errors instead of silently returning nil
- Missing file: `load: file not found 'path'`
- Evaluation error: `load: error in 'path': <details>`

### Module Loader Enhancement
- `jit_load_module_from_file` now evaluates ALL expressions in file, not just the first module form
- Expressions after `(module ...)` run in global scope — enables dispatch extensions alongside modules

### omni-torch Conversion
- `lib/torch.omni`: Converted from `load`-based to proper `(module torch (export ...) ...)`
- Dispatch extensions (`+`, `-`, `*`, `/` on Tensor) placed after module form in global scope
- `src/main.omni`: `(load ...)` → `(import "lib/torch.omni" :all)`
- Error propagation now surfaces FFI failures clearly

### Relative Import Resolution
- Added `source_dirs` stack (16 deep) to `Interp` struct for resolving imports relative to the importing file
- `push_source_dir` / `pop_source_dir` / `resolve_import_path` / `path_dir_len` helpers in jit.c3
- `jit_load_module_from_file`, `jit_eval_import_impl`, `prim_load`, and entry.c3 script runner all push/pop source dirs
- Fixes: imports now work from Neovim plugin (where CWD differs from source dir)

### Module System Robustness
- **Null-terminated import paths**: Parser now null-terminates `import_expr.path` buffer; also null-terminates `lib/<name>.omni` path
- **Failed module retry**: Modules that fail to load (loaded=false) can be re-defined on retry (zeroes stale name)
- **Idempotent file imports**: `jit_load_module_from_file` skips already-loaded modules (needed for files that import same dependency in multiple scopes)
- **Path-vs-declared-name resolution**: Path-based imports like `(import "../lib/torch.omni" :all)` intern the path as symbol, but the file declares `(module torch ...)`. Fallback uses `interp.modules[mod_count_before]` (first module registered during load) instead of `find_module(interned_path_name)`

### Bug Fixes
- Fixed `.pika` → `.omni` file extension in `jit_eval_import_impl`
- Fixed `Pika Lisp REPL` → `Omni Lisp REPL` in eval.c3

### Modified Files
- `src/lisp/jit.c3` — Error messages, ERROR propagation, module loader (all expressions, idempotent, path aliasing, source dir stack)
- `src/lisp/value.c3` — `last_call_name` in Interp, `name` in Closure, `source_dirs` stack, all display rewrites
- `src/lisp/primitives.c3` — `prim_load` error propagation + relative path resolution
- `src/lisp/parser.c3` — Null-terminated import path buffer
- `src/lisp/eval.c3` — REPL banner, symbol display ZString fix
- `src/entry.c3` — Push source dir when running script files
- `omni-torch/lib/torch.omni` — Rewritten as module + dispatch extensions
- `omni-torch/src/main.omni` — load → import, relative path `"../lib/torch.omni"`

### Tests
- 922 total (845 unified + 77 compiler), 0 failures

## 2026-02-24 (Session 32): P3C + P4D — Multi-arg HOFs, Primitive Consolidation, Nil<:List

### Summary
Broke the curried HOF convention: all stdlib higher-order functions now use multi-arg dispatch instead of curried chains. `map`, `filter`, `for-each`, `any?`, `every?` get 1-arg `^Closure` dispatch form (returns lambda for pipelines). `foldl`/`foldr`'s `f` takes 2 args directly. 12 primitives moved to dispatched (P4D). `Nil` is now a subtype of `List` (nil IS the empty list).

### P3C: Multi-arg Dispatched HOFs
- **map**: `(map f lst)` 2-arg dispatched on List/Array/Iterator; `(map f)` 1-arg ^Closure returns lambda
- **filter**: Same pattern — List/Array/Iterator dispatch + 1-arg ^Closure form
- **foldl**: `(foldl f acc lst)` — `f` takes 2 args `(f acc x)`, named-let TCO
- **foldr**: `(foldr f init lst)` — `f` takes 2 args `(f x acc)`, delegates to foldl+reverse
- **append, compose, nth, take, drop, zip, range**: Multi-arg (no currying)
- **for-each, any?, every?**: 2-arg dispatched on ^List + 1-arg ^Closure form
- **try, assert!**: 2-arg (was curried)
- **assoc, assoc-ref**: 2-arg (was curried)
- **reverse** moved before map/filter (dependency ordering)
- Compiler STDLIB_PRELUDE updated to match
- ~65 test call sites updated from curried to multi-arg syntax

### P4D: Primitive Consolidation
- 12 primitives moved from regular to dispatched: string-append, string-contains?, string-upcase, string-downcase, abs, floor, ceiling, round, truncate, sqrt, min, max
- Dispatched prims: 19 → 31, Regular prims: 100 → 88

### Type Hierarchy Fix
- `Nil` is now a subtype of `List` — nil IS the empty list in Lisp
- Enables `(map f nil)`, `(filter pred nil)`, `(every? pred nil)` to dispatch correctly

### Modified Files
- `stdlib/stdlib.lisp` — Complete HOF rewrite (multi-arg + dispatch)
- `src/lisp/eval.c3` — Nil<:List parent chain, 12 prims moved to dispatched
- `src/lisp/compiler.c3` — STDLIB_PRELUDE rewritten to multi-arg
- `src/lisp/tests.c3` — ~65 test call sites updated

### Tests
- 1290 total (845 unified + 77 compiler + 368 e2e), 0 failures

## 2026-02-24 (Session 31): Feature Roadmap Phases 2-3 — λ Syntax, UTF-8, Iterators

### Summary
Implemented Phases 2A, 2B, and 3 of the feature roadmap: λ lambda syntax, UTF-8 string support, Iterator type with lazy sequences. Phase 5A (typed effect dispatch) was implemented then reverted — it introduced a second dispatch mechanism in handle clauses inconsistent with the language's MethodTable-based dispatch design. The idiomatic approach is to use dispatched functions inside handlers instead.

### Phase 2A: λ Lambda Syntax
- Parser recognizes `λ` (U+03BB, UTF-8 0xCE 0xBB) as synonym for `lambda`
- `is_symbol_char()` accepts high bytes >= 0x80 for UTF-8 symbol support
- 4 new tests

### Phase 2B: UTF-8 String Support
- **New file**: `src/lisp/utf8.c3` (~130 lines) — codec: `utf8_codepoint_len`, `utf8_decode`, `utf8_encode`, `utf8_strlen`, `utf8_byte_offset`, `utf8_valid`
- `string-length` counts codepoints (not bytes); added `string-byte-length` for raw bytes
- `char-at`, `substring`, `string->list`, `jit_do_index` — all codepoint-indexed
- AOT runtime: `rt_utf8_cplen`, `rt_utf8_strlen`, `rt_utf8_byte_offset`
- 14 new tests

### Phase 3: Iterator Type + Lazy Sequences
- `V_ITERATOR` value tag (backed by `Value*` thunk closure)
- 5 new primitives: `iterator?`, `make-iterator`, `next`, `collect`, `to-array`
- `Iterator` registered as builtin type in TypeRegistry
- Stdlib: `iterator` dispatched constructor (List, Array, Dict, Iterator), `imap`, `ifilter`, `itake`, `idrop`, `izip`, `range-from`, `irepeat`, `icycle`, `ifoldl`, `iterator-empty`
- 28 new tests (10 primitive + 18 stdlib)

### Design Decision: No typed effect clause syntax
Typed effect dispatch via `((tag k (^Type arg)) body)` was implemented and reverted. It created a match-style dispatch inside handle blocks — a parallel mechanism to the MethodTable. Instead, use dispatched functions inside handlers:
```lisp
(define (on-show (^Int x))    "got int")
(define (on-show (^String s)) "got string")
(handle body ((show k x) (k (on-show x))))
```

### Modified Files
- `src/lisp/utf8.c3` — **NEW** UTF-8 codec
- `src/lisp/value.c3` — ITERATOR tag, iterator_val, make_iterator
- `src/lisp/parser.c3` — λ recognition
- `src/lisp/primitives.c3` — UTF-8 string ops, iterator primitives, string-byte-length
- `src/lisp/eval.c3` — Iterator type registration
- `src/lisp/jit.c3` — UTF-8 string indexing
- `src/lisp/runtime.c3` — UTF-8 helpers
- `src/lisp/tests.c3` — 46 new tests
- `stdlib/stdlib.lisp` — Iterator definitions (15 lines)
- `scripts/run_e2e.sh` — Added utf8.c3

### Tests
- 1290 total (845 unified + 77 compiler + 368 e2e), 0 failures

## 2026-02-24 (Session 30): Dead Code Removal + Negative Indexing + Consistency Fixes

### Summary
Removed 23 dead code items, added Python-style negative indexing everywhere, and fixed all 5 inconsistency categories found by audit agents.

### Dead Code Removed
- `CurriedPrim` struct (unused from auto-curry era)
- 8 unregistered primitive functions: `prim_is_string`, `prim_is_int`, `prim_is_symbol`, `prim_is_closure`, `prim_is_dict`, `prim_dict_ref`, `prim_dict_count`, `prim_is_number`, `prim_is_double` (type predicates defined in stdlib instead)
- `prim_make_array` + `prim_array_ref` (never registered, superseded by `array` dispatch + `ref` dispatch)
- 7x "type registry full" dead checks in eval.c3 (TypeRegistry.grow() prevents overflow)
- 7x "symbol table exhausted" dead checks in parser.c3 + primitives.c3 (SymbolTable.grow() prevents overflow)

### Negative Indexing (Python-style)
- `prim_ref`: array + string cases now support -1 = last, -2 = second-to-last, etc.
- `prim_array_set`: supports negative indices
- `prim_char_at`: supports negative indices
- `jit_do_index`: list (walks to count length), string, array — all support negative indices
- Consistent with existing `substring` negative index support

### Consistency Fixes
- **Arity checking**: All 9 predicates (null?, pair?, not, continuation?, boolean?, list?, procedure?, array?, instance?) now error on missing args instead of returning nil/true inconsistently
- **`prim_ref` cons handling**: Now walks cons chains like `jit_do_index` (supports arbitrary list indices + negative indexing), not just pair 0/1 access
- **Dispatch wrappers**: Inlined `prim_dict_has`, `prim_dict_remove`, `prim_dict_keys`, `prim_dict_values` into their dispatch wrappers — eliminated double type checks
- **Null checks**: Removed 10+ redundant `args[0] == null` guards (args are always valid Value* after length check)
- **Error messages**: `"modulo:"` → `"%:"`, `"unbound variable"` → includes variable name, `"not a function"` → includes actual type, `"args"` → `"argument(s)"` throughout

### Modified Files
- `src/lisp/primitives.c3` — Removed struct + 11 dead functions + 1 dead error check; added negative indexing to 4 functions; inlined 4 dict wrappers; fixed 9 predicate arity checks; removed redundant null checks; fixed error message style
- `src/lisp/eval.c3` — Removed 7 dead `register_type` return checks
- `src/lisp/parser.c3` — Removed 6 dead `intern` return checks
- `src/lisp/jit.c3` — Added negative indexing to `jit_do_index` (list, string, array); improved "unbound variable" + "not a function" messages; normalized arity error wording
- `src/lisp/tests.c3` — Added 17 new tests (13 negative indexing + 4 ref-on-list)

### Tests
- 865 tests pass (788 unified + 77 compiler), 0 failures (+17 from previous)

## 2026-02-23 (Session 29): Error Messages + Doc Limit Fixes

### Summary
Improved 20+ error messages across eval.c3, jit.c3, primitives.c3, parser.c3 — all cryptic or context-free errors now include type names, field names, expected/actual counts, and actionable descriptions. Fixed stale limits in all 3 doc files (match clauses 32→128, effect clauses 16→64, strings/symbols "4095"/"128"→"dynamic heap-allocated", `-` arity 2→1-2).

### Modified Files
- `src/lisp/eval.c3` — 6x "type registry full" → include limit, "unknown parent type" → include name, "empty path" → clarify meaning, "field not found" → include field+type names, "path segment not found" → include segment+value type, "not exported from module" / "symbol not found in module" → include symbol name, constructor errors → include type name and expected/got counts
- `src/lisp/jit.c3` — "negative list index" → "must be >= 0", "unsupported collection type" → include actual type, "perform type mismatch" → include effect name + expected/actual types, 4x "cannot apply null" → "cannot call nil (not a function)", 2x variadic lambda → include required/got counts
- `src/lisp/primitives.c3` — "make-array invalid size" → include actual value, "symbol table exhausted" → add "(out of memory)"
- `src/lisp/parser.c3` — 6x "symbol table exhausted" → add "(out of memory)"
- `docs/LANGUAGE_SPEC.md` — Fix `-` arity, match clauses, effect clauses, string/symbol limits
- `docs/FEATURES.md` — Fix match clauses, effect clauses, string description, symbol/string limits
- `docs/SYNTAX_SPEC.md` — Fix match clauses, effect clauses, symbol/string limits

### Tests
- 848 tests pass (771 unified + 77 compiler), 0 failures

## 2026-02-23 (Session 27): Project Scaffolding + FFI Binding Generator (--init, --bind)

### Summary
Added `--init` and `--bind` CLI commands for creating Omni projects and auto-generating FFI bindings from C headers using libclang.

### New Files
- `src/lisp/toml.c3` (~200 lines) — Minimal TOML parser for project.toml
- `src/lisp/libclang_bind.c3` (~300 lines) — libclang dlopen wrapper, C header parser via clang_visitChildren
- `src/lisp/bindgen.c3` (~150 lines) — Omni FFI module generator (ffi-open/ffi-call wrappers)

### Modified Files
- `src/entry.c3` — Added `run_init()` and `run_bind()` commands (+200 lines)

### Features
- `--init myproject`: Scaffolds project directory with project.toml, project.json, src/main.omni, lib/ffi/, include/
- `--bind [dir]`: Reads project.toml, parses C headers via libclang, generates typed Omni FFI modules
- TOML parser: [section.sub.name], key = "value", key = ["a", "b"], # comments
- C-to-Omni type mapping: int→'int/^Int, double→'double/^Double, char*→'string/^String, void*→'ptr/^Int
- snake_case → kebab-case conversion for function names
- Variadic C functions skipped with comment
- Helpful error messages when libclang not found (install instructions per distro)

### Tests
- All 848 existing tests pass (771 unified + 77 compiler), 0 failures
- Manual verification: --init creates correct structure, --bind generates correct ffi-call wrappers for libm (sin, cos, sqrt) and libc (strlen, strcmp, memcpy)

## 2026-02-23 (Session 28): Documentation Rework — Remove Stale Docs, Fix Auto-Curry References

### Summary
Removed 26 stale OmniList-era docs. Reworked all 7 remaining docs to fix inaccurate auto-curry references (removed in Session 25), update module system descriptions to reflect Session 26 redesign, and fix JIT section. Removed completed unified-dispatch-plan.md. Retitled type-system-syntax.md from "Proposal" to "Reference".

### Removed Files
- 26 stale OmniList-era docs (QUICK_REFERENCE.md, FFI_DESIGN_PROPOSALS.md, ARCHITECTURE_DIAGRAMS.md, etc.)
- `docs/unified-dispatch-plan.md` — described already-completed MethodTable + Val dispatch work

### Modified Files
- `docs/FEATURES.md` — Fixed lambda/define auto-curry→strict arity, module system→qualified access, JIT section→sole engine, application model
- `docs/LANGUAGE_SPEC.md` — Fixed intro, S-expression forms, lambda section, stdlib note, module section, footer
- `docs/SYNTAX_SPEC.md` — Replaced "Currying" section with "Partial Application" (_, |>, partial), fixed lambda section, module section, footer
- `docs/type-system-syntax.md` — Retitled from "Proposal" to "Reference", removed stale [effect] NOT-implemented entry, fixed curried type example
- `docs/PROJECT_TOOLING.md` — Created (previous session)

### Tests
- No code changes, documentation only

## 2026-02-23 (Session 26): Module System Redesign — Qualified Access, Selective Import, Re-Export

### Summary
Complete module system overhaul: modules are now first-class values (`V_MODULE`), `(import mod)` defaults to qualified-only access via dot-path (`mod.symbol`), with explicit selective/renamed/all imports. Added `export-from` for re-export. All fixed-size arrays in module structs converted to dynamic malloc+grow.

### Breaking Change
- `(import name)` no longer dumps all exports into scope — it only binds the module as a value for qualified access
- Use `(import name :all)` for old behavior, or `(import name (sym1 sym2))` for selective

### Phase 1: Module as First-Class Value
- **V_MODULE tag** in ValueTag enum, `Module* module_val` in Value union
- **`make_module()`** constructor, allocates in root_region
- **`print_value`/`print_value_buf`** handle V_MODULE: `#<module name>`
- **Import binds module value**: `jit_eval_import_impl` creates V_MODULE and binds it by module name
- **Dot-path access**: `eval_path` resolves V_MODULE segments — checks export list, looks up in module env

### Phase 2: Selective Import Syntax
- **Extended ExprImport**: `imports*`, `aliases*`, `import_count`, `import_all` fields
- **Parser**: `(import mod (sym1 sym2))`, `(import mod (sym :as alias))`, `(import mod :all)`
- **Pre-interned symbols**: `sym_as` (`:as`), `sym_all` (`:all`), `sym_export_from` (`export-from`)
- **Import eval**: qualified-only (default), selective with rename, or all exports

### Phase 3: Re-Export
- **E_EXPORT_FROM** ExprTag + ExprExportFrom struct
- **Parser**: `(export-from mod (sym1 sym2))` or `(export-from mod :all)`
- **JIT eval**: `jit_eval_export_from_impl` — finds current module, copies symbols from source, adds to export list
- **Works inside module bodies**: Detects current module via env matching

### Phase 4: Compiler Support
- Added E_EXPORT_FROM cases to: `find_free_vars`, `scan_lambdas_with_scope`, `compile_to_temp`, `serialize_expr_to_buf`
- `compile_export_from_flat` — no-op in compiled mode (symbols already inlined as globals)
- Dynamic `CompiledModule*` table with grow

### Phase 5: Dynamic Limits
- `Module.exports[256]` → `SymbolId*` + `export_capacity` (malloc + 2x grow)
- `ExprModule.exports[256]` → `SymbolId*` + `export_capacity`
- `ExprModule.body[256]` → `Expr**` + `body_capacity`
- `ExprImport.imports[64]`/`aliases[64]` → `SymbolId*` + `import_capacity`
- `ExprExportFrom.names[64]` → `SymbolId*` + `name_capacity`
- `CompiledModule[64]` → `CompiledModule*` + `compiled_module_capacity`
- All with destructors in `Interp.destroy`/`Compiler.free`

### Files Modified
- `src/lisp/value.c3` — V_MODULE tag, Value union, make_module, Module/ExprModule/ExprImport/ExprExportFrom structs, Interp symbols, print_value, destroy
- `src/lisp/jit.c3` — jit_eval_import_impl rewrite, jit_eval_export_from_impl, jit_do_export_from, jit_compile_export_from, dynamic export growth
- `src/lisp/eval.c3` — eval_path V_MODULE case for dot-path access
- `src/lisp/parser.c3` — parse_import extended (selective/:all), parse_export_from new, dynamic arrays
- `src/lisp/compiler.c3` — E_EXPORT_FROM in all dispatch sites, compile_export_from_flat, dynamic CompiledModule
- `src/lisp/tests.c3` — Updated ~12 module tests, added ~18 new tests (qualified, selective, rename, :all, export-from, module-as-value)

### Test Count
- Before: 761 unified + 77 compiler + 368 E2E = 1206 total, 0 failures
- After: 771 unified + 77 compiler + 368 E2E = 1216 total, 0 failures

## 2026-02-23 (Session 25): Remove Auto-Currying + Add Placeholder/Pipe/Guard Syntax

### Phase 1: Strict Arity Enforcement
- **JIT arity guards**: `jit_apply_value_impl` and `jit_apply_value_tail` now return arity mismatch errors when a non-variadic multi-param closure is called with 1 arg (instead of silently binding only the first param)
- **`format_arity_error` helper**: Formats "arity mismatch: expected N args, got M" messages
- **Removed compiler re-currying**: Deleted `scan_lambdas_with_scope` block that converted multi-param lambdas to nested single-param; multi-param closures now kept intact through compilation
- **Compiler multi-param invoke**: `emit_lambda_definitions` emits arg-list-unpacking code for multi-param non-variadic closures (receive cons list, unpack with `rt_car1/rt_cdr1`)
- **`prim_apply` updated**: Uses `jit_apply_multi_args` instead of one-at-a-time loop
- **`jit_apply_multi_args` refined**: Strict arity only for `param_count > 1`; single-param closures fall through to one-at-a-time loop (preserving curried function chains)

### Phase 2: `_` Placeholder Desugaring
- **Parse-time desugaring**: `_` in expression context (T_UNDERSCORE) produces E_VAR with `sym_placeholder` sentinel
- **Lambda wrapping in `parse_application`**: After collecting call args, scans for placeholder sentinels, generates `__pN` param names via `gensym_counter`, replaces in-place, wraps in nested single-param lambdas
- **Multiple `_` support**: `(f _ 2 _)` → `(lambda (__p1) (lambda (__p2) (f __p1 2 __p2)))` — returns curried function
- Added `sym_placeholder` to Interp struct

### Phase 3: `|>` Pipe Operator
- **Parse-time desugaring**: `(|> val step1 step2)` folds left-to-right, appending piped value as last arg to E_CALL nodes or wrapping bare functions
- **`parse_pipe` function**: ~45 lines, handles E_CALL steps (append arg) and bare symbol steps (wrap in call)
- Added `sym_pipe` to Interp struct

### Phase 4: Guard Patterns `(? pred)`
- **PAT_GUARD pattern type**: New enum value + `guard_pred`/`guard_sub` fields in Pattern union
- **Parser**: Detects `(? pred)` or `(? pred sub-pattern)` in `parse_pattern`
- **Evaluator**: `match_pattern` PAT_GUARD case evaluates predicate via `jit_eval` + `jit_apply_value`, with sub-pattern bindings in scope
- **`match_env` on Interp**: Set in `jit_do_match` so guard predicates can resolve free variables
- **Compiler support**: `collect_pattern_bindings`, `compile_pattern_check`, `compile_pattern_bindings` handle PAT_GUARD
- Added `sym_question` to Interp struct

### Phase 5: `partial` in Stdlib
- Added `(define (partial f .. initial-args) (lambda (.. new-args) (apply f ((append initial-args) new-args))))` to stdlib.lisp

### Files Modified
- `src/lisp/jit.c3` — `format_arity_error`, arity guards in apply_impl/apply_tail, refined multi_args strict arity, `match_env` set
- `src/lisp/parser.c3` — `_` placeholder in parse_expr, placeholder wrapping in parse_application, pipe dispatch + parse_pipe, guard in parse_pattern
- `src/lisp/compiler.c3` — Removed re-currying, multi-param invoke, `needs_arg_list` helper, PAT_GUARD in pattern compiler
- `src/lisp/value.c3` — PAT_GUARD enum + pattern fields, sym_placeholder/sym_pipe/sym_question/match_env on Interp, symbol inits
- `src/lisp/eval.c3` — PAT_GUARD case in match_pattern
- `src/lisp/primitives.c3` — `prim_apply` uses `jit_apply_multi_args`
- `src/lisp/tests.c3` — Updated "multi-param" test, added 18 new tests (arity, placeholder, pipe, guard, partial)
- `stdlib/stdlib.lisp` — Added `partial` function

### Tests
- Before: 1192 total (743 unified + 77 compiler + 367 e2e + 5 misc), 0 failures
- After: 1206 total (761 unified + 77 compiler + 368 e2e), 0 failures

## 2026-02-23 (Session 24): Remove Hard-Coded Limits — Dynamic Data Structures

### Phase 1: Tier 1 Crash/Corrupt Fixes
- **1A: SymbolTable dynamic** — `char[128] name` → `char*` (heap-allocated per symbol), fixed arrays → pointer + capacity with init/destroy/grow. Grows at 70% load. Removed MAX_SYMBOL_LEN, MAX_SYMBOLS, HASH_TABLE_SIZE.
- **1B: MatchResult dynamic** — `Binding[512]` → `Binding*` + capacity. match_ok() mallocs, match_fail() zero-cost (null). Added cleanup() method with calls at all return points.
- **1C: RT_MAX_PRIOR bumped** — `RT_MAX_PRIOR` 16→256 in runtime.c3 (pragmatic: tlocal can't easily be dynamic).

### Phase 2: Tier 2 Silent Data Loss Fixes
- **2A: MethodTable entries dynamic** — `MethodEntry[64]` → `MethodEntry*` + capacity. Init=8, grows in jit_eval_define. Free in destroy_value.
- **2B: StringVal.chars dynamic** — `char[4096]` → `char*` + capacity. Added strval_new/strval_ensure/strval_push helpers. Updated all 7+ primitives, parser, print_value. Removed MAX_STRING_LEN.
- **2C: LambdaDef captures dynamic** — `SymbolId[16]` → `SymbolId*` + capacity for captures and params in compiler.c3.

### Phase 3A: Bump Fixed AST/Parser Limits
- Increased 20+ constants: ExprLambda/ExprCall/ExprBegin/ExprModule arrays 64→256, Closure.params 64→256, Module.exports 128→256, MAX_MATCH_CLAUSES 32→128, MAX_EFFECT_CLAUSES 16→64, MAX_PATTERN_ELEMS 16→64, MAX_TYPE_FIELDS 16→64, MAX_TYPE_PARAMS 8→32, MAX_PATH_SEGMENTS 8→32, MacroDef/ExprDefineMacro clauses 8→32, CapturedBinding 32→64, UnionVariant 16→64, MethodSignature arrays 8→32, GensymMapping 16→64.

### Phase 3B: Dynamic Interpreter Tables
- **TypeRegistry dynamic** — `TypeInfo[128]` → `TypeInfo*` + capacity with hash table. Added grow() (doubles + rebuilds hash), destroy(). Auto-grows instead of returning INVALID_TYPE_ID.
- **MacroDef table dynamic** — `MacroDef[64]` → `MacroDef*` + capacity with hash table. Added Interp.grow_macro_table() with hash rebuild.
- **Module table dynamic** — `Module[32]` → `Module*` + capacity with hash table. Added Interp.grow_module_table() with hash rebuild.
- **Handler stack dynamic** — `EffectHandler[16]` → `EffectHandler*` + capacity. Added Interp.grow_handler_stack().
- **Reset stacks dynamic** — `Expr*[16]`/`Env*[16]` → pointer + capacity. Added Interp.grow_reset_stacks().
- **Prior results dynamic** — `Value*[16]` → `Value**` + capacity for both effect and shift. Added ensure_effect_prior/ensure_shift_prior.
- **CapturedCont prior_results dynamic** — `Value*[16]` → `Value**` + capacity. Malloc'd at capture time.
- Added Interp.destroy() to free all dynamic arrays.
- Removed MAX_MACROS, MAX_MODULES, MACRO_HASH_SIZE, MODULE_HASH_SIZE, MAX_TYPES, TYPE_HASH_SIZE constants.

### Phase 3C: Low-Priority Remaining Tables
- Primitive.name 32→64, FfiHandle.sym_cache 32→64, CompiledModule 32→64
- RT_MAX_HANDLERS 16→64, RT_MAX_CLAUSES 16→64, RT_VAR_TABLE_SIZE 256→512

### Files Modified
- `src/lisp/value.c3` — SymbolTable, TypeRegistry, MethodTable, StringVal, CapturedCont, Interp struct + init/grow/destroy
- `src/lisp/eval.c3` — MatchResult, destroy_value updates, GensymTable bump
- `src/lisp/jit.c3` — Handler/reset/prior dynamic growth, CapturedCont malloc, module hash updates
- `src/lisp/macros.c3` — Macro/module hash capacity updates, eval_define_macro growth
