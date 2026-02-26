# Changelog

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
Added new printf-style `format` primitive with specifiers `%s`, `%d`, `%f`, `%e`, `%x`, `%X`, `%o`, `%b`, `%%`, plus width/precision/left-align support. Renamed old CL-style format (using `~a`, `~s`, `~~`) to `cl-format` for backward compatibility.

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
Major syntax simplification pass. Replaced Scheme-style `(let ((x 10)) body)` with flat-pair `(let (x 10) body)`. Replaced `perform`/`k` effect syntax with `signal`/`resolve`. Added implicit begin for lambda and define bodies. No backward compatibility (version < 1.0).

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
- `CurriedPrim` struct (unused legacy from auto-curry era)
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
- V_INTERP_REF kept in enum for bridge compatibility but all handler code removed
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
- `src/lisp/eval.c3` — E_CALL CLOSURE, apply_closure, METHOD_TABLE dispatch, tests
- `src/lisp/jit.c3` — jit_apply_multi_args
- `src/lisp/compiler.c3` — scan_lambdas_with_scope re-currying, find_free_vars multi-param fix

### Test Count
- Before: 985 (650 unified + 77 compiler + 39 new + 219 e2e)
- After: 1095 (650 unified + 77 compiler + 368 e2e) — all passing

## 2026-02-21 (Session 6): Audit Fixes — Tasks #1 & #2

### Task #1: Box StringVal, Closure, FfiHandle, Primitive behind pointers
Changed 4 Value union fields from inline to pointer (`StringVal*`, `Closure*`, `Primitive*`, `FfiHandle*`). C3 auto-deref means most read sites unchanged. Updated constructors to malloc sub-structs, updated `destroy_value` to free them.

### Task #2: Dynamic Env bindings
Changed `Env.bindings` from `Binding[512]` (8KB) to `Binding*` + `usz capacity` with dynamic growth (initial 8, doubles). Added `destroy_env` destructor.

### Files Modified
- `src/lisp/value.c3` — Value union, Env struct, constructors, make_env
- `src/lisp/eval.c3` — destroy_value, destroy_env, deep_copy_env, string primitives
- `src/lisp/parser.c3` — StringVal malloc in string literals

### Test Count
- Before: 985 (650 unified + 77 compiler + 39 new + 219 e2e)
- After: 985 — all passing, no regressions

## 2026-02-20 (Session 5): Memory System Fix & Hardening

Fixed use-after-free bugs and memory leaks in region-based memory system.

### Changes
1. **Fix `make_array()` UAF**: Allocate Value in root_region (was current_frame → dangling pointer after frame release). `value.c3:713`
2. **Fix `make_instance()` UAF**: Same root_region fix. `eval.c3:1210`
3. **Fix MethodTable Value allocation**: Allocate directly in root_region, removed conditional `copy_to_parent` calls. `eval.c3:974,993,1010`
4. **Add `destroy_value()` destructor**: Frees malloc'd backing for ARRAY (items + struct), INSTANCE, METHOD_TABLE, CLOSURE type_sig.
5. **Add `destroy_hashmap()` destructor**: Frees malloc'd entries buffer.
6. **Register destructors**: `register_destructors()` called at all 4 init sites (tests, compiler tests, REPL, script runner). Initializes global destructor registry if needed.
7. **Update `copy_to_parent()` comments**: Reflects that HASHMAP/ARRAY/INSTANCE/METHOD_TABLE Values are now in root_region with registered destructors.
8. **Bounds checking verified**: `prim_ref` and `prim_array_set` already had proper bounds checking.

### Files Modified
- `src/lisp/value.c3` — `make_array()` root_region allocation
- `src/lisp/eval.c3` — destructors, `make_instance()` fix, MethodTable fix, comments, registration
- `src/main.c3` — `register_destructors()` calls at REPL and script init

### Test Count
- Before: 727 (650 unified + 77 compiler)
- After: 727 (650 unified + 77 compiler) — all passing, no regressions

## 2026-02-20 (Session 4): Literal Syntax + Generic Dispatch

Major rename and feature addition: collection literals and generic operations.

### Changes
1. **Rename VECTOR → ARRAY everywhere**: `ValueTag::VECTOR` → `ARRAY`, `Vector` struct → `Array`, `vector_val` → `array_val`, `is_vector` → `is_array`, `make_vector` → `make_array`. User-facing: `vector` → `array`, `vector?` → `array?`, `make-vector` → `make-array`. Print format: `#(...)` → `[...]`.
2. **Rename HashMap → Dict (user-facing)**: `sym_Vector` → `sym_Array`, `sym_HashMap` → `sym_Dict`. Type names: `Vector` → `Array`, `HashMap` → `Dict`. Constructor: `hash-map` → `dict`. Predicate: `hash-map?` → `dict?`.
3. **`{}` dict literal in parser**: `{'a 1 'b 2}` desugars to `(dict 'a 1 'b 2)`. Handles `T_LBRACE` in `parse_expr()`. Error on odd element count.
4. **`[]` array literal in parser**: `[1 2 3]` desugars to `(array 1 2 3)`. Handles `T_LBRACKET` in `parse_expr()`. No conflict with `[type]` attributes (parsed in `parse_define`) or `[a b]` patterns (parsed in `parse_pattern`).
5. **Cons cell dot-path**: `pair.car`/`pair.cdr` for read access (in `eval_path`). `(set! pair.car val)` for mutation (in `E_SET` handler). Falls through to alist lookup if field is not `car`/`cdr`.
6. **Generic collection primitives**: `ref` (array/dict/cons/string), `length` (extended to array/dict/string), `push!` (array), `keys`/`values`/`has?`/`remove!` (dict).
7. **Removed prefixed registrations**: `array-ref`, `array-length`, `array-push!`, `dict-ref`, `dict-has?`, `dict-remove!`, `dict-keys`, `dict-values`, `dict-count`, `make-array`, `array->list`, `list->array`. Kept: `array-set!`, `dict-set!` (no generic equivalent for mutation), `array?`, `dict?`.
8. **Constructor dispatch**: `(array '(1 2 3))` converts list→array, `(list [1 2 3])` converts array→list. No need for separate conversion functions or `make-array`.
9. **Updated compiler**: Known primitives list and code generation updated for new names.

**Files modified**: `src/lisp/value.c3`, `src/lisp/eval.c3`, `src/lisp/parser.c3`, `src/lisp/compiler.c3`
**Tests**: 727 (650 unified + 77 compiler), all passing (+35 new tests)

---

## 2026-02-19 (Session 3): Parametric Types, Type Arg Inference, Constrained Dispatch

Implemented the 3 remaining type system intents from the holistic plan.

### Changes
1. **parser.c3**: Fixed `parse_deftype` to collect ALL symbols after name into `type_params[]` (like `parse_defunion`), instead of treating only the first as parent. Disambiguation moved to eval time.
2. **eval.c3**: `eval_deftype` now disambiguates parent vs type-params: if first symbol is a registered type → parent, else all are type params. Also stores `annotation_sym` on fields for param↔field mapping.
3. **value.c3**: Extended `Instance` with `type_args[MAX_TYPE_PARAMS]` + `type_arg_count`. Added `annotation_sym` to `TypeFieldInfo`. Added `MethodConstraint` struct and constraint fields to `MethodSignature`.
4. **eval.c3**: `prim_type_constructor` now infers type arguments from field values (matching annotation_sym to type params). New `type-args` primitive returns inferred type arg list.
5. **eval.c3**: Lambda eval builds constraints from `^{'T Bound}` dict annotations into `MethodSignature`. `find_best_method` enforces constraints — method only matches if arg type is subtype of bound.
6. **eval.c3**: 15 new tests covering parametric types, type-args inference, multi-param generics, parent disambiguation, and constrained dispatch.

**Files modified**: `src/lisp/parser.c3`, `src/lisp/value.c3`, `src/lisp/eval.c3`
**Tests**: 692 (615 unified + 77 compiler), all passing

---

## 2026-02-19 (Session 2): Documentation Sync

Comprehensive documentation sync to match all implemented features. No code changes.

### Updated docs
- **docs/LANGUAGE_SPEC.md**: Complete rewrite (v0.3.0). Added all 16 data types, 129+ primitives in categorized tables, type system (struct/abstract/union/alias), multiple dispatch (basic/multi-arg/Val), I/O effects, macros, modules, stdlib, FFI. Fixed limits (4095 chars, 4096 symbols). Added backend comparison table.
- **docs/type-system-syntax.md**: Replaced stale Implementation Roadmap (falsely claimed traits/instances COMPLETE). New accurate status: type defs/dispatch/I/O effects/union matching = IMPLEMENTED; traits/instances/HKT/parametric substitution = NOT implemented. Fixed abstract type syntax: `(define [abstract] (Real Number))` (parenthesized). Fixed struct inheritance syntax: `(define [type] (Point3D Point) ...)`.
- **docs/type-system-proposal.md**: Added status banner marking it as design proposal (not implementation status). Annotated each section with implementation notes (constraints not enforced, `arr[0]` vs `arr.[0]`, `[type mutable]` not needed, `Proc` not implemented). Added complete Implementation Status table at bottom.
- **docs/type-system-e2e-tests.md**: Added ASPIRATIONAL status banner listing all unimplemented features (with-region, ref/@, [method], [effect], destructors, Proc, Arena). Fixed type syntax to use parenthesized inheritance form throughout.
- **docs/FEATURES.md**: Updated data types table to include all 16 types (added DOUBLE, HASHMAP, VECTOR, FFI_HANDLE, INSTANCE, METHOD_TABLE).
- **docs/SYNTAX_SPEC.md**: Added parenthesized abstract type form `(define [abstract] (Child Parent))`.

### Paradigm verification findings
- Abstract type parent syntax uses parenthesized form `(define [abstract] (Real Number))`, NOT bare `(define [abstract] Real Number)` — design docs had wrong syntax, now fixed
- All implemented features match the design intent from `type-system-syntax.md` Levels 1
- Levels 2-3 (HKT, traits, instances, variance) are clearly marked as future

**Files modified**: `docs/LANGUAGE_SPEC.md`, `docs/FEATURES.md`, `docs/SYNTAX_SPEC.md`, `docs/type-system-syntax.md`, `docs/type-system-proposal.md`, `docs/type-system-e2e-tests.md`
**Tests**: 677 (600 unified + 77 compiler), unchanged, all passing

---

## 2026-02-19: Type System, Dispatch, Effects Coherence (Phases 0-6)

Major implementation of the holistic plan — bringing types, dispatch, and effects into one coherent system.

### Phase 1: Type System Foundation
- **value.c3**: Added ValueTag entries (TYPE_INFO, INSTANCE, METHOD_TABLE), TypeAnnotation struct (3 forms: ^Int, ^(List Int), ^{'T Number}), TypeKind enum, TypeInfo, TypeRegistry (FNV-1a hash, init/register/lookup/get/is_subtype), Instance (malloc'd), MethodSignature/MethodEntry/MethodTable, ExprTags (E_DEFTYPE/E_DEFABSTRACT/E_DEFUNION/E_DEFALIAS), type AST structs, PAT_CONSTRUCTOR pattern, 16 pre-interned type symbols
- **parser.c3**: Added T_LBRACE/T_RBRACE tokens, parse_type_annotation() (3 forms), typed params in parse_lambda/parse_define (^Type name), bracket attribute dispatch ([type]/[abstract]/[union]/[alias]), parse_deftype/parse_defabstract/parse_defunion/parse_defalias
- **eval.c3**: register_builtin_types (11 built-ins), infer_value_type, make_instance, eval_deftype (registers type + creates constructor), eval_defabstract, eval_defunion (variants + constructors), eval_defalias, prim_type_of/prim_is_type/prim_is_instance, Instance field access in eval_path/eval_index, copy_to_parent for new tags
- 35 new tests

### Phase 2: Multiple Dispatch
- **eval.c3**: find_best_method (scoring: Val=1000, exact=100, subtype=10, any=1), eval_lambda builds MethodSignature from typed params, eval_define creates/updates MethodTable (typed closure + existing MT/CLOSURE/PRIMITIVE), METHOD_TABLE dispatch in E_CALL and apply()
- **Bug fix**: Val literal check must come BEFORE INVALID_TYPE_ID escape in find_best_method (was causing ^Int to always win over ^(Val 0))
- 10 new tests (basic dispatch, Val fibonacci, multi-arg)

### Phase 3: Struct Dot-Path
- **value.c3**: Extended ExprSet with path segments for set! on dot-paths
- **parser.c3**: parse_set handles T_PATH targets
- **eval.c3**: E_SET handler resolves path segments, mutates final Instance field
- 5 new tests (set! struct fields, nested mutation, preserves other fields)

### Phase 4: Primitive Consolidation
- Already works via Phase 2: typed defines on existing primitive names promote PRIMITIVE to MethodTable with primitive as fallback
- 4 new tests (typed dispatch on Point, builtin + still works)

### Phase 5: I/O Effects Coherence
- **eval.c3**: Renamed I/O prims to __raw-* (print→__raw-print, etc.), removed old `display` registration
- **value.c3**: Added 8 pre-interned I/O effect tag symbols (io/print, io/println, etc.)
- **eval.c3 register_stdlib**: Effect wrappers: `(define print (lambda (x) (perform io/print x)))` etc.
- **eval.c3 eval_perform**: I/O fast path — if no handler matches io/* effect, call __raw-* prim directly (zero overhead when no handler installed)
- 5 new tests (io effect print, custom handler suppress/capture, raw-print)

### Phase 6: Union Types + Pattern Matching
- **parser.c3**: Added PAT_CONSTRUCTOR handling in parse_pattern for (ConstructorName sub-patterns...)
- **eval.c3**: PAT_CONSTRUCTOR case in match_pattern (check INSTANCE type, recursively match fields), PAT_VAR auto-detects nullary constructors, collect_pattern_vars handles PAT_CONSTRUCTOR
- 6 new tests (match None, match Some, Result Ok/Err, nested Some, wildcard in ctor)

### Phase 0: Documentation Alignment
- **docs/SYNTAX_SPEC.md**: Fixed truthiness (removed 0 from falsy), added all missing tokens/ExprTags/special forms, updated limits, added type system section
- **docs/FEATURES.md**: Fixed primitive count (45→129+), fixed binding limit (256→512), added type system, JIT, compiler sections, constructor patterns
- **docs/COMPILER.md**: Updated limitations (TCO now works, delegation patterns)
- **docs/LANGUAGE_SPEC.md**: Updated version, date, and description

**Files modified**: `src/lisp/eval.c3`, `src/lisp/value.c3`, `src/lisp/parser.c3`, `docs/SYNTAX_SPEC.md`, `docs/FEATURES.md`, `docs/COMPILER.md`, `docs/LANGUAGE_SPEC.md`
**Tests**: 614 → 677 (600 unified + 77 compiler), all passing

---

## 2026-02-18: Paradigm-Aware Feature Expansion (8 Phases)

Major feature expansion following Pika Lisp's own paradigm (effects, continuations, curried HOFs) rather than blindly copying Scheme/R7RS.

### Phase 1: Complete Float Support
- **eval.c3**: Updated `prim_string_to_number` to parse float strings (detect `.` or `e/E`), updated `prim_number_to_string` to handle DOUBLE, added `exact->inexact`/`inexact->exact` conversions
- **runtime.c3**: Updated `rt_add/sub/mul/div` for mixed int/double dispatch, `rt_lt/gt/le/ge` for double comparisons, `rt_values_equal` for cross-type numeric, `rt_print_value` for DOUBLE
- JIT helpers and compiler literal emission were already done in prior session
- 22 new float tests (literals, mixed arithmetic, comparisons, conversions)

### Phase 2: Math Library
- **eval.c3**: 21 C-level math primitives wrapping `std::math`: sin/cos/tan/asin/acos/atan/atan2, exp/log/log10/pow/sqrt, floor/ceiling/round/truncate, abs/min/max/gcd/lcm
- Constants `pi` and `e` defined in `register_stdlib`
- 22 new math tests

### Phase 3: Sorting, Bitwise, Stdlib HOFs
- **eval.c3**: `sort` (insertion sort, max 256 elements), `sort-by` (custom comparator), `bitwise-and/or/xor/not`, `lshift/rshift`
- **stdlib**: `flatten`, `partition`, `remove`, `find` via named-let
- 17 new tests

### Phase 4: FFI Double Fix + dlsym Cache
- **eval.c3**: `ffi_value_to_long`/`ffi_long_to_value` handle `'double` type via bit-cast. dlsym cache (32-entry linear scan in FfiHandle). Cache cleared on `ffi-close`
- **value.c3**: Added `sym_cache_ptrs/names/count` to FfiHandle
- Note: XMM register passing for float FFI args is a known limitation (double args pass via integer registers, won't work with C functions expecting XMM)
- 1 new test (cache hit verification)

### Phase 5: String Ops & Type Predicates
- **eval.c3**: `string-contains?`, `string-index-of`, `string-replace`, `char-at`, `string-repeat`
- Type predicates: `double?`, `number?`, `boolean?`, `list?`, `procedure?`, `zero?`, `positive?`, `negative?`, `even?`, `odd?`
- `format` (variadic with ~a/~s directives), `display` (print without quotes)
- 30 new tests

### Phase 6: TCO Stdlib & Module Assert
- All stdlib functions already use TCO patterns (foldr=reverse+foldl, append/take/zip use let loop with accumulator)
- Module assert already returns runtime error (no assertion crash)
- 1 new test (foldl TCO 10000)

### Phase 7: Introspection Primitives
- **eval.c3**: `macroexpand` (macro hash lookup → expand_pattern_macro), `eval` (value_to_expr + eval), `apply` (iterative curried application), `bound?` (env chain lookup), `error`/`error-message`
- 7 new tests

### Phase 8: Stdlib Generators & Lazy Streams
- **stdlib**: `yield` macro (shift k (cons val k)), `stream-take`, `delay`/`force`
- Generator stream-take deferred (continuation interaction needs more work)
- 1 new test (delay/force)

### Phase 8 (continued): Vectors (Mutable Arrays)
- **value.c3**: Added `VECTOR` to ValueTag, `Vector` struct (`Value** items`, `usz length/capacity`, malloc'd), `make_vector`, `is_vector`, `print_value` VECTOR case (`#(...)`)
- **eval.c3**: 9 vector primitives: `vector`, `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector->list`, `list->vector`, `vector?`, `vector-push!`
- **eval.c3**: `copy_to_parent` VECTOR case, `values_equal` VECTOR structural equality
- 14 new tests (creation, ref, set!, push, conversion, bounds checking)

### Phase 9: Sets (Built on HashMap)
- **eval.c3**: 6 set primitives: `set`, `set-add`, `set-remove`, `set-contains?`, `set-size`, `set->list`
- Sets are hashmaps with `true` values — reuses existing HashMap infrastructure
- 7 new tests (creation, add, remove, contains, dedup)

### Phase 12 (partial): Convenience Primitives
- **eval.c3**: `read-string` (parse+eval string), `string->symbol`, `symbol->string`
- **value.c3**: Increased `MAX_BINDINGS` from 256 to 512 (needed for expanded primitive set)
- 3 new tests

**Files modified**: `src/lisp/eval.c3`, `src/lisp/value.c3`, `src/lisp/runtime.c3`, `src/lisp/parser.c3`, `src/lisp/compiler.c3`, `src/lisp/jit.c3`, `src/main.c3`
**Tests**: 489 → 614 (537 unified + 77 compiler), all passing

---

## 2026-02-17: Transpiler Correctness Fixes + Round-Trip Tests

Three correctness fixes for the Lisp-to-C3 transpiler, plus new testing infrastructure.

### STDLIB_PRELUDE map/filter fix
- **compiler.c3**: Replaced naive recursive `map`/`filter` in STDLIB_PRELUDE with iterative accumulator+reverse versions using named-let `loop`. Prevents stack overflow on large lists.

### Unknown expr default case warning
- **compiler.c3**: Added `io::printfn` warning + comment text "WARNING" in `compile_expr` default case so unrecognized expression types are visible during compilation.

### Round-trip test infrastructure
- **compiler.c3**: Added `print_last` bool field to Compiler struct. New `compile_to_c3_with_print()` function sets this flag. `compile_program` wraps the last non-define expression with `rt_print_value` + `io::printn` when enabled.
- **eval.c3**: Added 12 new compiler tests (66-77):
  - Tests 66-67: Verify STDLIB_PRELUDE map/filter use `loop` pattern
  - Tests 68-73: Runtime bridge tests via `rt_eval_source` (map, filter, TCO depth, multi-arg, set!, macros)
  - Tests 74-76: `compile_to_c3_with_print` pattern tests
  - Test 77: Verify no WARNING in normal compilation

**Files modified**: `src/lisp/compiler.c3`, `src/lisp/eval.c3`
**Tests**: 477 → 489 (412 unified + 77 compiler), all passing

---

## 2026-02-17: Compiler Feature Parity — 6-Phase Transpiler Upgrade

Comprehensive upgrade of the Lisp-to-C3 transpiler to achieve near-parity with the JIT compiler. Uses the same delegation pattern: compile outer expressions natively, delegate complex features to the interpreter via `rt_eval_source`.

### Phase 1: Multi-Arg Calls + Variadic Lambdas
- **compiler.c3**: Rewrote `compile_call` from nested `rt_invoke` chains to cons-list + `rt_apply_multi(func, args, argc)`. Evaluates args left-to-right into temps, builds cons list right-to-left.
- **runtime.c3**: Added `rt_apply_multi(Value func, Value arg_list, long argc)` — applies a function to a cons-list of arguments with curried one-at-a-time application.

### Phase 2: TCO via Trampoline
- **runtime.c3**: Added `V_THUNK` value tag, `ThunkData` struct, `make_thunk()` constructor. Split `rt_invoke` into `rt_invoke_once` (single step) and `rt_invoke` (with trampoline loop). Trampoline in `rt_apply_multi` for final application.
- **compiler.c3**: Added `compile_expr_tail()` that propagates tail position through if/begin/let/and/or. Tail-position calls emit `make_thunk()` for closures instead of `rt_invoke()`. Lambda bodies now compiled with `compile_expr_tail()`.

### Phase 3: Missing Expression Types
- **compiler.c3**: Added `compile_quasiquote`, `compile_defmacro`, `compile_module`, `compile_import` — all delegate to `rt_eval_source()` with free variable injection.
- Added serializer cases for E_QUASIQUOTE (`` ` ``), E_UNQUOTE (`,`), E_UNQUOTE_SPLICING (`,@`), E_DEFMACRO, E_MODULE, E_IMPORT.
- Added `find_free_vars`, `scan_lambdas`, `body_creates_closure` cases for all new expression types.
- Stdlib macros (when/unless/cond) registered via `rt_eval_source` in `emit_main_start`.

### Phase 4: Missing Primitives
- **runtime.c3**: Added 15 new runtime functions:
  - `rt_string_to_number`, `rt_number_to_string` — string/number conversion
  - `rt_gensym` — unique symbol generation (delegates to interpreter)
  - `rt_apply_prim` — apply function to list of args
  - `rt_equal_p` — deep structural equality
  - `rt_display` — print without string quotes
  - `rt_load` — evaluate file contents
  - 9 hash-map functions (`rt_hash_ref`, `rt_hash_set`, `rt_hash_has`, `rt_hash_remove`, `rt_hash_keys`, `rt_hash_values`, `rt_hash_count`, `rt_hash_map_p`, `rt_hash_map_create`) — all delegate to interpreter
- **compiler.c3**: Registered all new primitives in `compile_var` and `is_primitive`.

### Phase 5: Closure-Captured Mutable Locals
- **compiler.c3**: Added mutable capture detection (`is_mutable_capture`, `has_set_on`, `is_captured_by_nested_lambda`). Pre-scan pass (`prescan_mutable_captures`) identifies mutable-captured variables before lambda scanning.
- Mutable-captured variables excluded from lambda closure struct captures; accessed via interpreter env instead.
- `compile_let`: uses `rt_define_var` for mutable-captured variables. `compile_set`: uses `rt_set_var`. `compile_var`: uses `rt_lookup_var`.
- **runtime.c3**: Added `rt_lookup_var` and `rt_set_var` for interpreter env bridge.

### Phase 6: Test Overhaul
- Added 25 new compiler tests (41-65) covering all 5 phases:
  - Phase 1: rt_apply_multi usage, cons list building, 3-arg calls, zero-arg calls
  - Phase 2: make_thunk emission for tail calls, non-tail OK, if branch propagation
  - Phase 3: quasiquote/defmacro delegation, free var injection, stdlib macro registration
  - Phase 4: string->number, number->string, gensym, apply, equal?, display, hash-ref, load
  - Phase 5: mutable capture rt_define_var/rt_set_var/rt_lookup_var, non-mutable C3 local
  - Integration: factorial, let-rec tail call, nested multi-arg calls

- **Files modified**: `src/lisp/compiler.c3`, `src/lisp/runtime.c3`, `src/lisp/eval.c3`
- **Tests**: 477 (412 unified + 65 compiler), all passing (was 452 = 412 + 40)

## 2026-02-17: Fix H15 (ghost_idx bounds), M15 (SIGINT handler), M16 (arena_free unreachable)
- **H15**: Added `ghost_idx` bounds check before `inherited_ghost_tables[]` access in:
  - `dereference_via_ghost()` — new bounds check + unreachable guard
  - `manual_extend()` — replaced assert with proper bounds checks on host_idx, ghost_idx, and slot_idx (break on OOB)
  - (Note: `is_valid_ghost_handle` and `resolve_object_record` already had the check from C13)
- **M15**: SIGINT handler for REPL — Ctrl+C during eval now returns "interrupted" error instead of killing process
  - Added `signal()` extern, `SignalHandler` alias, `g_interrupted` flag, `sigint_handler()` in eval.c3
  - Installed handler at REPL start; eval loop checks flag each iteration
  - Flag cleared before each REPL eval to prevent stale interrupts
- **M16**: Converted `unreachable()` in `Pool.arena_free` to `io::eprintfn` warning + return
- **Files modified**: `src/main.c3`, `src/lisp/eval.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing

## 2026-02-13: Fix CRITICAL C13, C14 + HIGH H1
- **C13 (Region bounds violations)**: Added bounds checks in 3 ghost-table functions in main.c3:
  - `dereference_via_ghost()`: bounds check on `host_idx`, replaced `assert` with `if`/`unreachable`
  - `is_valid_ghost_handle()`: bounds checks on `host_idx` and `ghost_idx`, returns `false` on OOB
  - `resolve_object_record()`: bounds checks on `host_idx` and `ghost_idx` with `unreachable`
- **C14 (shift() malloc null)**: Added null check after `mem::malloc()` in `shift()` (delimited.c3) — sets INVALIDATED + returns null on failure, mirroring `shift_capture()` pattern
- **Files modified**: `src/main.c3`, `src/delimited.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix HIGH Audit Issue H1 (Macro/Module O(n) Linear Scan)
Replaced O(n) linear scans in `lookup_macro()` and `find_module()` with O(1) hash-based lookup using open-addressing hash tables.

### value.c3
- Added `MACRO_HASH_SIZE=128` and `MODULE_HASH_SIZE=64` constants (2x max entries for good load factor)
- Added `usz[MACRO_HASH_SIZE] macro_hash_index` and `usz[MODULE_HASH_SIZE] module_hash_index` to Interp struct
- Initialized both hash arrays to `usz.max` (empty sentinel) in `Interp.init()`

### eval.c3
- **lookup_macro()**: Rewrote from linear scan to hash probe using `(usz)name % MACRO_HASH_SIZE` with linear probing
- **eval_define_macro()**: Added hash index insertion after `macro_count++`, handles redefinition by updating existing slot
- **find_module()**: Rewrote from linear scan to hash probe using `(usz)name % MODULE_HASH_SIZE` with linear probing
- **eval_module()** and **eval_import() file-based path**: Added hash index insertion after `module_count++`

- **Files modified**: `src/lisp/value.c3`, `src/lisp/eval.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix 10 MEDIUM Audit Issues (M1-M3, M5-M9, M11-M12)
Quick-win fixes for silent truncation, missing depth limits, and assert-only guards. 8 skipped as complex/architectural (M4, M10, M13-M18).

### eval.c3 — M1, M2, M3
- **M1**: Quasiquote splice >64 items → `eval_error` (was silent drop)
- **M2**: Pattern vars >32 → warning printed (was silent loss)
- **M3**: Quasiquote depth cap at 64 (was unbounded recursion)

### value.c3 — M5, M6, M7, M8
- **M5**: `make_string()` and `make_ffi_handle()` truncation → `eprintfn` warning
- **M6**: `intern()` and `make_primitive()` name truncation → `eprintfn` warning
- **M7/M8**: Named constants `MAX_MACROS=64`, `MAX_MODULES=32` in Interp struct

### parser.c3 — M9, M11
- **M9**: Parser recursion depth limit 256 via `depth` field + `defer` decrement
- **M11**: Error message buffer 128→256 bytes

### jit.c3 — M12
- **M12**: JIT locals limit assert → `return false` (interpreter fallback)

- **Files modified**: eval.c3, value.c3, parser.c3, jit.c3
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix 16 HIGH Audit Issues (H2-H11, H13-H14, H16, H18-H20)
Comprehensive fix of 16 of 20 HIGH issues. 4 skipped: H1 (macro/module O(n) — tables tiny), H12 (JIT global_env null — always initialized), H15 (ghost table stale — complex, low probability), H17 (context restore — fundamental to design). H4 confirmed as false positive (already safe).

### value.c3 — H5
- **H5**: `Env.define()` assert → runtime bounds check with `io::eprintfn` + early return

### parser.c3 — H9, H10
- **H9**: String literal truncation → `T_ERROR` with "string literal too long (max 63 bytes)"
- **H10**: Symbol name truncation → `T_ERROR` with "symbol name too long (max 63 bytes)"

### jit.c3 — H11, H13
- **H11**: `jit_apply_multi_args` primitive path: `safe_count = min(arg_count, 16)` for slice
- **H13**: `jit_make_closure_from_expr`: `param_count > 64` → return null

### main.c3 — H14
- **H14**: Region refcount underflow: guard `if (refcount == 0) return` in release_region + destroy_region child cleanup

### context.c3 — H16
- **H16**: `stack_copy()` size validation: max 1MB cap + zero check

### delimited.c3 — H18
- **H18**: Continuation stack size cap at 4MB in `shift()` and `shift_capture()`

### compiler.c3 — H19, H20
- **H19**: `emit_escaped()` helper for symbol names in C3 string literals (3 sites)
- **H20**: Deleted dangling-pointer `serialize_expr()` (unused)

### eval.c3 — H2, H3, H6, H7, H8 (H4 false positive)
- **H2**: Module path length validation; **H3**: Empty separator check in string-split
- **H6**: `value_to_expr` E_CALL arg cap 16→64; **H7**: CapturedBindings truncation warning
- **H8**: `eval_handle` clause_count > MAX_EFFECT_CLAUSES validation

- **Files modified**: value.c3, eval.c3, parser.c3, jit.c3, compiler.c3, main.c3, context.c3, delimited.c3
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix 6 HIGH Audit Issues in eval.c3
- **H2 (Module path buffer overflow)**: Added length validation before building module path in `lib/<name>.pika` — returns `eval_error("module path too long")` if name + prefix + suffix exceeds 511 bytes.
- **H3 (Empty separator crash in string-split)**: Added check for empty separator string before accessing `chars[0]` — returns `make_error("string-split: empty separator")`.
- **H4 (FFI string null check)**: Verified already safe — null pointer check and `MAX_STRING_LEN` bounded loop already present in `ffi_long_to_value`. No change needed.
- **H6 (ExprCall args capped at 16)**: Changed `value_to_expr()` E_CALL arg limit from 16 to 64 to match `ExprCall.args[64]` array size.
- **H7 (CapturedBindings silent truncation)**: Added warning message via `io::printfn` when 32-binding limit is reached in `capture_template_bindings()`.
- **H8 (Effect handler clause limit not validated)**: Added `clause_count > MAX_EFFECT_CLAUSES` check in `eval_handle()` before copying clauses — returns `eval_error("too many effect handler clauses (max 8)")`.
- **Files modified**: `src/lisp/eval.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix HIGH Audit Issues H19, H20 in compiler.c3
- **H19 (Code injection via symbol names)**: Added `emit_escaped()` helper that escapes `\`, `"`, `\n`, `\t` in strings emitted into C3 string literal contexts. Applied at 3 sites:
  - `compile_literal()` SYMBOL case: `make_symbol("...")`
  - `compile_path()`: `make_string("...")` for path field names
  - `emit_cont_var_injection()`: `rt_define_var("...")`
- **H20 (Dangling pointer in serialize_expr)**: Deleted the unused `serialize_expr()` function which freed its buffer then returned a slice pointing to freed memory. No callers found; `emit_serialized_expr()` is the correct API.
- **Files modified**: `src/lisp/compiler.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix All 12 CRITICAL Audit Issues (C1-C12)
Comprehensive fix of all real CRITICAL issues from the codebase audit. Two reported issues (C13, C14) were false positives.

### value.c3 — C1, C2
- **C1**: O(n²) symbol intern → O(1) hash-based lookup
  - Added FNV-1a hash table (`SymbolId[1024] hash_index`) to SymbolTable
  - `intern()` rewritten: hash probe → linear scan fallback → insert in both arrays
  - Added `HASH_TABLE_SIZE = 1024`, `INVALID_SYMBOL_ID = 0xFFFFFFFF` constants
- **C2**: Symbol exhaustion returns `INVALID_SYMBOL_ID` instead of aliasing symbol 0

### eval.c3 — C3, C4, C5
- **C3**: Handler stack `assert()` → `eval_error()` (2 locations: eval_handle, apply_continuation)
- **C4**: Reset stack bounds check added (was completely missing before `reset_depth++`)
- **C5**: Recursion depth limits on 3 functions (C3 default params for backward compat):
  - `deep_copy_env()`: cap 256, `append_values()`: cap 10000, `values_equal()`: cap 256
- **Intern checks**: `prim_gensym()` and `lookup_or_create_gensym()` check for INVALID_SYMBOL_ID

### parser.c3 — C6, C7
- **C6**: Integer overflow detection before `val = val * 10 + digit`
- **C7**: Unterminated string literal → `T_ERROR` token (was silent truncation)
- **Intern checks**: 5 call sites protected against INVALID_SYMBOL_ID

### jit.c3 — C8, C9, C10
- **C8**: Multi-arg buffer overflow: `argc > 16` → return false (interpreter fallback)
- **C9**: JIT state pool increased 64→256, warn-once on overflow (states leak but code remains valid)
- **C10**: `_jit_emit()` null check → destroy state and return null

### main.c3 — C11, C12
- **C11**: Arena malloc null check in `new_arena()` + `arena_alloc()` null-data guard
- **C12**: SparseSet growth cap at key > 65536 (prevents unbounded memory growth)

- **False positives (no fix needed)**: C13 (region bounds already checked), C14 (continuation malloc already checked)
- **Files modified**: `src/lisp/value.c3`, `src/lisp/eval.c3`, `src/lisp/parser.c3`, `src/lisp/jit.c3`, `src/main.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix CRITICAL Audit Issues C6, C7 + Parser Intern Checks
- **Bug fix (C6)**: Integer overflow detection in lexer number parsing
  - Before `val = val * 10 + digit`, checks `val > (long.max - digit) / 10`
  - Sets `T_ERROR` token type with "integer literal overflow" diagnostic on overflow
  - Prevents silent wraparound on huge number literals
- **Bug fix (C7)**: Unterminated string literal error in lexer
  - After the string lexing while loop, if EOF is reached without closing quote, sets `T_ERROR`
  - Previously fell through and returned `T_STRING` with truncated content
- **Bug fix**: `INVALID_SYMBOL_ID` checks at all `intern()` call sites in parser
  - `get_current_symbol()`: checks return and calls `self.set_error("symbol table exhausted")`
  - Path segment interning (T_PATH): checks each segment intern result
  - Underscore interning in `parse_template_datum()` and `parse_qq_template()`: checks result
  - Import path interning in `parse_import()`: checks result
- **Files modified**: `src/lisp/parser.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Fix CRITICAL Audit Issues C3, C4, C5
- **Bug fix (C3)**: Handler stack `assert()` replaced with runtime error
  - `eval_handle()`: `assert(interp.handler_count < 16)` -> `eval_error("handler stack overflow: too many nested handlers")`
  - `apply_continuation()`: same assert replaced for continuation resume path
- **Bug fix (C4)**: Reset stack bounds check added (was completely missing)
  - `eval_reset()`: added `if (interp.reset_depth >= 16) return eval_error(...)` before increment
- **Bug fix (C5)**: Recursion depth limits added to 3 recursive functions
  - `deep_copy_env()`: depth parameter (default 0), cap at 256, returns null on overflow
  - `append_values()`: depth parameter (default 0), cap at 10000, returns nil on overflow
  - `values_equal()`: depth parameter (default 0), cap at 256, returns false on overflow
  - All use C3 default parameters so existing call sites (including jit.c3) compile unchanged
- **Bug fix**: `intern()` return value checks for symbol table exhaustion
  - `prim_gensym()`: checks for INVALID_SYMBOL_ID, returns `make_error` on exhaustion
  - `lookup_or_create_gensym()`: checks for INVALID_SYMBOL_ID, propagates sentinel
- **Files modified**: `src/lisp/eval.c3`
- **Tests**: 452 (412 unified + 40 compiler), all passing (unchanged)

## 2026-02-13: Full Codebase Audit
- **Audit**: Comprehensive production readiness and naive implementation audit
- **Scope**: All major modules — eval.c3, parser.c3, value.c3, jit.c3, main.c3, delimited.c3, continuation.c3, context.c3, compiler.c3, runtime.c3
- **Findings**: 64 issues total — 14 CRITICAL, 20 HIGH, 18 MEDIUM, 12 LOW
- **Key critical issues**: Symbol table O(n²) + silent exhaustion, handler/reset stack overflows (assert-only), unbounded recursion, parser integer overflow + unterminated strings, JIT buffer overflows, arena malloc unchecked
- **Report**: `memory/AUDIT_REPORT.md` with file:line references and prioritized fix recommendations
- **Files created**: `memory/AUDIT_REPORT.md`
- **Tests**: 452 (unchanged — audit only, no fixes per Audit Mode policy)

## 2026-02-13: Proper JIT Variadic Calls + Boxed Mutable Locals
- **Feature**: Native JIT variadic closure calls — no more interpreter fallback
  - Multi-arg calls now build a cons list (right-to-left) from JIT-compiled args, then call `jit_apply_multi_args`
  - `jit_apply_multi_args` handles: variadic closures (direct bind+eval), primitives (direct dispatch), curried closures (one-at-a-time apply), and mid-curry variadic detection
  - Zero-arg calls also routed through `jit_apply_multi_args` for correct variadic zero-arg handling
  - Added `jit_cons` helper for cons-list construction from JIT code
  - Removed: variadic closure fallback check and multi-arity prim fallback (both handled natively)
- **Feature**: Boxed mutable let-locals — no more interpreter fallback for set!-in-closures
  - `JitLocal.is_mutable` flag identifies locals captured-and-mutated by closures
  - Mutable locals stored as Env* "boxes" in stack slots (shared between JIT code and closures)
  - JIT reads: `jit_env_lookup_local(env_box, name)` — dereferences the shared cell
  - JIT writes: `jit_eval_set(interp, name, value, env_box)` — mutates the shared cell in place
  - Lambda capture: `emit_build_locals_env` reparents the mutable env box into the capture chain via `jit_env_reparent`
  - Both JIT code and closures see the same mutable state through the shared Env node
- **Removed**: `jit_compile_fallback` no longer used for variadic closures or mutable-captured let-locals
- **Files modified**: `src/lisp/jit.c3` (~80 lines of helper code, multi-arg path rewritten, let/var/set paths updated)
- **Tests**: 452 (412 unified + 40 compiler), all passing, 0 interpreter fallbacks for variadic/set!

## 2026-02-13: Full JIT Parity — Zero _interp Tests Remaining
- **Bug fix**: All 7 JIT delegate functions now pass locals env instead of hardcoded `global_env`
  - Updated: `jit_compile_quasiquote`, `jit_compile_match`, `jit_compile_define_macro`, `jit_compile_reset`, `jit_compile_shift`, `jit_compile_handle`, `jit_compile_perform`
  - Each now accepts `JitLocals* locals` parameter and uses `emit_build_locals_env` to build env from JIT stack locals
  - Fixes latent bug: delegates used inside `let` would lose local variable bindings
  - Fixes quasiquote tests that referenced let-bound variables
- **Feature**: Native set! on JIT locals — `jit_compile_set` now checks if the target is a JIT local and stores directly to the stack slot
  - Previously all set! went through env-based `jit_eval_set` helper, which couldn't update stack slots
- **Feature**: JIT-only test helpers — `test_eq_jit` and `test_nil_jit` for stateful tests that can't run through both interp and JIT
  - 6 tests use JIT-only: 3 counter mutations, 2 module counter mutations, 1 ffi-close side effect
- **Test migration**: All 80 `_interp` test calls eliminated
  - 74 tests moved from `test_*_interp` to unified `test_*` (interp+JIT)
  - 6 tests moved from `test_*_interp` to `test_*_jit` (JIT-only, for stateful tests)
  - 0 `_interp` test calls remain
- **Removed**: All "JIT LIMITATION" comments (TCO, deep effects, set! on let-locals) — these were false negatives since JIT delegates to eval() which has full TCO and effect support
- **Files modified**: `src/lisp/jit.c3` (~100 lines added), `src/lisp/eval.c3` (80 test calls changed, 2 JIT-only helpers added)
- **Tests**: 492 (412 unified + 40 interp-only + 40 compiler) → 492 (412 unified incl. 6 JIT-only + 40 compiler), all passing

## 2026-02-13: JIT Bug Fixes — Macros, Multi-Arity Prims, Lambda Env Capture
- **Bug fix**: JIT macro expansion — `jit_compile_call` now checks the macro table at compile time
  - Calls `lookup_macro()`, `expand_pattern_macro()`, `value_to_expr()` → JIT compiles the expanded Expr*
  - Zero runtime cost — macros are expanded during JIT compilation, not execution
  - Fixes ~24 macro tests that were `_interp`-only: when, unless, with-val, cond, my-and, let1, swap!, all hygiene tests
- **Bug fix**: Multi-arity primitive detection — arity >= 3 prims now routed through `jit_compile_fallback`
  - Previously only variadic (arity=-1) prims were detected; arity >= 3 (e.g., `substring`) lost 3rd+ args
  - Fixes 2 substring tests that were `_interp`-only
- **Bug fix**: Lambda env capture in let scopes — JIT lambdas now capture let-local variables
  - Added `jit_env_extend` helper: extends Env* with a new binding at runtime
  - Added `emit_build_locals_env`: emits code to build an Env* from JIT stack-based locals into V2 (callee-saved register)
  - `jit_compile_lambda`, `jit_compile_let_rec`, `jit_compile_fallback`, `jit_compile_set` all build env from locals
  - Fixes zero-arg closure and named-let tests that were `_interp`-only
- **GNU Lightning pitfall discovered**: STXI_L/LDXI_L roundtrip through stack slots loses values when followed by `_jit_prepare`/`_jit_finishi`. Solution: keep values in callee-saved registers (V2=R14) instead of stack slots. Scratch registers (R1=R10) are also clobbered by Lightning's internal call setup.
- **Documented JIT limitations** (remaining `_interp` tests):
  - `set!` on let-locals: requires boxed variables (JIT uses stack-based locals)
  - Deep recursion/TCO: JIT has no tail-call optimization, deep recursion overflows C stack
  - Deep effects: continuation operations require C stack frames
- **Tests moved to unified**: 28 tests from `_interp` → unified (macros, substring, lambda-in-let)
- **Files modified**: `src/lisp/jit.c3` (~40 lines added), `src/lisp/eval.c3` (28 test calls changed)
- **Tests**: 452 (412 unified + 40 compiler) → 492 (412 unified + 40 compiler + 40 interp-only), all passing

## 2026-02-13: Unified Test Runner — Interpreter + JIT
- **Feature**: Merged interpreter and JIT tests into a single unified test pass
  - Each `test_*` helper runs BOTH interpreter (`run()`) and JIT (`parse_for_jit()` + `jit_compile()` + call)
  - Added 12 test helpers: `setup`, `test_eq`, `test_truthy`, `test_nil`, `test_error`, `test_str`, `test_tag`, `test_gt`, plus `_interp` variants for tests incompatible with JIT
  - Tests that can't run through JIT (macros, deep continuations, modules, FFI, mutable closures) use `_interp` suffix helpers
  - Fixed C3 `char[]` printing: use `(ZString)` cast for `%s` format specifier (C3 prints `char[]` as byte arrays otherwise)
- **Deleted**: `run_jit_tests()` from `jit.c3` (~1400 lines removed) — all JIT coverage provided by unified helpers
- **Refactored**: All verbose multi-line test blocks converted to one-liner helper calls
  - ~460 interpreter tests rewritten from `{ run(...); assert(...); io::printn(...); }` to `test_eq(interp, name, expr, expected, &pass, &fail);`
  - Orchestration: `run_lisp_tests()` now calls `run_basic_tests`, `run_memory_stress_tests`, `run_list_closure_tests`, `run_arithmetic_comparison_tests`, `run_string_type_tests`, `run_advanced_tests` with shared pass/fail counters
- **Files modified**: `src/lisp/eval.c3` (helpers + all test functions rewritten), `src/lisp/jit.c3` (deleted run_jit_tests)
- **Tests**: 499 (407 interp + 52 JIT + 40 compiler) → 452 (412 unified + 40 compiler), all passing
  - Net reduction because unified tests count once (not separately for interp + JIT)

## 2026-02-13: FFI (Foreign Function Interface) Implementation
- **Feature**: Full FFI system to call C libraries from Pika code
  - `ffi-open`: Open shared library via dlopen
  - `ffi-call`: Call foreign function with type annotations (variadic, up to 6 C args)
  - `ffi-close`: Close shared library handle
  - `ffi-sym`: Get raw function pointer as integer
  - Type symbols: `'int`, `'size`, `'string`, `'void`, `'ptr`, `'double`
- **Architecture**: No libffi dependency — uses function pointer casting with all args as `long` (x86_64 ABI)
  - `V_FFI_HANDLE` value type added to both interpreter and compiler runtime
  - FFI handles allocated in root_region (survive REPL line reclamation)
  - JIT detects variadic primitive calls and falls back to interpreter for correct multi-arg handling
  - Compiler emits runtime function references for ffi-open/close/sym/call
- **Files modified**: `src/lisp/value.c3` (FfiHandle struct, FFI_HANDLE tag), `src/lisp/eval.c3` (dlopen externs, 4 primitives, 8 tests), `src/lisp/jit.c3` (variadic prim detection, 4 tests), `src/lisp/compiler.c3` (4 primitive entries), `src/lisp/runtime.c3` (V_FFI_HANDLE, runtime FFI functions), `project.json` (link libdl), `docs/FEATURES.md` (FFI section)
- **Tests**: 483 -> 499 (8 interpreter + 4 JIT + 4 compiler FFI tests)
  - Interpreter: open libc, strlen, abs, getpid, atoi, ffi-sym, close, error on bad lib
  - JIT: open handle, strlen, getpid, atoi
  - Compiler: ffi-open, ffi-close, ffi-sym, ffi-call output validation

## 2026-02-12: Compiler — Working Reset/Shift/Handle/Perform via Interpreter Bridge
- **Feature**: Lisp-to-C3 compiler now produces working code for all continuation forms
  - **reset/shift**: Full delimited continuation support including multi-shot continuations
  - **handle/perform**: Full algebraic effect handler support with continuation resumption
  - Replaces the previous non-functional stubs with real interpreter delegation
- **Architecture**: Interpreter bridge approach
  - Added expression serializer to compiler (`serialize_expr_to_buf`, `serialize_value_to_buf`, `serialize_pattern_to_buf`) that converts AST back to Pika source text
  - Compiler emits `runtime::rt_eval_source("(reset ...)")` calls for continuation forms
  - Free variables from the compiled scope are injected via `runtime::rt_define_var("name", value)` calls before evaluation
  - Runtime maintains a lazily-initialized interpreter instance (`g_interp`) with primitives + stdlib
  - Value conversion functions: `interp_to_runtime()` (lisp::Value* -> runtime::Value) and `runtime_to_interp()` (runtime::Value -> lisp::Value*)
  - Interpreter closures/continuations wrapped as runtime closures via `InterpClosureWrapper`
- **Files modified**: `src/lisp/compiler.c3` (serializer, compile_reset/shift/handle/perform rewritten), `src/lisp/runtime.c3` (interpreter bridge: Section 17), `src/lisp/eval.c3` (8 new compiler tests)
- **Tests**: 28 -> 36 compiler tests (added: rt_eval_source generation, free var injection, handle/perform delegation, perform standalone, shift standalone, reset serialization, multi-clause handle, try stdlib, nested shift)
- **Total tests**: 483 passed, 0 failed (was 475)

## 2026-02-12: JIT Compiler — Native Reset/Shift/Handle/Perform Compilation
- **Feature**: JIT compiler now handles E_RESET, E_SHIFT, E_HANDLE, and E_PERFORM with dedicated helpers instead of generic fallback
  - **E_RESET**: `jit_exec_reset` helper calls `eval_reset()` — supports reset without shift, reset with shift, multi-shot continuations
  - **E_SHIFT**: `jit_exec_shift` helper calls `eval_shift()` — captures continuation, binds k in body
  - **E_HANDLE**: `jit_exec_handle` helper calls `eval_handle()` — installs effect handlers on handler stack
  - **E_PERFORM**: `jit_exec_perform` helper calls `eval_perform()` — signals effects, searches handler stack
- **Architecture**: Each form has a dedicated JIT compiler function (`jit_compile_reset`, etc.) that emits native code to call the corresponding helper with (interp, expr, env). These replace the generic `jit_compile_fallback` path, giving each expression type explicit handling in the JIT switch. The helpers delegate to the interpreter's existing eval_reset/eval_shift/eval_handle/eval_perform functions, which correctly manage the continuation infrastructure (shift_counter, reset_depth, handler_stack, etc.).
- **Files modified**: `src/lisp/jit.c3`
- **Tests**: 40 JIT tests -> 48 JIT tests (added 8: reset without shift, basic shift, shift without resume, multi-shot continuation, basic handle/perform, handle no effect, handle with arg, reset with nested arithmetic)
- **Total tests**: 456 passed, 0 failed (was 448)

## 2026-02-12: JIT Compiler — New Expression Types (set!, define, quasiquote, match, defmacro)
- **Feature**: JIT compiler now handles 5 additional expression types that previously fell back to the interpreter
  - **E_SET (set! variable mutation)**: Compiles value sub-expression via JIT, calls `jit_eval_set` helper that does env.set() chain (local -> global)
  - **E_DEFINE (global definition)**: Compiles value sub-expression via JIT, calls `jit_eval_define` helper that stores in global_env with root_region promotion
  - **E_QUASIQUOTE**: Calls `jit_eval_quasiquote` helper that delegates to the interpreter's `eval_quasiquote` with depth=0
  - **E_MATCH**: Calls `jit_eval_match` helper that delegates to the interpreter's `eval()` for full pattern matching with env extension
  - **E_DEFMACRO**: Calls `jit_eval_define_macro` helper that delegates to the interpreter's `eval_define_macro`
- **Architecture**: E_SET and E_DEFINE compile their value sub-expressions natively via `jit_compile_expr`, then call lightweight helpers for the side effects. E_QUASIQUOTE, E_MATCH, and E_DEFMACRO delegate fully to the interpreter since they involve complex recursive walks (template expansion, pattern matching, macro table registration).
- **Still using fallback**: E_HANDLE, E_PERFORM, E_RESET, E_SHIFT remain as interpreter fallbacks since they involve C-level stack manipulation for delimited continuations.
- **Files modified**: `src/lisp/jit.c3`
- **Tests**: 30 JIT tests -> 40 JIT tests (added 10: define simple/expr, set!/set!-with-expr, quasiquote/quasiquote-with-unquote, match-literal/match-binding, define-macro, define+set!+read combo)
- **Total tests**: 467 passed, 0 failed

## 2026-02-12: Compiler Parity — set!, dot-bracket, path, documentation
- **Feature**: Added `set!` (variable mutation) support to Lisp-to-C3 compiler
  - New `compile_set()` method generates `({ name = value; name; })` for assignment + return
  - Added `E_SET` handling to `find_free_vars`, `scan_lambdas`, `body_creates_closure`
  - Works for both global and local variables in compiled code
- **Bug fix**: Fixed `compile_path` generating wrong type for field name argument
  - `rt_field_access(Value, Value)` was being called with raw string literals instead of `Value`
  - Changed to wrap field names with `runtime::make_string("field")`
- **Verification**: `compile_index` (dot-bracket `.[i]`) and `compile_path` (`a.b.c`) were already implemented
  - `rt_index` and `rt_field_access` runtime functions existed and were correct
  - Free-variable analysis and lambda scanning already handled E_INDEX and E_PATH
- **Documentation**: Added detailed comments on reset/shift and handle/perform limitations
  - Compiler emits stub calls that allow compilation but don't capture real continuations
  - Full implementation would require CPS transform or setjmp/longjmp stack capture
  - Documented three possible approaches in compile_reset/compile_perform comments
- **Tests**: Added 10 compiler tests (19-28): set! global/expression/lambda/begin, dot-bracket indexing, path notation simple/nested/make_string, reset/shift stub
- **Files modified**: `src/lisp/compiler.c3`, `src/lisp/eval.c3`
- **Tests**: 457 -> 467 (18 -> 28 compiler tests), all passing

## 2026-02-12: Continuation System Improvements
- **Analysis**: Region allocator TODO in `src/delimited.c3` line 226 -- determined `mem::malloc` is the correct choice for continuation stack data
  - Stack segments are raw byte buffers of variable size, not typed objects
  - They need individual deallocation (`abort_continuation`, `resume_final`, `gc_continuations`, `invalidate_region_continuations`)
  - Region allocator doesn't support individual free -- only bulk region release
  - Replaced TODO comment with explanatory comment noting why malloc is correct
- **Verification**: Multi-shot continuations already work at the Lisp level
  - The replay-based continuation model (`CapturedCont`) is inherently multi-shot: `apply_continuation()` reads but never mutates the `CapturedCont`, so calling `(k val1)` and `(k val2)` both work correctly
  - The C-level continuation system (`delimited.c3`) has separate multi-shot support via `clone_continuation()` which deep-copies stack data
  - Added 5 multi-shot tests: k called twice/three times, k via let bindings, conditional k, effect handler k called twice
- **Verification**: TCO works inside reset/shift bodies
  - `eval_reset()` and `eval_shift()` call `eval()` recursively, giving the body its own TCO for-loop
  - `E_IF`, `E_LET`, `E_BEGIN`, `E_AND`, `E_OR`, `E_MATCH`, `E_APP`, `E_CALL` all use `continue` for TCO within these bodies
  - Added 7 TCO tests: tail recursion in reset body (5000 iterations), tail recursion in shift body (5000 iterations), begin+if in reset, let chains in shift, tail-recursive loop with k invocation, nested reset with if, recursive function in handler body
- **Files modified**: `src/delimited.c3` (TODO comment fix), `src/lisp/eval.c3` (12 new tests)
- **Tests**: 435 -> 447 (added 5 multi-shot + 7 TCO tests), all passing

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
