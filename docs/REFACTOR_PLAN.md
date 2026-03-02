# God File / God Function Splitting Plan

## Current State: 6 god files, 10+ god functions

| File | Lines | Problem |
|------|-------|---------|
| jit.c3 | 4198 | 145 functions, god functions: jit_compile_expr, jit_handle_impl, jit_compile_call, jit_apply_multi |
| tests.c3 | 4318 | All tests in one file, 15+ test suite functions |
| compiler.c3 | 4123 | scan_lambdas (321 lines), find_free_vars (272), serialize_expr (261) |
| parser.c3 | 3462 | Lexer.advance (379 lines!), parse_expr (365), parse_define (303) |
| eval.c3 | 2759 | register_primitives (208), repl (188), copy_to_parent (142), match engine |
| value.c3 | 2752 | Value type + Interp struct + SymbolTable + TypeRegistry + constructors + destructors |
| primitives.c3 | 2707 | 97 primitives + HashMap + Array + format engine + string builder |

## Target Module Structure

C3 modules can span multiple files. Same module = shared access. Submodules = explicit boundary.

```
module lisp                    — core types: Value, Interp, Env, SymbolTable (value.c3 only)

module lisp::parse             — Lexer, Parser, TokenType (split from parser.c3)
  src/lisp/parse/lexer.c3      — Lexer struct + advance() split into token handlers
  src/lisp/parse/parser.c3     — Parser + parse_expr (split into parse_atom, parse_call, parse_special)
  src/lisp/parse/macros.c3     — macro expansion (already separate)

module lisp::jit               — JIT compilation + execution
  src/lisp/jit/compile.c3      — jit_compile, jit_compile_expr (the switch)
  src/lisp/jit/compile_call.c3 — jit_compile_call, multi-arg dispatch
  src/lisp/jit/apply.c3        — jit_apply_value, jit_apply_multi, apply_primitive
  src/lisp/jit/effects.c3      — jit_signal_impl, jit_handle_impl, jit_resolve
  src/lisp/jit/context.c3      — reset/shift, SavedInterpState, context switch wrappers
  src/lisp/jit/lightning.c3    — GNU Lightning FFI declarations + emit_* helpers

module lisp::eval               — evaluation infrastructure
  src/lisp/eval/run.c3          — run(), run_program(), repl()
  src/lisp/eval/register.c3     — register_primitives, register_stdlib, register_fast_path
  src/lisp/eval/match.c3        — pattern matching engine (MatchResult, match_value)
  src/lisp/eval/copy.c3         — copy_to_parent, promote_to_root, copy_env_to_scope

module lisp::prim               — primitive implementations
  src/lisp/prim/core.c3         — cons, car, cdr, list, null?, pair?
  src/lisp/prim/math.c3         — +, -, *, /, %, sin, cos, pow, etc.
  src/lisp/prim/string.c3       — string ops + StringVal builder (strval_*)
  src/lisp/prim/collection.c3   — HashMap, Array, set, dict, ref, length, push!
  src/lisp/prim/format.c3       — format, cl-format (223 + 72 lines)
  src/lisp/prim/io.c3           — print, println, read-file, write-file, load
  src/lisp/prim/type.c3         — type-of, is?, instance?, type system prims
  src/lisp/prim/coroutine.c3    — coroutine, resume, yield

module lisp::io                 — I/O subsystems (already reasonably sized)
  src/lisp/io/tcp.c3            — async.c3 → renamed
  src/lisp/io/tls.c3            — unchanged
  src/lisp/io/http.c3           — unchanged
  src/lisp/io/compress.c3       — unchanged
  src/lisp/io/json.c3           — unchanged
  src/lisp/io/unicode.c3        — unchanged

module lisp::deduce              — Datalog engine (already reasonably sized)
  src/lisp/deduce/store.c3       — LMDB storage, encode/decode, relation
  src/lisp/deduce/query.c3       — deduce-query, deduce-match, deduce-scan
  src/lisp/deduce/unify.c3       — unification engine (already separate)
  src/lisp/deduce/schema.c3      — contracts (already separate)

module lisp::test                — tests
  src/lisp/test/basic.c3         — run_basic_tests, run_arithmetic_tests
  src/lisp/test/advanced.c3      — effects, continuations, coroutines
  src/lisp/test/scope.c3         — scope/escape/TCO tests
  src/lisp/test/integration.c3   — pika, unicode, json, compression, deduce, http
  src/lisp/test/runner.c3        — test_eq, test_str, run_lisp_tests
```

## God Function Splitting

### Lexer.advance (379 lines → 5 functions)
```
Lexer.advance()           — dispatcher (50 lines): check first char, call handler
Lexer.scan_string()       — string literal parsing (~60 lines)
Lexer.scan_number()       — int/float parsing (~80 lines)
Lexer.scan_symbol()       — symbol/path parsing (~50 lines)
Lexer.scan_hash()         — # dispatch: #{}, #_, #N_, #r"" (~60 lines)
```

### parse_expr (365 lines → 8 functions)
```
parse_expr()              — dispatcher: check token type, call handler
parse_quote()             — quote/quasiquote/unquote
parse_literal()           — int, float, string, regex
parse_set_literal()       — #{}
parse_form_comment()      — #_, #N_
parse_list()              — (...) — calls parse_call or parse_special
parse_special_form()      — if, let, define, match, handle, begin, etc.
parse_call()              — function application
```

### parse_define (303 lines → 4 functions)
```
parse_define()            — dispatcher: check for [attr]
parse_define_type()       — [type], [abstract], [union], [alias]
parse_define_ffi()        — [ffi lib], [ffi λ]
parse_define_other()      — [schema], [relation], [rule], [macro], [effect]
```

### jit_compile_expr (115 lines switch → stays, but callees split)
The switch itself is fine — it dispatches to per-tag compile functions.
The callees are the problem:

### jit_compile_call (148 lines → 3 functions)
```
jit_compile_call()        — entry point, check inline optimizations
jit_compile_inline_op()   — inline arithmetic/comparison for known primitives
jit_compile_multi_arg()   — general multi-arg call dispatch
```

### jit_handle_impl (155 lines → 2 functions)
```
jit_handle_impl()         — outer loop: run body, check signals
jit_handle_dispatch()     — match signal to handler clause, evaluate
```

### prim_format (223 lines → 3 functions)
```
prim_format()             — entry point, iterate format string
format_directive()        — handle one % directive
format_value()            — convert Value to string for %s
```

### copy_to_parent (142 lines → stays)
The switch on ValueTag is inherent complexity — each tag has different copy semantics.
Not a god function, just comprehensive.

## Migration Order

Phase R1: Split primitives.c3 (easiest, most independent functions)
Phase R2: Split parser.c3 (Lexer.advance, parse_expr, parse_define)
Phase R3: Split eval.c3 (run, register, match, copy)
Phase R4: Split jit.c3 (compile, apply, effects, context)
Phase R5: Split tests.c3 (basic, advanced, scope, integration)
Phase R6: Split value.c3 (if needed — types vs constructors vs interp)
Phase R7: Split compiler.c3 (scan, free_vars, serialize)

Each phase: create submodule directory, move functions, update imports, build+test.

## Key C3 Module Rules
- Same module across files: all declarations shared (no import needed)
- Submodule: `module lisp::jit;` — must `import lisp` to access Value, Interp
- `@local` functions are file-private (not visible to other files in same module)
- No circular imports between submodules
