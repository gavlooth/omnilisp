# C3 Style Guide — Omni Lisp

## defer everything
- `defer mem::free(ptr)` immediately after `mem::malloc`
- `defer (void)file.close()` immediately after open
- `defer mdb_txn_abort(txn)` after `mdb_txn_begin` (commit replaces abort)
- `defer mdb_cursor_close(cursor)` after `mdb_cursor_open`
- `defer self.depth--` after `self.depth++`
- Rule: acquire → defer release → use. Never separate acquire from defer.

## malloc discipline
- Prefer scope-region (`alloc_value`, `alloc_env`) over `mem::malloc`
- `mem::malloc` only for: long-lived C structs, FFI buffers, Expr/Pattern nodes
- Every `mem::malloc` must have a matching `defer mem::free` or registered dtor
- Never `mem::free` a scope-region pointer. Never scope-release a malloc'd pointer.

## slices
- `buffer[0..n]` = n+1 elements (INCLUSIVE). Use `buffer[:n]` for n elements.
- `args[1..]` = from index 1 to end (exclusive start, like Rust)
- Always check `.len` before indexing

## errors
- Return `raise_error(interp, msg)` — returns ERROR-tagged Value*
- Check `result.tag == ERROR` after every `jit_apply_value` / `prim_*` call
- `EvalResult.error.message` is `char[256]` — use `(ZString)&r.error.message`

## naming
- Primitives: `prim_foo_bar` (C3 fn) → `"foo-bar"` (Omni name)
- `__raw-*` for effect fast-path primitives (hidden from user)
- `make_*` for Value constructors (`make_string`, `make_int`, `make_cons`)
- `is_*` for predicates (`is_string`, `is_cons`, `is_int`)

## register pattern
- Primitives: add to `regular_prims[]` array in `register_primitives`
- Dispatched: add to `dispatched_prims[]` (creates MethodTable)
- Effects: `register_fast_path(interp, "io/foo", "__raw-foo")`
- Update array size constant when adding entries

## scope-region rules
- `root_scope`: permanent values (primitives, global env, FFI_HANDLE, ARRAY, HASHMAP)
- `current_scope`: temporaries (per-call, freed on scope release)
- `copy_to_parent`: promotes value from child to parent scope
- `promote_to_root`: for values stored in root-scope containers (set!, array-set!)
- CLOSURE: refcount-shared via copy_to_parent, env_scope per closure

## testing
- `test_eq` / `test_str` / `test_tag`: test both interp + JIT paths
- `setup(interp, code)`: run code for side effects (define, assert)
- `run(code, interp)`: single expression, returns EvalResult
- Interp-only tests use `run()` in a raw block for stateful ops
