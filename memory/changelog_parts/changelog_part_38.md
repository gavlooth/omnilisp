# Memory Changelog Part 38

## 2026-04-27 - FFI Foreign Runtime Retained Optional Interop Closure

- `[FACT]` Bindgen dependency generation now carries `mode` and `generator`
  through the actual raw module, facade, and manifest write path. `mode` accepts
  `abi` or `api`; `generator` accepts `clang` or `cppinterop`.
- `[FACT]` `generator = "cppinterop"` requires `mode = "api"`. This keeps
  CppInterop as API/tooling metadata and output review markers, not a startup
  or runtime C++ dependency.
- `[FACT]` Native FFI buffer returns and CPU/CUDA Tensor buffer exports use
  borrowed `ForeignHandle` buffer descriptors. Unsupported tensor devices fail
  closed instead of exposing raw pointers.
- `[FACT]` CUDA/cuBLAS execution remains in the Tensor backend. The FFI
  foreign-runtime boundary for that lane is tensor-buffer marshalling only.
- `[FACT]` Python/Julia adapters and polyglot/plugin runtime support are not
  planned for the current FFI foreign-runtime area.
- `[FACT]` `scripts/check_status_consistency.sh` now expects
  `docs/areas/ffi-foreign-runtime.md` to be `green` because the retained
  optional lanes are implemented and Python/Julia plus polyglot/plugin are no
  longer required lanes.
- `[INVALIDATED]` Do not treat the old residual list of Python, Julia,
  polyglot/plugin, CppInterop, CUDA/cuBLAS, and tensor-buffer hooks as an active
  implementation backlog. CppInterop and tensor-buffer marshalling are the
  retained lanes; Python/Julia and polyglot/plugin are explicitly closed as not
  planned.
