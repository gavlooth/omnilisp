# omni-torch Migration Issues — Problems to Fix in Omni

Encountered during migration of omni-torch FFI bindings from old `ffi-open`/`ffi-call` syntax to new `define [ffi lib]`/`define [ffi lambda]` syntax, and creating a diffusion model demo.

---

## P1: Installed `omni` binary has no update mechanism

**What**: The `omni` wrapper at `~/.local/bin/omni` runs `~/.local/lib/omni/omni-bin` (built Feb 26). The dev build at `build/main` (Feb 27) has the new FFI syntax. The installed binary silently lacks the feature — produces cryptic `(eval):1: file exists:` errors instead of "unknown syntax".

**Impact**: Any user who builds Omni from source and installs it once will get stale behavior forever unless they manually `cp build/main ~/.local/lib/omni/omni-bin`.

**Fix**: Add `make install` target that copies `build/main` → `~/.local/lib/omni/omni-bin`. Or have the wrapper script just exec `build/main` in dev mode.

---

## P2: `define [ffi lambda]` eagerly resolves dlsym at module load — RESOLVED (Session 49b)

**Fix**: Implemented lazy resolution (option 1). `eval_ffi_fn` stores lib handle + C name in `FfiBoundFn` without calling `dlsym`. `prim_ffi_bound_call` resolves on first call and caches the pointer. Missing symbols only error when actually called.

---

## P3: Debug output floods stdout on every program run

**What**: Running ANY Omni program produces hundreds of `[debug] copy_to_parent: promoting tag=X to root region` lines. For the XOR demo, this is thousands of lines. There's no way to suppress it except `grep -v '^\[debug\]'`.

**Impact**: Unusable for piping output, automated testing, or any production use. Hides actual program output in noise.

**Fix**: Either:
- Remove these debug prints entirely (they were useful during development, not anymore)
- Gate behind `--debug` or `--verbose` flag
- Use stderr and only when `OMNI_DEBUG=1` env var is set

---

## P4: No built-in random number generation — RESOLVED (Session 48)

Added `random`, `random-int`, `time`, `time-ms`, `shell`, `getenv`, `exit`, `sleep` primitives.

---

## P5: No `tensor/from-list` constructor

**What**: Can't create a tensor from a list of Omni values. The only path is `omni-torch-from-blob-f32` which requires a raw memory pointer. Building arbitrary small tensors (like frequency vectors, weight initializations from specific values) requires convoluted workarounds.

**Impact**: The diffusion demo's sinusoidal time embedding had to use `exp(arange * ln(2))` instead of directly specifying `[1, 2, 4, 8, 16, 32, 64, 128]`.

**Fix**: Add a C shim function `omni_torch_from_list_f32(float* data, int64_t len)` and high-level wrapper `(tensor/from-list '(1.0 2.0 3.0))`.

---

## P6: XOR demo hangs with new FFI bindings — RESOLVED (Session 49)

**Root cause**: `eval_ffi_lib`/`eval_ffi_fn` used `env.extend()` (creates child env frames) instead of `env.define()` (in-place mutation). FFI bindings were in child frames invisible to `mod.env.lookup()`, so `import :all` found nothing.

**Fix**: Changed both to use `interp.global_env.define()` in eval.c3 lines 924 and 1092.

---

## P7: File-level expression limit (256) in module loader

**What**: `jit_load_module_from_file` uses a stack-allocated `Expr*[256]` array for file-level expressions. While the module body itself uses a dynamically growing array (no limit), any file with >256 top-level expressions will fail.

**Impact**: Not hit in this case, but a file with many dispatch extensions after a module could hit it. For example, a torch.omni with 20+ dispatch overloads × multiple type combinations could approach the limit.

**Fix**: Use a dynamically growing array for file-level expressions too, or increase the static limit to 1024.

---

## P8: No `make install` or version management

**What**: No standard way to install a built Omni binary. The `omni` wrapper script hardcodes `~/.local/lib/omni/omni-bin`. Building `c3c build` produces `build/main` but there's no `make install` to put it in the right place.

**Impact**: After every build, users must manually copy the binary. The omni-torch Makefile uses `omni` (the wrapper) which may run a stale binary.

**Fix**: Add install target to build system, or have `c3c build` output directly to the wrapper's expected path.

---

## Summary — Priority Order

| Priority | Issue | Status |
|----------|-------|--------|
| **P1** | Stale installed binary | Open |
| **P2** | Eager dlsym fails entire module | **RESOLVED** (Session 49b) |
| **P3** | Debug output floods | Open |
| **P4** | No random primitives | **RESOLVED** (Session 48) |
| **P5** | No tensor/from-list | Open |
| **P6** | XOR hangs with new FFI | **RESOLVED** (Session 49) |
| **P7** | File expression limit 256 | Open |
| **P8** | No make install | Open |
