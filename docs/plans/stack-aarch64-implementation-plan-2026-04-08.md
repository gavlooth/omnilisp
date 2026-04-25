# STACK-AARCH64-001 Implementation Plan (2026-04-08)

Status: historical implementation plan; not a live TODO lane

## Scope
- TODO lane: `STACK-AARCH64-001`
- Objective: implement a real `aarch64` stack-switch backend behind the existing backend contract seam without widening runtime call-site branching.
- Non-goal: broad runtime rewrites outside stack backend and stack-engine integration points.

## Current Baseline
- Runtime switch flow already routes through `stack_engine_backend_contract.c3`.
- `x86_64-sysv` is the only implemented backend.
- Non-x86 targets remain fail-closed.
- This plan assumes current stack semantics stay unchanged:
  - coroutine `switch_to` / `suspend` / `resume`,
  - multi-shot clone/resume,
  - effect/handler continuation resume behavior.

## Hard Constraints
- Preserve fail-closed behavior until all aarch64 runtime validation passes.
- Keep backend ABI/control-state details in backend files, not in generic runtime call sites.
- Do not claim arm64 support after compile-only success.
- Keep guard-page overflow handling and ownership/lifecycle logic in common lanes.

## Backend Contract Targets
Implement these for aarch64 under the existing seam:
- `stack_backend_init_context(...)`
- `stack_backend_save_fp_state(...)`
- `stack_backend_restore_fp_state(...)`
- `stack_backend_protected_switch(...)`
- `stack_backend_switch(...)`

No new runtime call-site conditionals should be introduced outside the contract.

## AArch64 ABI Plan (Implementation-Ready)
### 1) Context Save Set
Use AAPCS64 callee-preserved state as the minimum stable resume set:
- GPRs: `x19..x29`
- Stack pointer: `sp`
- Control transfer state: saved resume `pc` plus `lr` (`x30`)
- SIMD/FP: `v8..v15` (store full 128-bit lanes to avoid upper-lane corruption)
- FP control/status: `fpcr`, `fpsr` if required by test parity

### 2) `StackContext` Layout Strategy
- Introduce an aarch64-specific `StackContext` layout in `src/stack_engine_abi_switch.c3` under compile-time gating.
- Keep field order fixed and documented with byte offsets used by switch asm.
- Keep x86 layout untouched for existing builds.

### 3) Fresh-Stack Bootstrap
- Initialize `sp` from `stack_top` with 16-byte alignment.
- Seed initial control flow to `stack_ctx_trampoline` without assuming x86 return-address stack shape.
- Recommended backend behavior:
  - write `pc = &stack_ctx_trampoline`
  - set `lr = 0` sentinel
  - initialize preserved registers and SIMD/control state deterministically

### 4) AArch64 Switch Assembly
Implement `omni_context_switch` (`@if(env::AARCH64)`):
- Save current context into `old_ctx`:
  - `x19..x29`, `sp`, `lr`, resume `pc`
  - `v8..v15` (and optional `fpcr/fpsr`)
- Restore from `new_ctx`:
  - `x19..x29`, `sp`, `lr`
  - `v8..v15` (and optional `fpcr/fpsr`)
  - branch to saved `pc`

Execution rule:
- Do not rely on x86-style `push ret` entry assumptions.
- Use explicit branch-to-saved-PC mechanics on aarch64.

### 5) Guarded Switch Path
- Keep using `stack_backend_protected_switch(...)` as the only guarded transition entrypoint from runtime lanes.
- Ensure the aarch64 path still reports overflow via existing protected-switch return contract.

## Validation Plan
### Phase A: Build and smoke
- `c3c build` on arm64 and x86_64.
- Confirm non-implemented targets still fail closed with explicit backend errors.

### Phase B: Stack-engine correctness (arm64 runtime)
Run stack suite and require zero regressions:
- coroutine entry/yield/resume
- clone/resume multi-shot
- stack overflow recovery (alternate stack)
- FPU/SIMD preservation across switch

Baseline verifier:
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Require stack test summary to pass (same suite currently emitted by `run_stack_engine_tests(...)`).

### Phase C: Language-level continuation/effect parity (arm64)
Run targeted runtime tests that transit stack switching through language surfaces:
- coroutine primitives
- effect `resolve` / `with-continuation` paths
- continuation clone/resume paths

### Phase D: Support claim gate
Only after A/B/C pass:
- update docs/build notes to list arm64 stack backend support.
- keep unsupported targets fail-closed.

## Verified Arm64 Evidence (2026-04-08)
- Host: `aarch64`
- Stack suite: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
- Coroutine smoke: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(resume (Coroutine (lambda () (+ 1 2))))"` -> `3`
- Effect smoke: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
- Continuation/effect parity gate (closed):
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block (define replay-set-counter 0) (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) (+ (* 10 replay-set-r) replay-set-counter))"` -> `52`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`
  - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1 fail=0`

## Delivery Slices
1. Add aarch64 `StackContext` + switch asm in `src/stack_engine_abi_switch.c3`.
2. Add aarch64 backend init/fp/switch implementations in `src/stack_engine_backend_contract.c3`.
3. Add/adjust stack tests for explicit aarch64 parity checks (especially SIMD/FP + clone/resume + overflow).
4. Run arm64 runtime validation and record results in session artifacts.
5. Promote support docs only after verified runtime parity.

## Risk Register
- Wrong preserved-register set causes non-deterministic coroutine/effect corruption.
- Incorrect bootstrap/entry transfer can pass build and still fail on first switch.
- SIMD/control-state omission can produce latent numeric corruption after resume.
- Compile-only success is insufficient; runtime parity is the governing signal.

## Negative-Memory Guardrails
- Do not treat x86 stack-frame assumptions as portable bootstrap logic.
- Do not mark arm64 as supported from `c3c build` alone.
- Do not leak backend conditionals into generic runtime call sites.
