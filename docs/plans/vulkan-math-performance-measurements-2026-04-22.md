# Vulkan Math Performance Measurements (2026-04-22)

Purpose: provide a checked-in measurement checkpoint for the open
`TENSOR-100F` Vulkan SVD/eigen performance items before any tiled or staged
Jacobi rewrite is attempted.

## Probe

The reproducible probe is:

```sh
scripts/run_vulkan_math_perf_probe.sh
OMNI_VULKAN_MATH_PERF_SCALE=1 scripts/run_vulkan_math_perf_probe.sh
```

The script runs small capability-gated Vulkan fixtures and emits
`OMNI_BENCH_SUMMARY` lines. Each fixture measures the operation with Omni's
`time-ms` primitive inside the runtime process, so startup time is excluded
from the reported `ms` field. The default mode covers the 64/65 storage-backed
threshold and representative diagonal, zero, rank-deficient, and staged-solve
fixtures. The opt-in scale mode extends the same probe to larger spot checks.
It is not a broad validation gate.

## Current Results

Host: current development machine, existing `./build/main`, Vulkan available.

Command:

```sh
OMNI_VULKAN_MATH_PERF_SCALE=1 scripts/run_vulkan_math_perf_probe.sh
```

Results:

| Suite | Label | Dtype | n | ms | Result Probe |
| --- | --- | --- | ---: | ---: | --- |
| `vulkan_math_svd` | `f64_identity_64` | `Float64` | 64 | 332 | `[64 1.0 1.0]` |
| `vulkan_math_svd` | `f64_identity_65` | `Float64` | 65 | 320 | `[65 1.0 1.0]` |
| `vulkan_math_svd` | `f64_ones_65` | `Float64` | 65 | 358 | `[65 65.0 0.0]` |
| `vulkan_math_svd` | `f64_zero_65` | `Float64` | 65 | 325 | `[65 0.0 0.0]` |
| `vulkan_math_svd` | `f32_identity_65` | `Float32` | 65 | 323 | `[65 1.0 1.0]` |
| `vulkan_math_eigen` | `f64_identity_64` | `Float64` | 64 | 315 | `[64 1.0 1.0]` |
| `vulkan_math_eigen` | `f64_identity_65` | `Float64` | 65 | 336 | `[65 1.0 1.0]` |
| `vulkan_math_eigen` | `f64_ones_65` | `Float64` | 65 | 350 | `[65 65.0 0.0]` |
| `vulkan_math_eigen` | `c64_zero_65` | `Complex64` | 65 | 347 | `[65 0.0 0.0]` |
| `vulkan_math_general_eigenpairs` | `f64_exact_complex_shift_2` | `Float64` | 2 | 312 | `[2 0.0+1.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `f32_exact_complex_shift_2` | `Float32` | 2 | 326 | `[2 0.0+1.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `c128_triangular_3` | `Complex128` | 3 | 322 | `[3 4.0+0.0i -2.0+1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `c64_exact_complex_shift_2` | `Complex64` | 2 | 300 | `[2 0.0+1.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `f64_mixed_block_deflation_3` | `Float64` | 3 | 305 | `[3 2.0+0.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `f32_mixed_block_deflation_3` | `Float32` | 3 | 317 | `[3 2.0+0.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `c128_mixed_block_deflation_3` | `Complex128` | 3 | 306 | `[3 2.0+0.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_general_eigenpairs` | `c64_mixed_block_deflation_3` | `Complex64` | 3 | 320 | `[3 2.0+0.0i 0.0-1.0i vulkan vulkan]` |
| `vulkan_math_solve` | `f64_identity_65` | `Float64` | 65 | 323 | `[65 1.0 1.0]` |
| `vulkan_math_solve` | `f64_i_plus_ones_65` | `Float64` | 65 | 304 | `[65 0.999999999999997 1.0]` |
| `vulkan_math_svd` | `f64_identity_96` | `Float64` | 96 | 333 | `[96 1.0 1.0]` |
| `vulkan_math_svd` | `f64_identity_128` | `Float64` | 128 | 389 | `[128 1.0 1.0]` |
| `vulkan_math_svd` | `f64_identity_192` | `Float64` | 192 | 616 | `[192 1.0 1.0]` |
| `vulkan_math_svd` | `f64_ones_128` | `Float64` | 128 | 559 | `[128 128.0 0.0]` |
| `vulkan_math_eigen` | `f64_identity_128` | `Float64` | 128 | 323 | `[128 1.0 1.0]` |
| `vulkan_math_eigen` | `f64_ones_128` | `Float64` | 128 | 482 | `[128 128.0 0.0]` |
| `vulkan_math_solve` | `f64_identity_128` | `Float64` | 128 | 303 | `[128 1.0 1.0]` |
| `vulkan_math_solve` | `f64_i_plus_ones_128` | `Float64` | 128 | 326 | `[128 1.00000000000002 1.0]` |
| `vulkan_math_solve` | `f64_identity_192` | `Float64` | 192 | 314 | `[192 1.0 1.0]` |

Earlier coarse `/usr/bin/time` process-per-expression probes on the same
surface took about 1.5-1.6 s for 64x64 identity and 2.6-3.2 s for 65x65
fixtures. Those numbers include runtime startup and are useful only as smoke
evidence that the paths complete.

## Decision

The 64/65 and opt-in scale data do not justify a tiled or multi-dispatch
SVD/eigen rewrite in the current work item. The measured 65x65 fixtures are in
the same operating range as the 64x64 baseline once process startup is removed,
and the larger spot checks through 192 for SVD and 128 for eigen stay within a
sub-second focused-probe range on this host.

Close the current SVD/eigen performance residuals as measured with no rewrite
justified. Close the blocked trailing-update LU residual for the same reason:
the staged solve fixtures measured here do not show the existing route as a
bottleneck requiring a broader blocked LU algorithm. Future larger-size work
should be opened as a new item only when repeated measurements at a named size
show the current serial Jacobi/storage-scratch or staged solve shape is the
bottleneck.

Additional general eigenpair probes on 2026-04-22 show the native Vulkan
`matrix/eigenpairs` routes returning Vulkan-placed values/vectors for real
`Float64`/`Float32` and complex `Complex128`/`Complex64` exact-shift/triangular
fixtures. The hardening pass fixed exact 2x2 complex-shift spectra with direct
analytic eigenpair handling, then closed the mixed-block deflation gap for 3x3
spectra with a converged trailing scalar and a leading 2x2 rotation block. The
durable TODO live queue is closed as of 2026-04-22; future larger-size work
should be opened only when a named measurement shows the serial helper is the
bottleneck.

## Verification

Commands run:

```sh
bash -n scripts/run_vulkan_math_perf_probe.sh
git diff --check -- scripts/run_vulkan_math_perf_probe.sh
scripts/run_vulkan_math_perf_probe.sh
OMNI_VULKAN_MATH_PERF_SCALE=1 scripts/run_vulkan_math_perf_probe.sh
LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (ep (matrix/eigenpairs (to-device (Tensor Float64 [2 2] [0 -1 1 0]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (< (abs (- (abs (imag-part l0)) 1.0)) 0.000001) (< (+ (abs (- (* (Complex128 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.000001)))"
LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (ep (matrix/eigenpairs (to-device (Tensor Float32 [2 2] [0 -1 1 0]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (< (abs (- (abs (imag-part l0)) 1.0)) 0.001) (< (+ (abs (- (* (Complex64 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.001)))"
LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp
scripts/run_validation_container.sh env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp
```

The original performance probe completed with all listed fixtures available on
this host and no failures. The 2026-04-22 general eigenpair extension completed
the listed exact-shift/triangular fixtures with Vulkan output placement. Focused
advanced collections currently report `pass=2025 fail=0` after reconciling stale
ML/Vulkan mixed-migration contract tests and the Vulkan Float64
layer-normalization capability bit. The eigenpair failures from the stale
fail-closed expectations and mixed-block deflation regressions are gone. The
bounded-container focused advanced rerun also passed with `pass=1994 fail=0`;
the lower pass count is backend-availability gated, with zero failures.

Follow-up bounded global-gate validation on 2026-04-23 also passed after fixing
two non-eigen gate blockers exposed by the broader run: handled raise payload
string-allocation failure now propagates the original allocation ERROR, and
deduce no-data raises keep their documented payload-less degradation under
payload-map OOM without weakening generic/JIT strict payload construction
failure. FTXUI smoke programs were updated to read structured raise payload
messages. The final bounded global gate passed file-size gate, normal build,
all configured normal lisp slices, compiler slice, and FTXUI smokes; ASAN was
explicitly skipped because the current C3 toolchain reports address sanitizer
unsupported for this target.
