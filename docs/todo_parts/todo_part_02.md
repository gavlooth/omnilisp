# Active TODO Index Part 02

Source: `TODO.md`

      `matrix/svd`.
      - blocker/defer reason: CUDA has fixed-complex storage, map, contract,
        and structural matrix support, but no cuSOLVER runtime loader or SVD
        ABI. cuBLAS is not an SVD backend, and custom PTX should be limited to
        deterministic layout adapters.
      - next step: add runtime-loaded cuSOLVER DN support for
        `cusolverDnZgesvd` / `cusolverDnCgesvd`, adapter PTX for Omni
        row-major to cuSOLVER column-major storage and `VT` to row-major `V`,
        operation-specific capability bits, and CUDA routing for
        singular-values/norms before full factor output.
      - validation: CUDA PTX generation plus `ptxas`, cuSOLVER symbol probes
        and call counters, helper archive rebuild, CUDA direct smokes, no
        LAPACK/CPU fallback counters, focused host and bounded-container
        advanced tests, docs parity, Stage 3 source parity, and
        `git diff --check`.
    - [ ] `TENSOR-100H-COMPLEX-EIGEN` settle and implement fixed-width
      complex eigen/eigenpair contracts.
      - blocker/defer reason: native fixed-width complex Tensor storage now
        exists, but the public eigen result contract still has a `BigComplex`
        general `matrix/eigenpairs` legacy. Backend support must not fake
        support by lowering pointer-backed `BigComplex` or host-building it
        behind a GPU result.
      - next step: freeze the fixed-width contract first. Recommended
        direction: `matrix/eigenvalues` / `matrix/eigenvectors` cover
        symmetric/Hermitian inputs with component-width real values and
        dtype-preserving vectors; `matrix/eigenpairs` becomes the general
        fixed-width eigenpair surface returning fixed-width complex
        values/vectors for fixed-width numeric inputs. CUDA remains false
        unless cuSOLVER eigen support is introduced later.
      - validation: residual checks `A * v ~= lambda * v`, Hermitian and
        general cases, dtype/device/capability truth tables, no-hidden-fallback
        tests, focused host and bounded-container advanced tests, docs parity,
        Stage 3 source parity, and `git diff --check`.
    - [x] Add explicit Tensor view/layout representation metadata before
      enabling view-backed GPU dispatch.
      - closed: `tensor-layout` now returns a metadata `Dictionary` covering
        dtype, device, payload, layout, dense-row-major status, shape, strides,
        rank, element count, logical byte length, storage offset, concrete
        backing storage element/byte extent, view flags, owner, and write
        policy. Lazy expression payloads report zero storage extent until
        realization.
      - preserved contract: this is metadata-only. It does not add a public
        view constructor, recursive/view GPU kernels, or stride-aware helper
        ABI consumption. Dense CUDA/Vulkan kernels still require zero-offset
        dense row-major storage.
      - prerequisite state: dense row-major contiguous CPU/CUDA/Vulkan Tensor
        paths are shipped; unsupported layouts still fail closed.
      - negative-memory constraint: do not resume by only passing strides into
        Vulkan helpers. Strides without offset, backing extent, and alias owner
        metadata are not a safe view contract.
      - validation: implementation validation was owned by the runtime slice;
        this documentation update was checked with targeted diff hygiene.
    - [x] Define the first read-only Tensor view construction/operation
      contract.
      - closed: `matrix/transpose-view` is the explicit read-only rank-2
        transpose view constructor. It swaps logical shape and strides, keeps
        the source Tensor as owner, reports `tensor-layout` payload `view`,
        owner `view-source`, `owns-storage` false, and write-policy
        `read-only-view`.
      - shipped CPU surface: `ref`, flat `(Array view)`, flat `(List view)`,
        and CPU `realize` materialization observe the transposed logical
        indexing. Writes through the view fail closed.
      - preserved contract: existing `matrix/transpose` remains materializing
        for concrete tensors, but composes structurally when its input is
        already a transpose view.
      - backend boundary: this original view slice remained CPU-readable only
        until the follow-on Vulkan materialization item below landed. Broader
        arbitrary view-backed CUDA/Vulkan kernels still fail closed instead of
        unsafe offset/stride interpretation.
      - validation: original CPU view behavior and the follow-on Vulkan
        materialization boundary are covered by the current focused host and
        bounded-container advanced collections runs.
    - [x] Land Vulkan transpose-view materialization before broader
      view-aware GPU execution.
      - closed: direct rank-2 `matrix/transpose-view` values over dense
        zero-offset Vulkan storage can now materialize to dense Vulkan tensors
        through `realize`, `to-device 'vulkan`, and `to-device 'cpu` copyback.
      - shipped contract: this is explicit materialization only. It preserves
        logical transpose indexing, dtype, and placement for Tensor outputs,
        and it does not make arbitrary strided/view-backed Vulkan kernels
        eligible.
      - validation: direct Vulkan transpose-view smokes passed; host focused
        `advanced-collections-module` passed `1475/0`; bounded-container
        focused `advanced-collections-module` passed `1458/0`; targeted diff
        hygiene passed.
    - [ ] Add explicit view-aware Vulkan/copy-kernel support after the
      read-only CPU transpose-view and Vulkan materialization contracts are
      validated.
      - blocker/defer reason: `matrix/transpose-view` ships with CPU
        read/materialize support, and direct rank-2 Vulkan transpose-view
        materialization is landed. That materialization boundary is not broad
        view-aware kernel execution. Vulkan/CUDA helpers still require
        zero-offset dense row-major storage unless a specific helper ABI accepts
        offset/stride metadata.
      - concrete next step: choose one structural GPU operation, define the
        helper ABI for source offset/strides/backing extent, add alias/bounds
        tests, then route that operation without hidden CPU/GPU copies.
      - prerequisite state: direct rank-2 Vulkan transpose-view materialization
        validation is recorded above. Future view-aware execution still needs
        new offset/stride/backing-extent validation, including alias/source
        ownership and fail-closed unsupported-view cases.
      - negative-memory constraint: do not pass strides into Vulkan helpers
        opportunistically. View-aware backend execution requires offset,
        backing extent, owner/alias, and write-policy validation together.
      - validation: shader compile/`spirv-val` if a shader changes, helper
        rebuild, `c3c build --obj-out obj`, focused CPU/GPU parity smokes,
        host and bounded-container advanced collections, docs parity, Stage 3
        parity, and targeted `git diff --check`.
    - [x] Expand `Float32` scientific unary Vulkan coverage through dedicated
      helper opcodes, not the invalidated generic unary `map` mode-3 branch.
      - closed: dense row-major Vulkan `Float32` tensors now support `sin`,
        `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
        `log`, and `log10` through the separate unary helper ABI for both
        public `map` and direct Tensor unary math.
      - validation: shader compile/`spirv-val`, regenerated SPIR-V C embed,
        helper rebuild, `c3c build --obj-out obj`, direct Vulkan smokes, and
        focused host `advanced-collections-module` `pass=1198 fail=0`.
      - negative constraint: do not implement Vulkan `Float64` scientific
        unary math by downcasting; Vulkan GLSL rejected the double
        transcendental builtins under the current Vulkan 1.0 validation path.
    - [x] Define and implement scalar `Float32` runtime values.
      - closed: scalar `Float32` now has a runtime value tag/payload,
        constructor, type id, `Number` parent, `type-of` / `is?` support,
        stdlib `float32?`, copy/promotion/env-copy leaf handling,
        print/string/format conversion, numeric comparison/equality/order
        integration, basic arithmetic policy, schema/tuple/JSON/CSV/AOT
        surfaces, and Tensor `Float32` `ref` / `Array` / `List` / `Iterator`
        / contract extraction now returns scalar `Float32` instead of
        widening to `Float64`.
      - validation: `c3c build --obj-out obj`; direct runtime smokes for
        constructor/type/predicate, arithmetic, closure capture, schema,
        format/sort-by, Tensor extraction, structural matrix extraction,
        contract extraction, and `matrix/trace`; focused
        `advanced-core-semantics` `pass=71 fail=0`; arithmetic-comparison
        `pass=47 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1205 fail=0`; primitive docs
        parity and Stage 3 source parity.
    - [x] Add CUDA `Float32` placement, destination `realize`, copyback, and
      cuBLAS contract support.
      - closed: CUDA helpers now expose `fill_f32`, `sgemm`, and `sgemv`;
        public placement, CPU copyback, destination `realize`, scalar fills,
        lazy contract destination writes, `tensor-backends` capability
        reporting, and matching `Float32` cuBLAS contract layouts are routed
        through the existing explicit-device Tensor surfaces.
      - preserved contract: CUDA `map`, CPU-destination-from-device behavior,
        and mixed CUDA dtypes remain fail-closed/deferred. Scalar `Float32`
        runtime values are covered by the completed scalar checkpoint above;
        zero-size CUDA contract identity/fill is covered by the completed
        CUDA zero-size checkpoint below.
      - validation: helper rebuild, `c3c build --obj-out obj`, focused host
        `advanced-collections-module` `pass=1218 fail=0`, bounded-container
        focused `advanced-collections-module` `pass=1205 fail=0`, primitive
        docs parity, Stage 3 source parity, and targeted `git diff --check`.
    - [x] Add CUDA elementwise `map` kernels for the explicit dense row-major
      binary scope.
      - closed: CUDA now has embedded-PTX CUDA Driver API elementwise binary
        map kernels for dense row-major `Float64` and `Float32` tensors.
        Supported operations are `+`, `-`, `*`, `/`, `min`, and `max`.
        Supported operand shapes are tensor/scalar, scalar/tensor, exact-shape
        tensor/tensor, and right-aligned singleton-axis tensor/tensor
        broadcasting.
      - shipped paths: direct public `map` on CUDA tensors, lazy map
        realization to CUDA, and CUDA destination realization from lazy CUDA
        maps. There is no hidden CPU fallback.
      - capability reporting: `tensor-backends` now reports
        `elementwise-map-float64` and `elementwise-map-float32` capability keys
        for CPU, Vulkan, and CUDA.
      - preserved residuals: unsupported callables, mixed CPU/CUDA operands,
        mixed dtype/device operands, unsupported layout, and scientific CUDA
        map remain explicit backend-unsupported/fail-closed cases. Arithmetic
        unary CUDA map is covered by the completed checkpoint below.
      - validation: helper compile, `c3c build --obj-out obj`, direct
        CUDA-gated smokes for scalar/exact-shape/broadcast binary map and
        fail-closed residuals, host focused `advanced-collections-module`
        `pass=1240 fail=0`; earlier exact/scalar validation also passed
        bounded-container `memory-lifetime-smoke` `pass=228 fail=0`,
        bounded-container focused `advanced-collections-module` `pass=1224
        fail=0`, primitive docs parity, and Stage 3 source parity.
    - [x] Add CUDA arithmetic unary `map` kernels for the dense row-major
      `Float64`/`Float32` helper family.
      - closed: CUDA now has separate embedded-PTX unary map kernels and C ABI
        helpers for op ids `0..4`: `abs`, unary `-`, `sqrt`, identity
        (`map +`, `real-part`, `conjugate` on real tensors), and zero-fill
        (`imag-part` on real tensors).
      - shipped paths: direct public `map`, lazy map realization, CUDA
        destination realization from lazy unary CUDA maps, and direct Tensor
        primitives `abs`, unary `-`, `sqrt`, `real-part`, `imag-part`, and
        `conjugate` preserve CUDA placement for eligible dense row-major
        `Float64`/`Float32` tensors.
      - superseded residual note: scientific CUDA map/direct unary math op ids
        `5..19` and foreign CUDA clone semantics are covered by later checkpoints
        below; unsupported callables, mixed CPU/CUDA operands, mixed dtype/device
        operands, and unsupported layouts remain explicit fail-closed cases.
      - validation: helper compile, full helper rebuild, `c3c build --obj-out
        obj`, CUDA-available direct smokes for `abs`, `sqrt`, `imag-part`,
        Float32 `sqrt`, CUDA destination realization from lazy unary map, and
        scientific fail-closed checks, host focused `advanced-collections-module`
        `pass=1256 fail=0`, bounded-container focused
        `advanced-collections-module` `pass=1243 fail=0`, primitive docs
        parity, Stage 3 source parity, and targeted `git diff --check`.
    - [x] Add CUDA scientific unary `map` kernels for dense row-major
      `Float64`/`Float32`.
      - closed: CUDA now has generated CUDA C/libdevice PTX kernels for unary
        scientific op ids `5..19`: `sin`, `cos`, `tan`, `asin`, `acos`,
        `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, `log10`, `math/erf`,
        `math/erfc`, and `stats/normal-cdf`.
      - shipped paths: public `map`, lazy CUDA map realization, CUDA
        destination realization from lazy scientific maps, and direct Tensor
        scientific primitives preserve CUDA placement for eligible dense
        row-major `Float64`/`Float32` tensors.
      - CPU Tensor `math/erf`, `math/erfc`, and `stats/normal-cdf` are also
        implemented: `Float64` and `Float32` inputs preserve real float dtype,
        `BigInteger` inputs return `Float64`, `BigFloat` inputs preserve
        `BigFloat`, and `BigComplex` Tensor inputs fail closed because no
        complex error-function or distribution Tensor contract is shipped.
      - capability reporting: `tensor-backends` now reports
        `scientific-map-float64` and `scientific-map-float32` separately from
        the existing arithmetic `elementwise-map-*` bits.
      - preserved boundaries: unsupported layouts, mixed CPU/CUDA operands,
        mixed dtype/device operands, and unsupported callables remain explicit
        fail-closed cases.
      - validation: CUDA C source generated PTX with `/usr/local/cuda-13.0/bin/nvcc
        --ptx -arch=compute_75`; generated PTX assembled with
        `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75`; helper compile; full
        helper rebuild; `c3c build --obj-out obj`; CUDA-gated smokes for
        capability reporting, Float64 `map sin`, Float32 direct `log10`, direct
        `sin`, family `acos`/`log10`, and destination `realize`; later
        `math/erf`/`math/erfc` validation: regenerated CUDA PTX/ptxas, helper
        compile and rebuild, `c3c build --obj-out obj`, CUDA smokes, host
        focused `advanced-collections-module` `pass=1271 fail=0`, and
        bounded-container focused `advanced-collections-module` `pass=1258
        fail=0` with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`;
        `stats/normal-cdf` validation added opcode-19 PTX regeneration,
        ptxas, helper compile/rebuild, `c3c build --obj-out obj`, direct
        CPU/CUDA/Vulkan-boundary smokes, host focused
        `advanced-collections-module` `pass=1282 fail=0`, bounded-container
        focused `advanced-collections-module` `pass=1269 fail=0`, primitive
        docs parity, Stage 3 source parity, and targeted `git diff --check`.
    - [ ] Broaden CUDA `map` beyond the landed dense row-major arithmetic and
      scientific `Float64`/`Float32` scope only after dedicated semantic and
      validation gates.
      - blocker/defer reason: unsupported layouts need explicit storage-span /
        base-offset / owner metadata before CUDA kernels can safely read
        non-dense views; unsupported callables still need explicit dispatch
        contracts. The current kernels intentionally cover dense row-major
        supported callables only.
      - closed hardening slice: mixed CPU/CUDA operands now fail with the
        explicit `map: CUDA operands must remain CUDA-placed` diagnostic, and
        mixed CUDA Tensor dtypes fail with `tensor/dtype-mismatch` instead of a
        generic backend kernel message. CUDA and Vulkan map probes now check for
        relevant device operands before realizing child expressions, so an
        unsupported mixed CPU/CUDA lazy operand is rejected before CPU lazy
        materialization can run. This preserves the no-hidden-transfer rule; it
        does not add mixed-device execution support.
      - next step: choose one residual family first: unsupported layout/view
        metadata, explicit mixed-device execution support, or callable extension
        beyond the fixed op-id table.
      - prerequisite state: keep generated CUDA scientific PTX and the existing
        embedded arithmetic PTX as separate capability-gated modules; keep
        `tensor-backends` `elementwise-map-*` and `scientific-map-*` reporting
        as the baseline for shipped kernels.
      - negative constraint: do not treat the landed CUDA map support as
        arbitrary-layout support, and do not reintroduce hidden CPU
        materialization for unsupported CUDA operands.
      - validation: CUDA direct smokes for the chosen residual family,
        no-implicit-transfer regressions, helper rebuild if helper ABI changes,
        `c3c build --obj-out obj`, focused host tests, and bounded-container
        focused advanced collections. Mixed-operand hardening validation:
        `c3c build --obj-out obj`; host focused `advanced-collections-module`
        `pass=1261 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1248 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Add CUDA-first Tensor rounding through a dtype-changing
      `Tensor BigInteger` result path.
      - shipped contract: direct `floor`, `ceiling`, `round`, and `truncate`
        over CUDA-placed dense row-major `Float64`/`Float32` tensors compute
        on CUDA, copy fixed-width integer results back through a status-bearing
        helper, and materialize native CPU `Tensor BigInteger` output.
      - coverage: advanced stdlib module assertions are gated by the CUDA
        `rounding-big-integer` backend capability, require `BigInteger` dtype,
        and verify values after explicit `to-device 'cpu` copyback via `ref`,
        `Array`, and `List`.
      - negative-memory constraint: do not add `floor` / `ceiling` / `round` /
        `truncate` to the CUDA unary opcode table unless the result dtype path
        changes in the same slice.
      - validation: CUDA `Float64`/`Float32` direct smokes passed for
        `floor`, `ceiling`, `round`, and `truncate`; non-finite status smoke
        returned the Tensor integer range diagnostic; `map floor` remains
        backend-unsupported; `./scripts/build_omni_chelpers.sh`; `c3c build
        --obj-out obj`; host focused `advanced-collections-module`
        `pass=1322 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1305 fail=0`; parity and targeted
        diff hygiene passed.
    - [x] Add Vulkan Tensor rounding through a dtype-changing
      `Tensor BigInteger` result path.
      - shipped contract: direct `floor`, `ceiling`, `round`, and `truncate`
        over Vulkan-placed dense row-major `Float64`/`Float32` tensors compute
        on Vulkan when `shaderInt64` is available, copy checked fixed-width
        integer results back through a status-bearing helper, and materialize
        native CPU `Tensor BigInteger` output.
      - coverage: advanced stdlib module assertions are gated by the Vulkan
        `rounding-big-integer` backend capability, require `BigInteger` dtype,
        and verify values after explicit `to-device 'cpu` copyback.
      - negative-memory constraint: do not expose Vulkan rounding as same-dtype
        float output, do not downcast or approximate `BigInteger` results, and
        do not treat generic Vulkan `available`/`float64`/`float32` as proof of
        dtype-changing rounding support.
      - validation: Vulkan rounding shader compile/`spirv-val`; helper
        rebuild; `c3c build --obj-out obj`; direct Vulkan `Float64`/`Float32`
        smokes for `floor`, `ceiling`, `round`, and `truncate`; overflow
        range diagnostic; direct `map floor` fail-closed diagnostic; host
        focused `advanced-collections-module` `pass=1326 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1309 fail=0`; parity and targeted diff hygiene passed.
    - [x] Add CPU Tensor `stats/normal-quantile` with an explicit
      probability-domain contract.
      - closed: CPU Tensor `stats/normal-quantile` now applies elementwise for
        `Float64`, `Float32`, and `BigFloat` tensors, fails the whole operation
        on the first invalid probability outside `0 < p < 1`, rejects
        `BigComplex`, and keeps `BigInteger` non-empty tensors fail-closed
        because integer probabilities cannot satisfy the open probability
        interval.
      - validation: direct CPU smokes and host focused
        `advanced-collections-module` `pass=1296 fail=0`.
    - [x] Define shared GPU probability-domain status handling for
      `stats/normal-quantile`.
      - closed: CUDA scientific unary op `20` now carries a device `uint32`
        status word for data-dependent probability validation. Status `0`
        means success, `1` means probability outside `0 < p < 1`, and `2`
        means non-finite input; higher status wins for mixed invalid tensors,
        so non-finite input takes priority over a domain endpoint. The helper
        maps nonzero status to the scalar-compatible whole-operation
        diagnostics before result exposure.
      - superseded boundary: Vulkan `Float32` `stats/normal-quantile` now has
        a separate status-bearing helper. Vulkan `Float64`
        `stats/normal-quantile` remains fail-closed unless a double
        inverse-CDF approximation/status helper is explicitly implemented.
      - negative-memory constraint: do not reuse the existing statusless Vulkan
        unary helper for any data-dependent quantile path; status-bearing
        helpers are required.
      - validation: CUDA valid/invalid probability smokes, `c3c build
        --obj-out obj`, focused advanced collections, and targeted
        `git diff --check`.
    - [x] Add CUDA Tensor `stats/normal-quantile` after the GPU domain-status
      ABI lands.
      - closed: CUDA `Float64`/`Float32` dense row-major Tensor
        `stats/normal-quantile` now routes through generated CUDA C/libdevice
        PTX op `20`, preserves CUDA placement and float dtype for direct and
        `map` paths, and uses libdevice `normcdfinv` after status-checked
        probability validation.
      - superseded boundary: no arbitrary GPU callable support, no hidden CPU
        fallback, and no unsupported layout support landed in this slice.
        Vulkan quantile is covered by the later Vulkan-specific
        status-bearing helpers for `Float32` and `Float64`.
      - negative-memory constraint: do not return `NaN` or backend success for
        invalid probabilities, and do not route through CPU fallback.
      - validation: `nvcc --ptx`, `ptxas`, helper C compile,
        `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
        direct CUDA valid/invalid smokes, host focused
        `advanced-collections-module` `pass=1307 fail=0`, and
        bounded-container focused `advanced-collections-module`
        `pass=1294 fail=0`.
    - [x] Add Vulkan `Float32` Tensor `stats/normal-quantile` after the shared
      GPU domain-status ABI is proven.
      - closed: dense row-major Vulkan `Float32` Tensor
        `stats/normal-quantile` now uses a dedicated inverse-CDF shader plus a
        `uint32` status buffer. Direct Tensor unary math and
        `map stats/normal-quantile` preserve Vulkan placement and `Float32`
        dtype for valid probabilities. Status `1` reports finite probability
        values outside `0 < p < 1`; status `2` reports non-finite probability,
        and `atomicMax` keeps non-finite status priority for mixed invalid
        tensors.
      - superseded boundary: Vulkan `Float64` quantile is covered by the later
        Float64-specific status-bearing helper; the existing statusless Vulkan
        unary helper still must not receive op `20`.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, direct Vulkan valid/domain/non-finite/
        mixed-invalid/Float64 fail-closed smokes, host focused
        `advanced-collections-module` `pass=1311 fail=0`, and bounded-container
        focused `advanced-collections-module` `pass=1298 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Resolve Vulkan `Float64` Tensor `stats/normal-quantile` with an
      explicit double inverse-CDF approximation/status helper.
      - closed: dense row-major Vulkan `Float64` tensors now route public
        `(map stats/normal-quantile ...)` and direct
        `(stats/normal-quantile ...)` through the Float64 Vulkan quantile
        helper, preserving Vulkan placement and `Float64` dtype.
      - regression coverage:
        `src/lisp/tests_advanced_stdlib_module_groups.c3` checks direct and
        `map stats/normal-quantile` placement/dtype/value preservation for
        `0.025`, `0.5`, and `0.975`, plus probability `0`/`1` and non-finite
        error messages without CPU fallback or Float32 downcast.
      - preserved contract: do not infer Float64 quantile support from Vulkan
        Float32 or CUDA, do not downcast Float64 tensors to Float32, and do not
        route through CPU fallback.
      - validation: `glslangValidator -V --target-env vulkan1.0
        csrc/tensor_vulkan_normal_quantile_f64.comp -o
        /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`; `spirv-val
        --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`;
        `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
        Vulkan direct/map Float64 quantile smokes plus domain/non-finite
        smokes; host focused `advanced-collections-module` `pass=1317 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1300 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Add Vulkan `Float32` `stats/normal-cdf` through the dedicated unary
      helper.
      - closed: dense row-major Vulkan `Float32` tensors now route public
        `(map stats/normal-cdf ...)` and direct `(stats/normal-cdf ...)`
        through fixed op id `19`, preserving Vulkan placement and `Float32`
        dtype with a documented same-dtype shader approximation.
      - validation: shader compile/`spirv-val`, helper rebuild, direct Vulkan
        smokes, and host focused `advanced-collections-module`
        `pass=1296 fail=0`.
    - [x] Resolve Vulkan `Float64` `stats/normal-cdf` with an explicit double
      approximation or an explicit fail-closed policy.
      - closed: dense row-major Vulkan `Float64` tensors now route public
        `(map stats/normal-cdf ...)` and direct `(stats/normal-cdf ...)`
        through fixed op id `19` in the existing two-buffer Float64 unary
        helper, preserving Vulkan placement and `Float64` dtype.
      - implementation note: the shader uses a bounded piecewise polynomial
        approximation over `|x| < 8` and saturates outside that range. The
        fitted approximation was sampled against the CPU double oracle with
        maximum absolute error below `6e-8`; checked-in tests use a stricter
        `1e-6` tolerance around representative values.
      - preserved contract: no hidden CPU fallback and no Float64-to-Float32
        downcast. Vulkan `Float64` `stats/normal-quantile` is covered by its
        later Float64 inverse-CDF/status helper.
      - validation: `glslangValidator -V --target-env vulkan1.0
        csrc/tensor_vulkan_map_unary_f64.comp -o
        /tmp/omni_tensor_vulkan_map_unary_f64.spv`; `spirv-val --target-env
        vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`;
        `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
        Vulkan direct/map Float64 CDF smokes; Float64 quantile fail-closed
        smoke; host focused `advanced-collections-module` `pass=1313 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1300 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Define clone semantics for arbitrary foreign CUDA tensor payloads
      before accepting them across concrete clone/copy boundaries.
      - closed: valid foreign CUDA concrete payloads now clone by allocating a
        fresh Omni-owned CUDA buffer, copying device-to-device from the source
        handle, and installing `tensor_cuda_device_finalizer` on the clone. The
        source foreign finalizer remains the sole authority for the source
        handle.
      - preserved boundary: fake, stale, malformed, or otherwise invalid CUDA
        handles still fail closed rather than being accepted as cloneable
        storage.
      - regression coverage: the memory-lifetime suite covers the invalid fake
        CUDA handle path and a real foreign-finalizer CUDA handle path that
        round-trips copied values, uses a distinct clone handle, and verifies
        source finalizer ownership.
      - validation: `c3c build --obj-out obj`; bounded-container
        `memory-lifetime-smoke` `pass=229 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; host focused
        `advanced-collections-module` `pass=1258 fail=0`.
    - [x] Add explicit zero-size CUDA contract identity/fill support before
      relaxing CUDA zero-dimension contract gates.
      - closed: CUDA-placed dense row-major matching `Float64` or matching
        `Float32` rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2
        single-axis contractions now preserve zero-free outputs as
        CUDA-placed zero-length tensors and fill non-empty zero-contracted
        outputs with the dtype-preserving additive identity. The zero-fill
        path skips cuBLAS and therefore also succeeds when cuBLAS is forced
        unavailable for tests.
      - validation: `c3c build --obj-out obj`; direct CUDA availability-gated
        smokes for `Float64` zero-contracted rank-2, `Float32`
        zero-contracted rank-2 with scalar dtype preservation, zero-free
        rank-2 output preservation, and rank-1/rank-2 zero-contracted fill.
        Host focused `advanced-collections-module` was attempted but remained
        idle past ten minutes and was terminated as inconclusive;
        bounded-container focused `advanced-collections-module` passed
        `pass=1210 fail=0`.
    - [x] Add CUDA rank-1/rank-1 dot support for the existing cuBLAS contract
      family.
      - closed: CUDA-placed contiguous matching `Float64` or matching
        `Float32` rank-1/rank-1 single-axis contractions now return
        CUDA-placed scalar Tensor outputs. The implementation reuses the
        existing cuBLAS GEMV helper by treating the left vector as a one-row
        row-major matrix, so no new CUDA helper ABI was required.
      - preserved contract: zero-size rank-1 dots use the CUDA additive
        identity fill path; nonzero rank-1 dots still fail unavailable when
        cuBLAS is forced unavailable; CUDA `map`, mixed devices/dtypes,
        multi-axis CUDA contract, and zero-axis rank-1 outer product remain
        outside this slice.
      - validation: `c3c build --obj-out obj`; direct CUDA availability-gated
        smokes for `Float64` rank-1 dot, explicit-axis rank-1 dot, `Float32`
        rank-1 dot with scalar dtype preservation, zero-size rank-1 dot
        identity, rank-0 destination realization for `Float64` rank-1 dot,
        and rank-0 destination realization for `Float32` rank-1 dot;
        bounded-container focused `advanced-collections-module` passed
        `pass=1217 fail=0`.
    - [x] Decide and implement any non-CPU destination `realize` /
      destination-copy contract explicitly.
      - shipped contract: destination-form `realize` stays backend-neutral and
        now supports existing dense row-major CUDA and Vulkan `Float64` or
        `Float32` destinations for matching same-backend sources, CPU sources,
        supported lazy same-backend results, and scalar fills; CPU destinations
        still require explicit `to-device 'cpu` for device sources.
      - validation: public CPU/CUDA/Vulkan destination-copy tests, direct
        smokes, helper rebuild, `c3c build --obj-out obj`, host focused
        advanced collections, docs parity, Stage 3 source parity, and targeted
        `git diff --check` passed for this checkpoint. Bounded-container
        focused advanced collections also passed after the CUDA addition.
    - [ ] Reopen full blocked/tiled trailing-update LU only if new
      measurements justify it as a performance lane.
      - blocker/defer reason: current measured parallel solve baseline routes
        `n >= 65` through staged multi-dispatch solve; blocked trailing-update
        LU is not required for correctness closure.
      - next step: collect comparative measurements showing the existing
        staged route is the bottleneck before broadening algorithmic scope.
      - validation: checked-in measurement artifact, no-LAPACK solve tests,
        focused host plus bounded-container advanced collections tests.
    - [ ] Run broad/heavy bounded-container validation before closing the whole
      `TENSOR-100F` parent.
      - blocker/defer reason: recent source-level slices ran focused host and
        bounded-container validation, not full heavy/container-only gates. The
        scalar `Float32` slice now has a bounded focused
        `advanced-collections-module` pass, but parent-level closure still
        needs the broader Docker-bound suite required by repo policy.
      - next step: when the parent is otherwise ready to close, run the
        Docker-bound suite required by repo policy rather than host-heavy
        execution.
      - validation: record the exact bounded-container command, pass/fail
        counts, and any unresolved failures in this TODO or the session report.
    - [ ] Fix the validation Docker image C3 compiler architecture on arm64
      hosts.
      - blocker/defer reason: `scripts/build_validation_image.sh` currently
        rebuilds an arm64 Ubuntu image but `docker/validation.Dockerfile`
        downloads `c3-linux.tar.gz`, whose `/opt/c3/c3c` is x86-64. Running the
        container path without a host toolchain mount fails before tests with
        `/usr/local/bin/c3c: cannot execute binary file: Exec format error`.
      - next step: update the validation Dockerfile/build script to select a
        native arm64 C3 compiler artifact or build C3 from source for arm64;
        until then run bounded validation with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
      - validation: `docker run --rm --entrypoint /bin/sh
        omni-validation:2026-03-10 -lc 'uname -m; file /opt/c3/c3c'` must show
        matching arm64/aarch64, then rerun `scripts/run_validation_container.sh`
        without `OMNI_VALIDATION_TOOLCHAIN_ROOT`.
  - shipped `TENSOR-100F1` slice:
    - factored shared trailing-status copyback for serial Vulkan matrix
      helpers;
    - added dense row-major `Float64` Vulkan `matrix/cholesky`, returning a
      Vulkan-placed lower factor and raising `tensor/not-positive-definite`
      for nonsymmetric or non-SPD inputs without calling LAPACK;
    - added dense row-major `Float64` Vulkan `matrix/qr`, returning
      Vulkan-placed `q` and `r` reduced factors and raising
      `tensor/singular-matrix` for rank-deficient inputs without calling
      LAPACK;
    - added dense row-major Vulkan `Float64` `matrix/singular-values`,
      returning a Vulkan-placed rank-1 result. The helper now uses
      storage-buffer Gram scratch behind the public output payload, so
      `k = min(rows, columns) > 64` is supported without hidden CPU/LAPACK
      fallback while preserving 32-bit shader index guards;
    - routed Vulkan `matrix/norm` `'spectral` and `'nuclear` through the same
      singular-value helper, reading back only the scalar norm result;
    - fixed the CPU pure Cholesky fallback to accept empty square tensors.
  - shipped unary `+` / unary `abs` / unary `-` / unary `sqrt` Vulkan slice:
    - added a separate two-buffer `Float64` unary map shader/helper for
      identity, absolute value, negation, and square root instead of reviving the
      invalidated generic map mode-3 branch;
    - routed public `(map + <vulkan-tensor>)` through the identity opcode for
      dense row-major Vulkan-placed `Float64` tensors, and made direct
      `(+ <tensor>)` the Tensor identity form that preserves placement;
    - routed public `(map abs <vulkan-tensor>)` and direct
      `(abs <vulkan-tensor>)`, public `(map - <vulkan-tensor>)`, and direct
      `(- <vulkan-tensor>)` through that helper for dense row-major
      Vulkan-placed `Float64` tensors;
    - routed public `(map sqrt <vulkan-tensor>)` and direct
      `(sqrt <vulkan-tensor>)` through that helper for dense row-major
      Vulkan-placed `Float64` tensors;
    - routed public `(map real-part <vulkan-tensor>)`,
      `(map imag-part <vulkan-tensor>)`, `(map conjugate <vulkan-tensor>)`,
      and direct Tensor `real-part` / `imag-part` / `conjugate` through the
      helper for real-valued Vulkan `Float64` tensors;
    - routed public `(map min <vulkan-tensor> scalar)`,
      `(map max scalar <vulkan-tensor>)`, and direct Tensor `min` / `max`
      through the existing binary map helper for dense row-major Vulkan
      `Float64` tensor/scalar and tensor/tensor broadcast cases.
  - shipped Vulkan destination-form `realize` slice:
    - kept the public `(realize source destination)` API backend-neutral;
    - added existing-buffer Vulkan host-to-device, device-to-device, and
      scalar `Float64` fill helpers for destination realization;
    - supports matching dense row-major `Float64` CPU sources, Vulkan sources,
      lazy Vulkan results, and scalar fills into existing Vulkan destinations;
    - preserved CPU destination fail-closed behavior for device sources unless
      the caller explicitly copies with `to-device 'cpu`.
  - shipped scalar unary `-` surface fix:
    - fixed the documented scalar unary `-` surface by registering `-` as a
      primitive that accepts one or two arguments, while `prim_sub` still
      rejects extra arguments itself;
    - direct CPU Tensor unary `-` now preserves native Tensor dtype for
      `Float64`, `BigInteger`, `BigFloat`, and `BigComplex`;
  - shipped Vulkan helper factoring proof slice:
    - reused the existing compute-pipeline creation helper across the serial
      two-buffer, three-buffer, one-input/two-output, and one-input/three-output
      Vulkan dispatch helpers;
    - left descriptor set layout, pool allocation, command recording, and
      status-copy ABI unchanged for follow-up factoring.
    - direct Tensor unary math on Vulkan now fails closed for unsupported
      operations instead of materializing hidden CPU results;
    - results remain Vulkan-placed and require explicit `(to-device ... 'cpu)`
      for CPU inspection;
    - unsupported Vulkan unary callables such as `sin` still fail closed with
      `tensor/backend-unsupported`.
  - shipped audit-hardening slice:
    - added public zero-size Vulkan `matrix/norm` `'spectral` / `'nuclear`
      regressions for empty rows and empty columns, with unchanged `dgesvd`
      counter coverage;
    - added unchanged LAPACK counter guards for direct and lazy Vulkan
      fail-closed `matrix/eigenpairs`; earlier `matrix/svd`,
      `matrix/eigenvalues`, and `matrix/eigenvectors` fail-closed guards are
      superseded by the direct Vulkan slices below;
    - added diagnostic-order regressions proving invalid-but-Vulkan SVD and
      general eigenpair operands return `tensor/backend-unsupported` before CPU
      shape diagnostics;
    - added internal lazy-contract helper coverage for zero-axis and multi-axis
      Vulkan contractions;
    - updated the generic Vulkan contract failure context from stale
