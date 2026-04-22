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
    - [x] `TENSOR-100H-COMPLEX-EIGEN` settle and implement fixed-width
      complex eigen/eigenpair contracts.
      - closed/superseded: the fixed-width eigen contract is now frozen in
        `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md` and split
        into concrete tracked subitems in `docs/todo_parts/todo_part_01.md`.
      - shipped slices: CPU `Float64` `matrix/eigenpairs` now returns
        `Complex128` values/vectors; CPU Hermitian `Complex128`/`Complex64`
        `matrix/eigenvalues` and `matrix/eigenvectors` are implemented; CPU
        general `Float32`/`Complex128`/`Complex64` `matrix/eigenpairs` is
        implemented; Vulkan Hermitian `Complex128`/`Complex64`
        `matrix/eigenvalues`/`matrix/eigenvectors` is implemented.
      - remaining work: continue with
        `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` for backend-native
        non-Hermitian Vulkan complex `matrix/eigenpairs` after recording the
        solver design boundary.
      - validation: see the closed subitems in
        `docs/todo_parts/todo_part_01.md`; latest focused host
        `advanced-collections-module` passed `1975/0`.
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
    - [x] Land CUDA explicit-copy materialization for CPU-backed transpose
      views before broader view-aware GPU execution.
      - closed: explicit `to-device 'cuda` and destination-form `realize`
        into CUDA tensors now allow CPU-backed `matrix/transpose-view` sources
        by materializing them to dense CPU tensors at the public copy boundary
        before using the existing CUDA dense-storage copy helpers.
      - shipped contract: this is explicit copy-boundary materialization only.
        It does not make CUDA kernels, CUDA-backed views, arbitrary strided
        layouts, or raw backend helpers consume offset/stride metadata.
      - validation: `scripts/check_file_size_gate.sh`; `c3c build --obj-out
        obj`; host focused `advanced-collections-module` passed `1980/0`,
        including fail-closed coverage for Vulkan-backed transpose views sent
        to CUDA.
    - [x] `TENSOR-100F-STRIDE-AWARE-HELPERS-001` add explicit stride-aware Vulkan/CUDA kernel-helper support after
      the read-only CPU transpose-view and explicit materialization contracts
      are validated.
      - closed: the first CUDA stride-aware helper boundary is now shipped for
        zero-offset strided Tensor operands in the CUDA binary-map helper ABI.
        The helper validates storage span, builds per-output operand offset
        tables when operand strides differ from dense output strides, and
        preserves fail-closed behavior for unsupported device/layout cases.
      - shipped contract: `matrix/transpose-view` over CUDA-backed
        `Float64`/`Float32`/fixed-complex tensors produces CUDA read-only view
        metadata when the source is zero-offset dense row-major storage. CUDA
        binary map can consume those zero-offset strided CUDA view operands
        without hidden CPU/GPU materialization.
      - explicit non-contract: raw CUDA view materialization to CPU and CUDA
        unary/scientific map over strided view operands remain fail-closed
        because those helper ABIs do not yet accept stride metadata.
      - validation: helper rebuild; `c3c build --obj-out obj`; direct CUDA
        transpose-view/map probes; focused host `advanced-collections-module`
        passed `2004/0`; bounded-container focused
        `advanced-collections-module` passed `1973/0`.
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
    - [x] `TENSOR-100F-CUDA-MAP-BROADEN-001` broaden CUDA `map` beyond the landed dense row-major arithmetic and
      scientific `Float64`/`Float32` scope only after dedicated semantic and
      validation gates.
      - closed: the open layout/view residual is resolved for the first
        semantic boundary. CUDA `map` now keeps zero-offset CUDA
        transpose-view operands on device for supported binary arithmetic/min/max
        ops, including scalar/tensor and tensor/tensor cases, and the lazy map
        realization path preserves the same behavior.
      - shipped contract: the broadened CUDA map surface is still capability
        gated by `elementwise-map-float64`, `elementwise-map-float32`, and
        fixed-complex map capabilities. It does not add mixed-device execution,
        arbitrary nonzero-offset views, unsupported callables, or hidden CPU
        materialization.
      - closed hardening slice: mixed CPU/CUDA operands now fail with the
        explicit `map: CUDA operands must remain CUDA-placed` diagnostic, and
        mixed CUDA Tensor dtypes fail with `tensor/dtype-mismatch` instead of a
        generic backend kernel message. CUDA and Vulkan map probes now check for
        relevant device operands before realizing child expressions, so an
        unsupported mixed CPU/CUDA lazy operand is rejected before CPU lazy
        materialization can run. This preserves the no-hidden-transfer rule; it
        does not add mixed-device execution support.
      - future reopen rule: open a new item, not this closed residual, only for
        a concrete next family such as nonzero storage offsets, unary/scientific
        strided CUDA map, mixed-device execution, or callable extension beyond
        the fixed op-id table.
      - prerequisite state: keep generated CUDA scientific PTX and the existing
        embedded arithmetic PTX as separate capability-gated modules; keep
        `tensor-backends` `elementwise-map-*` and `scientific-map-*` reporting
        as the baseline for shipped kernels.
      - negative constraint: do not treat the landed CUDA map support as
        arbitrary-layout support, and do not reintroduce hidden CPU
        materialization for unsupported CUDA operands.
      - validation: helper rebuild; `c3c build --obj-out obj`; direct CUDA
        transpose-view layout/map probes; focused host
        `advanced-collections-module` `pass=2004 fail=0`; bounded-container
        focused `advanced-collections-module` `pass=1973 fail=0`.
        Mixed-operand hardening validation: `c3c build --obj-out obj`; host focused
        `advanced-collections-module` `pass=1261 fail=0`; bounded-container
        focused `advanced-collections-module` `pass=1248 fail=0` with
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
    - [x] `TENSOR-100F-LU-BLOCKED-PERF-001` reopen full blocked/tiled trailing-update LU only if new
      measurements justify it as a performance lane.
      - closed 2026-04-22: `scripts/run_vulkan_math_perf_probe.sh` now includes
        staged Vulkan solve fixtures. The measured `Float64` route does not
        justify reopening full blocked/tiled trailing-update LU: 65x65 identity
        measured 323 ms, 65x65 `I + ones` measured 304 ms, 128x128 identity
        measured 303 ms, 128x128 `I + ones` measured 326 ms, and 192x192
        identity measured 314 ms.
      - future reopen rule: open a new item, not this closed residual, only if
        repeated larger solve measurements at a named size show the staged
        route is the bottleneck and a blocked trailing-update LU design has a
        clear correctness/placement validation path.
      - validation: checked-in measurement artifact, no-LAPACK solve tests,
        focused host plus bounded-container advanced collections tests.
    - [x] `TENSOR-100F-BROAD-VALIDATION-001` run broad/heavy bounded-container validation before closing the whole
      `TENSOR-100F` parent.
      - closed 2026-04-22: broad validation ran through the bounded container
        gate before parent closure.
      - repair surfaced by the broad run: `allocator-validation` first exposed
        that `ast_arena_alloc` continued allocating after the current chunk was
        corrupt (`used > capacity`); the allocator now fails closed in that
        state before adding another chunk.
      - repair surfaced by the broad run: the `advanced` slice exposed that the
        JIT tail-position `List` / `Array` constructor shortcut bypassed
        one-argument conversion validation for improper lists and malformed
        iterator tails; one-argument calls now route through the primitive
        constructor semantics while variadic calls keep the tail fast path.
      - validation: bounded-container `allocator-validation` passed
        `pass=1 fail=0`; bounded-container `advanced` passed
        `pass=3377 fail=0`; `scripts/run_validation_container.sh
        scripts/run_global_gates.sh` passed the file-size gate, normal build,
        all configured normal lisp slices, compiler slice, and FTXUI smokes.
      - ASAN note: the current C3 toolchain reports address sanitizer
        unsupported for this target, so `scripts/run_global_gates.sh` now
        records that condition as an explicit ASAN skip instead of continuing
        into mislabeled ASAN tests.
      - diagnostic classification: `memory-lifetime-smoke` emits boundary
        graph-audit diagnostic lines while returning `fail=0`; this is now
        classified under `MEM-GRAPH-AUDIT-DIAGNOSTIC-20260422-001` as expected
        output from the passing negative regression
        `root splice debug audit rejects releasing temp edge`, not a Vulkan math
        baseline blocker.
    - [x] Fix the validation Docker image C3 compiler architecture on arm64
      hosts.
      - closed 2026-04-22: `docker/validation.Dockerfile` now keeps the
        checked C3 release tarball path for `amd64`/`x86_64` and builds C3
        from the checked source tarball on `arm64`/`aarch64`, because the C3
        release does not publish a Linux arm64 binary tarball.
      - shipped contract: the validation image now pins C3 `v0.7.11`, matching
        the repo's active host compiler contract, and installs LLVM/LLD/Polly
        19 for the arm64 source-build path.
      - negative-memory constraint: do not use Android aarch64 C3 release
        artifacts in the Ubuntu validation image; they are not a native Linux
        toolchain.
      - validation: `scripts/build_validation_image.sh` passed on an arm64
        Docker host; `docker run --rm --entrypoint /bin/sh
        omni-validation:2026-03-10 -lc 'uname -m; file /opt/c3/c3c; c3c -V'`
        reported `aarch64`, C3 `0.7.11`, LLVM `19.1.1`, and
        `aarch64-unknown-linux-gnu`; `scripts/run_validation_container.sh c3c
        build --obj-out obj` passed without `OMNI_VALIDATION_TOOLCHAIN_ROOT`.
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
