## 2. Data Types

### 2.1 Core Types

| Type | Tag | Description | Example |
|------|-----|-------------|---------|
| nil | `NIL` | Empty / absence value | `nil`, `()` |
| int | `INT` | 64-bit signed integer | `42`, `-17` |
| Float64 | `DOUBLE` | 64-bit floating point | `3.14`, `-0.5` |
| BigInteger | `BIG_INTEGER` | Arbitrary-precision exact integer | `(BigInteger "9223372036854775808")` |
| BigFloat | `BIG_FLOAT` | High-precision decimal float | `(BigFloat "1.25")` |
| BigComplex | `BIG_COMPLEX` | High-precision decimal complex value | `(BigComplex 1 2)` |
| Complex128 | `COMPLEX128` | Fixed-width complex value with two Float64 components | `(Complex128 1 2)` |
| Complex64 | `COMPLEX64` | Fixed-width complex value with two Float32 components | `(Complex64 1 2)` |
| string | `STRING` | Immutable string (heap-allocated) | `"hello"` |
| symbol | `SYMBOL` | Interned identifier | `'foo`, `'hello` |
| cons | `CONS` | Pair / list cell | `(cons 1 2)`, `'(1 2 3)` |
| closure | `CLOSURE` | User-defined function with environment | `(lambda (x) x)` |
| continuation | `CONTINUATION` | Captured delimited continuation | via `capture` |
| primitive | `PRIMITIVE` | Built-in function | `+`, `car` |
| partial | `PARTIAL_PRIM` | Explicit partially applied primitive | `(partial + 3)` |
| error | `ERROR` | Error value | `(error "oops")` |
| Dictionary | `HASHMAP` | Mutable hash table | `{'a 1}`, `(Dictionary 'a 1)` |
| Array | `ARRAY` | Mutable dynamic array | `[1 2 3]`, `(Array 1 2 3)` |
| Coroutine | `COROUTINE` | User-level coroutine | `(Coroutine (lambda () body))` |
| Tensor | `TENSOR` | Homogeneous n-dimensional numeric storage | `(Tensor [[1 2] [3 4]])` |
| void | `VOID` | Singleton no-result value | `#<void>` |
| ffi-handle | `FFI_HANDLE` | Foreign library handle | `(define [ffi lib] libc "libc.so.6")` |
| instance | `INSTANCE` | User-defined type instance | `(Point 3 4)` |
| method-table | `METHOD_TABLE` | Multiple dispatch table | internal |

Omni does not currently define a builtin `Empty`/bottom type. `nil`/`Nil`
cover the language's empty/false value, while `Void` is now a real builtin
singleton type/value used by FFI `^Void` returns as well.

`Integer` is the fixed-width signed integer surface. `BigInteger` is the
arbitrary-precision exact integer surface backed by Boost.Multiprecision in the
current runtime. `+`, `-`, `*`, `abs`, `gcd`, `lcm`, and the `long.min / -1`
division overflow case promote overflowing `Integer` results to `BigInteger`.
`/`, `%`, ordering comparisons, `min`, and `max` support `BigInteger` values;
bitwise operations support exact `Integer`/`BigInteger` operands. `parse-number`
returns fixed-width `Integer` values when the decimal input fits and promotes
valid wider decimal integers to `BigInteger`.
Integer source literals cover the full signed fixed-width range, including
`-9223372036854775808`; positive overflow and negative underflow still fail at
lex time.
Use `(BigInteger "...")` when an explicit arbitrary-precision integer
constructor is needed.

`BigFloat` is the high-precision decimal float surface backed by
Boost.Multiprecision in the current runtime. `BigFloat` values are `Number`
values, support `String` and finite `Float64` conversion, participate in `+`,
`-`, `*`, `/`, ordering comparisons, `abs`, `min`, and `max`, and preserve
`BigFloat` results when mixed with `Integer`, `BigInteger`, or `Float64`.
Core scalar math primitives preserve `BigFloat` results when a `BigFloat`
operand participates for trigonometric, inverse trigonometric, hyperbolic,
exponential, logarithmic, power/root, gamma/error-function, and
standard-normal distribution helpers. `floor`, `ceiling`, `round`, and
`truncate` round `BigFloat` exactly to `Integer` or `BigInteger` when the
integer result is inside the supported allocation cap; huge integer
materializations fail closed instead of narrowing through `Float64`.
Fixed-width `Float64` remains the result type for non-`BigFloat` floating
inputs.
`parse-number` promotes syntactically valid floating inputs that overflow
`Float64` to `BigFloat`.

`BigComplex` is the high-precision complex-number surface backed by decimal
real and imaginary parts in the current runtime. `(BigComplex real imag)`
constructs a complex value from non-complex numeric parts; a one-argument
constructor creates a zero-imaginary value from a number or finite decimal
string. BigComplex values are `Number` values, support `String`, `+`, `-`,
`*`, `/`, unary `-`, `=`, hashing/equality, and scope-boundary copy/promotion.
`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
`log`, `log10`, `sqrt`, and `pow` preserve BigComplex results when a complex
operand participates. `real-part` and `imag-part` return `BigFloat`
components for BigComplex inputs, `imag-part` returns `0` for real scalar
inputs, and `conjugate` preserves real scalars while flipping a BigComplex
imaginary sign. `abs` returns a `BigFloat` magnitude. `atan2` remains a
real-plane helper and rejects complex operands. Complex values are intentionally
not ordered, so `<`, `>`, `<=`, `>=`, `min`, `max`, `positive?`, and
`negative?` fail closed for BigComplex operands.

`Complex128` and `Complex64` are fixed-width complex-number surfaces.
`(Complex128 real [imag])` constructs a complex value from finite numeric
parts representable as `Float64`; `(Complex64 real [imag])` constructs from
finite numeric parts representable as `Float32`. The optional imaginary part
defaults to zero. Fixed-width complex values are `Number` values, support
`String`, `+`, `-`, `*`, `/`, unary `-`, `=`, hashing/equality, and
scope-boundary copy/promotion. `real-part` and `imag-part` return `Float64`
components for `Complex128` and `Float32` components for `Complex64`;
`conjugate` preserves the fixed-width complex dtype and `abs` returns the
matching floating magnitude. Scientific/transcendental scalar math beyond
these component/arithmetic helpers remains fail-closed for fixed-width complex
until a dedicated complex approximation contract is defined.

`Tensor` is the canonical rank-polymorphic scientific numeric aggregate. The
current runtime slice registers the type descriptor, constructor, print
surface, lifetime copy/promotion paths, tensor `ref`, and introspection
primitives (`tensor?`, `dtype`, `shape`, `rank`, `length`, and
`tensor-layout`). The explicit
constructor surfaces are `(Tensor Float64 shape data-or-scalar)`,
`(Tensor Float32 shape data-or-scalar)`,
`(Tensor BigInteger shape data-or-scalar)`,
`(Tensor BigFloat shape data-or-scalar)`,
`(Tensor BigComplex shape data-or-scalar)`,
`(Tensor Complex128 shape data-or-scalar)`, and
`(Tensor Complex64 shape data-or-scalar)`, where `shape` is an array or
proper list of non-negative integers and `data-or-scalar` is either a scalar
numeric fill value or an array/proper list with exactly the shape product's
element count. The inferred-shape constructor surface is `(Tensor data)`,
`(Tensor data dtype)`, or `(Tensor dtype data)`, where `dtype` is `Float64`,
`Float32`, `BigInteger`, `BigFloat`, `BigComplex`, `Complex128`, or
`Complex64`. `Float64` and `Float32` tensors accept real numeric values that
can narrow to the target finite float width; `BigInteger` tensors accept exact
`Integer` and `BigInteger` values; `BigFloat` tensors preserve arbitrary finite
`BigFloat` range; `BigComplex` tensors preserve complex numeric values and
promote real numeric leaves to zero-imaginary BigComplex elements;
`Complex128` and `Complex64` tensors preserve fixed-width complex numeric
values and promote representable real numeric leaves to zero-imaginary
fixed-width complex elements. Tensor
`ref` uses `(ref tensor index-array)`. Constructor dispatch is the public
materialization model: `(Array iterator)` and `(List iterator)` consume
iterators, `(Tensor iterator)` consumes a finite numeric iterator into tensor
storage, and `(Array tensor)` / `(List tensor)` convert tensor values to flat
row-major element collections. Use `shape` when rank metadata is needed.
`realize` remains a low-level Tensor storage primitive for exact destination
writes and compatibility with existing tensor-storage tests; it is not the
general lazy-computation counterpart to iterators. Unary `+` is the identity
operation for Tensor values and preserves the Tensor's current placement.
Tensor-dispatched `map` is
the elementwise tensor operation for `Float64`, `Float32`, `BigInteger`,
`BigFloat`, `BigComplex`, `Complex128`, and `Complex64` tensors; `contract` is
the summed-axis operation for `Float64`, `Float32`, `BigInteger`, `BigFloat`,
`BigComplex`, `Complex128`, and `Complex64` tensors. For real
Tensor dtypes, `real-part` and `conjugate` preserve dtype and values, while
`imag-part` returns a same-shape zero tensor of the same dtype. For
`BigComplex` tensors, `real-part` and `imag-part` produce `BigFloat` tensors
and `conjugate` produces a `BigComplex` tensor. For `Complex128` tensors,
`real-part`, `imag-part`, and `abs` produce `Float64` tensors; for `Complex64`
tensors they produce `Float32` tensors. `conjugate` preserves fixed-width
complex Tensor dtype. Tensor `abs` applies elementwise magnitude; real Tensor
dtypes preserve dtype and complex tensors return same-shape component-width
floating tensors. Tensor `sqrt` applies
elementwise square root; `Float64` and `BigInteger` Tensor inputs return
`Float64` tensors, `BigFloat` tensors preserve `BigFloat`, and `BigComplex`
tensors preserve `BigComplex`. Tensor `sin`, `cos`, `tan`, `asin`, `acos`,
`atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, and `log10` apply the same
elementwise unary scientific-math contract: `Float64` and `BigInteger` inputs
return `Float64` tensors, while `BigFloat` and `BigComplex` tensors preserve
their dtype. Tensor `pow` supports tensor-scalar, scalar-tensor, and broadcast
tensor-tensor powers: `BigComplex` wins the result dtype if either input is
complex, otherwise `BigFloat` wins if either input is BigFloat, otherwise the
result is a `Float64` tensor. Tensor `atan2` supports tensor-scalar,
scalar-tensor, and broadcast tensor-tensor real-plane arctangent: `BigFloat`
inputs preserve `BigFloat`, other real/exact inputs return `Float64` tensors,
and complex Tensor operands fail closed. Fixed-width complex Tensor operands
also fail closed for Tensor scientific/transcendental operations that do not
have an explicit component-width complex contract. Tensor `floor`, `ceiling`, `round`,
and `truncate` return same-shape `BigInteger` tensors for real inputs, using
exact BigFloat rounding when the source dtype is `BigFloat`; complex Tensor
operands fail closed. GPU support for these rounding primitives must preserve
that dtype-changing contract; it must not expose them as same-dtype floating
Tensor map results. CUDA and Vulkan dense row-major `Float64`/`Float32`
support is reported with the backend `rounding-big-integer` capability and
materializes CPU `Tensor BigInteger` output after checked device integer
copyback. Vulkan rounding is additionally gated by Vulkan integer shader
support. Generic Vulkan `available`, `float64`, or `float32` capability is not
sufficient proof of dtype-changing rounding.
Tensor `min` and `max` support the
same tensor-scalar, scalar-tensor, and broadcast tensor-tensor real comparison surface:
`BigFloat` wins if either input is BigFloat, `Float64` wins if either input is
Float64, otherwise the result is a `BigInteger` tensor. Complex Tensor
operands fail closed. Tensor `gcd` and `lcm` support tensor-scalar,
scalar-tensor, and broadcast tensor-tensor exact integer operations. Tensor
operands must be native `BigInteger` tensors, scalar operands must be exact
integers, and results are native `BigInteger` tensors.
`matrix/transpose` transposes rank-2 Tensor values, preserving native dtype
and cloning owned high-precision element handles for `BigInteger`, `BigFloat`,
and `BigComplex` tensors. For ordinary concrete Tensor inputs it remains a
materializing transform. If the input is already a transpose view,
`matrix/transpose` composes structurally by returning the source orientation
rather than materializing a second transposed buffer. `matrix/transpose-view`
constructs an explicit read-only rank-2 transpose view over a source Tensor.
The view swaps logical shape and strides, keeps the source Tensor as the view
owner, reports `owner` `view-source`, `owns-storage` false, and uses
`write-policy` `read-only-view`. CPU `ref`, `(Array view)`, `(List view)`,
and CPU `realize` materialization observe the transposed logical indexing.
Mutation and destination writes through the view fail closed. GPU and raw
contiguous copy kernels do not consume arbitrary view offset/stride metadata.
Vulkan supports the explicit `matrix/transpose-view` materialization boundary:
direct rank-2 transpose views over dense zero-offset Vulkan storage can be
materialized into dense Vulkan tensors by `realize`, `to-device 'vulkan`, and
`to-device 'cpu` copyback. CUDA and all broader strided/view-backed GPU kernel
execution still fail closed with Tensor backend diagnostics. `matrix/diagonal`
extracts the main diagonal from a
rank-2 Tensor into a rank-1 Tensor of length `min(rows, columns)`, preserving
native dtype and cloning owned high-precision element handles. `matrix/trace`
sums the diagonal of a square rank-2 Tensor and returns a scalar in the
Tensor's public scalar family: `Float64` for `Float64` and `Float32` Tensor
storage, or `BigInteger`, `BigFloat`, and `BigComplex` for the matching
high-precision Tensor dtypes.
`matrix/diagonal-matrix` builds a square rank-2 Tensor from a rank-1 Tensor,
placing cloned input values on the main diagonal and the dtype's zero value in
off-diagonal cells. `matrix/identity` builds a square identity Tensor from a
non-negative integer size, defaulting to `Float64` and accepting an optional
`Float64`, `Float32`, `BigInteger`, `BigFloat`, or `BigComplex` dtype
argument. `matrix/rank` returns the numerical rank of a rectangular rank-2
`Float64`, `Float32`, `Complex128`, or `Complex64` Tensor as an `Integer`,
using a default tolerance of `1e-12` or an optional non-negative finite numeric
tolerance. Complex rank uses magnitude-based pivoting. `matrix/norm` returns a
`Float64` scalar norm for rank-2 `Float64`, `Float32`, `Complex128`, or
`Complex64` Tensors. It defaults to the Frobenius norm and accepts an optional
selector: `'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, or
`'nuclear`. `Float64`, `Float32`, `Complex128`, and `Complex64` support every
selector. `Float32` spectral/nuclear selectors use native `Float32`
singular-value kernels on CPU and eligible Vulkan placement; complex
spectral/nuclear selectors use fixed-width complex singular values while direct
complex selectors use complex magnitudes. `matrix/solve` solves rank-2
`Float64`, `Float32`,
`Complex128`, or `Complex64`
Tensor linear systems with a square coefficient matrix and a rank-1 or rank-2
matching right-hand Tensor; the result preserves the right-hand side rank and
Tensor dtype, and singular systems raise `tensor/singular-matrix`.
`matrix/solve` is the public surface;
backend labels such as `tensor/lapack` are implementation ownership, not
user-facing solver names. `matrix/lu` factors a square rank-2 `Float64`,
`Float32`, `Complex128`, or `Complex64`
Tensor with partial pivoting and returns a dictionary containing `lu` for the
combined factors, `pivots` for the final 0-based row order, and `swap-count`
for permutation parity; singular inputs raise `tensor/singular-matrix`.
`matrix/determinant` uses the same partial-pivot LU semantics to return a
scalar determinant preserving the numerical family for square rank-2
`Float64`, `Float32`, `Complex128`, or `Complex64` Tensors; singular
matrices return the dtype's zero scalar. `matrix/inverse` returns the inverse
of a nonsingular square rank-2 `Float64`, `Float32`, `Complex128`, or
`Complex64` Tensor with the same shape and dtype; singular inputs raise
`tensor/singular-matrix`. `matrix/qr` computes a reduced QR decomposition for
rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensors with rows
greater than or equal to columns, returning a dictionary containing `q` with
shape `[rows columns]` and `r` with shape `[columns columns]`; rank-deficient
inputs raise `tensor/singular-matrix`. Complex QR uses the Hermitian inner
product. `matrix/cholesky` computes the lower-triangular Cholesky factor for
square symmetrical positive-definite real tensors or square Hermitian
positive-definite complex tensors and returns a Tensor with the same shape and
dtype; nonsymmetric, non-Hermitian, or non-positive-definite inputs raise
`tensor/not-positive-definite`. `matrix/singular-values` returns the
descending singular values of a rank-2 `Float64` or `Float32` Tensor as a
rank-1 Tensor with the input dtype and shape `[min(rows, columns)]`.
`matrix/svd` computes a reduced singular value decomposition for
rank-2 `Float64` or `Float32` Tensors and returns a dictionary containing `u` with shape
`[rows k]`, `s` with shape `[k]`, and `v` with shape `[columns k]`, where
`k = min(rows, columns)` and the input reconstructs as
`u * diag(s) * transpose(v)`. Tensor factor outputs preserve input dtype and
eligible Vulkan placement. `matrix/eigenvalues` computes
descending real eigenvalues for square symmetric rank-2 `Float64` Tensors.
`matrix/eigenvectors` returns a dictionary containing `values` and `vectors`
for the same symmetric-real contract; eigenvectors are stored as columns
aligned with `values`. Nonsymmetric inputs raise `tensor/not-symmetric`.
`matrix/eigenpairs` computes general nonsymmetric eigenpairs for square
rank-2 `Float64` Tensors and returns a dictionary containing `values` as a
rank-1 `BigComplex` Tensor with shape `[n]` and `vectors` as a rank-2
`BigComplex` Tensor with shape `[n n]`. The `vectors` columns align with
`values`, eigenpairs are sorted by descending eigenvalue magnitude with
deterministic real/imaginary tie-breakers, and empty square matrices return
empty `[0]` values and `[0 0]` vectors. The implementation may use runtime
`LAPACKE_dgeev` when available and otherwise retains a pure fallback under the
same public contract.
Tensor operations may produce lazy Tensor expression payloads under the
existing `Tensor` value, with backend acceleration left as an optimization
behind the same semantic surface. User code should not name or depend on a
separate `TensorExpr` type. `tensor-layout` returns metadata for the current
Tensor representation as a `Dictionary` with keys `dtype`, `device`,
`payload`, `layout`, `dense-row-major`, `shape`, `strides`, `rank`,
`element-count`, `byte-length`, `storage-offset`, `storage-elements`,
`storage-bytes`, `is-view`, `owns-storage`, `owner`, and `write-policy`.
Values are ordinary Omni symbols, arrays, booleans, and numbers. Current
payload symbols are `concrete`, `map`, `contract`, and `view`; current layout
symbols are `dense-row-major` and `strided`; current owner symbols are `self`,
`view-source`, and `expression`; current write-policy symbols are `mutable`,
`immutable`, `mutable-view`, and `read-only-view`. Read-only transpose views
report payload `view`, layout `strided`, `is-view` true, `owns-storage` false,
owner `view-source`, and write-policy `read-only-view`. Lazy expression payloads
report logical `element-count` and `byte-length`, but `storage-elements` and
`storage-bytes` are `0` until the expression is realized into concrete
storage. `tensor-layout` is descriptive metadata, not permission for every
kernel to consume view storage. Dense GPU kernels and raw copy helpers still
require zero-offset dense row-major inputs and destinations unless a later
kernel explicitly says otherwise. Device placement is explicit:
`device` reports the
current Tensor placement, ordinary CPU tensors report `'cpu`, and `to-device`
with target `'cpu` realizes to CPU Tensor storage. Destination-form
`(realize expr out)` writes into an existing CPU Tensor destination, or into
an existing dense row-major CUDA or Vulkan `Float64`, `Float32`, `Complex128`,
or `Complex64` destination when that backend is usable and supports the
requested storage dtype. CPU destinations still reject device-placed sources;
copy those with explicit `to-device 'cpu` first. CUDA destinations accept
matching CPU/CUDA/lazy supported CUDA `Float64` or `Float32` Tensor sources,
matching raw-copyable `Complex128` or `Complex64` CPU/CUDA Tensor sources, and
matching scalar fills. Vulkan destinations accept matching CPU/Vulkan/lazy
Vulkan `Float64` or `Float32` Tensor sources, matching raw-copyable
`Complex128` or `Complex64` CPU/Vulkan Tensor sources, and matching scalar
fills. Unsupported dtypes, unsupported lazy device expressions, and
cross-backend sources fail closed with Tensor backend diagnostics.
CUDA/cuBLAS support
remains explicit-device only: ordinary Tensor operations do not imply CPU/GPU
transfer, and GPU support stays behind `Tensor` rather than introducing a
public `GpuTensor` or `CudaTensor` type. When runtime-loaded CUDA support is
usable, `to-device` with target `'cuda` copies concrete zero-offset dense
row-major `Float64`, `Float32`, `Complex128`, or `Complex64` CPU Tensor
storage to CUDA and `to-device` with target `'cpu` copies CUDA storage back to
native CPU Tensor storage. Missing or unusable CUDA fails
closed with a Tensor backend diagnostic. `tensor-backends` reports structured
backend availability dictionaries; CPU is available in normal builds, while
CUDA availability depends on runtime CUDA probing. cuBLAS reports
dtype-specific `float64` and `float32` capability bits. CPU, Vulkan, and CUDA
entries report `elementwise-map-float64`, `elementwise-map-float32`,
`scientific-map-float64`, and `scientific-map-float32` operation capability
keys; CUDA and Vulkan also report `complex128` and `complex64` storage
capability bits. Vulkan reports `elementwise-map-complex128` and
`elementwise-map-complex64` when its fixed-width complex elementwise map
kernels are available; CUDA reports the same operation bits when its generated
PTX/status complex map helper is available. CUDA and Vulkan report
`contract-complex128` and `contract-complex64` only when their fixed-width
complex contraction kernels are available; complex storage capability and
complex elementwise-map capability do not imply complex contraction support.
CUDA and Vulkan also report the dtype-changing
`rounding-big-integer` capability when their integer-result rounding path is
available. Backend entries report `stats-normal-float64` and
`stats-normal-float32` separately from broad `scientific-map-*` coverage so
partial `stats/normal-cdf` and `stats/normal-quantile` support is
discoverable without implying every scientific unary map is present. CPU, CUDA,
cuBLAS, and Vulkan entries also expose the backend-neutral
ML suite capability keys `ml-linear`, `ml-linear-direct-float64`,
`ml-linear-direct-float32`, `ml-neural-relu-float64`,
`ml-neural-relu-float32`, `ml-neural-sigmoid-float64`,
`ml-neural-sigmoid-float32`, `ml-neural-tanh-float64`,
`ml-neural-tanh-float32`, `ml-neural-gelu-float64`,
`ml-neural-gelu-float32`, `ml-reduction-float64`,
`ml-reduction-float32`, `ml-conv1d-direct-float64`,
`ml-conv1d-direct-float32`, `ml-conv2d-direct-float64`,
`ml-conv2d-direct-float32`, `ml-pool2d-direct-float64`,
`ml-pool2d-direct-float32`, `ml-layer-normalization-float64`,
`ml-layer-normalization-float32`, `ml-batch-normalization-float64`, `ml-batch-normalization-float32`, `ml-scaled-dot-product-attention-float64`, `ml-scaled-dot-product-attention-float32`, `ml-convolution`, `ml-neural-map`,
`ml-normalization`, `ml-attention`, `ml-autograd`, `ml-optimizer`, and
`ml-graph-execution`; `ml-linear` is true for complete CPU dense `Float64` and
`Float32` Tensor execution and remains false for GPU backends until the full
operation family ships. The narrow `ml-linear-direct-float64` and
`ml-linear-direct-float32` partial capabilities are true for CPU and for
Vulkan when the corresponding placement dtype is available; the Vulkan route
covers concrete matching `Float64` or `Float32` `ml/linear` input, weights,
and optional bias through `Tensor` `contract` plus broadcast `map`.
Vulkan-only expressions may participate only when existing Tensor realization
lowers them to concrete dense Vulkan storage of the same dtype without CPU fallback;
`ml/linear/batched-reduce` uses the same narrow Vulkan entry. The other ML keys
stay explicit `false` until a backend ships the named operation family, except
`ml-normalization` and `ml-attention`, which reflect their shipped narrow dtype keys.
`ml/relu` applies `max(input, 0)` to `Float64` or `Float32` Tensor inputs,
preserving dtype and Tensor placement; CPU, CUDA, and Vulkan expose the narrow
`ml-neural-relu-float64`/`ml-neural-relu-float32` bits when that route is
available while broad `ml-neural-map` remains false until the activation family
ships. `ml/sigmoid`, `ml/tanh`, and `ml/gelu` are canonical `ml/*`
activation surfaces, not aliases for bare scientific math names; the current
validated ML activation route supports `Float32` Tensor inputs and preserves
Tensor placement through composed `Tensor` map kernels. `ml/gelu` uses the
standard tanh approximation; the `Float64` transcendental ML activation bits
remain false and `Float64` inputs fail closed until their numerical policy is
validated. `ml/sum`, `ml/mean`, and population `ml/variance` are canonical
`ml/*` axis reductions for CPU and Vulkan `Float64` and `Float32` tensors.
`ml/max` also supports CPU and Vulkan `Float64`/`Float32` tensors. CPU
`ml/logsumexp` supports Float64 and Float32; Vulkan `ml/logsumexp` supports
Float32 and keeps Float64 fail-closed until its exp/log policy is validated.
These reductions accept a single integer
axis or an array/proper list of axes, remove reduced axes from the result shape,
preserve dtype, reject duplicate/out-of-range axes with Tensor diagnostics, and
fail closed for unsupported backends without CPU fallback. `ml/softmax` accepts
a single integer axis, uses max-shifted normalization, preserves input shape and
dtype, rejects axis lists, and supports Vulkan Float32 while keeping Vulkan
Float64 fail-closed. `ml/layer-normalization(input axis [epsilon])` normalizes each single-axis slice in place,
preserves input shape and dtype, requires positive finite epsilon, supports CPU `Float64`/`Float32`, and supports direct Vulkan `Float32`. `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])` applies explicit rank-1 affine/stat tensors along the channel axis and supports CPU `Float64`/`Float32` plus direct Vulkan `Float32`. `ml/scaled-dot-product-attention(query key value [mask] [scale])` computes max-shifted attention for shapes `[... Q D]`, `[... K D]`, and `[... K V]`, accepts optional additive `[Q K]` or batched masks plus positive finite scale, supports CPU `Float64`/`Float32` and direct dense Vulkan `Float32`, and fails closed for mixed placement or Vulkan `Float64`.
`ml/mean-squared-error(predictions targets)` accepts same-shape, same-dtype CPU or Vulkan
`Float64`/`Float32` tensors and returns a scalar tensor containing the population mean squared error. `ml/cross-entropy(logits
targets axis)` accepts same-shape probability/one-hot target tensors, uses
max-shifted log-softmax over the explicit class axis, returns the mean loss
over non-class positions, and supports Vulkan `Float32` while keeping Vulkan
`Float64` fail-closed until its exp/log policy is validated. `ml/grad` currently accepts CPU `linear-mean-squared-error`, `linear-activation-mean-squared-error`, and `linear-softmax-cross-entropy` gradient specs; these return ordinary dictionaries with loss, output, input-gradient, and parameter-gradients, validate probability targets for softmax CE, and keep CUDA/Vulkan backward fail-closed. `ml/sgd-step(parameters gradients learning-rate)` applies an immutable CPU SGD update over matching array/dictionary parameter trees with dense `Float64`/`Float32` tensor leaves. `ml/clip-gradients(gradients max-norm)` is available for CPU max-norm gradient-tree clipping. `ml/optimizer-step(spec parameters gradients state)` returns updated parameters and explicit optimizer state for CPU SGD, Adam, AdamW, and RMSProp specs and accepts optional `clip-norm`; these optimizers also support all-Vulkan dense row-major `Float32` leaves while preserving Vulkan placement and explicit optimizer state. `tensor-backends` reports this narrow support with per-optimizer `ml-optimizer-*-float32` fields; broad `ml-optimizer` remains false until the optimizer family is complete. `ml/save-optimizer(spec state [path])` and `ml/load-optimizer(source)` checkpoint explicit optimizer spec/state dictionaries and revalidate the checkpoint envelope, supported spec, and state container on load; CUDA optimizer kernels plus Vulkan clipping kernels remain fail-closed.
`ml/conv1d(input kernel stride padding dilation groups)` is the first
convolution surface: it requires dense row-major `input[batch channels width]`
and `kernel[out-channels in-channels-per-group kernel-width]`, supports CPU
`Float64`/`Float32`, and supports direct Vulkan `Float32` through the narrow
`ml-conv1d-direct-float32` capability. Channels must divide evenly by `groups`,
and arbitrary views/strides and mixed CPU/Vulkan operands fail closed.
`ml/conv2d(input kernel stride-height stride-width padding-height padding-width
dilation-height dilation-width groups)` extends the same explicit convolution
contract to dense row-major NCHW input tensors and OIHW kernels. CPU supports
`Float64`/`Float32`, Vulkan supports direct dense `Float32` through
`ml-conv2d-direct-float32`, groups split input/output channels evenly, and
mixed CPU/Vulkan or unsupported-layout operands fail closed before fallback.
`ml/max-pool2d(input window-height window-width stride-height stride-width
padding-height padding-width)` and `ml/avg-pool2d` complete the first explicit
dense pooling surface for row-major NCHW tensors. Pooling excludes padding from
average divisors, rejects all-padding windows, supports CPU `Float64`/`Float32`,
and supports direct dense Vulkan `Float32` through `ml-pool2d-direct-float32`.
The broad `ml-convolution` key is true for CPU and for Vulkan when the direct
Float32 convolution/pooling family is available; it does not imply arbitrary
views, transposed layouts, or backward kernels.
`nn/*` is the Omni Neural DataSpec namespace for inspectable neural-network
data. `nn/sequential`, `nn/dense`, `nn/conv1d`, `nn/conv2d`,
`nn/max-pool2d`, `nn/avg-pool2d`, `nn/flatten`, `nn/activation`, and the
activation shorthand constructors `nn/relu`, `nn/sigmoid`, `nn/tanh`,
`nn/gelu`, and `nn/softmax` produce normalized dictionary specs, not hidden
mutable layer objects. `nn/validate(spec)` returns the input spec when the
dictionary satisfies the frozen DataSpec schema and raises `nn/invalid-spec`
with a diagnostic payload otherwise.

`nn/init(spec [options])` is the lifecycle primitive. `spec` must be a
DataSpec. `options` is optional and may contain:

- `dtype`: `Float64` or `Float32` (defaults to `Float32`)
- `device`: `'cpu`, `'cuda`, or `'vulkan` (defaults to `'cpu`)
- `seed`: non-negative integer (defaults to `1`)
- `mode`: `'eval` or `'train` (defaults to `'eval`)

`nn/init` returns a transparent model bundle dictionary:

```lisp
(Dictionary
  'kind 'model
  'spec spec
  'params params
  'state state
  'mode mode
  'dtype Float32/Float64
  'device 'cpu/'cuda/'vulkan
  'metadata metadata)
```

`dtype`, `device`, and `seed` are explicit and deterministic across runs when
the same `spec` and options are supplied. `seed` is stored in `metadata` and the
same initializer family plus placement and dtype requests always yields the same
parameter tensor contents. `nn/init` uses each parameterized layer's
`kernel-init` / `bias-init` dictionary family (`zeros`, `ones`, `uniform`,
`normal`, `xavier-uniform`, `glorot-uniform`, `kaiming-uniform`,
`kaiming-normal`) to construct concrete `Float32`/`Float64` parameter tensors
and move them to the requested device. Current parameterized layer constructors
are `nn/dense`, `nn/conv1d`, and `nn/conv2d`; convolution constructors accept
the same optional `use-bias`, `activation`, `kernel-init`, and `bias-init`
options as dense specs.
`mode`, `dtype`, and `device` are surfaced on the returned model for downstream
`nn/apply` and `nn/predict` behavior.

`nn/predict` is the model convenience path:

- takes `(nn/predict model input)`;
- requires `model` to be a normalized model bundle; and
- requires `(nn/mode model)` to be `'eval` (it does not coerce `'train` models).

`nn/apply` is the explicit-data path for inference:

- takes either `(nn/apply model input)` for a model bundle, or
- `(nn/apply spec params state input [options])` using extracted model data from
  `nn/spec`, `nn/parameters`, and `nn/state`;
- optional `options` must currently be an empty `Dictionary`; non-empty options
  are reserved and fail closed instead of being silently ignored.

`nn/apply` and `nn/predict` lower supported specs in the first inference family:

- `nn/dense` → `ml/linear`
- `nn/conv1d` → `ml/conv1d`
- `nn/conv2d` → `ml/conv2d`
- `nn/max-pool2d` → `ml/max-pool2d`
- `nn/avg-pool2d` → `ml/avg-pool2d`
- `nn/activation` variants (`nn/relu`, `nn/sigmoid`, `nn/tanh`, `nn/gelu`,
  `nn/softmax`) → corresponding `ml/relu`, `ml/sigmoid`, `ml/tanh`,
  `ml/gelu`, `ml/softmax`
- `nn/flatten` performs explicit CPU tensor flatten materialization.

Unsupported mixed-device, unsupported-layout, and unsupported-backend paths for
inference are fail-closed with `tensor/backend-unsupported`; there is no hidden
CPU fallback in these execution paths.

Unsupported `nn/flatten` paths (non-CPU input/device/layout combination) are
fail-closed as `tensor/backend-unsupported` instead of hidden fallback.

Convolution bias addition in inference is currently CPU materialized and fail-closed
through `tensor/backend-unsupported` unless the convolution output and bias are
CPU dense row-major `Float64`/`Float32`.

`nn/summary(model)` returns an inspection dictionary with:

- `kind` (`summary`)
- `mode`
- `dtype`
- `device`
- `layers`
- `parameter-tensors`
- `parameter-elements`

`nn/summary` is a deterministic inspector for model metadata and parameter
allocation size.
`nn/save-spec(spec [path])` and `nn/load-spec(source)` are the spec-level
checkpoint pair. `nn/save-spec` accepts a valid non-model DataSpec and returns a
JSON checkpoint string unless `path` is supplied, in which case it writes the
checkpoint and returns `Void`. `nn/load-spec` accepts either a checkpoint JSON
string or a path, restores the DataSpec, and re-runs `nn/validate`.

`nn/save(model [path])` and `nn/load(source)` are the model-bundle checkpoint
pair. A model checkpoint preserves the transparent `nn/init` bundle fields
(`spec`, `params`, `state`, `mode`, `dtype`, `device`, `metadata`) and records
parameter tensor dtype, shape, flat data, and original placement metadata.
Loading reconstructs the tensors and restores their recorded device through the
explicit `to-device` path. Malformed checkpoint envelopes, payload-family
mismatches, unsupported tensor dtypes, or invalid restored DataSpecs fail closed
with `nn/invalid-spec`.
`ml/save-optimizer(spec state [path])` and `ml/load-optimizer(source)` are the
optimizer-state checkpoint pair. They serialize a supported optimizer spec and
ordinary state `Dictionary` as an `optimizer` payload, either returning a JSON
checkpoint string or writing to a path. Loading accepts a checkpoint JSON string
or path, restores an ordinary `Dictionary` with `kind`, `spec`, and `state`, and
rejects wrong payload families or invalid optimizer specs before returning.
`ml/linear/batched-reduce` is a public rank-`>=2` batched projection surface
that preserves the same dtype and output-shape semantics as `ml/linear` while
rejecting rank-1 inputs via `tensor/shape-mismatch` and rejecting mixed-device
or unsupported-layout Vulkan operands through `tensor/backend-unsupported`
diagnostics before any fallback.
`ml/linear` computes an affine dense projection:
`input[..., in_features]` by `weights[out_features, in_features]`, with an
optional `bias[out_features]`, producing `input[..., out_features]`. Non-CPU
Tensor operands fail closed with `tensor/backend-unsupported` except for direct
concrete Vulkan `Float32` input and weights, with optional concrete Vulkan
`Float32` bias, which lower through Tensor `contract` plus broadcast `map`.
CUDA-placed dense row-major
`Float64` and `Float32` tensors support
binary elementwise `map` for `+`, `-`, `*`, `/`, `min`, and `max`;
arithmetic/component unary `map` for unary `+`, `abs`, unary `-`, `sqrt`,
`real-part`, `imag-part`, and `conjugate`; and, when the scientific module
loads, scientific unary `map` for `sin`, `cos`, `tan`, `asin`, `acos`,
`atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, `log10`, `math/erf`,
`math/erfc`, `stats/normal-cdf`, and `stats/normal-quantile`. Arithmetic
kernels use embedded PTX; scientific kernels use PTX generated from CUDA C and
libdevice, and CUDA quantile uses a device status word so invalid
probabilities fail before result exposure. Supported CUDA map operand shapes are tensor/scalar, scalar/tensor,
exact-shape tensor/tensor, and right-aligned singleton-axis tensor/tensor
broadcasting for binary map, and exact-shape Tensor input for unary map;
direct public `map`, lazy CUDA map realization, and CUDA destination
realization from lazy CUDA maps preserve CUDA placement. Direct Tensor `abs`,
unary `-`, `sqrt`, `real-part`, `imag-part`, `conjugate`, and supported
scientific unary primitives preserve CUDA placement for eligible real CUDA
tensors. Unsupported callables, mixed CPU/CUDA operands, mixed dtype/device
operands, and unsupported layouts fail closed with Tensor backend diagnostics
rather than staging through CPU. When cuBLAS is available
for the input dtype,
CUDA-placed dense row-major matching `Float64` or matching `Float32`
rank-2/rank-2 single-axis contractions may execute through cuBLAS GEMM for
`[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`; rank-2/rank-1, rank-1/rank-2, and
rank-1/rank-1 dot contractions may execute through cuBLAS GEMV. Results are
CUDA-placed Tensors. In the same supported CUDA layout family, zero free
dimensions produce CUDA-placed zero-length outputs and zero contracted
dimensions produce dtype-preserving additive-identity outputs without
requiring a cuBLAS call. Unsupported CUDA contract cases fail with backend
diagnostics rather than silently copying to CPU. Destination-form `realize`
can write matching dense row-major `Float64` or `Float32` CPU sources, CUDA
sources, supported lazy CUDA map or contract results, and scalar fills into an
existing CUDA-placed destination without changing the public API. Concrete
CUDA Tensor clone/copy support applies to Omni-owned CUDA payloads and valid
foreign CUDA concrete payloads; foreign CUDA clones allocate fresh Omni-owned
CUDA storage and keep fake or invalid CUDA handles fail-closed.
When `tensor-backends` reports `elementwise-map-complex128` or
`elementwise-map-complex64`, CUDA `Complex128` and `Complex64` tensors support
dense row-major elementwise `map` for binary `+`, `-`, `*`, and `/`, plus
unary `+`, `abs`, unary `-`, `real-part`, `imag-part`, and `conjugate`.
Generic CUDA complex `map` preserves the complex Tensor dtype; map
`real-part`, `imag-part`, and `abs` use zero imaginary components where
applicable. Direct CUDA complex `abs`, `real-part`, and `imag-part` return
component-width real CUDA tensors (`Float64` for `Complex128`, `Float32` for
`Complex64`), while direct unary `-` and `conjugate` preserve complex dtype.
CUDA complex division by zero reports structured `tensor/domain-error`;
nonrepresentable CUDA complex results fail as invalid arguments before result
exposure.
When `tensor-backends` reports `contract-complex128` or
`contract-complex64`, CUDA `Complex128` and `Complex64` tensors support the
same dense row-major single-axis contract families as the CUDA cuBLAS real
path, using non-conjugating complex multiply-add. Results remain CUDA-placed
fixed-width complex tensors and require explicit `to-device 'cpu` before CPU
inspection. Unsupported complex contraction layouts, axis families, mixed
devices, mixed dtypes, missing capability, and matrix operations fail closed
without hidden CPU fallback or lowering through real tensors or `BigComplex`.
Vulkan is the portable
explicit GPU backend direction: `tensor-backends` reports a structured
`vulkan` entry with explicit `Float64` and `Float32` kernel capability.
`to-device` with target `'vulkan` copies concrete zero-offset dense row-major
`Float64`, `Float32`, `Complex128`, or `Complex64` CPU Tensor storage into
opaque Vulkan storage when runtime-loaded Vulkan support is usable, and
`to-device` with target `'cpu` copies Vulkan storage back to native CPU Tensor
storage.
Missing or unusable Vulkan fails closed with a Tensor backend diagnostic;
destination-form `realize` can write matching dense row-major `Float64` or
`Float32` CPU or Vulkan sources, matching raw-copyable `Complex128` or
`Complex64` CPU or Vulkan sources, lazy Vulkan results, and scalar fills into
an existing Vulkan-placed destination without changing the public API; complex
Vulkan storage capability is separate from complex operation capability;
`map` supports dense row-major Vulkan `Float64` and `Float32` elementwise
arithmetic `+`, `-`, `*`, `/`, `min`, and `max` plus unary helpers for
Tensor/scalar, scalar/Tensor, exact-shape Tensor/Tensor inputs, and
right-aligned singleton-axis Tensor/Tensor broadcasting through embedded SPIR-V
kernels, returning Vulkan-placed tensors. When `tensor-backends` reports
`elementwise-map-complex128` or `elementwise-map-complex64`, Vulkan
`Complex128` and `Complex64` tensors support dense row-major elementwise
`map` for `+`, `-`, `*`, and `/` across tensor/scalar, scalar/tensor,
exact-shape tensor/tensor, and right-aligned singleton-axis broadcast forms.
Generic complex `map` supports unary `+`, `abs`, unary `-`, `real-part`,
`imag-part`, and `conjugate` while preserving the complex Tensor dtype;
`abs`, `real-part`, and `imag-part` called directly on Vulkan complex tensors
return component-width real Vulkan tensors. Complex Vulkan division reports a
structured `tensor/domain-error` for zero denominators and fails before
exposing non-representable results. Vulkan `Float32` tensors also support
direct and mapped `stats/normal-cdf` through a same-dtype shader approximation
and direct/mapped `stats/normal-quantile` through a status-bearing inverse-CDF
shader. Vulkan `Float64` tensors support direct and mapped `stats/normal-cdf`
through a double piecewise polynomial approximation and direct/mapped
`stats/normal-quantile` through a Float64 status-bearing inverse-CDF helper.
Direct Vulkan `floor`, `ceiling`, `round`, and `truncate` require
`tensor-backends` Vulkan `rounding-big-integer`; same-dtype Vulkan float
rounding output is not a valid Tensor rounding contract.
Public Vulkan `contract` supports Vulkan-placed dense row-major rank-N
matching `Float64` or `Float32` tensors with zero or more explicit contracted
axis pairs. When `tensor-backends` reports `contract-complex128` or
`contract-complex64`, the same public `contract` surface also supports
dense row-major fixed-width complex contraction for matching `Complex128` or
`Complex64` tensors, using non-conjugating complex multiply-add. Output axes
are ordered as free left axes followed by free right axes. Results are
Vulkan-placed tensors and require explicit `to-device 'cpu` before CPU
inspection. Unsupported Vulkan map and contract cases, including unsupported
callables, incompatible broadcasting shapes, unsupported layouts/dtypes,
missing complex contract capability, unsupported fixed-width complex
contraction families, and mixed CPU/Vulkan operands, fail closed rather than
silently copying to CPU.
`matrix/transpose` supports Vulkan-placed dense row-major `Float64`, `Float32`,
`Complex128`, or `Complex64` rank-2 concrete tensors through embedded SPIR-V
kernels, returning a Vulkan-placed materialized transposed Tensor that
preserves the input dtype.
CUDA and broad Vulkan kernels do not consume arbitrary `matrix/transpose-view`
payloads yet; view-backed matrix kernels fail closed rather than interpreting
offset/stride metadata. Direct rank-2 transpose views over dense zero-offset
Vulkan storage can be explicitly materialized to dense Vulkan tensors through
placement, destination `realize`, and copyback boundaries.
`matrix/diagonal` and `matrix/diagonal-matrix` support Vulkan-placed dense
row-major `Float64`, `Float32`, `Complex128`, or `Complex64` inputs through
embedded SPIR-V structural kernels, returning Vulkan-placed Tensor results that
preserve the input dtype. CUDA and Vulkan fixed-width complex structural
matrix support is reported separately as `matrix-structural-complex128` and
`matrix-structural-complex64`; when the selected backend reports the relevant
capability, dense row-major `Complex128` or `Complex64` tensors support
`matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
`matrix/trace` on that backend. `matrix/trace` reads back only the scalar
result required by the public scalar return contract. CUDA complex numerical
matrix kernels and Vulkan complex numerical matrix kernels outside
`matrix/lu`, `matrix/determinant`, `matrix/solve`, and `matrix/inverse`
remain fail-closed
unless a specific matrix operation has its own backend contract and capability
reporting. Matrix support is not implied by complex storage, complex
elementwise `map`, complex `contract`, or structural matrix capability.
`matrix/rank` supports Vulkan-placed dense row-major `Float64`, `Float32`,
`Complex128`, or `Complex64` rank-2 inputs through embedded SPIR-V reducers
and reads back only the scalar `Integer` result required by the public scalar
return contract. Complex rank uses magnitude-based pivoting and tolerance.
`matrix/lu` supports Vulkan-placed dense row-major square `Float64`,
`Float32`, `Complex128`, or `Complex64` inputs through embedded SPIR-V
partial-pivot factorization kernels.
The returned dictionary keeps `lu` as a Vulkan-placed Tensor preserving the
input dtype while `pivots` and `swap-count` remain ordinary host metadata
required by the public contract. Complex support is reported through
`matrix-numerical-complex128` and `matrix-numerical-complex64`.
`matrix/solve` supports Vulkan-placed dense row-major square `Float64` or
`Float32`, `Complex128`, or `Complex64` coefficient tensors with matching
Vulkan-placed rank-1 or rank-2 right-hand tensors through embedded SPIR-V
Gaussian-elimination kernels.
Results remain Vulkan-placed tensors preserving RHS dtype; singular systems
raise `tensor/singular-matrix`. `Float64` larger systems route through the
measured thresholded parallel Vulkan solve helper; `Float32` larger systems
route through the native staged parallel Vulkan solve helper at the matched
parity threshold.
`matrix/determinant` supports Vulkan-placed dense row-major square `Float64`,
`Float32`, `Complex128`, or `Complex64` inputs through embedded SPIR-V reducers
and reads back only the scalar result required by the public scalar return
contract.
`matrix/inverse` supports CPU and Vulkan-placed dense row-major square
`Float64`, `Float32`, `Complex128`, or `Complex64` inputs through
Gauss-Jordan kernels, returning an inverse Tensor preserving input dtype and
placement for Vulkan inputs and raising `tensor/singular-matrix` for singular
inputs.
`matrix/cholesky` supports Vulkan-placed dense row-major square `Float64`,
`Float32`, `Complex128`, or `Complex64` inputs through embedded SPIR-V
Cholesky factorization kernels, returning a Vulkan-placed lower factor Tensor
preserving input dtype and raising `tensor/not-positive-definite` for
nonsymmetric, non-Hermitian, or non-SPD inputs.
`matrix/qr` supports Vulkan-placed dense row-major rank-2 `Float64`,
`Float32`, `Complex128`, or `Complex64` inputs with rows greater than or equal
to columns through embedded SPIR-V reduced QR kernels, returning Vulkan-placed
`q` and `r` factor Tensors preserving input dtype and raising
`tensor/singular-matrix` for rank-deficient inputs.
`matrix/norm` supports Vulkan-placed dense row-major `Float64`, `Float32`,
`Complex128`, and `Complex64` inputs for default/`'frobenius`, `'one`,
`'infinity`, `'max`, `'spectral`, and `'nuclear`. Complex direct selectors use
complex magnitudes; complex spectral/nuclear selectors use native Vulkan
complex singular-value helpers. These Vulkan paths read back only the scalar
result required by the public scalar return contract.
