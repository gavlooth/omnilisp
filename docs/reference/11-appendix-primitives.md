# Appendix A: Primitive Reference

**[Back to Index](../OMNI_REFERENCE.md)**

---

### Primitive Surface Audit: Command vs Query (2026-03-17)

Audit source of truth:
- registered primitive table in `src/lisp/eval_init_primitives.c3`.

Normative classification contract:
- command-style primitive: successful completion returns `Void` (or is a non-returning process command such as `exit`).
- query-style primitive: returns data/handle/predicate values; when absence semantics apply, absence remains `nil` (not `Void`).

Audit snapshot:
- total registered primitive names: `263`
- command-style names: `42`
- query-style names: `221`

#### Core Mutation Classification

| Primitive | Style | Success contract |
|------|-------|-------------|
| `set!` | command-style | Returns `Void` after successful mutation dispatch (`array`/`dict`) |
| `push!` | command-style | Returns `Void` after append |
| `remove!` | command-style | Returns `Void` after key removal |
| `set-add` | command-style | Returns `Void` after insert |
| `set-remove` | command-style | Returns `Void` after delete |
| `unsafe-free!` | command-style | Returns `Void` after releasing supported heap backing |
| `ref` | query-style | Returns looked-up value, or `nil` on absence |
| `has?` | query-style | Returns truthy on hit, `nil` on miss |
| `set-contains?` | query-style | Returns truthy on hit, `nil` on miss |
| `keys` | query-style | Returns key list |
| `values` | query-style | Returns value list |

#### I/O Classification

| Primitive(s) | Style | Success contract |
|------|-------|-------------|
| `__raw-print`, `__raw-println`, `__raw-newline`, `__raw-display` | command-style | Return `Void` after console side effect |
| `__raw-write-file` | command-style | Returns `Void` after write completion |
| `__raw-fs-close`, `__raw-fs-rename`, `__raw-fs-unlink` | command-style | Return `Void` after lifecycle/path mutation |
| `fs-close`, `fs-rename`, `fs-unlink` | command-style | Return `Void` |
| `__raw-udp-bind`, `__raw-udp-close`, `__raw-tcp-close`, `__raw-process-kill`, `__raw-signal-unhandle`, `__raw-async-sleep`, `__raw-tls-close` | command-style | Return `Void` after side-effecting operation |
| `sleep`, `exit` | command-style | `sleep` returns `Void`; `exit` is process-terminal |
| `__raw-read-file`, `__raw-file-exists?`, `__raw-read-lines`, `__raw-fs-open`, `__raw-fs-read`, `__raw-fs-write`, `__raw-fs-stat`, `__raw-fs-readdir` | query-style | Return data/handles/metrics (`nil` only for absence-style queries) |
| `fs-open`, `fs-read`, `fs-write`, `fs-stat`, `fs-readdir` | query-style | Return handles/data/metrics |
| `__raw-tcp-connect`, `__raw-tcp-listen`, `__raw-tcp-accept`, `__raw-tcp-read`, `__raw-tcp-write`, `__raw-udp-socket`, `__raw-udp-send`, `__raw-udp-recv`, `__raw-pipe-connect`, `__raw-pipe-listen`, `__raw-process-spawn`, `__raw-process-wait`, `__raw-signal-handle`, `__raw-dns-resolve`, `__raw-tls-connect`, `__raw-tls-server-wrap`, `__raw-tls-read`, `__raw-tls-write`, `__raw-http-get`, `__raw-http-request` | query-style | Return handles, payloads, or result dictionaries |

#### Scheduler Classification

| Primitive(s) | Style | Success contract |
|------|-------|-------------|
| `fiber-cancel`, `run-fibers` | command-style | Return `Void` on successful scheduler command completion |
| `__raw-thread-cancel`, `__raw-task-cancel` | command-style | Return `Void` on successful cancellation command |
| `spawn`, `await`, `__raw-offload`, `__raw-offload-batch`, `__raw-thread-spawn`, `__raw-thread-spawn-batch`, `__raw-thread-join`, `__raw-thread-join-timeout`, `__raw-task-spawn`, `__raw-task-spawn-batch`, `__raw-task-join`, `__raw-task-join-timeout` | query-style | Return ids/handles/results (`join-timeout` may return `nil` timeout absence) |

#### Exhaustive Classification Rule

Command-style primitive set (`42` names):
`set!`, `push!`, `remove!`, `set-add`, `set-remove`, `unsafe-free!`, `deduce/commit`, `deduce/abort`, `deduce/fact!`, `deduce/retract!`, `deduce/clear!`, `deduce/drop!`, `deduce/rule!`, `__raw-print`, `__raw-println`, `__raw-newline`, `__raw-display`, `__raw-write-file`, `__raw-fs-close`, `__raw-fs-rename`, `__raw-fs-unlink`, `fs-close`, `fs-rename`, `fs-unlink`, `fs-close`, `fs-rename`, `fs-unlink`, `__raw-udp-bind`, `__raw-udp-close`, `__raw-tcp-close`, `__raw-process-kill`, `__raw-signal-unhandle`, `__raw-async-sleep`, `__raw-thread-cancel`, `__raw-task-cancel`, `__raw-tls-close`, `sleep`, `exit`, `fiber-cancel`, `run-fibers`.

All remaining registered primitive names are query-style.

### Dispatched Primitives (33)

These support user extension via method tables.

| Name | Arity | Category |
|------|-------|----------|
| `+` | 1-2 | Arithmetic |
| `-` | 2 | Arithmetic |
| `*` | 2 | Arithmetic |
| `/` | 2 | Arithmetic |
| `%` | 2 | Arithmetic |
| `=` | 2 | Comparison |
| `<` | 2 | Comparison |
| `>` | 2 | Comparison |
| `<=` | 2 | Comparison |
| `>=` | 2 | Comparison |
| `length` | 1 | Collection size; cons/list dotted terminal tails count as one element |
| `ref` | 2 | Collection lookup; cons/list chains support positive and negative indexes |
| `push!` | 2 | Collection |
| `keys` | 1 | Collection |
| `values` | 1 | Collection |
| `has?` | 2 | Collection |
| `remove!` | 2 | Collection |
| `sort` | 1 | Sorting |
| `sort-by` | 2 | Sorting |
| `string-append` | variadic | String |
| `string-contains?` | 2 | String |
| `string-upcase` | 1 | String |
| `string-downcase` | 1 | String |
| `string-casefold` | 1 | String |
| `string-titlecase` | 1 | String |
| `abs` | 1 | Math |
| `floor` | 1 | Math |
| `ceiling` | 1 | Math |
| `round` | 1 | Math |
| `truncate` | 1 | Math |
| `sqrt` | 1 | Math |
| `min` | 2 | Math |
| `max` | 2 | Math |

### Regular Primitives

**List Operations:**

| Name | Arity | Description |
|------|-------|-------------|
| `cons` | 2 | Construct pair |
| `car` | 1 | First element |
| `cdr` | 1 | Rest |
| `List` | variadic | Canonical list constructor / conversion surface |
| `null?` | 1 | Is nil? |
| `pair?` | 1 | Is cons? |
| `list` | variadic | Approved public helper for list construction/conversion |

**Boolean:**

| Name | Arity | Description |
|------|-------|-------------|
| `not` | 1 | Logical negation |

**I/O (raw, used by effect fast path):**

| Name | Arity | Description |
|------|-------|-------------|
| `__raw-print` | 1 | Raw print |
| `__raw-println` | 1 | Raw println |
| `__raw-newline` | 0 | Raw newline |
| `__raw-display` | 1 | Raw display |
| `__raw-read-line` | 0 | Raw stdin line reader |
| `__raw-read-file` | 1 | Raw file read |
| `__raw-write-file` | 2 | Raw file write |
| `__raw-file-exists?` | 1 | Raw file check |
| `__raw-read-lines` | 1 | Raw line reader |

**Filesystem Handles:**

| Name | Arity | Description |
|------|-------|-------------|
| `fs-open` | variadic | Open file handle with mode/options |
| `fs-read` | 2 | Read bytes/chars from handle |
| `fs-write` | 2 | Write bytes/chars to handle |
| `fs-close` | 1 | Close file handle |
| `fs-stat` | 1 | File metadata lookup |
| `fs-readdir` | 1 | Read directory entries |
| `fs-rename` | 2 | Rename/move file |
| `fs-unlink` | 1 | Remove file path |

`fs-*` is the canonical filesystem family; long-form compatibility aliases are
not exported.

**String:**

| Name | Arity | Description |
|------|-------|-------------|
| `string-join` | 2 | Join list with separator |
| `substring` | 3 | Extract substring |
| `string-split` | 2 | Split by delimiter |
| `string-length` | 1 | Codepoint length |
| `string-byte-length` | 1 | Byte length |
| `string-trim` | 1 | Trim whitespace |
| `string-index-of` | 2 | Find index |
| `string-replace` | 3 | Replace occurrences |
| `char-at` | 2 | Char at index |
| `string-repeat` | 2 | Repeat N times |

**Type & Introspection:**

| Name | Arity | Description |
|------|-------|-------------|
| `type-of` | 1 | Type name as symbol |
| `is?` | 2 | Type/subtype check |
| `instance?` | 1 | Is type instance? |
| `continuation?` | 1 | Is continuation? |
| `procedure?` | 1 | Is callable? |
| `bound?` | 1 | Is name defined? |
| `type-args` | 1 | Parametric type args |
| `iterator?` | 1 | Is iterator value? |
| `foreign-describe` | 1 | Return `ForeignHandle` or FFI-bound callable metadata dictionary |
| `foreign-release` | 1 | Explicitly release an owned/releasable `ForeignHandle` payload and return `Void` |

`Nil` is the language-level empty/false value type. `Void` is now a real
singleton value constructor with zero arguments, and FFI `^Void` returns map to
that same runtime value.
`false` is a distinct boolean false value. Both `nil` and `false` are falsy
(quoted `'false` is still a symbol literal).

**Conversion:**

| Name | Arity | Description |
|------|-------|-------------|
| `parse-number` | 1 | Parse number |
| `String` | 1 | Canonical string constructor/coercion surface; dispatches string, number, symbol, and proper list-of-string-fragment conversion |
| `Float` | 1-2 | Canonical floating constructor; `(Float x)` defaults to `Float64`, `(Float x 64)` and `(Float x "64")` produce `Float64`, while `(Float x 32)` and `(Float x "32")` produce scalar `Float32` |
| `Float32` | 1 | Canonical binary32 constructor/coercion surface; accepts representable numeric and numeric-string inputs and fails closed on non-finite or out-of-range narrowing |
| `Float64` | 1 | Canonical binary64 constructor/coercion surface |
| `Integer` | 1 | Canonical integer constructor/coercion surface; truncates finite doubles toward zero |
| `BigComplex` | 1-2 | High-precision complex constructor from a real part and optional imaginary part |
| `Complex128` | 1-2 | Fixed-width complex constructor with two finite `Float64` components; optional imaginary part defaults to zero |
| `Complex64` | 1-2 | Fixed-width complex constructor with two finite `Float32` components; optional imaginary part defaults to zero |
| `Symbol` | 1 | Canonical symbol constructor/coercion surface |

Callable core type symbols also provide constructor/coercion surface here:
`Integer`, `BigInteger`, `BigFloat`, `BigComplex`, `Complex128`, `Complex64`,
`Float`, `Float32`, `Float64`, `String`,
`Symbol`, `Boolean`, `Nil`, `Void`, `Closure`, `List`, `Array`, `Dictionary`,
`Dict`, `Set`, `Iterator`, `Coroutine`, `TimePoint`, and `Tensor`.

**Math:**

| Name | Arity | Description |
|------|-------|-------------|
| `sin` | 1 | Sine |
| `cos` | 1 | Cosine |
| `tan` | 1 | Tangent |
| `sinh` | 1 | Hyperbolic sine |
| `cosh` | 1 | Hyperbolic cosine |
| `tanh` | 1 | Hyperbolic tangent |
| `asin` | 1 | Arc sine |
| `acos` | 1 | Arc cosine |
| `atan` | 1 | Arc tangent |
| `atan2` | 2 | Two-arg arctangent |
| `exp` | 1 | Exponential |
| `log` | 1 | Natural log |
| `log10` | 1 | Log base 10 |
| `pow` | 2 | Power |
| `real-part` | 1 | Real component of any number; returns `BigFloat` for BigComplex inputs |
| `imag-part` | 1 | Imaginary component of any number; returns `0` for real scalar inputs |
| `conjugate` | 1 | Complex conjugate; preserves real scalar inputs |
| `floor` | 1 | Floor; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `ceiling` | 1 | Ceiling; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `round` | 1 | Round; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `truncate` | 1 | Truncate; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `math/lgamma` | 1 | Natural log of absolute gamma value |
| `math/erf` | 1 | Error function; applies elementwise to supported Tensor inputs |
| `math/erfc` | 1 | Complementary error function; applies elementwise to supported Tensor inputs |
| `stats/normal-cdf` | 1 | Standard normal cumulative distribution function; applies elementwise to supported Tensor inputs |
| `stats/normal-quantile` | 1 | Standard normal inverse cumulative distribution function; applies elementwise to supported CPU, CUDA, and Vulkan Float32 Tensor inputs |
| `ml/grad` | 1 | Data-oriented gradient spec evaluator; currently supports CPU linear MSE, linear-activation MSE, and linear softmax cross-entropy gradients |
| `ml/sgd-step` | 3 | Immutable SGD parameter-tree update for CPU Float64/Float32 Tensor leaves: parameters, gradients, learning-rate |
| `ml/clip-gradients` | 2 | CPU max-norm gradient clipping over dense Float64/Float32 Tensor leaves in explicit gradient trees |
| `ml/optimizer-step` | 4 | Data-oriented optimizer spec step; currently supports CPU and all-Vulkan Float32 SGD, Adam, AdamW, and RMSProp over explicit parameter/state trees |
| `ml/save-optimizer` | 2-3 | Serialize a supported optimizer spec and explicit state Dictionary checkpoint to JSON, or write it to a path and return `Void` |
| `ml/load-optimizer` | 1 | Load an optimizer checkpoint from JSON or path and return an ordinary `{kind spec state}` Dictionary after envelope/spec/state-container validation |
| `ml/sum` | 2 | Axis sum reduction for supported Tensor inputs; drops reduced axes from the result shape |
| `ml/mean` | 2 | Axis mean reduction for supported Tensor inputs; drops reduced axes from the result shape |
| `ml/variance` | 2 | Axis population-variance reduction for supported Tensor inputs; drops reduced axes from the result shape |
| `ml/max` | 2 | Axis maximum reduction for supported Tensor inputs; drops reduced axes from the result shape |
| `ml/logsumexp` | 2 | Stable max-shifted log-sum-exp reduction for supported Tensor inputs; drops reduced axes from the result shape |
| `ml/softmax` | 2 | Stable single-axis softmax for supported Tensor inputs; preserves input shape |
| `ml/layer-normalization` | 2-3 | Single-axis layer normalization for supported Tensor inputs; preserves input shape and accepts optional positive epsilon |
| `ml/batch-normalization` | 6-7 | Explicit-stat batch normalization for supported Tensor inputs; preserves input shape and accepts optional positive epsilon |
| `ml/scaled-dot-product-attention` | 3-5 | Max-shifted scaled dot-product attention for supported Tensor query/key/value inputs with optional additive mask and scale |
| `ml/cross-entropy` | 3 | Scalar cross-entropy loss for same-shape probability/one-hot targets over an explicit class axis |
| `ml/mean-squared-error` | 2 | Scalar mean-squared-error loss for same-shape supported Tensor inputs |
| `ml/conv1d` | 6 | Dense NCW 1D convolution with OCK kernels, explicit stride/padding/dilation/groups, and supported Tensor inputs |
| `ml/conv2d` | 9 | Dense NCHW 2D convolution with OIHW kernels, explicit stride/padding/dilation/groups, and supported Tensor inputs |
| `ml/max-pool2d` | 7 | Dense NCHW 2D max pooling with explicit window/stride/padding and supported Tensor inputs |
| `ml/avg-pool2d` | 7 | Dense NCHW 2D average pooling with explicit window/stride/padding and supported Tensor inputs |
| `nn/validate` | 1 | Validate an Omni Neural DataSpec dictionary and return it, or raise `nn/invalid-spec` with diagnostics |
| `nn/init` | 1-2 | Build/initialize a model bundle from a valid Omni Neural DataSpec with optional init options (`dtype`, `device`, `seed`, `mode`) and return fields `kind` (`'model`), `spec`, `params`, `state`, `mode`, `dtype`, `device`, `metadata` |
| `nn/sequential` | variadic | Construct a normalized sequential layer spec from one or more layer specs |
| `nn/dense` | 2-3 | Construct a normalized dense layer spec from input/output features and optional options |
| `nn/conv1d` | 7-8 | Construct a normalized dense NCW convolution layer spec with optional activation/bias/initializer options |
| `nn/conv2d` | 7-8 | Construct a normalized dense NCHW convolution layer spec with optional activation/bias/initializer options |
| `nn/max-pool2d` | 3 | Construct a normalized max-pooling layer spec from window/stride/padding pairs |
| `nn/avg-pool2d` | 3 | Construct a normalized average-pooling layer spec from window/stride/padding pairs |
| `nn/flatten` | 0-2 | Construct a normalized flatten layer spec with optional start/end axes |
| `nn/activation` | 1 | Construct a normalized activation layer spec |
| `nn/relu` | 0 | Construct a ReLU activation layer spec |
| `nn/sigmoid` | 0 | Construct a sigmoid activation layer spec |
| `nn/tanh` | 0 | Construct a tanh activation layer spec |
| `nn/gelu` | 0 | Construct a GELU activation layer spec |
| `nn/softmax` | 0 | Construct a softmax activation layer spec |
| `nn/apply` | 2, 4-5 | Run inference from normalized model or explicit `(spec params state input [options])` data; optional options must currently be empty and supported layers lower to `ml/*` |
| `nn/predict` | 2 | Run inference through a model bundle in `'eval` mode only |
| `nn/summary` | 1 | Return inspectable summary metadata from a model bundle (`kind`, `mode`, `dtype`, `device`, `layers`, `parameter-tensors`, `parameter-elements`) |
| `nn/spec` | 1 | Extract the normalized model spec from a model bundle |
| `nn/parameters` | 1 | Extract the parameter tree from a model bundle |
| `nn/state` | 1 | Extract the state tree from a model bundle |
| `nn/mode` | 1 | Read the model mode (`'train` / `'eval`) from a model bundle |
| `nn/save-spec` | 1-2 | Serialize a non-model Omni Neural DataSpec checkpoint to a JSON string, or write it to a path and return `Void` |
| `nn/load-spec` | 1 | Load a non-model DataSpec checkpoint from a JSON string or path and revalidate it |
| `nn/save` | 1-2 | Serialize a model bundle checkpoint, including spec, params, state, mode, dtype, device, metadata, tensor dtype/shape/data, and placement metadata |
| `nn/load` | 1 | Load a model bundle checkpoint from a JSON string or path, restore parameter tensors, and revalidate the model |
| `gcd` | 2 | Greatest common divisor |
| `lcm` | 2 | Least common multiple |

Core scalar math functions return `BigFloat` when a `BigFloat` operand
participates for trigonometric, inverse trigonometric, hyperbolic, exponential,
logarithmic, power/root, gamma/error-function, and standard-normal helpers.
`floor`, `ceiling`, `round`, and `truncate` instead return exact integer
values, narrowing to `Integer` when representable and promoting to `BigInteger`
otherwise. Huge integer materializations fail closed.
Non-`BigFloat` floating inputs continue to return `Float64`.
`BigComplex`, `Complex128`, and `Complex64` participate in `+`, `-`, `*`, `/`,
unary `-`, `=`, and `abs` within their compatible complex families. Fixed-width
complex operations may mix with real numeric values and promote `Complex64` to
`Complex128` when a `Complex128` operand participates; they do not implicitly
coerce pointer-backed `BigComplex` to fixed-width storage. `abs` returns a
`BigFloat` magnitude for `BigComplex`, a `Float64` magnitude for `Complex128`,
and a `Float32` magnitude for `Complex64`. `real-part` and `imag-part` return
`BigFloat` components for `BigComplex`, `Float64` components for `Complex128`,
and `Float32` components for `Complex64`; real scalar inputs keep their value
as the real part and use `0` as the imaginary part. `conjugate` preserves real
scalar inputs and returns the matching complex family for complex inputs. For
`BigComplex`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
`tanh`, `exp`, `log`, `log10`, `sqrt`, and `pow` preserve complex results when
a complex operand participates. Fixed-width complex scientific/transcendental
operations beyond arithmetic, component helpers, `abs`, and `conjugate` remain
fail-closed until a dedicated approximation/precision contract lands. `atan2`
remains real-valued and rejects complex operands. Ordered comparisons and
ordered helpers such as `min`, `max`, `positive?`, and `negative?` fail closed
for complex operands.

**Bitwise:**

| Name | Arity | Description |
|------|-------|-------------|
| `bitwise-and` | 2 | AND |
| `bitwise-or` | 2 | OR |
| `bitwise-xor` | 2 | XOR |
| `bitwise-not` | 1 | NOT |
| `lshift` | 2 | Left shift |
| `rshift` | 2 | Right shift |

**Collections:**

| Name | Arity | Description |
|------|-------|-------------|
| `Array` | variadic | Canonical array constructor / conversion surface |
| `Dictionary` | variadic | Canonical dictionary constructor |
| `Dict` | variadic | Allowed shorthand constructor alias for `Dictionary` |
| `Set` | variadic | Create set |
| `set-add` | 2 | Add to set |
| `set-remove` | 2 | Remove from set |
| `set-contains?` | 2 | Set membership |
| `length` | 1 | Set cardinality |
| `List` | 1 | Convert set to list |

**Tensor:**

| Name | Arity | Description |
|------|-------|-------------|
| `Tensor` | 1-3 | Construct native CPU `Float64`, `Float32`, `Complex128`, `Complex64`, `BigInteger`, `BigFloat`, or `BigComplex` tensor storage as `(Tensor data)`, `(Tensor data dtype)`, `(Tensor iterator)`, `(Tensor iterator dtype)`, `(Tensor dtype shape data-or-scalar)`, or `(Tensor dtype shape iterator)` |
| `tensor?` | 1 | Predicate for native tensor values |
| `length` | 1 | Tensor element count |
| `dtype` | 1 | Tensor dtype symbol |
| `shape` | 1 | Tensor shape array |
| `rank` | 1 | Tensor rank |
| `tensor-layout` | 1 | Tensor metadata dictionary: dtype/device/payload/layout/shape/strides/rank/element-count/byte-length/storage offset and extent/view ownership/write policy |
| `contract` | 3-4 | Summed-axis tensor contraction for native tensor dtypes |
| `realize` | 1-2 | Low-level tensor storage primitive: return concrete tensor storage or write tensor/scalar source into a destination tensor |
| `matrix/transpose-view` | 1 | Construct a read-only rank-2 transpose view over a Tensor source |

Tensor indexing uses generic `ref`. Tensor length uses generic `length`.
Tensor elementwise operations use generic `map` through `^Tensor` dispatch
methods. Lazy tensor expression payloads are internal to `Tensor`; there is no
public `TensorExpr` primitive or type. `tensor-layout` is query-style
introspection only. It reports payload values `concrete`, `map`, `contract`,
or `view`; layout values `dense-row-major` or `strided`; owner values `self`,
`view-source`, or `expression`; and write-policy values `mutable`,
`immutable`, `mutable-view`, or `read-only-view`. `matrix/transpose-view` is
the first public view constructor. Its views are read-only, source-owned, and
CPU-readable through `ref`, `Array`, `List`, and `realize`. The view contract
does not make GPU kernels or raw copy helpers accept strided/view-backed
inputs; unsupported device/copy paths fail closed.

`Complex128` and `Complex64` Tensor support includes CPU execution and
capability-gated CUDA/Vulkan storage round-trips. CPU construction, `ref`,
flat collection conversion, destination `realize`, tensor `map`, `contract`,
`real-part`, `imag-part`, and `conjugate` preserve the fixed-width complex
contract. `real-part` and `imag-part` return `Float64` tensors for
`Complex128` inputs and `Float32` tensors for `Complex64` inputs; `conjugate`
returns the same complex dtype. CUDA and Vulkan may accept explicit
`to-device` placement and explicit `to-device 'cpu` copyback for
`Complex128` or `Complex64` concrete dense storage when `tensor-backends`
reports the matching `complex128` or `complex64` capability. CUDA/Vulkan
storage capability is not operation capability. Vulkan fixed-width complex
`map` is available only when `tensor-backends` reports
`elementwise-map-complex128` or `elementwise-map-complex64`; it supports dense
row-major `+`, `-`, `*`, `/`, unary `+`, `abs`, unary `-`, `real-part`,
`imag-part`, and `conjugate` without hidden CPU fallback. CUDA fixed-width
complex `map` uses the same operation capability bits when its generated PTX
helper is available and supports binary `+`, `-`, `*`, `/`; unary `abs`,
unary `-`, identity/`+`, `real-part`, `imag-part`, and `conjugate`. Generic
CUDA complex map preserves complex dtype, including zero-imaginary results for
map `real-part`, `imag-part`, and `abs`; direct CUDA `real-part`, `imag-part`,
and `abs` return component-width real CUDA tensors. CUDA/Vulkan complex
`contract` is available only when `tensor-backends` reports
`contract-complex128` or `contract-complex64`; supported contractions compute
on the selected device, preserve fixed-width complex dtype and placement, and
require explicit `to-device 'cpu` for CPU inspection. CUDA/Vulkan complex
structural matrix support is available on Vulkan when `tensor-backends` reports
`matrix-structural-complex128` or `matrix-structural-complex64`; supported
`matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
`matrix/trace` operations compute on Vulkan and preserve fixed-width complex
dtype/placement except for `matrix/trace`, which reads back the public scalar
result. CUDA structural complex matrix kernels and CUDA/Vulkan complex
numerical matrix kernels still fail closed unless a matrix operation has its
own explicit backend contract and capability reporting. Matrix support is not
implied by complex storage, complex elementwise `map`, or complex `contract`.

**Coroutines:**

| Name | Arity | Description |
|------|-------|-------------|
| `Coroutine` | 1 | Canonical coroutine constructor |
| `resume` | 1 | Resume coroutine |
| `yield` | 1 | Yield value |
| `coroutine?` | 1 | Type check |

**Error:**

| Name | Arity | Description |
|------|-------|-------------|
| `error` | 1 | Create error value |
| `error-message` | 1 | Extract message |

**Iterator:**

| Name | Arity | Description |
|------|-------|-------------|
| `next` | 1 | Get next (value . rest) pair |

Preferred iterator consumption uses collection constructors:
`(List it)` and `(Array it)`.

**Misc:**

| Name | Arity | Description |
|------|-------|-------------|
| `gensym` | 0 | Generate unique symbol |
| `format` | variadic | Printf-style formatting |
| `cl-format` | variadic | CL-style formatting |
| `read-string` | 1 | Parse string to Lisp |
| `eval` | 1 | Evaluate expression |
| `apply` | 2 | Apply function to arg list |
| `macroexpand` | 1 | Expand macro |
| `load` | 1 | Load and evaluate file |
| `unsafe-free!` | 1 | Free heap backing; returns `Void` |

**Schema:**

| Name | Arity | Description |
|------|-------|-------------|
| `validate` | 2 | Validate value against schema |
| `schema-explain` | 2 | Explain schema validation failures |

**Explainability:**

| Name | Arity | Description |
|------|-------|-------------|
| `explain` | 2 | Selector-locked explain surface: `(explain 'dispatch <form>)` or `(explain 'effect <form>)` |

**System / OS:**

| Name | Arity | Description |
|------|-------|-------------|
| `shell` | 1-2 | Execute shell command |
| `getenv` | 1 | Read environment variable |
| `time` | 0 | UNIX timestamp (seconds) |
| `time-ms` | 0 | UNIX timestamp (milliseconds) |
| `sleep` | 1 | Sleep for N seconds |
| `exit` | 0-1 | Exit process with optional status code |
| `random` | 0 | Random floating-point in [0, 1) |
| `random-int` | 1 | Random integer in [0, n) |

**Concurrency:**

| Name | Arity | Description |
|------|-------|-------------|
| `atomic` | 1 | Create atomic integer |
| `atomic-add!` | 2 | Atomic fetch-and-add |
| `atomic-read` | 1 | Atomic load |
| `atomic-cas!` | 3 | Compare-and-swap |
| `spawn` | 1 | Spawn fiber task |
| `await` | 1 | Await spawned task result |
| `run-fibers` | 0 | Run scheduled fibers to completion (returns `Void`) |
| `fiber-cancel` | 1 | Cancel a fiber (always returns `Void` for valid fiber ids; already-done/running targets are no-op completions) |

**Regex (Pika):**

| Name | Arity | Description |
|------|-------|-------------|
| `re-match` | 2 | First match |
| `re-fullmatch` | 2 | Full string match |
| `re-find-all` | 2 | All matches |
| `re-split` | 2 | Split by pattern |
| `re-replace` | 3-4 | Replace matches |
| `re-match-pos` | 2 | Match with positions |
| `re-find-all-pos` | 2 | All matches with positions |

**PEG Grammar (Pika):**

| Name | Arity | Description |
|------|-------|-------------|
| `pika/grammar` | variadic | Define grammar |
| `pika/parse` | 2 | Parse input |
| `pika/fold` | 3 | Fold over parse tree |
| `pika/grammar-rules` | 1 | List rule names |
| `pika/match-span` | 3 | Match specific rule |
| `pika/parse-lisp` | 1 | Parse Lisp syntax |

**Unicode:**

| Name | Arity | Description |
|------|-------|-------------|
| `string-normalize` | 2 | Unicode normalize |
| `string-graphemes` | 1 | Split into graphemes |
| `string-codepoints` | 1 | List of codepoints |
| `char-category` | 1 | Unicode category |
| `char-width` | 1 | Display width hint (-1/0/1/2) |
| `char-property` | 2 | Unicode property lookup by symbol |

**JSON:**

| Name | Arity | Description |
|------|-------|-------------|
| `json-parse` | 1-2 | Parse JSON string |
| `json-emit` | 1-2 | Emit compact JSON (optional options list for writer flags) |
| `json-emit-pretty` | 1-2 | Emit pretty JSON (optional options list for writer flags) |
| `json-get` | 2 | JSON Pointer-style lookup on Omni JSON values |

**TOML / CSV:**

| Name | Arity | Description |
|------|-------|-------------|
| `TimePoint` | variadic | Canonical TimePoint constructor |
| `time-point?` | 1 | TimePoint predicate |
| `toml-parse` | 1-2 | Parse TOML string (optional options list: `((check-utf8 false))`)
| `csv-parse` | 1-2 | Parse CSV text into rows (`delimiter` string or option list including `strict`; strict/default enforces RFC-4180 CRLF row endings) |
| `csv-emit` | 1-2 | Emit rows as CSV text (`delimiter`, `line-ending`, `quote-char`, `quote-style`, `nil-as`, `strict`; strict/default line ending is `\\r\\n`) |

**Compression:**

| Name | Arity | Description |
|------|-------|-------------|
| `gzip` | 1-2 | Gzip compress (optional level 0..12) |
| `gunzip` | 1 | Gzip decompress |
| `deflate` | 1-2 | Deflate compress (optional level 0..12) |
| `inflate` | 1-2 | Deflate decompress |
| `zlib-compress` | 1-2 | Zlib compress (optional level 0..12) |
| `zlib-decompress` | 1-2 | Zlib decompress |
| `adler32` | 1 | Adler-32 checksum |
| `crc32` | 1 | CRC-32 checksum |

**Networking:**

| Name | Arity | Description |
|------|-------|-------------|
| `__raw-tcp-connect` | 2 | Raw TCP connect |
| `__raw-tcp-listen` | variadic | Raw TCP listen |
| `__raw-tcp-accept` | 1 | Raw TCP accept |
| `__raw-tcp-read` | variadic | Raw TCP read |
| `__raw-tcp-write` | 2 | Raw TCP write |
| `__raw-tcp-close` | 1 | Raw TCP close |
| `__raw-udp-socket` | 0 | Raw UDP socket create |
| `__raw-udp-bind` | variadic | Raw UDP bind |
| `__raw-udp-send` | variadic | Raw UDP send |
| `__raw-udp-recv` | variadic | Raw UDP receive |
| `__raw-udp-close` | 1 | Raw UDP close |
| `__raw-pipe-connect` | 1 | Raw Unix socket connect |
| `__raw-pipe-listen` | 1 | Raw Unix socket listen |
| `__raw-process-spawn` | variadic | Raw process spawn |
| `__raw-process-wait` | 1 | Raw process wait |
| `__raw-process-kill` | variadic | Raw process kill |
| `__raw-signal-handle` | variadic | Raw signal watcher install |
| `__raw-signal-unhandle` | 1 | Raw signal watcher remove |
| `__raw-dns-resolve` | 1 | Raw DNS resolve |
| `__raw-async-sleep` | 1 | Raw sleep |
| `__raw-offload` | 1 | Raw pooled offload job |
| `__raw-offload-batch` | 1 | Raw pooled offload job batch |
| `__raw-thread-spawn` | 1 | Raw OS thread spawn |
| `__raw-thread-spawn-batch` | 1 | Raw OS thread spawn batch |
| `__raw-thread-join` | 1 | Raw OS thread join |
| `__raw-thread-join-timeout` | 1 | Raw OS thread join with timeout pair |
| `__raw-thread-cancel` | 1 | Raw OS thread cancel |
| `__raw-task-spawn` | 1 | Raw pooled task spawn |
| `__raw-task-spawn-batch` | 1 | Raw pooled task spawn batch |
| `__raw-task-join` | 1 | Raw pooled task join |
| `__raw-task-join-timeout` | 1 | Raw pooled task join with timeout pair |
| `__raw-task-cancel` | 1 | Raw pooled task cancel |
| `__raw-tls-connect` | variadic | Raw TLS connect |
| `__raw-tls-server-wrap` | 3 | Raw TLS server wrap |
| `__raw-tls-read` | variadic | Raw TLS read |
| `__raw-tls-write` | 2 | Raw TLS write |
| `__raw-tls-close` | 1 | Raw TLS close |
| `__raw-http-get` | 1 | Raw HTTP GET |
| `__raw-http-request` | variadic | Raw HTTP request |

**Deduce (Database):**

| Name | Arity | Description |
|------|-------|-------------|
| `deduce` | variadic | Unified database interface |
| `deduce/open` | variadic | Open deduce database handle |
| `deduce/open-named` | variadic | Open/register named relation handle |
| `deduce/block` | variadic | Open deduce transaction handle (`'read`, `'write`, `'write-deferred`) |
| `deduce/commit` | variadic | Commit transaction (`Void`) |
| `deduce/abort` | variadic | Abort transaction (`Void`) |
| `deduce/scan` | variadic | Scan relation tuples |
| `deduce/scan-range` | variadic | Bounded relation scan |
| `deduce/query` | variadic | Query rows by predicate |
| `deduce/count` | variadic | Count relation tuples |
| `deduce/match` | variadic | Pattern match relation rows |
| `deduce/fact!` | variadic | Assert fact (`Void`) |
| `deduce/retract!` | variadic | Retract fact (`Void`) |
| `deduce/clear!` | variadic | Clear relation (`Void`) |
| `deduce/drop!` | variadic | Drop relation (`Void`) |
| `deduce/rule!` | variadic | Install rule (`Void`) |
| `deduce/explain` | variadic | Explain deduce query/rule plan |
| `deduce/analyze` | variadic | Analyze deduce relation/rule surface |
