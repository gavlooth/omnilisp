# Collections, Strings & Math

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 9. Collections

Omni has three collection types plus sets.

### Lists (Linked)

```lisp
'(1 2 3)                   ;; quoted list
(list 1 2 3)               ;; explicit construction
(List [1 2 3])             ;; canonical constructor/conversion surface
(cons 1 (cons 2 nil))      ;; manual construction
(car '(1 2 3))             ;; => 1
(cdr '(1 2 3))             ;; => (2 3)
(length '(1 2 3))          ;; => 3
```

### Arrays (Contiguous, Mutable)

```lisp
[1 2 3]                    ;; array literal (canonical constructor surface: Array)
(Array 1 2 3)              ;; canonical construction
(ref arr 0)                ;; => first element
(push! arr 4)              ;; append element (mutates, returns Void)
(set! arr 0 99)            ;; generic update form (returns Void)
(length arr)               ;; => size
(Array '(1 2 3))           ;; list -> array conversion
(list [1 2 3])             ;; array -> list conversion
```

### Dicts (Hash Map, Mutable)

```lisp
{'name "Alice" 'age 30}    ;; dict literal (canonical constructor surface: Dictionary)
(Dictionary 'name "Alice")  ;; canonical construction
(Dictionary "name" "Alice") ;; string keys are supported
(ref d 'name)               ;; => "Alice"
(ref (Dictionary "name" "Alice") "name") ;; => "Alice"
(set! d 'email "a@b")       ;; generic update form (returns Void)
(has? d 'age)               ;; => true
(keys d)                    ;; => (age name)    ; canonical key order
(values d)                  ;; => (30 "Alice")  ; aligned with keys order
(remove! d 'age)            ;; remove key (returns Void)
(length d)                  ;; => number of entries
```

### Sets

```lisp
(Set 1 2 3)                ;; create set
(set-add s 4)              ;; => Void (mutates set in place)
(set-remove s 2)           ;; => Void (mutates set in place)
(set-contains? s 1)        ;; => true
(length s)                 ;; => number of elements
(List s)                   ;; => canonical ordered list of set elements
```

Sets now have a distinct builtin `Set` runtime type symbol. `type-of` reports
`Set` for `(Set ...)` values, and printed set values use constructor-shaped
syntax such as `(Set 1 2 3)`.

Naming policy for new code/examples:
- prefer `List`, `Array`, and `Dictionary` as constructor/coercion surfaces
- keep `list` as an approved idiomatic public helper

Dictionary key policy:
- symbols are preferred for internal language-owned maps (`'name`, `'code`)
- strings are preferred at data boundaries (JSON/HTTP/config payloads)
- keys are value-typed (`Dictionary` supports symbol, string, int, etc. keys)

Ordering contract:
- `keys` and `values` are deterministic and share the same canonical key order.
- `List` is deterministic and uses canonical element order.
- Treat dict/set literal insertion order as non-authoritative for iteration APIs.

### Tensors

`Tensor` is Omni's rank-polymorphic scientific numeric aggregate. The current
runtime ships native `Float64`, `Float32`, `BigInteger`, `BigFloat`,
`BigComplex`, `Complex128`, and `Complex64` tensor storage, generic `length` introspection, tensor indexing through
generic `ref`, tensor-dispatched `map` for all native dtypes, tensor
`contract`, and constructor-driven materialization for iterator and Tensor
conversion surfaces. `realize` remains a low-level destination-storage
primitive, not the general lazy abstraction. `BigInteger`,
`BigFloat`, `BigComplex`, `Complex128`, and `Complex64` tensors support constructor/ref/flat collection
conversion/concrete storage paths, elementwise `map`, and summed-axis
`contract`. Tensors also support elementwise `real-part`, `imag-part`, and
`conjugate`: real Tensor dtypes preserve dtype for real/conjugate and return
same-dtype zeros for imaginary components, while BigComplex component
extraction returns BigFloat tensors, Complex128 component extraction returns
Float64 tensors, and Complex64 component extraction returns Float32 tensors.
`conjugate` preserves fixed-width complex Tensor dtype. Tensor `abs` applies
elementwise magnitude; real Tensor dtypes preserve dtype, BigComplex tensors
return BigFloat magnitude tensors, Complex128 tensors return Float64 magnitude
tensors, and Complex64 tensors return Float32 magnitude tensors. Tensor `sqrt` applies elementwise square root:
Float64 and BigInteger Tensor inputs return Float64 tensors, BigFloat tensors
preserve BigFloat, and BigComplex tensors preserve BigComplex. Tensor `sin`,
`cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`,
and `log10` follow the same unary scientific-math dtype contract. Tensor
`pow` supports tensor-scalar, scalar-tensor, and broadcast tensor-tensor
powers; `BigComplex` wins the result dtype, then `BigFloat`, otherwise the
result is a Float64 tensor. Tensor `atan2` supports tensor-scalar,
scalar-tensor, and broadcast tensor-tensor real-plane arctangent; `BigFloat`
inputs preserve BigFloat, other real/exact inputs return Float64 tensors, and
complex Tensor operands fail closed. Tensor `stats/normal-cdf` and supported
Tensor `stats/normal-quantile` paths apply elementwise to real Tensor inputs:
`Float64` and `Float32` preserve float dtype, `BigInteger` returns `Float64`
where the operation's domain permits it, `BigFloat` preserves dtype, and
`BigComplex` fails closed. CUDA-placed dense row-major `Float64`/`Float32`
tensors and Vulkan-placed dense row-major `Float64`/`Float32` tensors also
support `stats/normal-quantile`. CPU/CUDA/Vulkan Tensor
`stats/normal-quantile` fails the whole operation on the first invalid
probability before exposing output. Tensor `floor`, `ceiling`, `round`, and
`truncate` return
same-shape BigInteger tensors for real inputs, with exact BigFloat rounding for
BigFloat tensors; complex Tensor operands fail closed. Backend implementations
must preserve this dtype-changing result contract rather than returning
same-dtype float Tensor outputs.
Tensor `min` and `max` support tensor-scalar, scalar-tensor, and broadcast
tensor-tensor real comparison. `BigFloat` wins if either input is BigFloat,
`Float64` wins if either input is Float64, otherwise the result is a
`BigInteger` tensor. Tensor `gcd` and `lcm` support tensor-scalar,
scalar-tensor, and broadcast tensor-tensor exact integer operations. Tensor
operands must be native `BigInteger` tensors, scalar operands must be exact
integers, and results are native `BigInteger` tensors. `matrix/transpose`
transposes rank-2 Tensor values while preserving native dtype, including
`Float64`, `BigInteger`, `BigFloat`, and `BigComplex`. For concrete Tensor
inputs it materializes a transposed Tensor. When the input is already a
transpose view, `matrix/transpose` composes structurally back to the source
orientation instead of allocating a second transposed buffer.
`matrix/transpose-view` constructs a read-only rank-2 transpose view. The view
keeps the source Tensor as owner, swaps logical shape and strides, reports
payload `view`, owner `view-source`, `owns-storage` false, and write-policy
`read-only-view`, and supports CPU `ref`, `(Array view)`, `(List view)`, and
CPU `realize` materialization. Writes through the view fail closed, and
CUDA placement and broader CUDA/Vulkan device kernels do not consume arbitrary
view metadata yet. Vulkan supports explicit materialization for direct rank-2
transpose views over dense zero-offset Vulkan storage through `realize`,
`to-device 'vulkan`, and `to-device 'cpu` copyback. `matrix/diagonal`
extracts the main diagonal from a rank-2 Tensor into a rank-1 Tensor of length
`min(rows, columns)`, preserving native dtype and cloning owned high-precision
element handles. `matrix/trace` sums the diagonal of a square rank-2 Tensor
and returns a scalar in the Tensor's native numeric family: `Double` for
`Float64`, `Float32` for `Float32`, or `BigInteger`, `BigFloat`, and
`BigComplex` for the matching high-precision Tensor dtypes.
`matrix/diagonal-matrix` builds a square rank-2
Tensor from a rank-1 Tensor, placing cloned input values on the main diagonal
and the dtype's zero value in off-diagonal cells. `matrix/identity` builds a
square identity Tensor from a non-negative integer size, defaulting to
`Float64` and accepting an optional `Float64`, `Float32`, `BigInteger`,
`BigFloat`, or `BigComplex` dtype argument. `matrix/rank` returns the
numerical rank of a rectangular rank-2 `Float64`, `Float32`, `Complex128`, or
`Complex64` Tensor as an `Integer`, using a default tolerance of `1e-12` or an
optional non-negative finite numeric tolerance. Complex rank uses
magnitude-based pivoting. `matrix/norm` returns a `Float64` scalar norm for
rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensors. It
defaults to the Frobenius norm and accepts an optional selector:
`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, or `'nuclear`.
`Float64`, `Float32`, `Complex128`, and `Complex64` support every selector.
Complex direct norms use complex magnitudes; complex spectral and nuclear norms
use the fixed-width complex singular-value path and still return `Float64`
scalars. `matrix/solve`
solves rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor linear
systems with a square coefficient matrix and a rank-1 or rank-2 matching
right-hand Tensor. The result preserves RHS rank and dtype, and singular
systems raise `tensor/singular-matrix`. `matrix/lu` factors a square rank-2
`Float64`, `Float32`, `Complex128`, or `Complex64` Tensor with partial pivoting
and returns a dictionary with `lu` for the combined factors, `pivots` for the
final 0-based row order, and `swap-count` for permutation parity.
`matrix/determinant` uses the same partial-pivot LU semantics to return a
scalar determinant in the input numerical family; singular matrices return the
dtype's zero scalar. `matrix/inverse` returns the inverse of a nonsingular
square rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor and
raises `tensor/singular-matrix` for singular inputs. `matrix/qr` computes a
reduced QR decomposition for rank-2 `Float64`, `Float32`, `Complex128`, or
`Complex64` Tensors with rows greater than or equal to columns, returning a
dictionary with `q` shape `[rows columns]` and `r` shape `[columns columns]`;
rank-deficient inputs raise `tensor/singular-matrix`. Complex QR uses the
Hermitian inner product. `matrix/cholesky` computes the lower-triangular
Cholesky factor for square symmetric positive-definite real tensors or square
Hermitian positive-definite complex tensors and returns a Tensor with the same
shape and dtype; nonsymmetric, non-Hermitian, or non-positive-definite inputs
raise `tensor/not-positive-definite`. For Vulkan-placed dense row-major
`Float32`
inputs, `matrix/determinant`,
`matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
`matrix/qr` use native serial Float32 Vulkan kernels; Tensor results remain
Vulkan-placed and preserve `Float32`, while scalar and host metadata results
preserve the existing public contracts. `matrix/singular-values` returns the
descending singular values of a rank-2 `Float64`, `Float32`, `Complex128`, or
`Complex64` Tensor with shape `[min(rows, columns)]`. `Float64` and
`Complex128` inputs return `Float64` singular-value tensors; `Float32` and
`Complex64` inputs return `Float32` singular-value tensors. Vulkan-placed dense
row-major inputs return Vulkan-placed real singular-value tensors under the
same dtype and shape contract. Complex singular values use realification and
collapse duplicate real singular-value pairs.
`matrix/svd` computes a reduced singular value decomposition for
rank-2 `Float64` Tensors and returns a dictionary with `u` shape `[rows k]`,
`s` shape `[k]`, and `v` shape `[columns k]`, where
`k = min(rows, columns)`. Vulkan-placed dense row-major `Float32` inputs return
Vulkan-placed `Float32` factor tensors under the same shape contract; CPU
`Float32` inputs return `Float32` factor tensors. `matrix/eigenvalues` computes
descending real eigenvalues for square symmetric rank-2 `Float64` Tensors, and
`matrix/eigenvectors` returns a dictionary with aligned `values` and
`vectors` columns. Nonsymmetric inputs raise `tensor/not-symmetric`.
`matrix/eigenpairs` computes general nonsymmetric eigenpairs for square
rank-2 `Float64` Tensors and returns `BigComplex` `values` shape `[n]` and
aligned `BigComplex` `vectors` shape `[n n]`, with vector columns aligned to
the sorted values. Empty square matrices return empty `[0]` values and
`[0 0]` vectors. Runtime `LAPACKE_dgeev` may accelerate the operation when
available; missing backend support retains the pure fallback.

```lisp
(define x (Tensor [[1.0 2.0 3.0] [4.0 5.0 6.0]]))

(shape x)                  ;; => [2 3]
(rank x)                   ;; => 2
(dtype x)                  ;; => Float64
(device x)                 ;; => cpu
(ref x [1 -1])             ;; => 6.0
(Array x)                  ;; => [1.0 2.0 3.0 4.0 5.0 6.0]
(List x)                   ;; => (1.0 2.0 3.0 4.0 5.0 6.0)

(define big (Tensor BigFloat [1] [(BigFloat "1e309")]))
(dtype big)                ;; => BigFloat
(String (ref big [0]))     ;; => "1e+309"

(define exact (Tensor BigInteger [1] [(BigInteger "9223372036854775808")]))
(dtype exact)              ;; => BigInteger
(String (ref exact [0]))   ;; => "9223372036854775808"

(define complex (Tensor BigComplex [1] [(BigComplex 1 2)]))
(dtype complex)            ;; => BigComplex
(String (ref complex [0])) ;; => "1+2i"
(String (ref (real-part complex) [0])) ;; => "1"
(String (ref (imag-part complex) [0])) ;; => "2"
(String (ref (conjugate complex) [0])) ;; => "1-2i"
(String (ref (abs (Tensor BigComplex [1] [(BigComplex 3 4)])) [0])) ;; => "5"
(String (ref (sqrt (Tensor BigComplex [1] [(BigComplex -1 0)])) [0])) ;; => "0+1i"
(ref (exp (Tensor BigInteger [1] [0])) [0]) ;; => 1.0
(define f32 (Tensor Float32 [2] [1.25 2.5]))
(dtype f32)                ;; => Float32
(ref f32 [1])              ;; => 2.5 ; scalar Float32
(String (dtype (map + f32 0.5))) ;; => "Float32"
(define c128 (Tensor Complex128 [1] [(Complex128 3 4)]))
(dtype c128)               ;; => Complex128
(String (ref c128 [0]))    ;; => "3.0+4.0i"
(dtype (real-part c128))   ;; => Float64
(String (ref (conjugate c128) [0])) ;; => "3.0-4.0i"
(define c64 (Tensor Complex64 [1] [(Complex64 3 4)]))
(dtype (imag-part c64))    ;; => Float32
(ref (tensor-layout (Tensor Float64 [2 3] [1 2 3 4 5 6])) 'strides) ;; => [3 1]
(ref (tensor-layout (Tensor Float32 [2] [1 2])) 'layout) ;; => dense-row-major
(ref (pow (Tensor Float64 [2 1] [2 3]) (Tensor Float64 [1 2] [2 3])) [1 1]) ;; => 27.0
(String (ref (atan2 (Tensor Float64 [1] [1.0]) 1.0) [0])) ;; => "0.785398163397448"
(String (dtype (floor (Tensor Float64 [1] [3.7])))) ;; => "BigInteger"
(String (ref (ceiling (Tensor BigFloat [1] [(BigFloat "9223372036854775808.1")])) [0])) ;; => "9223372036854775809"
(String (ref (min (Tensor BigInteger [1] [(BigInteger "5")]) 4) [0])) ;; => "4"
(String (dtype (max (Tensor BigFloat [1] [(BigFloat "2.5")]) 4))) ;; => "BigFloat"
(String (ref (gcd (Tensor BigInteger [1] [(BigInteger "18")]) 12) [0])) ;; => "6"
(String (ref (lcm (Tensor BigInteger [2 1] [3 4]) (Tensor BigInteger [1 2] [5 6])) [1 1])) ;; => "12"
(ref (imag-part x) [0 0]) ;; => 0.0

(define y (Tensor Float64 [2 3] 0.0))
(Tensor (map (lambda (v) (+ v 1)) (Iterator [1 2 3]))) ;; => rank-1 Float64 Tensor
(realize (map + x 1.0) y) ;; low-level destination write into y
(realize (Tensor Float64 [] 3.0) (Tensor Float64 [] 0.0))
(realize 9 (Tensor Float64 [0] 0.0))
(realize (map + (Tensor Float64 [0] 0.0) 1.0) (Tensor Float64 [0] 0.0))
(ref (to-device (map + (Tensor Float64 [1] [2]) 1.0) 'cpu) [0]) ;; => 3.0

(define a (Tensor Float64 [2 3] [1 2 3 4 5 6]))
(define b (Tensor Float64 [3 2] [7 8 9 10 11 12]))
(ref (contract a b [1 0]) [1 1]) ;; => 154.0

(define coeffs (Tensor Float64 [2 2] [2 1 1 3]))
(define rhs (Tensor Float64 [2] [1 2]))
(ref (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6])) [2 1]) ;; => 6.0
(define tv (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])))
(ref tv [2 1]) ;; => 6.0
(ref (tensor-layout tv) 'payload) ;; => view
(ref (tensor-layout tv) 'write-policy) ;; => read-only-view
(ref (matrix/diagonal (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1]) ;; => 5.0
(ref (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3])) [2 2]) ;; => 3.0
(ref (matrix/identity 3) [2 2]) ;; => 1.0
(matrix/trace (Tensor Float64 [2 2] [1 2 3 4])) ;; => 5.0
(matrix/rank (Tensor Float64 [2 3] [1 2 3 2 4 6])) ;; => 1
(matrix/rank (Tensor Float32 [2 3] [1 0 0 0 1 0])) ;; => 2
(matrix/rank (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)])) ;; => 2
(matrix/norm (Tensor Float64 [2 2] [3 4 0 0])) ;; => 5.0
(matrix/norm (Tensor Float32 [2 2] [3 4 0 0])) ;; => 5.0
(matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)])) ;; => 5.0
(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'one) ;; => 9.0
(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'spectral) ;; => 3.0
(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'nuclear) ;; => 5.0
(ref (matrix/solve coeffs rhs) [1]) ;; => 0.6

(define lu (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])))
(ref (ref lu 'lu) [1 0]) ;; => 0.5
(ref lu 'pivots) ;; => [0 1]
(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1])) ;; => -2.0
(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 0]) ;; => 0.6

(define qr (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])))
(ref (ref qr 'r) [0 1]) ;; => 1.0

(define chol (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])))
(ref chol [1 0]) ;; => 1.0

(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [0]) ;; => 3.0
(define svd (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])))
(ref (ref svd 's) [0]) ;; => 3.0

(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0]) ;; => 3.0
(define eigen (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])))
(ref (ref eigen 'vectors) [0 0]) ;; => 1.0
(define general-eigen (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])))
(String (ref (ref general-eigen 'values) [0])) ;; => "0+1i"
```

Tensor `map` and `contract` may return lazy expression payloads under the
existing `Tensor` value; there is no public `TensorExpr` type and no public
`delay` / `force` protocol for tensor or iterator laziness. Constructor
dispatch is the canonical terminal consumer: `(Array iterator)` and
`(List iterator)` consume iterators, `(Tensor iterator)` consumes a finite
numeric iterator into tensor storage, and `(Tensor lazy-tensor)` normalizes
through the Tensor constructor surface. `(Iterator tensor)` is the matching
collection-view conversion for Tensor values: it yields flat row-major scalar
elements like `(Array tensor)` and `(List tensor)`, realizes lazy CPU Tensor
payloads into Tensor storage before iteration, and fails closed on non-CPU
device tensors until the caller explicitly copies with `to-device 'cpu`.
`realize` remains a low-level Tensor storage primitive for exact-shape/dtype
destination writes. Elementwise
destination realization may alias an input tensor; contraction destination
realization rejects destinations that recursively alias either source tensor,
while zero-byte tensor storage is not treated as an alias.
`contract` accepts paired-axis arrays such as `[1 0]` and `[[1 0] [2 3]]`;
the explicit left/right axis-list form remains available when it is clearer.
Contraction over a zero-size contracted axis produces the additive identity for
the output cell. Zero-size dimensions are valid shape dimensions. Scalar
broadcasting and right-aligned singleton-axis tensor-tensor broadcasting are
supported. `tensor-layout` is the public Tensor metadata query. It returns a
`Dictionary` with `dtype`, `device`, `payload`, `layout`, `dense-row-major`,
`shape`, `strides`, `rank`, `element-count`, `byte-length`,
`storage-offset`, `storage-elements`, `storage-bytes`, `is-view`,
`owns-storage`, `owner`, and `write-policy`. Current payload values are
`concrete`, `map`, `contract`, and `view`; layout values are
`dense-row-major` or `strided`; owner values are `self`, `view-source`, or
`expression`; and write-policy values are `mutable`, `immutable`,
`mutable-view`, or `read-only-view`. Read-only transpose views report payload
`view`, layout `strided`, owner `view-source`, `owns-storage` false, and
write-policy `read-only-view`. Lazy expression payloads report logical element and byte
lengths, while `storage-elements` and `storage-bytes` remain `0` until
realized concrete storage exists. `tensor-layout` describes view metadata, but
backend execution remains fail-closed for view-backed storage unless a specific
kernel says otherwise. Dense CUDA or Vulkan kernels and raw copy helpers still
require zero-offset dense row-major storage.
`(Array tensor)` and `(List tensor)` are explicit collection conversions: they
materialize lazy tensor expressions if needed and return flat row-major element
values. They do not encode shape nesting; use `shape`
alongside the converted data when preserving rank is required. Optional backend
acceleration remains behind the pure `Tensor` fallback. Device placement is
explicit: `device` reports `'cpu` for ordinary Tensor values, and `to-device`
with target `'cpu` realizes a Tensor expression to CPU storage. Destination
`(realize expr out)` writes into an existing CPU Tensor destination, or into an
existing dense row-major CUDA or Vulkan `Float64` or `Float32` destination
when that backend is usable. CPU destinations still reject device-placed
sources; copy those with explicit `to-device 'cpu` first. CUDA destinations
accept matching CPU/CUDA/lazy supported CUDA `Float64` or `Float32` Tensor
sources and matching scalar fills; Vulkan destinations accept matching
CPU/Vulkan/lazy Vulkan `Float64` or `Float32` Tensor sources and matching
scalar fills. Unsupported dtypes, unsupported lazy device expressions, and
cross-backend sources fail closed with Tensor backend diagnostics.
CUDA/cuBLAS support remains explicit-device only: ordinary `map`, `contract`,
one-argument `realize`, CPU-destination `realize`, and `matrix/*` operations do
not silently move values between CPU and GPU. `to-device` with target `'cuda`
copies concrete `Float64` or `Float32` CPU Tensor storage to CUDA when
runtime-loaded CUDA support is usable, and fails closed when CUDA is missing or
unusable.
`to-device` with target `'cpu` copies CUDA Tensor storage back to native CPU
storage. CUDA destination-form `realize` can write matching dense row-major
`Float64` or `Float32` CPU sources, CUDA sources, supported lazy CUDA map or
contract results, and scalar fills into an existing CUDA-placed destination
without introducing a backend-specific public API. `tensor-backends` returns
structured backend availability dictionaries; CPU is available in normal
builds, CUDA availability depends on runtime probing, cuBLAS reports `float64`
and `float32` capability bits separately, and CPU/Vulkan/CUDA entries report
`elementwise-map-float64`, `elementwise-map-float32`,
`scientific-map-float64`, and `scientific-map-float32` capability keys.
CUDA and Vulkan also report the dtype-changing `rounding-big-integer`
capability when their integer-result rounding path is available. Direct Vulkan
`floor`, `ceiling`, `round`, and `truncate` require that key and fail closed
rather than returning same-dtype float tensors when it is unavailable.
CUDA-placed dense row-major `Float64` or `Float32` tensors support binary
elementwise `map` for `+`, `-`, `*`, `/`, `min`, and `max`;
arithmetic/component unary `map` for unary `+`, `abs`, unary `-`, `sqrt`,
`real-part`, `imag-part`, and `conjugate`; and scientific unary `map` for
`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
`log`, `log10`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
`stats/normal-quantile` when the CUDA scientific module loads. Arithmetic
kernels use embedded PTX, while scientific kernels use PTX generated from CUDA
C and libdevice; CUDA quantile uses a device status word so invalid
probabilities fail before result exposure. Supported CUDA binary map operand shapes are tensor/scalar,
scalar/tensor, exact-shape tensor/tensor, and right-aligned singleton-axis
tensor/tensor broadcasting. Direct public `map`, lazy map realization to CUDA,
and CUDA destination realization from lazy CUDA maps preserve CUDA placement.
Direct Tensor `abs`, unary `-`, `sqrt`, `real-part`, `imag-part`,
`conjugate`, and supported scientific unary primitives also preserve CUDA
placement for eligible real CUDA tensors. Unsupported callables, mixed CPU/CUDA
operands, mixed dtype/device operands, and unsupported layouts fail closed with
backend diagnostics instead of silently copying to CPU.
CUDA-placed dense
row-major matching `Float64` or matching `Float32` rank-2/rank-2 single-axis
contractions may execute through cuBLAS GEMM for `[1 0]`, `[0 0]`, `[1 1]`,
and `[0 1]`; rank-2/rank-1, rank-1/rank-2, and rank-1/rank-1 dot
contractions may execute through cuBLAS GEMV. Results are CUDA-placed Tensors.
In those supported CUDA layouts, zero free dimensions preserve CUDA-placed
zero-length outputs and zero contracted dimensions fill non-empty outputs with
the dtype-preserving additive identity without calling cuBLAS. Unsupported
CUDA contract cases fail with backend diagnostics instead of silently copying
to CPU. CUDA concrete Tensor clone/copy supports Omni-owned CUDA payloads and
valid foreign CUDA concrete payloads by cloning into fresh Omni-owned CUDA
storage; fake or invalid CUDA handles remain fail-closed. Vulkan is
reported as a portable explicit GPU backend through `tensor-backends` with
explicit `Float64` and `Float32` kernel capability bits.
`to-device 'vulkan` copies concrete `Float64` or eligible `Float32` CPU Tensor
storage into opaque Vulkan storage when runtime-loaded Vulkan support is usable, and
`to-device 'cpu` copies Vulkan storage back to native CPU storage. Vulkan
destination-form `realize` can write matching dense row-major `Float64` or
`Float32` CPU or Vulkan sources, lazy Vulkan results, and scalar fills into an existing
Vulkan-placed destination without introducing a backend-specific public API.
Vulkan `map` supports dense row-major `Float64` and `Float32` elementwise arithmetic
`+`, `-`, `*`, `/`, `min`, and `max` plus unary `+`, `abs`, unary `-`,
`sqrt`, `real-part`, `imag-part`, and `conjugate`; `Float32` Vulkan tensors
also support `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
`tanh`, `exp`, `log`, `log10`, `stats/normal-cdf`, and
`stats/normal-quantile` through dedicated unary helpers. `stats/normal-cdf`
uses same-dtype shader approximations for `Float32` and `Float64`, while
`stats/normal-quantile` uses status-bearing inverse-CDF helpers for both
float dtypes. Direct Vulkan `floor`, `ceiling`, `round`, and `truncate` are
not inferred from these same-dtype float capabilities; they require the
separate dtype-changing `rounding-big-integer` result path. These apply for
Tensor/scalar, scalar/Tensor, exact-shape Tensor/Tensor inputs, and
right-aligned singleton-axis Tensor/Tensor broadcasting through embedded
SPIR-V compute shaders, returning Vulkan-placed tensors. Direct `min` and
`max` also support matching dense row-major Vulkan `Float64` or `Float32`
operands. Vulkan
`Float32` serial factor/solve operations and staged parallel `Float32`
`matrix/solve` are supported for eligible dense row-major operands; CPU
`Float32` SVD/factor oracles are supported.
`matrix/transpose` supports Vulkan-placed dense row-major `Float64`, `Float32`,
`Complex128`, or `Complex64` rank-2 concrete tensors through embedded SPIR-V
kernels, returning a Vulkan-placed materialized transposed Tensor that
preserves the input dtype.
`matrix/transpose-view` is CPU-readable and also supports explicit Vulkan
materialization for direct rank-2 transpose views over dense zero-offset Vulkan
storage. `realize`, `to-device 'vulkan`, and `to-device 'cpu` copyback produce
dense tensors with values matching the logical transpose. CUDA and broader
strided/view-backed GPU kernels still fail closed for view payloads.
`matrix/diagonal` and `matrix/diagonal-matrix` support Vulkan-placed dense
row-major `Float64`, `Float32`, `Complex128`, or `Complex64` inputs through
embedded SPIR-V structural kernels, returning Vulkan-placed Tensor results that
preserve the input dtype. CUDA and Vulkan fixed-width complex structural matrix
support is reported as `matrix-structural-complex128` and
`matrix-structural-complex64`; when the selected backend reports the relevant
capability, dense row-major `Complex128` or `Complex64` tensors support
`matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`, and
`matrix/trace` on that backend. Complex numerical matrix kernels outside
landed Vulkan `matrix/lu`, `matrix/determinant`, `matrix/solve`,
`matrix/inverse`, `matrix/rank`, and direct `matrix/norm` reducers remain
fail-closed until their specific operation contracts and capability bits land.
`matrix/rank` supports Vulkan-placed dense row-major `Float64`, `Float32`,
`Complex128`, or `Complex64` rank-2 inputs through embedded SPIR-V reducers
and reads back only the scalar `Integer` result required by the public scalar
return contract. Complex rank uses magnitude-based pivoting and tolerance.
`matrix/lu` supports Vulkan-placed dense row-major square `Float64`,
`Float32`, `Complex128`, or `Complex64` inputs through embedded SPIR-V
partial-pivot factorization kernels.
The returned dictionary keeps `lu` as a Vulkan-placed Tensor preserving the
input dtype while `pivots` and `swap-count` remain ordinary host metadata
required by the public contract. Complex numerical capability is reported by
`matrix-numerical-complex128` and `matrix-numerical-complex64`.
`matrix/solve` supports Vulkan-placed dense row-major square `Float64`,
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
`matrix/norm` supports Vulkan-placed dense row-major `Float64` and `Float32`
inputs for default/`'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`,
and `'nuclear`. Vulkan `Complex128` and `Complex64` inputs support every norm
selector; direct selectors use complex magnitudes and spectral/nuclear selectors
use native Vulkan complex singular-value kernels. These Vulkan paths read back
only the scalar result required by the public scalar return contract.
`matrix/singular-values` supports Vulkan-placed dense row-major rank-2
`Float64`, `Float32`, `Complex128`, and `Complex64` inputs through embedded
storage-backed Gram/Jacobi singular-value shaders, returning a Vulkan-placed
rank-1 real Tensor for `k = min(rows, columns)`, including `k > 64`, without
hidden CPU/LAPACK fallback. Complex inputs return component-width real singular
values (`Float64` for `Complex128`, `Float32` for `Complex64`). Direct
`matrix/svd` uses the same storage-backed Gram
strategy for Vulkan-placed factor outputs and preserves the input float dtype.
Shader non-convergence raises `tensor/no-convergence`.
`contract` supports Vulkan-placed dense row-major rank-N matching `Float64` or
`Float32` tensors with zero or more explicit contracted axis pairs. Output axes
are ordered as free left axes followed by free right axes. Results are
Vulkan-placed tensors and require explicit `to-device 'cpu` before CPU
inspection. Unsupported Vulkan map and contract cases, including unsupported
callables, incompatible broadcasting shapes, unsupported layouts/dtypes, and
mixed CPU/Vulkan operands, fail closed with backend diagnostics instead of
silently copying to CPU. Direct
`matrix/svd` supports Vulkan-placed dense row-major rank-2 `Float64` or
`Float32` inputs for `k = min(rows, columns)`, including `k > 64`, and returns
Vulkan-placed reduced factors with the input float dtype through storage-backed
Gram scratch without hidden CPU/LAPACK fallback.
Direct `matrix/eigenvalues` and `matrix/eigenvectors` support Vulkan-placed
dense row-major square symmetric `Float64` inputs, including `n > 64` within
helper resource limits, 32-bit shader index guards, and the Jacobi iteration
guard, returning Vulkan-placed values and aligned vector columns. Nonsymmetric
Vulkan inputs raise `tensor/not-symmetric`; shader non-convergence raises
`tensor/no-convergence`, without hidden CPU/LAPACK fallback. Missing
Vulkan/Float64 capability, unsupported shapes/layouts/dtypes, resource bounds,
and stride/view-backed inputs fail closed with Tensor backend diagnostics.
Direct `matrix/eigenpairs` remains CPU-only/fail-closed on Vulkan while its
public output contract is pointer-backed `BigComplex`.
Zero-size contracted axes in supported Vulkan layouts produce the
additive identity, matching CPU Tensor semantics. The public surface is not
`GpuTensor`/`CudaTensor`/`VulkanTensor` types or backend-named math operations.

```lisp
(ref
  (to-device
    (contract
      (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)
      (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)
      [0 0])
    'cpu)
  []) ;; => 140.0
```

```lisp
(ref
  (to-device
    (contract
      (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)
      (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'vulkan)
      [1 0])
    'cpu)
  [1 1]) ;; => 154.0
```

### Generic Operations

These work across collection types:

| Operation | Lists | Arrays | Dicts | Strings |
|-----------|-------|--------|-------|---------|
| `ref` | by index across cons chains | by index | by key | codepoint char at index |
| `length` | count; dotted terminal tails count as one element | count | count | codepoint count |
| `push!` | — | append | — | — |

### Access Syntax

Omni keeps related but distinct access operations:

- `expr.name` is a distinct path-step operation (module/instance/dict-symbol/cons step semantics).
- `expr.[key]` is postfix dynamic/index access syntax aligned with `ref` collection lookup semantics.
- `(ref coll key)` is the canonical dynamic collection lookup operation.
- For cons/list chains, `ref` supports positive and negative indexes across the
  full chain. A non-`nil` dotted terminal tail is the final indexable element,
  and `length` counts it as one element.

Examples:

```lisp
(define user {'name "Alice" 'age 30})
user.name                         ;; => "Alice"

(define arr [0 1 2])
arr.[2]                           ;; => 2

(ref {2 "int-key"} 2)            ;; => "int-key"
(ref {"2" "string-key"} "2")     ;; => "string-key"
(ref {[2] "array-key"} [2])      ;; => "array-key"
```

Removed forms:

```lisp
.name
.1
.'key
.[expr]
```

These removed forms now hard-error. Use `expr.name`, `expr.[key]`, or `ref`.
For higher-order code, write the lambda explicitly:
`(lambda (x) (ref x 'name))`.
Compatibility/removal details are centralized in
`docs/SURFACE_COMPATIBILITY.md`.

### Postfix Index Access

```lisp
lst.[0]                 ;; first element of list
arr.[2]                 ;; third element of array
dict.['key]             ;; dict key lookup
str.[0]                 ;; character at index
tensor.[i].[j]          ;; chained indexing
arr.[-1]                ;; last element (negative indexing)
```


---

## 10. String Operations

```lisp
(string-append "hello" " " "world")     ;; => "hello world"
(string-join ", " '("a" "b" "c"))       ;; => "a, b, c"
(substring "hello" 1 3)                 ;; => "el"
(substring "hello" -3 -1)              ;; => "ll" (negative indices)
(string-split "a,b,c" ",")             ;; => ("a" "b" "c")
(string-length "hello")                 ;; => 5
(string-byte-length "héllo")             ;; => 6
(List "abc")                            ;; => ("a" "b" "c")
(String '("a" "b" "c"))                ;; => "abc"
(string-upcase "hello")                 ;; => "HELLO"
(string-downcase "HELLO")               ;; => "hello"
(string-trim "  hello  ")               ;; => "hello"
(string-contains? "hello world" "world") ;; => true
(string-index-of "hello" "ll")          ;; => 2
(string-replace "hello" "l" "r")        ;; => "herro"
(char-at "hello" 0)                     ;; => "h"
(string-repeat "ha" 3)                  ;; => "hahaha"
```

### Conversion

```lisp
(parse-number "42")       ;; => 42
(parse-number "3.14")     ;; => 3.14
(String 42)         ;; => "42"
(Symbol "foo")      ;; => foo (symbol)
(String 'foo)       ;; => "foo"
```

### Formatting

```lisp
;; printf-style
(format "Hello %s, you are %d" "Alice" 30)
;; => "Hello Alice, you are 30"

;; CL-style
(cl-format "~a is ~a" "pi" 3.14)
;; => "pi is 3.14"
```

---

## 11. Math & Numeric

### Arithmetic

| Op | Description | Example |
|----|-------------|---------|
| `+` | Addition | `(+ 1 2)` => `3` |
| `-` | Subtraction / negate | `(- 5 3)` => `2`, `(- 5)` => `-5` |
| `*` | Multiplication | `(* 3 4)` => `12` |
| `/` | Division | `(/ 10 3)` => `3` (integer), `(/ 10.0 3)` => `3.33..` |
| `%` | Modulo | `(% 10 3)` => `1` |

### Math Library

```lisp
(sin 1.0)   (cos 1.0)   (tan 1.0)       ;; trig
(asin 0.5)  (acos 0.5)  (atan 1.0)      ;; inverse trig
(atan2 1.0 1.0)                           ;; two-arg arctangent
(exp 1.0)   (log 2.718) (log10 100.0)    ;; exponential/log
(pow 2.0 10.0)  (sqrt 4.0)               ;; power/root
(floor 3.7) (ceiling 3.2) (round 3.5) (truncate 3.9) ;; rounding
(abs -42)   (min 3 7)   (max 3 7)        ;; misc
(gcd 12 8)  (lcm 4 6)                    ;; number theory
```

### Bitwise

```lisp
(bitwise-and 0xFF 0x0F)    ;; => 15
(bitwise-or 0xF0 0x0F)     ;; => 255
(bitwise-xor 0xFF 0x0F)    ;; => 240
(bitwise-not 0)             ;; => -1
(lshift 1 10)               ;; => 1024
(rshift 1024 5)             ;; => 32
```

### Numeric Predicates

```lisp
(zero? 0)        ;; => true
(positive? 5)    ;; => true
(negative? -1)   ;; => true
(even? 4)        ;; => true
(odd? 3)         ;; => true
```

### Constants

```lisp
pi   ;; => 3.141592653589793
e    ;; => 2.718281828459045
```
