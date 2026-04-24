## 7. Primitives

### 7.1 Arithmetic (5)

| Prim | Arity | Description |
|------|-------|-------------|
| `+` | 1-2 | Addition (int or Float64); `(+ n)` is identity |
| `-` | 1-2 | Subtraction; `(- n)` negates |
| `*` | 2 | Multiplication |
| `/` | 2 | Integer/float division |
| `%` | 2 | Modulo |

Binary primitives no longer auto-partial. A bare one-argument call to a binary-only
primitive like `(* 3)` is an arity error outside rewrite contexts such as `|>`.
Use `_` / `_n` placeholders, `|>` pipe, or `partial` for partial application.

### 7.2 Comparison (5)

| Prim | Description |
|------|-------------|
| `=` | Structural equality |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |

### 7.3 List Operations (7)

| Prim | Arity | Description |
|------|-------|-------------|
| `cons` | 2 | Construct pair |
| `car` | 1 | First element |
| `cdr` | 1 | Rest element |
| `list` | variadic | Create list; single collection arg dispatches conversion (`(list [1 2 3])`, `(list (Iterator ...))`) |
| `List` | variadic | Canonical list constructor/conversion surface; single Iterator input is consumed and single Tensor input materializes to a flat row-major list (`list` remains a public helper) |
| `length` | 1 | Generic: list, array, dict, string, or tensor element count |
| `null?` | 1 | Check if nil |
| `pair?` | 1 | Check if cons |

### 7.4 Boolean (1)

| Prim | Description |
|------|-------------|
| `not` | Logical negation |

### 7.5 I/O (4, via effects)

| Prim | Description |
|------|-------------|
| `print` | Output value (no newline) |
| `println` | Output value with newline |
| `display` | Display value |
| `newline` | Output newline |

I/O primitives go through algebraic effects (`io/print`, `io/println`, etc.). When no handler is installed, a fast path calls raw primitives directly (zero overhead). Custom handlers can intercept, suppress, or redirect I/O.

### 7.6 String Operations (15)

| Prim | Arity | Description |
|------|-------|-------------|
| `string-append` | variadic | Concatenate strings |
| `str` | 1 | Interpolate `{expr}` holes in a string using lexical bindings and `String` coercion; escape literal braces as `{{` and `}}` |
| `string-join` | 2 | Join list with separator |
| `substring` | 3 | Extract substring (negative indices supported) |
| `string-split` | 2 | Split by delimiter |
| `string-length` | 1 | String length in UTF-8 codepoints |
| `string-byte-length` | 1 | String length in bytes |
| `string-upcase` | 1 | Uppercase |
| `string-downcase` | 1 | Lowercase |
| `string-trim` | 1 | Trim whitespace |
| `string-contains?` | 2 | Substring search |
| `string-index-of` | 2 | Find index of substring |
| `string-replace` | 3 | Replace occurrences |
| `char-at` | 2 | Character at index |
| `string-repeat` | 2 | Repeat string N times |

### 7.7 Type Predicates (12)

| Prim | Description |
|------|-------------|
| `string?` | Is string? |
| `int?` | Is integer? |
| `float32?` | Is Float32? |
| `float64?` | Is Float64? |
| `complex64?` | Is Complex64? |
| `complex128?` | Is Complex128? |
| `number?` | Is int, Float32, Float64, BigInteger, BigFloat, BigComplex, Complex128, or Complex64? |
| `symbol?` | Is symbol? |
| `closure?` | Is closure? |
| `continuation?` | Is continuation? |
| `boolean?` | Is true or false? |
| `list?` | Is proper list? |
| `procedure?` | Is callable? |
| `dict?` | Is dict? |
| `array?` | Is array? |

### 7.8 Numeric Predicates (4)

| Prim | Description |
|------|-------------|
| `zero?` | Is zero? |
| `positive?` | Is positive? |
| `negative?` | Is negative? |
| `even?` / `odd?` | Parity check |

### 7.9 File I/O (5, via effects)

| Prim | Arity | Description |
|------|-------|-------------|
| `read-line` | 0 | Read one line from standard input; returns `nil` on EOF |
| `read-file` | 1 | Read file as string |
| `write-file` | 2 | Write string to file (returns `Void` on success) |
| `file-exists?` | 1 | Check file existence |
| `read-lines` | 1 | Read file as list of lines |
| `load` | 1 | Load and evaluate a .omni file |

### 7.10 Dictionary Operations (1)

| Prim | Arity | Description |
|------|-------|-------------|
| `Dictionary` / `Dict` | variadic | Create a dictionary from key-value pairs; `{a 1 b 2}` is equivalent to `(Dictionary 'a 1 'b 2)` |

Canonical constructor surface is `Dictionary` (with `Dict` shorthand).
Dictionary keys are value-typed: symbols, strings, integers, and other stable
value keys are supported.
In `{}` literals only, a bare symbol in key position is implicitly quoted:
`{name "Alice" age 30}` is equivalent to `{'name "Alice" 'age 30}`.
Non-symbol keys remain explicit, for example `{(+ 1 2) "three"}`.
Style guidance:
- prefer symbol keys for internal language/runtime maps
- prefer string keys for external payload maps (JSON/HTTP/config)

### 7.11 Array Operations (1)

| Prim | Arity | Description |
|------|-------|-------------|
| `Array` | variadic | Create array; `[1 2 3]` is equivalent to this; single collection arg dispatches conversion (`(Array '(1 2 3))`, `(Array (Iterator ...))`, `(Array tensor)`) |

Canonical constructor surface is `Array`.

### 7.12 Generic Collection Operations (6)

| Prim | Arity | Description | Supported types |
|------|-------|-------------|-----------------|
| `ref` | 2 | Lookup by key/index | Array (int), Dictionary (any key type), cons/list chain (int), string (char) |
| `push!` | 2 | Append element | Array |
| `keys` | 1 | List of keys | Dictionary |
| `values` | 1 | List of values | Dictionary |
| `has?` | 2 | Check key existence | Dictionary |
| `remove!` | 2 | Remove by key | Dictionary |

Note: `length` (Section 7.3) is also generic — works on lists, arrays, dicts, and strings.

For cons/list chains, `ref` indexes across the full cons chain and supports
negative indexes. A non-`nil` dotted terminal tail is addressable as the final
element, and `length` counts it as one terminal element.
Iteration order contract:
- `keys` and `values` use the same canonical key order.
- Canonical key order is deterministic by key type/value (for common key kinds:
  numeric ascending, string/symbol lexicographic, time-point chronological).
- `values` ordering is key-aligned (`(values d)[i]` corresponds to `(keys d)[i]`).

### 7.13 Set Operations (6)

| Prim | Arity | Description |
|------|-------|-------------|
| `Set` | variadic | Create set |
| `set-add` | 2 | Add element |
| `set-remove` | 2 | Remove element |
| `set-contains?` | 2 | Check membership |
| `length` | 1 | Set cardinality |
| `List` | 1 | Return set elements as list (canonical order) |

Set order contract:
- `List` returns elements in deterministic canonical element order
  (same comparator family as dictionary keys).

### 7.14 Math Library (27)

| Prim | Description |
|------|-------------|
| `sin`, `cos`, `tan` | Trigonometric |
| `asin`, `acos`, `atan` | Inverse trig |
| `atan2` | Two-argument arctangent |
| `exp`, `log`, `log10` | Exponential/logarithmic |
| `pow`, `sqrt` | Power/root |
| `real-part`, `imag-part`, `conjugate` | Complex component access and conjugation; accept any numeric value |
| `math/lgamma` | Natural log of absolute gamma value; domain/range failures raise errors |
| `math/erf`, `math/erfc` | Error function and complementary error function |
| `stats/normal-cdf`, `stats/normal-quantile` | Standard normal CDF and inverse CDF |
| `floor`, `ceiling`, `round`, `truncate` | Rounding; `BigFloat` inputs return exact `Integer`/`BigInteger` results up to the supported allocation cap |
| `abs` | Absolute value; exact `Integer` overflow promotes to `BigInteger`; supports `BigFloat` |
| `min`, `max` | Binary min/max; supports exact `BigInteger` and `BigFloat` comparison |
| `gcd`, `lcm` | Number theory; supports exact `Integer`/`BigInteger` operands |

When a `BigFloat` operand participates, `sin`, `cos`, `tan`, `asin`, `acos`,
`atan`, `atan2`, `exp`, `log`, `log10`, `pow`, `sqrt`, `math/lgamma`,
`math/erf`, `math/erfc`, `stats/normal-cdf`, and `stats/normal-quantile`
return `BigFloat` results. `stats/normal-quantile` keeps its probability-domain
contract and raises when the input is not strictly between `0` and `1`.
`floor`, `ceiling`, `round`, and `truncate` are the exception to the BigFloat
result rule: they return exact integer values, narrowing to `Integer` when
representable and promoting to `BigInteger` otherwise.

CPU Tensor `math/erf`, `math/erfc`, `stats/normal-cdf`, and
`stats/normal-quantile` apply elementwise. `Float64` and `Float32` Tensor
inputs preserve their float dtype for these operations, `BigInteger` Tensor
inputs return `Float64` tensors where the operation's domain permits it,
`BigFloat` Tensor inputs preserve `BigFloat`, and `BigComplex` Tensor inputs
fail closed until complex error-function or distribution contracts are
defined. CPU Tensor `stats/normal-quantile` fails the whole operation on the
first probability outside `0 < p < 1`. CUDA-placed dense row-major `Float64`
and `Float32` Tensor inputs route `math/erf`, `math/erfc`,
`stats/normal-cdf`, and `stats/normal-quantile` through the CUDA scientific
unary helper when that capability is available; CUDA quantile preserves
placement/dtype for valid probabilities and reports the same probability
diagnostic before exposing output for invalid probabilities. Vulkan-placed
dense row-major `Float32` Tensor inputs route `stats/normal-cdf` and
`stats/normal-quantile` through Vulkan helpers; Vulkan quantile uses a
status-bearing inverse-CDF shader with the same diagnostics. Vulkan
`Float64` Tensor inputs route `stats/normal-cdf` through the Vulkan Float64
shader approximation and `stats/normal-quantile` through the Vulkan Float64
inverse-CDF helper. Unsupported devices/layouts still fail closed.

### 7.15 Bitwise Operations (6)

| Prim | Description |
|------|-------------|
| `bitwise-and`, `bitwise-or`, `bitwise-xor` | Bitwise logic; supports exact `Integer`/`BigInteger` operands |
| `bitwise-not` | Bitwise complement; supports exact `Integer`/`BigInteger` operands |
| `lshift`, `rshift` | Bit shifting; non-negative shifts use exact integer semantics and may promote to `BigInteger`; negative shifts return `0`; shift counts above the bounded exact-shift cap fail closed |

### 7.16 Conversion (6)

| Prim | Description |
|------|-------------|
| `parse-number` | Parse string to number |
| `String` | Canonical string constructor/coercion surface; dispatches string, number, symbol, and proper list-of-string-fragment conversion |
| `Float` | Canonical floating constructor; `(Float x)` defaults to `Float64`, `(Float x 64)` and `(Float x "64")` are explicit binary64, and `(Float x 32)` / `(Float x "32")` produce scalar `Float32` |
| `Float32` | Native 32-bit float scalar constructor; accepts representable numeric and numeric-string inputs and fails closed on non-finite or out-of-range narrowing. |
| `Float64` | Canonical Float64 constructor/coercion surface; accepts finite BigInteger and BigFloat inputs when representable as a finite Float64 |
| `Integer` | Canonical integer constructor/coercion surface; truncates finite numeric inputs toward zero and accepts in-range BigInteger/BigFloat inputs |
| `BigInteger` | Canonical arbitrary-precision exact integer constructor/coercion surface; accepts integers and decimal strings |
| `BigFloat` | Canonical high-precision decimal float constructor/coercion surface; accepts numeric values and finite decimal strings |
| `BigComplex` | Canonical high-precision complex constructor; accepts one numeric value or explicit real/imaginary numeric parts |
| `Complex128` | Fixed-width complex constructor; accepts one or two finite numeric parts representable as Float64 |
| `Complex64` | Fixed-width complex constructor; accepts one or two finite numeric parts representable as Float32 |
| `Symbol` | Canonical symbol constructor/coercion surface |

Numeric conversion policy:
- Narrowing to `Integer` (`Integer`, `truncate`) truncates toward zero.
- Narrowing requires finite numeric input and an in-range `Integer` result.
- `parse-number` returns `Integer`, `BigInteger`, `Float64`, or `BigFloat`:
  valid decimal integers that exceed the fixed-width `Integer` range promote to
  `BigInteger`, and valid floating inputs that overflow `Float64` promote to
  `BigFloat`.
- `parse-number` returns `nil` on parse failure or floating input outside the
  supported `BigFloat` range.
- Constructor/coercion narrowing failures use deterministic recoverable code `type/arg-mismatch`.
- Dispatch does not do implicit numeric widening; cross-numeric calls require explicit conversion.

### 7.17 Introspection & Meta (7)

| Prim | Description |
|------|-------------|
| `type-of` | Type name as symbol |
| `is?` | Type/subtype check |
| `instance?` | Check if type instance |
| `eval` | Evaluate expression |
| `apply` | Apply function to arg list |
| `macroexpand` | Expand macro |
| `bound?` | Check if name is defined |

### 7.17.1 Tensor Construction And Introspection

These primitives are implemented for native `Tensor` values. The current
concrete storage dtypes are `Float64`, `Float32`, `BigInteger`, `BigFloat`,
`BigComplex`, `Complex128`, and `Complex64`; tensor `map` and `contract`
support all seven dtypes on CPU. Scalar `Float32`, `Complex128`, and
`Complex64` are runtime values, so `ref`, `Array`, `List`, and `Iterator`
expose matching Tensor elements as scalar values.
`to-device 'cpu` preserves `Float32`; Vulkan also supports `Float32`
placement/copyback, destination `realize`, dense row-major elementwise `map`,
unary helpers, direct `min`/`max`, rank-N `contract`, fixed-width complex
structural matrix operations when `tensor-backends` reports
`matrix-structural-complex128` or `matrix-structural-complex64`, direct
`matrix/rank`, direct `matrix/norm` selectors, direct
`matrix/singular-values`, direct `matrix/svd`, and serial matrix factor/solve
surfaces (`matrix/determinant`, `matrix/lu`, `matrix/solve`,
`matrix/inverse`, `matrix/cholesky`, and `matrix/qr`) plus staged parallel
Vulkan `matrix/solve` when `tensor-backends` reports `float32 true`. CPU
`Float32` factor/SVD routines are landed. CUDA `Float32` placement/copyback,
destination `realize`, matching cuBLAS contract layouts, and fixed-width
complex structural matrix operations are landed.

| Prim | Description |
|------|-------------|
| `Tensor` | Construct a native tensor as `(Tensor data)`, `(Tensor iterator)`, `(Tensor data dtype)`, `(Tensor dtype data)`, or `(Tensor dtype shape data-or-scalar)` |
| `tensor?` | Predicate for native tensor values |
| `dtype` | Return the tensor dtype symbol, currently `'Float64`, `'Float32`, `'BigInteger`, `'BigFloat`, `'BigComplex`, `'Complex128`, or `'Complex64` |
| `shape` | Return the tensor shape as an array of dimensions |
| `rank` | Return the tensor rank |
| `tensor-layout` | Return a metadata dictionary describing dtype, device, payload kind, layout, shape, strides, storage offset/extent, ownership, view status, and write policy |
| `contract` | Contract two tensors as `(contract a b axis-pairs)` or `(contract a b left-axes right-axes)` |
| `matrix/transpose` | Transpose a rank-2 Tensor while preserving native dtype |
| `matrix/transpose-view` | Construct a read-only rank-2 transpose view over a Tensor source |
| `matrix/diagonal` | Extract the main diagonal of a rank-2 Tensor as a rank-1 Tensor while preserving native dtype |
| `matrix/diagonal-matrix` | Build a square rank-2 Tensor from a rank-1 diagonal Tensor while preserving native dtype |
| `matrix/identity` | Build a square identity Tensor, defaulting to `Float64` with optional dtype selection |
| `matrix/trace` | Return the scalar diagonal sum of a square rank-2 Tensor while preserving native numeric family |
| `matrix/rank` | Return the numerical rank of a rectangular rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor as an `Integer` |
| `matrix/norm` | Return a `Float64` matrix norm for a rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor; defaults to Frobenius and accepts `'frobenius`, `'one`, `'infinity`, `'max`, `'spectral`, or `'nuclear` |
| `matrix/solve` | Solve a rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` linear system with rank-1 or rank-2 matching right-hand side, preserving RHS rank and dtype |
| `matrix/lu` | Factor a square rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor with partial pivoting; returns dtype-preserving `lu` factors plus host metadata |
| `matrix/determinant` | Return the determinant of a square rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor using partial-pivot LU semantics |
| `matrix/inverse` | Return the inverse of a nonsingular square rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor while preserving Tensor dtype |
| `matrix/qr` | Compute reduced QR for a rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor with rows >= columns, preserving Tensor dtype |
| `matrix/cholesky` | Compute the lower-triangular Cholesky factor of a square symmetric positive-definite real Tensor or Hermitian positive-definite complex Tensor, preserving Tensor dtype |
| `matrix/singular-values` | Return descending singular values for a rank-2 `Float64`, `Float32`, `Complex128`, or `Complex64` Tensor; complex inputs return component-width real tensors |
| `matrix/svd` | Compute reduced SVD for a rank-2 `Float64` or `Float32` Tensor; returns `u`, `s`, and `v` |
| `matrix/eigenvalues` | Compute descending real eigenvalues for a square symmetric rank-2 `Float64` Tensor |
| `matrix/eigenvectors` | Compute real eigenvalues and aligned eigenvector columns for a square symmetric rank-2 `Float64` Tensor |
| `matrix/eigenpairs` | Compute general eigenvalues and aligned eigenvector columns for a square rank-2 `Float64` Tensor; returns `BigComplex` tensors |
| `realize` | Low-level tensor storage primitive: return a concrete tensor, or write a tensor/scalar source into a destination tensor as `(realize expr [out])` |

Tensor indexing is part of generic `ref`. `BigInteger`, `BigFloat`,
`BigComplex`, `Complex128`, and `Complex64` tensors
support constructor/ref/flat collection conversion/concrete storage paths
and tensor-dispatched `map` and `contract`. Tensor elementwise operations are
part of generic `map`; unary tensor inputs, tensor-scalar inputs,
scalar-tensor inputs, exact-shape tensor-tensor inputs, and right-aligned
singleton-axis tensor-tensor broadcasting are supported for `Float64`,
`Float32`, `BigInteger`, `BigFloat`, `BigComplex`, `Complex128`, and
`Complex64` tensors.
Scalar arguments are coerced into the first tensor input's dtype and
broadcast over the tensor shape. Rank-0 tensors broadcast as tensor scalars, and
incompatible tensor shapes raise `tensor/shape-mismatch`. Tensor `map` and
`contract` may return lazy Tensor expression payloads under the existing
`Tensor` value; there is no public `TensorExpr` type and no separate
`delay`/`force` protocol for tensor laziness. `tensor-layout` reports Tensor
metadata as a `Dictionary`; concrete CPU/CUDA/Vulkan dense tensors report
zero storage offset, physical row-major strides such as `[3 1]` for shape
`[2 3]`, and `owner` `self`, lazy map/contract tensors report expression
payload metadata without exposing a public expression type, and
`matrix/transpose-view` tensors report payload `view`, owner `view-source`,
and write-policy `read-only-view`. Transpose views are CPU-readable through
`ref`, `Array`, `List`, and `realize`, but they do not relax the current GPU
requirement for zero-offset dense row-major storage. Constructor dispatch
is the canonical materialization boundary: `(Array iterator)` and `(List iterator)`
consume iterators, `(Tensor iterator)` consumes a finite numeric iterator into
Tensor storage, and `(Tensor lazy-tensor)` normalizes a Tensor expression
through the ordinary Tensor constructor path. `(Iterator tensor)`,
`(Array tensor)`, and `(List tensor)` are explicit collection conversions:
they materialize lazy CPU tensor expressions if needed and expose flat
row-major element values. They do not encode shape nesting; use `shape`
alongside the converted data when preserving rank is required. Tensor iteration
over non-CPU device storage fails closed until the caller explicitly copies with
`to-device 'cpu`; it is not a hidden CPU/GPU transfer boundary. The `realize`
primitive remains a low-level Tensor storage operation, mainly for
exact-shape/dtype destination writes; it is not the general lazy sequence
abstraction. Elementwise destination realization may
update an input tensor in place; contraction destination realization rejects
destinations that alias either source tensor. Tensor contraction uses paired
axes as
`(contract a b [left-axis right-axis])` for one contracted pair or
`(contract a b [[left-axis right-axis] ...])` for multiple pairs. The explicit
left/right axis-list form `(contract a b left-axes right-axes)` is also
accepted. Axis lists may be arrays or proper lists of integers, negative axes
normalize from the end of each tensor's rank, each paired contracted dimension
must match, and the result shape is all non-contracted left axes followed by
all non-contracted right axes.

```lisp
(define x (Tensor [[1.0 2.0 3.0] [4.0 5.0 6.0]]))
(ref x [1 2])   ; => 6.0
(ref x [1 -1])  ; => 6.0

(define z (map + x 1.0))
(ref z [1 2])   ; => 7.0
(ref (Tensor (map (λ (v) (+ v 1)) (Iterator [1 2 3]))) [1]) ; => 3.0

(define a (Tensor Float64 [2 3] [1 2 3 4 5 6]))
(define b (Tensor Float64 [3 2] [7 8 9 10 11 12]))
(ref (contract a b [1 0]) [1 1]) ; => 154.0

(ref
  (to-device
    (contract
      (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)
      (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)
      [0 0])
    'cpu)
  []) ; => 140.0

(define coeffs (Tensor Float64 [2 2] [2 1 1 3]))
(define rhs (Tensor Float64 [2] [1 2]))
(ref (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6])) [2 1]) ; => 6.0
(ref (matrix/diagonal (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1]) ; => 5.0
(ref (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3])) [2 2]) ; => 3.0
(ref (matrix/identity 3) [2 2]) ; => 1.0
(matrix/trace (Tensor Float64 [2 2] [1 2 3 4])) ; => 5.0
(matrix/rank (Tensor Float64 [2 3] [1 2 3 2 4 6])) ; => 1
(matrix/rank (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)])) ; => 2
(matrix/norm (Tensor Float64 [2 2] [3 4 0 0])) ; => 5.0
(matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)])) ; => 5.0
(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'one) ; => 9.0
(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'spectral) ; => 3.0
(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'nuclear) ; => 5.0
(ref (matrix/solve coeffs rhs) [1]) ; => 0.6
(define factors (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])))
(ref (ref factors 'lu) [1 0]) ; => 0.5
(ref factors 'pivots) ; => [0 1]
(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1])) ; => -2.0
(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 0]) ; => 0.6
(define qr (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])))
(ref (ref qr 'r) [0 1]) ; => 1.0
(define chol (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])))
(ref chol [1 0]) ; => 1.0
(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [0]) ; => 3.0
(define svd (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])))
(ref (ref svd 's) [0]) ; => 3.0
(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0]) ; => 3.0
(define eigen (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])))
(ref (ref eigen 'vectors) [0 0]) ; => 1.0
(define general-eigen (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])))
(String (ref (ref general-eigen 'values) [0])) ; => "0+1i"

(Tensor z)                  ; constructor-normalized Tensor storage
(define y (Tensor Float64 [2 3] 0.0))
(realize x y)               ; => y, after copying x into y
(realize 1.0 y)             ; => y, after filling y with 1.0
(realize z y)               ; low-level destination write into y
```

### 7.18 Error Handling (2)

| Prim | Description |
|------|-------------|
| `error` | Create error value |
| `error-message` | Extract message from error |

### 7.19 Miscellaneous (5)

| Prim | Description |
|------|-------------|
| `gensym` | Generate unique symbol |
| `format` | Format string with values |
| `sort` | Sort list |
| `sort-by` | Sort list by comparator |
| `read-string` | Parse string to Lisp value |

### 7.20 FFI (Declarative)

```lisp
;; Declare a library handle
(define [ffi lib] libc "libc.so.6")

;; Bind a C function as a native Omni function
(define [ffi λ libc] (strlen (^String s)) ^Integer)
(define [ffi λ libc] (abs (^Integer n)) ^Integer)

;; Grouped C ABI module sugar over [ffi lib] plus [ffi λ]
(define [ffi module] libc2 "libc.so.6"
  (strlen (^String s)) ^Integer
  (abs (^Integer n)) ^Integer
  (fopen (^String path) (^String mode))
    ^{'name File 'ownership owned 'finalizer fclose})

(strlen "hello")  ; => 5
(abs -42)          ; => 42
```

- Uses libffi via C wrapper for portable ABI support
- Type annotations: `^Integer` -> sint64, `^Float64` -> Float64, `^String`
  -> copied plain `char*` boundary, `^ForeignHandle` -> boxed opaque foreign
  handle, `^Void` -> void, `^Boolean` -> sint64
- `^ForeignHandle` is the simple default foreign-handle annotation. It
  represents opaque foreign resources as `FFI_HANDLE` boxes; user
  code does not pass raw integer addresses.
- In `ffi λ`, FFI-local metadata dictionaries may refine a foreign handle
  policy. `^{'name File 'ownership owned 'finalizer fclose}` implies
  `ForeignHandle`; the explicit `^{'type ForeignHandle ...}` form is also
  accepted. Dictionary entries are key/value pairs; bare symbol keys in `{}` are
  implicitly quoted, and explicit quoted symbol keys remain accepted.
  Omni does not use colon keywords.
- Supported `ForeignHandle` metadata keys are `'type`, `'name`, `'ownership`,
  `'finalizer`, and `'nullability`. Supported ownership values are `borrowed`,
  `owned`, and `manual`; supported nullability values are `nullable` and
  `non-null`. An owned return handle requires a finalizer. Owned parameter
  policies are rejected because a call argument cannot transfer Omni-side
  finalizer authority safely in the current runtime.
- Foreign handles carry a small common runtime descriptor internally:
  runtime kind, handle kind, and capability bits. The user-facing
  introspection surface is `(foreign-describe value)`. For a `ForeignHandle`,
  it returns a dictionary using quoted-symbol keys: `'type`, `'runtime`,
  `'kind`, `'ownership`, `'name`, `'live`, and `'capabilities`. Capability
  sequences are arrays of symbols so the shape matches metadata sequence fields
  such as `'parameters [..]`. Opaque foreign-resource handle names such as
  `File` are returned as symbols; C ABI library handle names remain strings
  because they are the `dlopen` target. C ABI library handles reflect
  load/resolve/reflect capabilities; direct call capability is reflected on
  `ForeignCallable` descriptors for bound functions.
- `(foreign-describe ffi-bound-function)` returns reflection metadata for C ABI
  functions declared through `[ffi λ]` (`[ffi lambda]` remains accepted): `'type 'ForeignCallable`, `'runtime
  'c-abi`, `'kind 'function`, `'name`, `'c-name`, `'library`, `'parameters`,
  `'returns`, `'resolved`, `'available`, and `'capabilities`. `'parameters` is
  an array of descriptor dictionaries; `'returns` is one descriptor dictionary.
  Each descriptor includes user-facing `'type` and ABI-level `'abi-type`.
  `ForeignHandle` descriptors also preserve `'name`, `'ownership`,
  `'nullability`, and `'finalizer` metadata when present.
- `(foreign-release handle)` is the explicit release boundary for releasable
  `ForeignHandle` payloads. It returns `Void`, is idempotent for already-closed
  handles, clears the payload pointer observed by `foreign-describe`, and
  makes released handles stop reflecting as `'ownership owned` because the
  live pointer and release capability are gone. It
  rejects ordinary non-releasable library or borrowed resource handles. This is
  only for foreign resources and does not add refcount or garbage-collected
  ownership for Omni values.
- Declarative `ffi λ` accepts only `^Integer`, `^Float64`, `^String`,
  `^ForeignHandle`, `^Boolean`, and `^Void` at the base annotation level;
  unsupported annotations fail at definition time instead of defaulting to
  foreign-handle metadata.
- Argument conversion is fail-closed:
  - `^Integer`: Omni `Integer` only
  - `^Float64`: Omni `Float64` or `Integer`
  - `^Boolean`: Omni `true` / `false` only
  - `^String`: Omni `String`, or `nil` for a null plain `char*`
  - `^ForeignHandle`: live `FFI_HANDLE`, or `nil` for null
  - FFI-local metadata dictionaries in `ffi λ` use ordinary Omni dictionary syntax; bare symbol keys in `{}` auto-quote, explicit quoted keys remain accepted, and no colon keywords are used.
- Return conversion follows the same no-raw-pointer rule: non-null `^String`
  C plain `char*` returns are copied into Omni `String` values, null string
  returns become `nil`, non-null `^ForeignHandle` C pointer returns become
  `ForeignHandle` values, and null pointer returns become `nil`.
  Generated bindings keep byte pointers such as `signed char*` and
  `unsigned char*` as `ForeignHandle` values instead of string-shaped
  parameters or returns.
  For plain character pointers, `const char*` and `char const*` are
  string-input shaped; `char* const` is still mutable pointee storage and stays
  on the mutable string-buffer wrapper path.
  Only single-level plain `char*` pointers are string-shaped; pointer-to-pointer
  spellings such as `char**` and `const char**` remain opaque `ForeignHandle`
  values.
  Name-based mutable string-buffer direction inference treats `inout` as more
  specific than `out`, so names such as `inout_buffer` generate
  `buffer-direction=inout`.
- Declarative `variadic` bindings are currently rejected at definition time until the runtime carries explicit fixed/variadic metadata
- Execution mode contract: interpreter/JIT `ffi λ` enforces `ForeignHandle`
  metadata dictionaries. AOT lowering carries the same policy into generated
  declarations with per-parameter handle family/nullability descriptors and a
  return handle name/ownership/finalizer descriptor.
- `Nil` is the language-level empty/false value type; `Void` is a real singleton runtime value/type, and FFI `^Void` returns produce that value
- Lazy dlsym: symbol resolution deferred to first call and cached
- Grouped FFI module syntax is shipped as parser sugar:
  `(define [ffi module] lib "path" (fn (^Type arg)) ^Return ...)` expands to
  one `[ffi lib]` declaration plus one `[ffi λ]` declaration per function. The
  body is a flat sequence of `(signature, return-annotation)` pairs: no `->`
  syntax and no bracketed body entries. Malformed body sequences fail closed,
  and `ForeignHandle` metadata dictionaries use the same policy validation and
  AOT descriptors as the single-function form.

### 7.21 Constants

| Name | Value |
|------|-------|
| `true` | Symbol `true` |
| `false` | Boolean false |
| `pi` | 3.141592653589793 |
| `e` | 2.718281828459045 |

### 7.22 Memory

| Primitive | Args | Description |
|-----------|------|-------------|
| `unsafe-free!` | 1 | Free heap backing of array/dict/instance/string and return `Void`. Value becomes an error — accessing it after free raises "use after unsafe-free!". No-op on int/nil/other non-heap types also return `Void`. |

### 7.23 Regex and Pika Parsing

Pika is Omni's built-in parser and grammar substrate, not an optional helper
module. The regex primitives and `pika/...` grammar primitives expose the same
language-core matching foundation that Omni uses for source parsing and grammar
work. Slash in `pika/...` is ordinary symbol syntax; it names a compact
always-present primitive family rather than a module dereference.

#### Regex primitives

| Primitive | Args | Description |
|-----------|------|-------------|
| `re-match` | 2 | First match anywhere in input (or `nil`) |
| `re-fullmatch` | 2 | Match entire input (or `nil`) |
| `re-find-all` | 2 | Non-overlapping match list |
| `re-split` | 2 | Split input on regex matches |
| `re-replace` | 3-4 | Replace first match, or all with `'global` |
| `re-match-pos` | 2 | Match positions as `(start end)` or `nil` |
| `re-find-all-pos` | 2 | List of `(start end)` pairs |

Supported regex constructs:
- literals, char classes (`[a-z]`, `[^x]`), shorthand classes (`\d`, `\w`, `\s`)
- quantifiers (`*`, `+`, `?`, `{n}`, `{n,m}`, `{n,}`)
- alternation (`|`), grouping (`(...)`, `(?:...)`)
- lookahead (`(?=...)`, `(?!...)`)
- anchors (`^`, `$`)

Regex semantics notes:
- Deterministic Pika-style matching (no catastrophic backtracking engine behavior).
- Possessive quantifiers (`*+`, `++`, `?+`) are accepted with deterministic semantics.
- Invalid patterns are rejected strictly (unclosed groups, malformed bounds, bad ranges, trailing junk).
- Bounded quantifier upper/lower bounds are validated (`{n,m}` with large bounds is rejected).

#### Pika grammar primitives

| Primitive | Args | Description |
|-----------|------|-------------|
| `pika/grammar` | variadic | Define a named grammar from quoted `(rule ...)` forms |
| `pika/parse` | 2 | Parse input with named grammar |
| `pika/fold` | 3 | Fold parse tree with user function |
| `pika/grammar-rules` | 1 | List rule names for a named grammar |
| `pika/parse-lisp` | 1 | Parse Omni/Lisp source with the built-in Pika grammar |

Pika grammar notes:
- Use quoted rule forms with `pika/grammar`, for example:
  `(pika/grammar 'g '(rule start (seq "a" "b")))`
- Unreachable rules are pruned from compiled grammar graphs.
- Scanner clauses are context-aware; there is no fixed small scanner-slot cap.

**Total: 130+ primitives**

### 7.24 Primitive Command/Query Contract

Primitive return semantics are classified into two normative styles:
- command-style: successful completion returns `Void` (or is a non-returning command such as `exit`).
- query-style: returns data/handle/predicate values; if absence semantics apply, absence is represented with `nil` (never `Void`).

Audit source of truth is the registered primitive table in `src/lisp/eval_init_primitives.c3`.
Audit snapshot (`2026-03-17`):
- total registered primitive names: `263`
- command-style names: `42`
- query-style names: `221`

Core mutation, I/O, and scheduler command/query classification tables are maintained in:
- `docs/reference/11-appendix-primitives.md` (`Primitive Surface Audit: Command vs Query`).

Exhaustive classification rule:
- names in the audited command-style set are command-style by contract,
- every other registered primitive name is query-style by contract.

---
