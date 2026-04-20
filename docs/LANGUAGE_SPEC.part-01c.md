`matrix/singular-values` supports Vulkan-placed dense row-major rank-2
`Float64`, `Float32`, `Complex128`, and `Complex64` inputs through embedded
storage-backed Gram/Jacobi singular-value shaders, returning a Vulkan-placed
rank-1 component-width real Tensor for `k = min(rows, columns)`, including
`k > 64`, without hidden CPU/LAPACK fallback. Direct `matrix/svd` uses the same
storage-backed Gram strategy for Vulkan-placed factor outputs and preserves the
input float dtype. Shader non-convergence raises `tensor/no-convergence`.
Public `contract` supports Vulkan-placed dense row-major rank-N matching
`Float64` or `Float32` tensors with zero or more explicit contracted axis
pairs. Output axes are ordered as free left axes followed by free right axes.
Results are Vulkan-placed tensors and require explicit `to-device 'cpu` before
CPU inspection.
Unsupported Vulkan map and contract cases, including unsupported callables,
incompatible broadcasting shapes, unsupported layouts/dtypes, and mixed
CPU/Vulkan operands, fail closed rather than silently copying to CPU.
Direct `matrix/svd` supports Vulkan-placed dense row-major rank-2 `Float64` or
`Float32` inputs for `k = min(rows, columns)`, including `k > 64`, returns
Vulkan-placed reduced `u`, `s`, and `v` factors preserving the input float
dtype, and uses storage-backed Gram scratch without hidden CPU/LAPACK fallback.
CPU `Float32` SVD is supported through native CPU `Float32` oracles. Direct
`matrix/eigenvalues` and `matrix/eigenvectors` support Vulkan-placed dense
row-major square symmetric `Float64` inputs, including `n > 64` within helper
resource limits, 32-bit shader index guards, and the Jacobi iteration guard.
They return Vulkan-placed values and aligned vector columns, raise
`tensor/not-symmetric` for nonsymmetric Vulkan inputs, and map shader
non-convergence to `tensor/no-convergence`, without hidden CPU/LAPACK fallback.
Missing Vulkan/Float64 capability, unsupported shapes/layouts/dtypes, resource
bounds, and stride/view-backed inputs fail closed with Tensor backend
diagnostics.
Direct `matrix/eigenpairs` remains CPU-only/fail-closed on Vulkan while its
public output contract is pointer-backed `BigComplex`.
Zero-size contracted axes in supported Vulkan layouts produce additive-identity
output, matching CPU Tensor semantics.
Backend-flavored mathematical names are not part of the normal Tensor surface.

### 2.2 Truthiness

Normative predicate contract:
- Predicate positions in `if`, `when`, and `match` guards use the same
  truthiness rules.
- **Falsy:** `nil`, `false`
- **Truthy:** everything else.

| Predicate input | Example | Truthiness | Notes |
|---|---|---|---|
| `nil` | `nil` | falsy | Absence value |
| `false` | `false` | falsy | Boolean false |
| `Void` | `#<void>` | truthy | Command/effect completion token |
| numbers | `0`, `-1`, `3.14` | truthy | Zero is still truthy |
| strings | `""`, `"omni"` | truthy | Empty string is truthy |
| collections | `'()`, `[]`, `{}` | truthy | Empty collections are truthy |

### 2.3 `Void` vs `Nil` Contract

Normative rule:
- `Void` means successful command/effect completion with no payload.
- `Nil` means absence/query-miss (or falsey result in predicate-style APIs).
- `Void` is an operational completion token, not a data/absence sentinel.
- APIs should not encode query-miss/optional absence using `Void`; use `Nil`.
- `Void` is truthy under Omni truthiness rules.

Contract examples:

```lisp
(type-of (block (define x 1) (set! x 2)))   ; => 'Void
(type-of (let (d {'a 1}) (remove! d 'a)))   ; => 'Void
(if (block (define x 1) (set! x 2)) 1 0)    ; => 1  (Void is truthy)

(type-of (ref {'a 1} 'missing))             ; => 'Nil
(type-of (has? {'a 1} 'missing))            ; => 'Nil
```

### 2.4 Equality

`=` performs structural equality:
- Integers and Float64 values: numeric comparison
- Strings: character-by-character
- Symbols: identity (interned)
- Lists: recursive structural equality
- Other types: identity

---
