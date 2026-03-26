# Appendix B: Stdlib Reference

**[Back to Index](../OMNI_REFERENCE.md)**

Functions and macros defined in `stdlib/stdlib.lisp`.

---

### Macros

| Name | Signature | Description |
|------|-----------|-------------|
| `when` | `(when test body...)` | If test, evaluate body |
| `unless` | `(unless test body...)` | If not test, evaluate body |
| `branch` | `(branch (c1 e1) ... [(_ default)])` | Multi-branch conditional chain |
| `with-defaults` | `(with-defaults n1 v1 ... body)` | Default values for dict params |
| `stream-yield` | `(stream-yield val)` | Generator-style yield |

`branch` notes:
- `_` is the default marker and must appear only in final position.
- Without `_`, `branch` returns `nil` when no condition is truthy.

### Higher-Order Functions

| Name | Signature | Description |
|------|-----------|-------------|
| `map` | `(map f coll)` | Apply f to each element (dispatched: List, Array, Iterator) |
| `filter` | `(filter pred coll)` | Keep matching elements (dispatched) |
| `foldl` | `(foldl f acc lst)` | Left fold |
| `foldr` | `(foldr f init lst)` | Right fold |
| `reverse` | `(reverse lst)` | Reverse list |
| `append` | `(append a b)` | Concatenate lists |
| `flatten` | `(flatten lst)` | Flatten one level |
| `for-each` | `(for-each f lst)` | Apply for side effects |
| `any?` | `(any? pred lst)` | Any element matches? |
| `every?` | `(every? pred lst)` | All elements match? |
| `find` | `(find pred lst)` | First match or nil |
| `remove` | `(remove pred lst)` | Remove matching |
| `partition` | `(partition pred lst)` | Split by predicate: `(kept . rejected)` |

### Sequence Operations

| Name | Signature | Description |
|------|-----------|-------------|
| `nth` | `(nth n lst)` | Nth element (0-indexed) |
| `take` | `(take n lst)` | First N elements |
| `drop` | `(drop n lst)` | Skip first N elements |
| `zip` | `(zip a b)` | Zip two collections |
| `range` | `(range n)` | List 0 to n-1 |
| `foldr` | `(foldr f init coll)` | Right fold (finite collections) |

### Iterator Combinators

| Name | Signature | Description |
|------|-----------|-------------|
| `Iterator` | `(Iterator coll)` | Canonical iterator constructor/conversion surface |
| `range-from` | `(range-from n)` | Infinite: n, n+1, n+2... |
| `repeat` | `(repeat x)` | Infinite repetition |
| `cycle` | `(cycle coll)` | Infinite cycle |

Note: `map`, `filter`, `take`, `drop`, `zip`, and `foldl` are dispatched and
operate lazily when passed an `Iterator`.
Force an iterator with constructors: `(List it)` or `(Array it)`.

### Utilities

| Name | Signature | Description |
|------|-----------|-------------|
| `id` | `(id x)` | Identity function |
| `compose` | `(compose f g)` | `(lambda (x) (f (g x)))` |
| `partial` | `(partial f args...)` | Partial application |
| `default` | `(default v fallback)` | Return v if non-nil, else fallback |
| `parse` | `(parse format src [opts])` | Thin dispatched format parse helper (`'json`, `'toml`, `'csv`) |
| `emit` | `(emit format value [opts])` | Thin dispatched format emit helper (`'json`, `'json-pretty`, `'csv`) |

### Error Handling

| Name | Signature | Description |
|------|-----------|-------------|
| `try` | `(try thunk handler)` | Catch `raise` effects |
| `assert!` | `(assert! condition msg)` | Raise if condition fails |

### Lazy Evaluation

| Name | Signature | Description |
|------|-----------|-------------|
| `delay` | `(delay thunk)` | Create memoized lazy value |
| `force` | `(force p)` | Force evaluation |

### Generators

| Name | Signature | Description |
|------|-----------|-------------|
| `stream-take` | `(stream-take n gen)` | Take N from generator |

### Handler Helpers

| Name | Signature | Description |
|------|-----------|-------------|
| `with-trampoline` | `(with-trampoline thunk)` | Trampoline bouncer |

### I/O Wrappers

These redefine I/O to go through effects (fast path when no handler):

`print`, `println`, `display`, `newline`, `read-file`, `write-file`,
`file-exists?`, `read-lines`, `filesystem-open`, `filesystem-read`,
`filesystem-write`, `filesystem-close`, `filesystem-stat`,
`filesystem-read-directory`, `filesystem-rename`, `filesystem-unlink`,
`transmission-control-connect`, `transmission-control-listen`,
`transmission-control-accept`, `transmission-control-read`,
`transmission-control-write`, `transmission-control-close`,
`user-datagram-socket`, `user-datagram-bind`, `user-datagram-send`,
`user-datagram-receive`, `user-datagram-close`,
`pipe-connect`, `pipe-listen`, `process-spawn`, `process-wait`,
`process-kill`, `signal-handle`, `signal-unhandle`, `domain-name-resolve`, `async-sleep`,
`offload`, `task-spawn`, `task-join`, `task-join-timeout`, `task-cancel`,
`task-spawn-batch`, `thread-spawn`, `thread-spawn-batch`, `thread-join`,
`thread-join-timeout`, `thread-cancel`,
`transport-layer-security-connect`, `transport-layer-security-server-wrap`,
`transport-layer-security-read`, `transport-layer-security-write`,
`transport-layer-security-close`,
`http-get`, `http-request`

Compatibility shorthands are also exported:
- filesystem wrappers: `fs-open`, `fs-read`, `fs-write`, `fs-close`,
  `fs-stat`, `fs-readdir`, `fs-rename`, `fs-unlink`
- TCP wrappers: `tcp-connect`, `tcp-listen`, `tcp-accept`, `tcp-read`,
  `tcp-write`, `tcp-close`
- UDP wrappers: `udp-socket`, `udp-bind`, `udp-send`, `udp-recv`, `udp-close`
- DNS wrapper: `dns-resolve`
- TLS wrappers: `tls-connect`, `tls-server-wrap`, `tls-read`, `tls-write`,
  `tls-close`

### Type Predicates (Stdlib-Defined)

`int?`, `double?`, `number?`, `string?`, `symbol?`, `boolean?`, `list?`,
`closure?`, `array?`, `dict?`

### Numeric Predicates (Stdlib-Defined)

`zero?`, `positive?`, `negative?`, `even?`, `odd?`
