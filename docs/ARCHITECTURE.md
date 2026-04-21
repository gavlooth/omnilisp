# Omni Architecture Decisions

This document records architecture decisions that are normative for runtime and
language-surface behavior.

## ADR-2026-03-06-A: Effects-First Failure Contract

Status: Accepted (2026-03-06)

### Context

Omni currently exposes mixed failure styles across primitives and stdlib
wrappers (`nil`, `ERROR` values, and `signal raise`). This creates ambiguity
for users and drift risk for implementers.

### Decision

Public APIs MUST classify failures into exactly one of these classes:

1. `absence`
2. `recoverable-op-failure`
3. `programmer-error`
4. `internal-runtime-error`

### Failure Classes

| Class | Meaning | Example |
|------|---------|---------|
| `absence` | Data/query is valid, but no result exists | no regex match, key not present |
| `recoverable-op-failure` | Operation failed due to environment/input state and caller can choose fallback/retry | file not found, parse failure, network timeout |
| `programmer-error` | Call contract violated by caller or invalid user program shape | wrong argument type/shape, invalid API mode |
| `internal-runtime-error` | Runtime invariant violation or internal corruption risk | impossible tag state, invalid ownership boundary state |

### Mapping to Surface Behavior

| Class | Surface behavior | Notes |
|------|------------------|-------|
| `absence` | return `nil` (or `false` for predicate APIs) | MUST NOT use `raise` |
| `recoverable-op-failure` | `signal raise` with canonical payload | resumable/catchable at effect boundary |
| `programmer-error` | `signal raise` with canonical payload | deterministic error shape, catchable for tooling and REPL flows |
| `internal-runtime-error` | hard runtime error | non-resumable; do not downcast to `nil` or ordinary `raise` |

### Canonical Error Payload Schema

All `signal raise` payloads for runtime/stdlib APIs MUST use:

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Omni has no dedicated keyword type. Use quoted symbols as dict keys.

Field requirements:

| Field | Type | Requirement |
|------|------|-------------|
| `'code` | symbol | stable machine code (for tests/routing) |
| `'message` | string | human-readable explanation |
| `'domain` | symbol | subsystem source (`io`, `parser`, `regex`, `scheduler`, `deduce`, `type`, `runtime`) |
| `'data` | dict or `nil` | structured context; no required keys globally |

### Examples

Correct (`recoverable-op-failure`):

```lisp
(signal raise
  {'code 'io/not-found
   'message "read-file: path not found"
   'domain 'io
   'data {'path "missing.txt"}})
```

Correct (`absence`):

```lisp
(find (lambda (x) (= x 999)) '(1 2 3))
; => nil
```

Correct (`programmer-error`):

```lisp
(signal raise
  {'code 'scheduler/invalid-handle
   'message "tcp-accept: expected listener handle"
   'domain 'scheduler
   'data {'api 'tcp-accept}})
```

### Counterexamples

Not allowed for new or migrated APIs:

```lisp
; Wrong: returns nil for operational failure (not absence)
(define (read-file* path)
  (if (file-exists? path) (__raw-read-file path) nil))

; Wrong: unstructured raise payload
(signal raise "oops")

; Wrong: wraps internal invariant break as recoverable effect
(signal raise {'code 'runtime/impossible-state 'message "..." 'domain 'runtime 'data nil})
```

### API Classification Baseline (2026-03-06)

This table classifies existing public APIs against this contract.

| API | Primary class | Contracted surface | Current style (2026-03-06) | Target |
|-----|---------------|--------------------|------------------------------|--------|
| `find` | `absence` | `nil` when not found | `nil` | keep |
| `re-match` | `absence` + `recoverable-op-failure` | `nil` for no match; `raise` for invalid pattern | mixed (`nil` + implementation-specific failure) | normalize to contract |
| `read-file` | `recoverable-op-failure` | `raise` on I/O failure | mixed across call paths | migrate to canonical `raise` payload |
| `write-file` | `recoverable-op-failure` | `raise` on I/O failure | mixed across call paths | migrate to canonical `raise` payload |
| `pika/parse` | `recoverable-op-failure` | `raise` on malformed grammar/input | mixed (`ERROR`/raise patterns) | migrate to canonical `raise` payload |
| `deduce` actions (`'open`, `'query`, `'fact!`, ...) | `recoverable-op-failure` or `programmer-error` | `raise` for operation/config misuse | mixed (`ERROR` + raise) | migrate to canonical `raise` payload |
| `unsafe-free!` follow-on access | `programmer-error` | `raise` with deterministic payload | message-based error path | migrate to canonical `raise` payload |

## ADR-2026-04-21-A: Scope/Region Ownership Contract

Status: Accepted (2026-04-21)

### Context

Omni's runtime memory model has moved from experimental boundary handling to a
settled dual-lane scope/region design. The implementation truth is recorded in
`memory/DESTINATION_ARENA_PLAN.md`, `memory/CHANGELOG.md`, and the current
boundary tests, but future work needs one project-facing contract that is easy
to review before adding new runtime value shapes.

### Decision

`ScopeRegion` is the ownership unit for ordinary Omni language values.

Each region has two allocation lanes:

| Lane | Purpose | Contract |
|------|---------|----------|
| `TEMP` | transient evaluation data | May be reset or released at scope boundaries. Values reachable only from TEMP do not survive return/env/root publication. |
| `ESCAPE` | boundary survivors | Holds values being committed across return, env-copy, root/global store, or other publication boundaries. |

Boundary publication must prove this invariant:

> A committed ESCAPE root must not retain a reachable Omni-owned edge into
> releasing TEMP.

Boundary copy/promotion must use the shared boundary helpers and shared
promotion context. Do not add ad-hoc per-callsite copying in eval/JIT paths.
`scope_adopt` is retired from normal return flow and must not be reintroduced
as a shortcut around boundary commit, env copy, mutation copy, or promotion.

Ordinary language values must not gain independent per-type reference-count
lifetime systems. The owner is the scope/region. Wrappers may cross boundaries,
but ownership authority stays with region retain/release and boundary
promotion/copy.

### Exceptions

Opaque foreign resources may use local lifetime management only when all of
these are true:

1. The payload does not own an Omni `Value` graph, or it exposes explicit
   traversal/copy/promotion/destructor hooks for every Omni-owned edge.
2. There is exactly one finalizer authority.
3. Boundary promotion either preserves the explicit ownership contract or fails
   closed.
4. Tests cover return boundary, closure/env capture, root/global store if
   applicable, failure rollback, and destruction.

Examples of allowed opaque-resource families include `FFI_HANDLE` boxes over
foreign pointers and backend/device handles whose Omni ownership is represented
by stable handles plus explicit finalizers. These are exceptions, not the model
for ordinary `Instance`, `Closure`, `Array`, `HashMap`, `TensorVal`, or other
language values that carry Omni edges.

### Durable Graph Rule

Temporary runtime graphs may be cyclic when the evaluator needs that shape.
Durable published data should trend toward trees/DAGs plus interned atoms and
stable handles.

Long-lived AST, IR, query-plan, library, Tensor metadata, scheduler, server,
and backend state should prefer:

- indices or stable IDs,
- handles with explicit release/finalizer authority,
- arrays of compact records,
- interned symbols/shapes,
- immutable atoms,
- explicit parent/child ownership records.

Arbitrary durable cycles require an explicit design note naming the owner,
destructor authority, boundary traversal behavior, and regression coverage.

### Handles and API Boundaries

Ordinary `Value*`, `Env*`, and runtime pointers are acceptable inside one
scope/lane and within tightly owned runtime helper calls. Cross-subsystem or
cross-lifetime APIs should prefer stable handles or IDs.

This policy applies especially to Tensor backend buffers, FFI resources,
scheduler/server handles, query DB handles, model/checkpoint handles, and
long-lived mutable graph stores. Each handle family must have one release or
finalizer authority and must fail closed if the handle cannot be validated.

### Static Allocation Routing

Static or JIT allocation intent is an optimization layer, not the correctness
foundation. Compile-time escape analysis may reduce promotion work, but Omni's
dynamic Lisp surface, continuations, effects, mutation, FFI, root stores, and
runtime dispatch still require dynamic boundary enforcement.

Static/JIT allocation improvements must sit behind the same runtime invariants:

- return/env/root publication goes through boundary helpers;
- copy-site and commit counters remain observable;
- memory-lifetime tests still prove survival and no TEMP edges under committed
  ESCAPE roots.

### Boundary Vocabulary

Use these terms consistently:

| Term | Meaning |
|------|---------|
| boundary commit | Publishing a scoped result across a lifetime boundary after proving/copying ownership. |
| destination ESCAPE build | Constructing a return/root/env survivor directly in the target ESCAPE lane. |
| promotion | Copying or rebuilding a value graph from a releasing scope into a surviving lane/scope. |
| splice | Moving eligible ESCAPE chunks/destructors from child to parent without copying payloads. |
| fallback copy | Conservative graph copy used when direct reuse/splice cannot prove ownership. |
| reusable target-chain value | A value already owned by the target scope chain or root and safe to reuse. |
| releasing TEMP | TEMP lane of the scope being returned from, reset, or released. |
| releasing ESCAPE | ESCAPE lane of the releasing scope; may be spliced or copied depending on boundary state. |
| mixed/uncertain provenance | Any value graph whose reachable ownership cannot be proven cheaply; must fail closed or copy. |

### Examples

Returning a list:

- Build the returned cons spine in destination ESCAPE when the return shape is
  known, or promote/copy it through the boundary helpers.
- The committed list must not retain `car` or `cdr` edges into releasing TEMP.

Returning an iterator:

- Treat iterator payloads as graph-carrying wrappers.
- Copy/promote the closure or thunk payload unless target-chain ownership is
  proven.

Closure capture:

- Captured env frames must be copied or reused only when every frame, parent,
  and binding value has surviving ownership.
- Do not mutate non-persistent shared env parents in place.

Root/global define:

- Values stored in root/global state must be root-owned or explicitly promoted.
- A root entry must not retain a child TEMP edge.

Tensor lazy expression:

- Tensor expression nodes carry Omni `Value*` edges such as map operands,
  closures, contract operands, or view sources.
- Boundary copy/promotion and graph audit must traverse those edges unless the
  payload is an explicit opaque foreign handle with no Omni-owned children.

FFI handle or backend handle:

- The wrapper may use a local finalizer over the foreign resource.
- The wrapper must not hide Omni-owned children unless it exposes traversal and
  copy/promotion behavior.

Mutable collection update:

- Barriered updates must not store releasing-scope values into longer-lived
  arrays, dictionaries, sets, objects, or env frames.
- If ownership cannot be proven, copy/promote before storing.

Failure during ESCAPE publication:

- Partially materialized destination graphs must roll back heap payloads,
  destructors, and retained resources they introduced.
- Failure must not leave a published root with TEMP reachability.
