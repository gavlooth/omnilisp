## 8. Explainability Contract

`deduce/explain` output should include at minimum:

- normalized logical query/rule form,
- chosen physical operators,
- join order,
- index access per atom,
- estimated vs observed row counts (when available).

Explain output must be deterministic for deterministic stats snapshots.

Current implementation status note:

- `deduce/explain` is now available on both canonical and dispatch surfaces:
  - `(deduce/explain db [head-symbol|rule-index])`
  - `(deduce 'explain db [head-symbol|rule-index])`
- current deterministic payload skeleton includes:
  - top-level: `kind`, `status`, `rule-index`, `rule-count`, `head-predicate`,
    `join-order`, `steps`
  - per-step (`steps` list): `position`, `predicate`, `negated`, `bound-mask`,
    `selected-index`, `operator`, `filter-pushdown`, `projection-mask`,
    `counters`
- per-step `counters` dictionary keys:
  - `rows-read`, `rows-emitted`, `index-hits`, `index-misses`, `join-probes`,
    `counter-kind`
  - current `counter-kind` is `observed`; values are sourced from runtime
    execution rather than schema estimates, while top-level
    `surface-kind = 'planner-snapshot` and `goal-directed-execution-path`
    remain planner-snapshot fields.
- `deduce/analyze` exposes the same runtime-observed step counters under
  top-level `rule-execution`, where each entry contains:
  - `rule-index`
  - `head-predicate`
  - `join-order`
  - `steps`
- operator values are explicit symbols derived from planned access mode:
  `full-scan`, `index-scan`, `negated-scan`, `negated-index-probe`.
- this v1 surface remains planner-focused in operator naming/order, while the
  attached counters now reflect runtime-observed execution.
- for current conjunctive rules, `deduce/explain.steps[*]` and
  `deduce/analyze.rule-execution[*].steps[*]` share the same planner-derived
  `join-order`, `predicate`, `operator`, and `selected-index` shape; runtime
  truth is carried by the observed counters rather than a separate reordered
  execution trace.
- the current counter surfaces are intentionally distinct:
  - `deduce/stats.last-goal-directed-read-step-counters` records the last
    actual preserved-bound runtime read for that relation
  - `deduce/analyze.rule-execution[*].steps[*].counters` records the observed
    counters from the `deduce/analyze` execution itself
  - `deduce/explain.steps[*].counters` records the observed counters from the
    `deduce/explain` execution itself
  These surfaces are truthful but not interchangeable, and their observed
  `rows-read` / `rows-emitted` / `join-probes` values may differ after the
  same earlier read.

## 8.1 Provenance / Why-Result Payload Shape

This is the canonical shape for future provenance-aware Deduce surfaces.
It is designed to answer "why did this row appear?" with a deterministic
derivation trace that matches the existing `kind`/`status`/payload style used
by `deduce/explain` and `deduce/analyze`.

Canonical surface names for this payload family:

- `deduce/why-result` for selected row or fact lineage
- provenance-aware `deduce/analyze` when a caller asks for derivation tracing

Current shipped public slice:

- `(deduce/why-result relation val...)` is public today for `subject-kind = row`
  on stored tuple subjects
- extensional stored rows return `status = ok` with one deterministic `seed`
  path and one `fact` support frame
- stored rows in relations with exactly one non-negated, non-aggregate,
  extensional rule also return `status = ok` when every support tuple is fully
  reconstructible from the head row
  - current supported shapes include direct-copy, one-body variable
    projection/permutation rules, and multi-body extensional rules whose body
    variables are all bound by the head row
  - the derived payload carries one deterministic `derived` path and a `fact`
    support frame for each reconstructed supporting row
- exact-one-rule extensional derived relations also now support search-based
  non-recursive lineage when some body variables are not bound by the head row
  - if the observed DB state yields one deterministic support path, the payload
    returns `status = ok`
  - if multiple support paths exist, the payload returns `status = partial`
    with `truncated = true` and the first deterministic path
- exact-one-rule non-recursive mixed-body lineage is also now supported when
  derived body predicates are themselves already provable by the shipped
  exact-one-rule provenance helper surface
  - unique mixed-body support chains return `status = ok`
  - multiple mixed-body support chains return `status = partial` with the
    first deterministic path
- multi-rule non-recursive lineage is also now supported when one or more
  matching rules are already provable by the shipped non-recursive why-result
  helper surface
  - if exactly one support path exists across those supported matching rules,
    the payload returns `status = ok`
  - if multiple support paths exist across those supported matching rules, the
    payload returns `status = partial` with `truncated = true` and the first
    deterministic path
- first positive recursive closure lineage is also now supported when the
  recursive support chain can be proven through the shipped row-subject helper
  surface without revisiting the same `(predicate, tuple)` subject
  - a single recursive support chain returns `status = ok`
  - multiple recursive support chains return `status = partial` with
    `truncated = true` and the first deterministic path
  - recursive closure payloads now also append a `rule-step` support frame for
    each derived child subject that was used in the chosen proof path
  - recursive closure `max-depth` now reflects that deeper derived step
- missing stored rows return `status = missing`
- broader stored rows in relations with derived-rule lineage currently return
  `status = error` with code
  `deduce/why-result-derived-subject-not-yet-supported`
- `why-result` now also exposes optional top-level
  `goal-directed-read-context` metadata when the relation has last-read
  goal-directed runtime state
  - this mirrors the existing `last-goal-directed-read-*` admin truth for the
    relation, not a proof-specific derivation step
  - it may appear on `ok`, `partial`, `missing`, or `error` payloads
- `why-result` now also attaches optional path-local
  `goal-directed-read-context` metadata on the matching proof path when the
  relation’s last goal-directed `deduce/query`, `deduce/match`,
  `deduce/scan`, or `deduce/scan-range` observed a bounded complete row set
  of at most `8` rows and the traced tuple belongs to that stored subject set
  - this is currently limited to bounded-complete goal-directed `query`,
    `match`, `scan`, and `scan-range` results with no more than `8`
    matched rows
  - for `query`, `match`, and `scan-range`, that bounded-complete path-local
    slice applies in both plain and selector-scoped goal-directed reads;
    selector-scoped payloads keep their concrete `selector-rule-index`
  - `scan` now also participates in that bounded-complete path-local slice for
    selector-scoped reads; those payloads keep their concrete
    `selector-rule-index`
  - plain no-op `query` and `scan-range` reads now also store that bounded
    complete row set, so matching root proof paths can carry the same
    path-local context even when the last read stayed on `no-op`
  - plain no-op `match` reads now also participate in that bounded root-path
    slice for matching rows
  - selector-scoped valid row reads now also keep truthful path-local context
    across the current shipped shapes:
    - selector-scoped `match` and `scan` can stay on `no-op` and still carry
      bounded path-local context with their concrete `selector-rule-index`
    - selector-scoped `query` and `scan-range` can stay on their shipped
      ephemeral demand paths and still carry bounded path-local context with
      their concrete `selector-rule-index`
  - matching derived support frames inside the chosen proof path now also
    carry that same path-local context when their `(relation, tuple)` pair
    matches a bounded-complete last-read subject set from the current shipped
    `query` / `scan` / `scan-range` slices
  - matching fact support frames inside the chosen proof path now also carry
    that same path-local context when their `(relation, tuple)` pair matches
    a bounded-complete last-read subject set
  - when the root tuple itself did not match but the support frames carrying
    bounded path-local context all come from the same relation-local last-read
    state, the proof path now also inherits that same
    `goal-directed-read-context`
  - when multiple support-frame relations contribute different bounded
    path-local contexts to the chosen proof path, the path now exposes
    `goal-directed-read-contexts` as a list of those distinct contexts
    instead of forcing a fake single merged context
  - other rows on the same relation keep only the top-level relation-read
    context
- broader proof-path-integrated goal-directed provenance beyond those
  bounded-complete root/fact-frame/rule-step row-matching slices and other
  broader subject kinds,
  remain roadmap work

Top-level payload:

- `kind`: `why-result`
- `status`: `ok`, `missing`, `partial`, or `error`
- `surface-kind`: `provenance-snapshot`
- `subject-kind`: `row`, `fact`, or `binding`
- `subject`: normalized selector data for the traced result
- `path-count`: number of derivation paths included
- `max-depth`: maximum path depth included in the payload
- `truncated`: whether the trace was truncated by depth or path cap
- `paths`: ordered list of derivation paths

Status contract:

- `status = ok`
  - the subject was found
  - at least one derivation path is present
  - `path-count > 0`
  - `paths` is non-empty
- `status = missing`
  - the subject was not found in the observed DB state
  - `path-count = 0`
  - `paths = []`
  - `truncated = false`
- `status = partial`
  - the subject was found, but the returned derivation payload is
    intentionally incomplete due to path cap, depth cap, or an explicitly
    unsupported support-frame class inside an otherwise valid result
  - `path-count > 0`
  - `paths` is non-empty
  - `truncated = true`
- `status = error`
  - the request could not produce a truthful provenance snapshot at all
  - `path-count = 0`
  - `paths = []`
  - `subject` may still be echoed when normalization succeeded before the
    error boundary

Envelope requirements:

- all four statuses keep the same top-level envelope keys:
  - `kind`
  - `status`
  - `surface-kind`
  - `subject-kind`
  - `subject`
  - `path-count`
  - `max-depth`
  - `truncated`
  - `paths`
- future public surfaces may add optional diagnostic keys, but the envelope
  above stays canonical
- `paths` ordering must be deterministic for a fixed DB state even when
  `status = partial`
- current optional diagnostic keys include:
  - `goal-directed-read-context`

Subject shapes:

- `subject-kind = row`
  - `relation`
  - `tuple`
  - `bindings` when the caller supplied a binding-oriented selector
- `subject-kind = fact`
  - `predicate`
  - `tuple`
  - `fact-index` when the fact is addressable by stored row index
- `subject-kind = binding`
  - `predicate`
  - `bindings`

Path shape:

- `path-id`: stable numeric id within the payload
- `path-kind`: `derived` or `seed`
- `head-predicate`: predicate that produced the traced result
- `head-tuple`: tuple at the head of the path
- `rule-index`: rule that produced the head tuple, when `path-kind = derived`
- `support`: ordered list of support frames from leaf facts to the head
- `truncated`: whether this individual path was depth-truncated
- current optional path-local diagnostic keys include:
  - `goal-directed-read-context` for the current exact-one goal-directed
    `query` / `match` / `scan` / `scan-range` slice

Support frame shape:

- `kind`: `fact` or `rule-step`
- `frame-index`: stable numeric id within the path
- `predicate`
- `tuple`
- `rule-index`: present for rule-step frames
- `step-index`: body step within the rule, when relevant
- `selected-index`: chosen access path for the step, when relevant
- `operator`: explicit operator symbol when the frame comes from a planned step
- `bound-mask`: bound-variable mask when the frame comes from a planned step
- `bindings`: the substitution visible at this frame, when available
- `depends-on`: optional list of earlier `frame-index` values that justify this
  frame

Example shape:

```lisp
{
  kind: why-result,
  status: ok,
  surface-kind: provenance-snapshot,
  subject-kind: row,
  subject: {
    relation: ancestor,
    tuple: [alice, bob]
  },
  path-count: 1,
  max-depth: 3,
  truncated: false,
  paths: [
    {
      path-id: 0,
      path-kind: derived,
      head-predicate: ancestor,
      head-tuple: [alice, bob],
      rule-index: 4,
      support: [
        {
          kind: fact,
          frame-index: 0,
          predicate: parent,
          tuple: [alice, carol]
        },
        {
          kind: fact,
          frame-index: 1,
          predicate: parent,
          tuple: [carol, bob]
        },
        {
          kind: rule-step,
          frame-index: 2,
          predicate: ancestor,
          rule-index: 4,
          step-index: 1,
          selected-index: 0,
          operator: index-scan,
          bound-mask: [true, false],
          bindings: {?x: alice, ?y: bob}
        }
      ],
      truncated: false
    }
  ]
}
```

Design constraints:

- path ordering must be deterministic for a fixed database state
- repeated derivations of the same result should be preserved as distinct paths
- support frames should be sufficient to reconstruct a human-readable proof
  without requiring planner internals
- no hidden counters or implicit joins should be needed to interpret the payload

## 9. Error and Rejection Contract

Program construction and query planning errors must use domain `deduce` with
stable codes, including:

- `deduce/arity-mismatch`
- `deduce/relation-not-found`
- `deduce/unsafe-rule`
- `deduce/non-stratifiable-program`
- `deduce/planner-no-valid-plan`

Operational DB failures continue using existing Deduce DB/txn error surfaces.

## 10. Testing Contract

Minimum regression matrix:

1. Rule validation:
- valid rule acceptance,
- unsafe variable rejection,
- arity mismatch rejection,
- non-stratifiable negation rejection.

2. Evaluator parity:
- naive vs semi-naive equality on recursive programs.

3. Planner behavior:
- selective predicates avoid full scans, broad scans stay full-scan, and
  skew-sensitive join orderings remain stable (assert through explain payloads).

4. Runtime semantics:
- query absence remains `nil`,
- mutation success remains `Void`.

## 11. Compatibility Contract

- existing `deduce` API command surfaces remain valid.
- internal engine changes are transparent unless explicitly version-gated.
- any syntax expansion for rules/queries must preserve existing programs or
  provide a deliberate migration path.
