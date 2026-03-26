# Deduce Materialized Declaration Policy Decision

Date: 2026-03-25
Status: current naming/contract decision for declaration-time policy widening

## Current shipped truth

Today the only approved declaration-time materialization policy is `manual`:

- `[relation db materialized] rel ...`
- `[relation db materialized manual] rel ...`

That surface means:

- declaration-time materialization intent is recorded,
- the relation is still refreshable only through explicit `deduce/refresh!`,
- no read-side or write-side automatic refresh contract is implied.

## Why this needed an explicit decision

The backlog had started treating “support another declaration-time policy” as
if it were only a parser/schema naming problem. It is not. Any new spelling
would also commit the language to a concrete maintenance trigger and runtime
contract.

## Canonical naming decisions

The naming problem is now decided even though the non-manual policies are not
implemented yet.

### Approved canonical names by trigger family

| Trigger family | Canonical policy spelling | Meaning |
|---|---|---|
| explicit refresh only | `manual` | Materialized intent is recorded, but maintenance only runs through explicit `deduce/refresh!`. |
| before ordinary reads | `on-read` | Reads may trigger maintenance before returning results. |
| after committed base writes | `on-base-commit` | Maintenance is driven by successful base mutation commits. |
| on open / reopen | `on-open` | Maintenance runs when a DB or relation is opened/reopened. |
| external/background scheduler step | `scheduled` | Maintenance runs through an external scheduler/background cadence, not directly on read/write/open. |

These names are approved as the only sensible vocabulary for those trigger
families. They are naming decisions, not implementation claims.

### Rejected spellings

#### `auto`

Rejected.

- Too vague for a canonical language-facing spelling.
- Hides the trigger boundary that users actually need to reason about.

#### `eager`

Rejected.

- Borrowed vocabulary with ambiguous meaning in Omni terms.
- Could describe multiple incompatible triggers.

#### `background`

Rejected in favor of `scheduled`.

- `scheduled` says more about the contract surface: maintenance occurs on an
  external cadence/step, not “somewhere in the background.”

#### `on-write`

Rejected in favor of `on-base-commit`.

- The contract boundary in Deduce is commit-shaped, not “any write call.”
- `on-base-commit` is clearer about transaction visibility and failure scope.

## Decision

The declaration-time materialization policy surface stays frozen at `manual`
only until a concrete non-manual maintenance engine exists.

Current explicit decision:

- no new declaration-time policy spelling is approved today beyond `manual`
- no additional keyword is reserved “in advance”
- runtime work must not invent a parser spelling before the maintenance trigger
  and semantics are defined
- when widening does happen, it must use one of the approved trigger-shaped
  names above rather than `auto`, `eager`, or another vague alias

## Recommendation for the first future widening

The first non-manual declaration policy should be `on-read`.

Why `on-read` is the best first widening:

- it keeps write/commit behavior explicit instead of moving hidden maintenance
  cost into commit paths immediately
- it does not require an external scheduler surface
- it does not pretend reopen/open-time rebuilding is solved before rule and
  dependency catalog persistence exists
- it naturally reuses the existing relation-scoped maintenance mental model:
  refresh the requested relation’s dependency closure when a stale read
  actually needs it

The other approved names remain valid for later work, but they are not the
recommended first widening:

- `on-base-commit` is a later write-path policy once commit-time maintenance is
  real
- `on-open` is a later reopen policy once rule/dependency catalog persistence
  is real
- `scheduled` is a later policy if the runtime grows an explicit scheduler-side
  maintenance contract

### Chosen first-future contract target

When the first non-manual maintenance engine becomes real, implement
`on-read` first.

Intended first contract shape:

- trigger:
  before an ordinary read on a stale materialized relation
- scope:
  refresh the requested relation’s dependency closure, matching the current
  targeted refresh boundary instead of refreshing the whole DB by default
- success promise:
  a successful read observes refreshed materialized data
- failure boundary:
  if maintenance cannot run truthfully, the read fails rather than silently
  pretending the stale snapshot is fresh
- current reopen interaction:
  until rule/dependency catalog persistence exists, reopened DBs remain on the
  current conservative boundary and do not pretend `on-read` can rebuild from
  missing catalogs

## Backlog consequence

- `B6.3e1` closes as the explicit decision to keep the declaration surface
  frozen at `manual`
- any future widening belongs in the follow-up runtime item only after a real
  non-manual maintenance contract exists, and it must use the approved name
  for the matching trigger family
- the first such implementation target is now explicitly `on-read`
