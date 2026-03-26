# Deduce Next Integrity Class Decision (2026-03-25)

## Decision

The next widened Deduce integrity class is `check`.

This fixes the canonical class name for the next integrity lane after the
already shipped `key`, `unique`, and `ref` surfaces.

## Canonical Naming

- canonical class name: `check`
- rejected aliases: `assert`, `predicate`, `guard`

Those rejected names should not appear in backlog wording, docs, or later
surface work for this lane.

## Why `check`

- It complements the current integrity set cleanly:
  `key` and `unique` cover identity and exclusivity,
  `ref` covers target existence,
  `check` covers row-local validity predicates.
- It is the least ambiguous established constraint term for a boolean
  validity condition over candidate row data.
- It scopes the first widening to a smaller semantic jump than more complex
  graph- or schedule-dependent integrity classes.

## Current Boundary

This decision originally fixed the class name only. The current tree now also
ships the first concrete `check` surface:

- declarations accept unary column checks in the form
  `(check predicate column)`
- `deduce/schema` and `deduce/analyze` expose the declaration/admin payload
  baseline
- immediate `fact!`, derived rule-head publish, and deferred
  `write-deferred` commit-time validation enforce declared checks
- generic integrity history surfaces now expose `violation-class = 'check`
- relation-local `deduce/stats` and DB-wide `deduce/analyze` now expose
  dedicated `check-integrity-violation-count` counters

There is no remaining `check`-specific backlog slice in the current queue.

## Deliberate Non-Decisions

- The exact declaration form for `check` is still future work.
- Whether the first shipped `check` surface is relation-level only or also
  allows a narrower column-local sugar is still future work.
- This decision does not choose any additional integrity class beyond
  `check`.
