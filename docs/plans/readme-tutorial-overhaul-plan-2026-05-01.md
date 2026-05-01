# README Tutorial Overhaul Plan

Status: `open`
As of: 2026-05-01

Objective: turn `README.md` into a single user-facing entrypoint that covers
the Omni surface in sections, with tutorial-style examples for every major
language feature, while keeping `docs/LANGUAGE_SPEC.md` and `docs/README.md`
as the normative authority and doc-map layers.

Tracked by: `DOCS-README-001` in `docs/todo_parts/todo_part_18.md`.

## Active Hypothesis

The current README is a narrow quick start that fragments the learning path
across multiple docs. A sectioned tutorial README can carry first-pass
comprehension without duplicating normative spec text.

## Current Approach

Reorganize `README.md` into ordered sections that read top-down:

1. What Omni is and how to build or run it.
2. Core syntax and evaluation model.
3. Bindings, functions, arity, and lambdas.
4. Collections, access, and destructuring.
5. Literals, strings, and typed literal forms.
6. Mutation and update forms.
7. Control flow and pattern matching.
8. Effects and continuations.
9. Modules, imports, and scoped module open.
10. Types, truthiness, constructors, and conversions.
11. Validation, tooling, and reference links.

Keep each section anchored by a small tutorial example set that demonstrates
the feature in use, not just a syntax fragment.

Where the README needs exact rules, link out to:

- `docs/LANGUAGE_SPEC.md`
- `docs/SYNTAX_SPEC.md`
- `docs/ARCHITECTURE.md`
- `docs/OMNI_REFERENCE.md`

## Validation Path

- Cross-check each example against the normative docs before landing it.
- Make sure README links resolve and the section order stays coherent for a
  first-time reader.
- Run `git diff --check` and a markdown link sanity pass after the rewrite.

## Next Checkpoint

Approved section outline and example set in `README.md`, with the doc map
updated to match the new README shape.

## Negative-Memory Constraints

- Do not turn README into a second normative spec.
- Do not keep fragmented one-off examples that force readers to jump around
  for basic language coverage.
- Do not invent or preserve stale aliases or paths while restyling examples.
