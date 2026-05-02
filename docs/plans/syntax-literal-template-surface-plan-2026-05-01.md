# Syntax Literal Template Surface Plan

Status: `completed`
As of: 2026-05-01

Objective: introduce a short, readable canonical reader literal template
surface for syntax construction, with `#syntax` as the proposed name, while
keeping `quote` and the hygienic macro system intact.

Tracked by:

- `SYNTAX-TEMPLATE-001`
- `SYNTAX-TEMPLATE-002`
- `SYNTAX-TEMPLATE-003`

## Active Hypothesis

`quasiquote` / `unquote` are correct but visually noisy for the primary
language surface. A dedicated `#syntax` template form can improve readability
for user-facing examples without replacing hygienic macro expansion.

## Current Approach

1. Freeze the canonical surface name as `#syntax` and define the placeholder
   contract for inserted and spliced subforms.
2. Add parser and expander support for the template literal surface.
3. Update docs and examples to prefer `#syntax` for template construction while
   keeping `quote` for literal data and `syntax-match` / `template` for
   hygienic macro generation.

## Canonical Contract (frozen 2026-05-01)

`#syntax datum` is a reader-level template literal form. It produces
`E_QUASIQUOTE` AST nodes and reuses the existing JIT/AOT quasiquote
infrastructure unchanged.

Inside a `#syntax` template:

- `#{exp}` means unquote the expression `exp` (like `,exp` in quasiquote).
- `#{.. exp}` means splice the list produced by `exp` (like `,@exp` in quasiquote).
- Nested `#syntax` is supported: `#syntax #syntax x` produces nested
  `E_QUASIQUOTE` nodes.

Comma (`,`) and comma-at (`,@`) are rejected inside `#syntax` templates
with a clear parse error directing the user to `#{exp}` / `#{.. exp}`.

Compatibility policy: `quasiquote` / `unquote` / `unquote-splicing` remain
fully supported. `#syntax` is the preferred user-facing surface for new
examples and documentation.

## Proposed TODO Items

- [x] `SYNTAX-TEMPLATE-001`: decide and document the canonical `#syntax`
  placeholder grammar and reader contract.
- [x] `SYNTAX-TEMPLATE-002`: implement `#syntax` parsing/expansion and regression
  tests for insert and splice forms.
- [ ] `SYNTAX-TEMPLATE-003`: migrate README and reference examples to `#syntax`
  and record the compatibility policy for legacy `quasiquote` / `unquote`
  usage.

## Validation Path

- Confirm the syntax contract against `docs/SYNTAX_SPEC.md` and
  `docs/LANGUAGE_SPEC.md`.
- Add parser/expander regression tests that cover literal data, single-form
  insertion, and splice insertion.
- Run `git diff --check` after docs and parser updates.

## Next Checkpoint

Canonical `#syntax` contract and placeholder grammar written down, with TODO
items open and linked from the repo backlog.

## Negative-Memory Constraints

- Do not remove `quote`; it still serves the general Lisp data literal use
  case.
- Do not turn reader literal tags into a second macro system.
- Do not keep `quasiquote` as the primary user-facing syntax once `#syntax`
  is available and documented.
