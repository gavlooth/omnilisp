# Omni Surface Compatibility

**Updated:** 2026-04-09

This document is the single compatibility source for removed or renamed
language surface syntax.

Policy:
- Canonical surface wins.
- Removed forms should fail closed with deterministic diagnostics.
- Do not reintroduce migration aliases by default.

## 1. Canonical Surface (Current)

| Area | Canonical form(s) |
|---|---|
| Function expressions | `lambda` (plain `λ` accepted) |
| Sequencing | `block` |
| Local binding | flat-pair `let`, `let ^rec`, named `let` |
| Delimited control | `checkpoint`, `capture` |
| Effects | `signal`, `handle`, `resolve`, `with-continuation` |
| Access syntax | `expr.name`, `expr.[key]`, `(ref expr key)` |
| Macro definition | `(define [macro] name (syntax-match ...))` |
| Value dispatch constructor | `Value` |
| Transaction command | `(deduce 'block db ['read|'write|'write-deferred])` |

## 2. Removed/Changed Forms

| Removed or non-canonical form | Replacement | Notes |
|---|---|---|
| `fn` | `lambda` | `λ` remains accepted as equivalent input spelling |
| `begin`, `do` | `block` | sequencing is `block` only |
| `reset`, `shift` | `checkpoint`, `capture` | old names removed from public surface |
| Scheme grouped let: `(let ((x 1) (y 2)) ...)` | `(let (x 1 y 2) ...)` | flat pairs, sequential left-to-right |
| `letrec` | `let ^rec` | recursive local binding surface |
| leading-dot accessors: `.name`, `.'key`, `.1`, `.[expr]` | `expr.name` / `expr.[key]` / `ref` | leading-dot callable shorthand removed |
| callable quoted-symbol accessor: `('name dict)` | `(ref dict 'name)` or `dict.name` | no implicit accessor function values |
| macro clause DSL forms | single-transformer `syntax-match` macro form | legacy clause macro definitions removed |
| `with-handlers`, `handle-chain` | explicit `handle`/`resolve` composition or `handle/chain` where applicable | old helper spellings are non-canonical |
| `Val` dispatch constructor | `Value` | parser/runtime diagnostics should point to `Value` |

## 3. Access Semantics Contract

Access syntax is split intentionally:

- `expr.name`:
  path-step access for module/instance/dict-symbol/cons semantics.
- `expr.[key]`:
  postfix index/dynamic access syntax.
- `(ref expr key)`:
  canonical dynamic collection lookup.

There is no standalone accessor shorthand and no callable quoted-symbol accessor
surface.

Higher-order accessor function values must be explicit:

```lisp
(lambda (x) (ref x 'name))
```

## 4. Compatibility Diagnostics Expectations

When removed syntax is used, diagnostics should be explicit and migratory:

- identify the removed form,
- provide canonical replacement,
- avoid silent fallback to legacy behavior.

Examples:
- using `.name` should hard-error and point to `expr.name` / `ref`.
- using `('name dict)` should hard-error and point to `ref` or path syntax.
- using `fn` should hard-error and point to `lambda`.

## 5. Contributor Rules

- Update this file in the same patch when a surface form is removed, renamed,
  or frozen as canonical.
- Do not leave removed syntax references scattered across multiple top-level
  docs. Point to this file instead.
- If a migration window is explicitly approved by the owner, document:
  - exact accepted forms,
  - expected removal date/slice,
  - deterministic diagnostics after removal.
