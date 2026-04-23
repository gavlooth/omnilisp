# Omni Lisp Reference

**Version 0.2.1** | **April 2026**

This file is a navigation index for the reference chapters.
It is not the normative language contract.

Normative sources:
- `docs/LANGUAGE_SPEC.md`
- `docs/SYNTAX_SPEC.md`
- `docs/ARCHITECTURE.md`
- `docs/ERROR_MODEL.md`

Compatibility/removal policy:
- `docs/SURFACE_COMPATIBILITY.md`

Authority and coverage map:
- `docs/DOCS_CONTRACT.md`

---

## Chapters

| # | Chapter | Sections |
|---|---|---|
| 0 | [Overview, Data Types & Truthiness](reference/00-overview.md) | 1-3 |
| 1 | [Special Forms, Pattern Matching & Destructuring](reference/01-special-forms.md) | 4-6 |
| 2 | [Functions & Partial Application](reference/02-functions.md) | 7-8 |
| 3 | [Collections, Strings & Math](reference/03-collections.md) | 9-11 |
| 4 | [Type System & Multiple Dispatch](reference/04-type-system.md) | 12-13 |
| 5 | [Macros & Modules](reference/05-macros-modules.md) | 14-15 |
| 6 | [Effects, Continuations, Coroutines, Iterators & Errors](reference/06-effects.md) | 16-20 |
| 7 | [I/O, Networking & JSON](reference/07-io-networking.md) | 21-23 |
| 8 | [Regex, PEG, Compression, Unicode & Deduce](reference/08-libraries.md) | 24-28 |
| 9 | [Concurrency, FFI & Schema Validation](reference/09-concurrency-ffi.md) | 29-31 |
| 10 | [System, Reader Syntax & CLI Tooling](reference/10-system-tooling.md) | 32-34 |

## Reference Notes

| | Note | |
|---|---------|---|
| N1 | [Memory, Ownership & Lifetime Notes](reference/14-memory-ownership-lifetime.md) | runtime design notes and proposals |

## Appendices

| | Appendix | |
|---|---|---|
| A | [Primitive Reference](reference/11-appendix-primitives.md) | built-in primitives |
| B | [Stdlib Reference](reference/12-appendix-stdlib.md) | stdlib functions/macros |
| C-D | [Limits & EBNF Grammar](reference/13-appendix-limits-grammar.md) | resource limits + grammar |

---

If reference content disagrees with normative docs, follow normative docs.
