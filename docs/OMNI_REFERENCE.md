# Omni Lisp Reference

**Version 0.2.0** | **March 2026**

Omni is a Lisp with modern semantics: strict-arity lambdas, algebraic effects,
delimited continuations, multiple dispatch, structural types, and region-based
memory. It compiles via GNU Lightning JIT and runs without a garbage collector.

---

## Chapters

| # | Chapter | Sections |
|---|---------|----------|
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

## Appendices

| | Appendix | |
|---|---------|---|
| A | [Primitive Reference](reference/11-appendix-primitives.md) | ~180 built-in primitives |
| B | [Stdlib Reference](reference/12-appendix-stdlib.md) | ~80 stdlib functions/macros |
| C-D | [Limits & EBNF Grammar](reference/13-appendix-limits-grammar.md) | Resource limits, formal grammar |

---

*Omni Lisp -- A Lisp with delimited continuations, algebraic effects,
strict-arity lambdas, multiple dispatch, and structural types.*
