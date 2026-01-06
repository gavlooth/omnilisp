# Temporary TODO: Syntax Modernization Plan

Objective: Map the current C compiler and runtime implementation to the new `LANGUAGE_REFERENCE.md` standard.

## 1. Syntax Divergence Analysis
- [ ] Identify where function definitions use `(define (f x) ...)` instead of `(define f [x] ...)`.
- [ ] Locate `let` bindings that might use nested parens `((x 1))` vs `[x 1]`.
- [ ] Check dynamic access usage `.(expr)` vs `.[expr]`.
- [ ] Scan for mutation operators `set!` vs the new `put!`/`update!` distinction.
- [ ] Check type annotation syntax `{}` usage.

## 2. Compiler/Reader Updates
- [ ] Reader: Update to parse `[]` as binding/slot vectors in specific contexts.
- [ ] Reader: Ensure `{}` is reserved for types.
- [ ] Parser: Update `define` and `lambda` to expect `[]` for arguments.
- [ ] Reader: Implement `.[ ]` path access syntax.

## 3. Runtime/Evaluator Updates
- [ ] Implement `put!` and `update!` primitives.
- [ ] Update `set!` semantics if necessary (ensure it returns value).
- [ ] Verify `update` functional transformation logic.

## 4. Test Suite Migration
- [ ] Identify tests that need syntax updates.
