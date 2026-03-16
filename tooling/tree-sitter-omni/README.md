# tree-sitter-omni

This directory contains a first-party Tree-sitter grammar scaffold for Omni
Lisp, aligned with the current repository syntax surface in:

- `docs/SYNTAX_SPEC.md`
- `docs/LANGUAGE_SPEC.md`
- `src/lisp/parser_*.c3`

## Scope

The grammar is intentionally practical rather than fully semantic:

- it parses Omni source into stable expression-oriented nodes,
- it models Omni-specific prefixes and postfixes (`'`, `` ` ``, `,@`, `.expr`, `expr.[key]`, `#_`, `#r"..."`),
- it treats special forms as regular lists and leaves keyword/binding
  recognition to query files.

That choice keeps the grammar resilient while the parser continues to evolve.

## Files

- `grammar.js`: core grammar
- `queries/highlights.scm`: keyword and literal highlighting
- `queries/injections.scm`: regex literal injection
- `queries/locals.scm`: minimal binding/scope hints
- `queries/textobjects.scm`: structural captures for calls, blocks, declarations, and comments
- `queries/folds.scm`: fold regions for list-style Omni forms
- `tree-sitter.json`: grammar metadata for editor integrations

## Generate

```bash
cd tooling/tree-sitter-omni
tree-sitter generate
```

## Quick Parse Check

```bash
cd tooling/tree-sitter-omni
printf "(define (inc x) (+ x 1))\n(handle (signal 'ask 1) (ask v (resolve (+ v 1))))\n" > examples/sample.omni
tree-sitter parse examples/sample.omni
```

## Current Notes

- Paths are parsed structurally as `path` plus repeated `path_segment`.
- Leading-dot accessors are parsed as `accessor`, not as symbols.
- Postfix indexing is parsed as `index_expression` with one or more `index_clause`
  nodes, so chained forms like `matrix.[i].[j]` stay grouped.
- `let`/`define`/`lambda` semantics are recognized in queries, not hard-coded into
  the grammar.
- Textobject captures stay structural and grammar-backed: they target form
  shapes such as function shorthand, plain bindings, module/type declarations,
  block-style special forms, and generic call forms.
- Folding is currently list-oriented on purpose, so editors can fold module,
  declaration, and nested expression bodies without semantic analysis.
