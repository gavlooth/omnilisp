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
- `queries/indents.scm`: structural indentation for bracketed Omni forms
- `tree-sitter.json`: grammar metadata for editor integrations

## Generate

```bash
cd tooling/tree-sitter-omni
npm run generate
```

## Quick Parse Check

```bash
cd tooling/tree-sitter-omni
printf "(define (inc x) (+ x 1))\n(handle (signal 'ask 1) (ask v (resolve (+ v 1))))\n" > examples/sample.omni
npm run parse
```

## Corpus Tests

```bash
cd tooling/tree-sitter-omni
npm test
```

## Query Checks

```bash
cd tooling/tree-sitter-omni
npm run query:highlights
npm run query:textobjects
npm run query:indents
npm run test:queries
```

## Current Notes

- Paths are parsed structurally as `path` plus repeated `path_segment`.
- Leading-dot accessors are parsed as `accessor`, not as symbols.
- Postfix indexing is parsed as `index_expression` with one or more `index_clause`
  nodes, so chained forms like `matrix.[i].[j]` stay grouped.
- `let`/`define`/`lambda` semantics are recognized in queries, not hard-coded into
  the grammar.
- Removed spellings such as `fn`, `begin`, `do`, `letrec`, `reset`, and `shift`
  are parser-level migration diagnostics, not structural Tree-sitter parse
  errors. Query checks therefore verify that those spellings do not receive
  canonical highlight captures instead of pretending the grammar rejects them.
- Textobject captures stay structural and grammar-backed: they target form
  shapes such as function shorthand, plain bindings, module/type declarations,
  block-style special forms, and generic call forms.
- Folding and indentation are intentionally bracket-structural, so editors can
  fold and indent nested Omni forms without special-form-specific parser logic.
