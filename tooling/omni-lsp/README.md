# omni-lsp

`omni-lsp` is a small language server for Omni Lisp.

Current feature set:

- full document sync,
- diagnostics driven by `omni --check --json`,
- pull diagnostics via `textDocument/diagnostic`,
- workspace pull diagnostics via `workspace/diagnostic`,
- document symbols for module and declaration forms,
- monikers for local declaration sites and exact-name workspace fallback,
- workspace symbol search across open and workspace-root Omni documents,
- document highlights for current-buffer declaration names,
- local-first go-to-definition with exact-name workspace fallback,
- local-first go-to-declaration with exact-name workspace fallback,
- local-first go-to-implementation with exact-name workspace fallback,
- local-first go-to-type-definition for type-like declarations,
- call hierarchy preparation plus incoming/outgoing calls for current-buffer,
  open-workspace, and unopened-workspace declarations,
- local references plus exact-name workspace fallback, and local rename plus
  exact-name workspace fallback, for top-level declaration names,
- local code actions for converting between shorthand function `define` and
  explicit `lambda` bindings, plus body `block` wrapping/inlining rewrites,
- syntactic signature help for local and workspace function calls,
- parameter inlay hints for unambiguous local and workspace function calls,
- folding ranges for multiline Omni forms,
- structural selection ranges for nested Omni forms,
- linked editing ranges for parameter names within a single declaration body,
- indentation-only document and range formatting,
- on-type formatting for current-line indentation after newline and `)` input,
- semantic tokens for declaration sites, parameters, core form heads, and
  lexical literals/comments,
- document links for import/export targets,
- declaration code lenses with resolve-time workspace-aware reference counts for
  unique names, and local-only counts for overloaded local names,
- keyword, builtin, local declaration, and workspace declaration completion,
  with source-backed `completionItem/resolve` docs for declaration items,
- hover help for core forms, builtin values, and local-first declaration
  snippets with workspace fallback.

The server is intentionally thin. It shells out to the installed `omni` CLI for
non-executing syntax/check diagnostics so editor behavior tracks the same parser
that the runtime uses.

## Run

```bash
python3 tooling/omni-lsp/omni_lsp.py
```

Override the Omni binary path:

```bash
OMNI_LSP_OMNI_BIN=/path/to/omni python3 tooling/omni-lsp/omni_lsp.py
```

If you point at a raw build artifact like `build/main`, also provide the runtime
library path:

```bash
OMNI_LSP_OMNI_BIN=/path/to/build/main \
OMNI_LSP_LD_LIBRARY_PATH=/usr/local/lib \
python3 tooling/omni-lsp/omni_lsp.py
```

## Neovim `lspconfig`

```lua
vim.lsp.config("omni_lsp", {
  cmd = { "python3", "/absolute/path/to/Omni/tooling/omni-lsp/omni_lsp.py" },
  filetypes = { "omni" },
  root_markers = { "project.json", ".git" },
})

vim.lsp.enable("omni_lsp")
```

## Notes

- Diagnostics use the first-party `omni --check --json` surface.
- Pull diagnostics reuse that exact same check path and return full
  `textDocument/diagnostic` reports for clients that prefer pull over publish.
- Workspace pull diagnostics use the same check path too and currently return
  full or unchanged per-document reports across open and unopened workspace
  `.omni` files under discovered roots.
- Document symbols and local completion are syntactic scans of the active
  buffer, so they work for common Omni layouts without semantic indexing.
- Monikers are local-first and exact-name workspace-backed: declaration sites
  return first-party Omni monikers derived from container, declaration detail,
  and symbol name, while unresolved caller-side symbols fall back to matching
  workspace declarations with `import`-style monikers.
- Completion appends declaration names from other open Omni documents and
  unopened workspace-root Omni files after current-buffer declarations, reusing
  the same cached workspace declaration summaries as `workspace/symbol`.
- Completion item resolution is also source-backed: local and workspace
  declaration items resolve to markdown snippets from the owning declaration
  form, while builtin and special-form items keep their inline static docs.
- Signature help is syntactic and list-based: it uses the innermost call form,
  serves current-buffer declarations first, falls back to exact-name workspace
  function declarations from open and unopened Omni files, and provides static
  signatures for a small set of core special forms.
- Inlay hints reuse the same declaration data and only emit parameter-name hints
  when the current-buffer or workspace function match is unambiguous, so
  overloaded names with conflicting parameter labels stay quiet instead of
  guessing.
- Folding ranges are purely structural: any multiline list, vector, or map form
  can fold, which covers module bodies, multiline declarations, `block` forms,
  and nested multiline special forms without semantic analysis.
- Selection ranges are also structural: they expand from the symbol under the
  cursor to the enclosing nested forms, which makes editor expand-selection
  features line up with Omni’s list-based syntax.
- Linked editing ranges are intentionally narrow and local: they currently bind
  a parameter declaration together with same-name uses inside that declaration
  body, while skipping obvious nested `define`, `module`, and `quote` forms so
  the server does not blindly cross simple shadowing boundaries.
- Formatting is intentionally conservative for now: it normalizes leading
  indentation and trims trailing whitespace from touched lines, but it does not
  attempt a full pretty-printer or rewrite intra-line spacing.
- On-type formatting reuses that same conservative formatter and currently
  applies only a line-scoped indentation fix when the client requests it after
  newline or `)` input.
- Semantic tokens are local to the current buffer and intentionally syntactic:
  they mark declaration sites, parameter bindings, special-form heads, builtin
  type/value names, numeric literals, strings, and comments without trying to
  infer cross-module meaning or full type information.
- Document links are syntactic: they resolve string imports like
  `"models.omni"` relative to the current file, and symbol targets in `import`
  or `export-from` forms through exact-name workspace module declarations.
- Code lenses are intentionally conservative too: they annotate declarations in
  the current document with exact-name reference counts, excluding declaration
  sites. Unique declaration names use the same workspace-aware exact-name
  reference path as the rest of the server, while overloaded local names stay
  local-only so the server does not guess which overload a workspace call
  belongs to. `textDocument/codeLens` returns lightweight unresolved lenses, and
  `codeLens/resolve` fills in the count plus the first-party
  `omni.showReferences` command payload so editor clients can choose how to
  present the supplied reference locations.
- Workspace symbol search is also syntactic. It searches open Omni documents
  first, then scans `.omni` files under workspace roots discovered from
  `project.json` or `.git` markers near open documents.
- Workspace-root file results are cached in-process behind a path/mtime/size
  manifest plus parsed symbol summaries, so repeated queries reuse unchanged
  unopened files without rereading or rescanning their declaration forms while
  still picking up on-disk edits when the workspace file signature changes.
- Code actions are currently local declaration rewrites, not semantic fixes:
  they convert `(define (name args) body)` to `(define name (lambda (args) body))`
  and the inverse when the bound value is a `lambda`, and they can also wrap
  multi-form function bodies in an explicit `(block ...)` or inline a redundant
  explicit `block` body back into direct declaration forms.
- Hover uses static docs for core forms/builtins and source-backed snippets for
  current-buffer declarations first, then falls back to exact-name declaration
  matches from open and unopened workspace Omni files when the active buffer has
  no local declaration, including overloaded local names.
- Document highlights are local to the current buffer and classify declaration
  sites separately from ordinary reads.
- Definition, references, and rename stay syntactic: definition resolves
  current-buffer declarations first, then falls back to exact-name matches from
  open and unopened workspace Omni files when the active buffer has no local
  declaration for that symbol. Definition and declaration also treat import
  string targets as navigable links and jump to the imported file’s entry
  declaration when the cursor is on `"module.omni"` text. References now follow
  the same local-first shape:
  they stay current-buffer when the active buffer owns the declaration, and
  otherwise fall back to exact-name matches across open and unopened workspace
  Omni files. Rename now follows the same boundary: it stays current-buffer for
  local declarations, and otherwise returns exact-name workspace edits across
  open and unopened workspace Omni files.
- Declaration currently follows the same local-first, exact-name workspace
  fallback path as definition.
- Implementation currently follows the same local-first, exact-name workspace
  fallback path as definition.
- Type definition currently resolves only type-like declarations (`type`,
  `abstract`, `union`, `alias`) using the same local-first and exact-name
  workspace fallback path.
- Call hierarchy stays syntactic too: preparation prefers the exact declaration
  under the cursor when the symbol is already on a declaration site, otherwise
  it falls back to same-name local or workspace declarations. Incoming and
  outgoing calls are derived from recursive scans of declaration bodies, so they
  work across open and unopened workspace files without semantic indexing.
- The server still does not attempt cross-module semantic analysis yet.

## Smoke Test

```bash
python3 tooling/omni-lsp/tests/smoke_test.py
```
