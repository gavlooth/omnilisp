# Editor Tooling Roadmap

Purpose: track the concrete follow-up work needed to turn Omni's new editor
tooling scaffold into a stable first-party developer experience.

Status: active, non-release-blocking tooling roadmap.

## Execution Rules

1. Prefer machine-readable Omni CLI surfaces over editor-specific parsing hacks.
2. Keep Tree-sitter, LSP, and editor plugin behavior aligned with
   `docs/LANGUAGE_SPEC.md`.
3. Land stable protocol/CLI surfaces before adding semantic editor features that
   depend on them.
4. Validate new tooling paths with lightweight targeted checks before expanding
   scope.

## Priority Order

1. Machine-readable CLI check/eval surfaces
2. Tree-sitter hardening + packaging
3. LSP upgrade onto structured diagnostics
4. Neovim REPL/plugin upgrade onto structured transport
5. Formatter/indentation support
6. Test/doc/bootstrap integrations

## Work Queue

### 1. Structured CLI Diagnostics

- [ ] Add `omni --check <file>` as a parse/analysis-only path that does not
      execute user code.
- [ ] Add `omni --check --json <file>` output with:
  - [ ] exact file URI/path,
  - [ ] 0-based or clearly documented line/column ranges,
  - [ ] stable diagnostic code,
  - [ ] severity,
  - [ ] human-readable message.
- [ ] Ensure `--check --json` returns non-zero on diagnostics and zero on clean
      input.
- [ ] Add regression coverage for:
  - [ ] parser syntax errors,
  - [ ] compile/lowering errors,
  - [ ] malformed module/import inputs,
  - [ ] deterministic output schema.
- [ ] Document the contract in `docs/PROJECT_TOOLING.md` and `docs/man/omni.1`.

Exit criteria:
- editor tooling no longer needs to regex human compile output for diagnostics.

### 2. Structured REPL / Eval Transport

- [ ] Add a machine-readable eval surface for tools:
  - [ ] either `omni --repl --json`,
  - [ ] or a dedicated `omni --eval --json`.
- [ ] Return structured success/error payloads for each submitted form:
  - [ ] value rendering,
  - [ ] error code/message,
  - [ ] source range when available,
  - [ ] completion marker per request.
- [ ] Preserve interactive human REPL behavior as the default path.
- [ ] Add request/response correlation so editor clients can safely send more
      than one eval without ambiguous output parsing.
- [ ] Document transport guarantees and failure modes.

Exit criteria:
- Neovim/plugin integrations can stop scraping ANSI REPL text for control flow.

### 3. Tree-sitter Hardening

- [ ] Expand grammar coverage tests for:
  - [ ] type annotations,
  - [ ] macro/template forms,
  - [ ] regex literals,
  - [ ] path/index chaining,
  - [ ] effect handlers and continuations,
  - [ ] module/import/export forms,
  - [ ] destructuring-heavy `let`/`match` inputs.
- [ ] Add corpus fixtures for currently accepted syntax and rejected legacy
      syntax.
- [ ] Tighten highlight queries for current canonical forms only.
- [ ] Add indent/folds/textobjects queries if the grammar shape is stable enough.
- [ ] Package Omni grammar for straightforward `nvim-treesitter` consumption.
- [ ] Add a small verification script or documented command set for grammar
      regeneration and query checks.

Exit criteria:
- Tree-sitter grammar is treated as a maintained compatibility surface, not only
  a scaffold.

### 4. LSP Baseline Upgrade

- [ ] Switch `tooling/omni-lsp` diagnostics from compile-output regex parsing to
      `omni --check --json`.
- [ ] Add document symbols for:
  - [ ] top-level `define`,
  - [ ] type/effect/relation/module declarations,
  - [ ] method-table style declarations where applicable.
- [ ] Add completion sources for:
  - [ ] special forms,
  - [ ] builtin constructors,
  - [ ] current buffer top-level symbols,
  - [ ] module import candidates if cheap enough.
- [ ] Add hover surfaces backed by first-party docs/CLI symbol description.
- [ ] Add go-to-definition for local/top-level buffer symbols before attempting
      cross-module resolution.
- [ ] Add lightweight integration tests for initialize/open/change/hover/complete.

Exit criteria:
- Omni LSP is editor-usable without depending on fragile text scraping.

### 5. Neovim Plugin Upgrade

- [ ] Keep `tooling/omni-nvim` wired to the structured Omni eval transport once
      available.
- [ ] Replace delimiter-scanning current-form selection with Tree-sitter-backed
      node/form selection when the grammar stabilizes.
- [ ] Add operator/motion support for eval-current-form / eval-root-form /
      eval-selection workflows.
- [ ] Add transcript window options for split direction, auto-scroll, and clear.
- [ ] Add basic result annotations or virtual text for single-form evals if the
      transport is stable enough.
- [ ] Add local docs/help tags or a concise `:help omni.nvim` path.

Exit criteria:
- Omni Neovim workflow reaches a practical Conjure-like baseline for daily use.

### 6. Formatting and Indentation

- [ ] Decide whether Omni gets:
  - [ ] full formatter,
  - [ ] indentation engine first,
  - [ ] or both in phases.
- [ ] Define canonical formatting rules for:
  - [ ] special forms,
  - [ ] vector/dict literals,
  - [ ] `handle` clauses,
  - [ ] type/method declarations,
  - [ ] macro/template blocks.
- [ ] Expose a CLI entrypoint (`omni --fmt` or equivalent) before editor-only
      integrations.
- [ ] Add range formatting support if the formatter architecture allows it.
- [ ] Integrate with Neovim formatter tooling once the CLI contract is stable.

Exit criteria:
- developers have one canonical formatting path instead of editor-local heuristics.

### 7. Test, Docs, and Bootstrap Integration

- [ ] Add `--describe <symbol>` or equivalent machine-readable symbol help if
      hover/doc lookup needs a first-party source.
- [ ] Add `--test-suite --json` or equivalent structured test output for editor
      test runners.
- [ ] Add project/bootstrap docs showing how to wire:
  - [ ] `tooling/tree-sitter-omni`,
  - [ ] `tooling/omni-lsp`,
  - [ ] `tooling/omni-nvim`.
- [ ] Update `--init` templates or docs with recommended editor/tooling setup.
- [ ] Add a small example editor integration section to the main README once the
      contracts stabilize.

Exit criteria:
- Omni tooling is discoverable and reproducible for new contributors.

## Recommended Immediate Sequence

- [ ] Implement `omni --check --json`.
- [ ] Implement structured eval transport (`--repl --json` or `--eval --json`).
- [ ] Rebase `tooling/omni-lsp` onto the structured CLI contracts.
- [ ] Harden Tree-sitter corpus/query coverage.
- [ ] Upgrade `tooling/omni-nvim` to use structured eval + Tree-sitter form
      selection.
- [ ] Start formatter/indentation implementation.
