# Omni Editor Tooling

This directory holds first-party editor integrations that track the current
Omni language syntax and CLI surface.

Current packages:

- `tree-sitter-omni/` — Tree-sitter grammar and query files for highlighting.
- `omni-lsp/` — lightweight stdio language server for diagnostics, completion,
  and hover.
- `omni-nvim/` — Neovim plugin for driving a live `omni --repl` session.

Design rules:

- Reuse the installed `omni` binary where possible instead of duplicating
  parser or runtime logic in editor plugins.
- Keep editor tooling syntax aligned with `docs/LANGUAGE_SPEC.md`.
- Prefer small independent packages so editors can adopt one surface at a time.

Suggested order of adoption:

1. Tree-sitter for highlighting and textobjects.
2. LSP for diagnostics and keyword help.
3. Neovim REPL client for interactive evaluation.
