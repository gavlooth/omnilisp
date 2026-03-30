# Library Examples

This subtree is reserved for examples that are centered on a specific external
library or backend integration rather than the canonical Omni product example.

Current library lanes:

- `ftxui/` for terminal UI examples using the FTXUI C ABI shim.

Rules:

- Keep library examples self-contained.
- Do not mix library-specific demos into `examples/finwatch/`.
- Prefer one subdirectory per library/backend so future integrations can land
  without flattening the examples tree.
