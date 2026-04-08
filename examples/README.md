# Examples

## Canonical Product Example

- `examples/finwatch/main.omni`
- Domain: financial-service webserver

Future feature demonstrations should use `finwatch` by default.

### Finwatch Frontend Dashboard

- `examples/finwatch/value_dashboard.html`
- `examples/finwatch/value_dashboard_sample.csv`
- Purpose: practical value screener UI for filtering stocks by valuation, quality,
  leverage, and margin of safety criteria.

### Finwatch TUI Stock Chart

- `examples/finwatch/yahoo_stock_tui.omni`
- Purpose: terminal-only stock chart viewer rendered with the shipped
  `ui.graph` TUI surface.

### Finwatch Tutorial Module

- `examples/finwatch/alerts.omni`
- `examples/finwatch/alerts_tutorial_smoke.omni`
- Purpose: minimal effect-handler + dispatch tutorial for price and analytics
  alerts, with a collector/log handler pair.

## Library Examples

- `examples/libraries/`
- Purpose: keep third-party integration demos separate from the canonical
  product example tree.

### FTXUI

- `examples/libraries/ftxui/`
- Purpose: dedicated subpath for TUI/library-bound FTXUI examples and smoke
  programs built on the C ABI shim.
- Example: `examples/libraries/ftxui/yahoo_stock_tui.omni`
  - Terminal-only live Yahoo Finance stock chart viewer rendered with `ui.graph`.
  - Set `OMNI_STOCK_SYMBOL` to change the ticker without editing the file.
