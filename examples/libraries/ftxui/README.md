# FTXUI Examples

This directory is the dedicated example path for Omni's FTXUI integration.

User-facing reference:

- [docs/UI_REFERENCE.md](../../../docs/UI_REFERENCE.md)

Use it for:

- minimal FTXUI smoke programs,
- focused widget/layout examples,
- event-loop and callback demos,
- future parity/regression examples for the FTXUI C ABI shim.

Do not place these examples under `examples/finwatch/`. The product example and
the library integration examples serve different purposes and should remain
separate.

Current contents:

- `ui.omni` — canonical public `ui.*` facade with the first `ui.run`
  node-tree convenience wrapper and `ui.loop` effect-tree loop wrapper over the
  FTXUI backend.
- `lib/ui/nodes.omni` — internal node/data helper module used by the public
  `ui` facade.
- `lib/ui/effects.omni` — internal effect helper module used by the public
  `ui` facade.
- `lib/ui/layout.omni` — internal layout helper module used by the public
  `ui` facade.
- `lib/ui/style.omni` — internal style helper module used by the public
  `ui` facade.
- `lib/ui/runtime.omni` — internal runtime dispatcher used by the public
  `ui` facade.
- `lib/ui/ftxui.omni` — concrete FTXUI backend bridge used by the public
  `ui` facade.
- `smoke.omni` — naming and effect-shape smoke using `ui.text`, `ui.window`,
  and module-qualified `ui.open` / `ui.render` effect tags.
- `module_value_smoke.omni` — verifies real module import plus `ui.*` member
  access.
- `module_effect_smoke.omni` — verifies real module-owned effects resolve
  through `signal ui.open` and `handle (ui.open ...)`.
- `module_direct_smoke.omni` — verifies default dotted module imports for
  `ui.nodes`, `ui.layout`, `ui.style`, `ui.effects`, `ui.runtime`, and
  `ui.ftxui`.
- `module_backend_smoke.omni` — verifies `ui.runtime.dispatch_to` ->
  `ui.ftxui.dispatch` non-interactive backend lifecycle execution plus
  fail-closed unsupported effect shapes.
- `module_interactive_loop_smoke.omni` — verifies `ui.loop` ->
  `ui.ftxui.loop` blocking app-loop execution plus explicit fail-closed
  unsupported effect shapes.
- `demo.omni` — first live FTXUI-backed runner demo. Press `q` or `Esc` to
  exit.
- `yahoo_stock_tui.omni` — terminal-only live Yahoo Finance stock chart viewer rendered with `ui.graph`
  - Override the ticker with `OMNI_STOCK_SYMBOL` (for example `OMNI_STOCK_SYMBOL=MSFT`)

Current live runner coverage:

- `ui.text`
- `ui.paragraph`
- `ui.graph`
- `ui.button`
- `ui.input`
- `ui.checkbox`
- `ui.menu`
- `ui.hbox`
- `ui.vbox`
- `ui.stack`
- `ui.spacer`
- `ui.window`
- `ui.border`
- `ui.frame`
- `ui.flex`
- `ui.width`
- `ui.height`
- `ui.loop`

Next likely additions:

- session-owned read/update/render loop support on top of the blocking
  effect-tree loop,
- richer widget/container composition examples,
- selection-style demos and richer decorator coverage,
- focused smoke coverage for ABI additions.
