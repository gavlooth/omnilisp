# UI Library Facade Plan (2026-03-27)

Status: `completed-facade-backend-session` (raw event-payload reads remain a separate backend ABI question)
Owner: Codex workflow

## Purpose

Define the next implementation plan for the high-level UI library only.

This plan is intentionally narrower than the FTXUI shim plan:

- it covers the public `ui` library surface,
- it covers internal `ui.*` module organization,
- it covers library runtime/effect structure,
- it does not treat raw FTXUI ABI parity as the main planning unit.

## Scope

In scope:

1. the canonical public `ui` facade,
2. internal `ui` submodule structure,
3. high-level node/layout/style/effect/runtime APIs,
4. the immediate migration path from the current `examples/libraries/ftxui/ui.omni`
   scaffold into a real library shape,
5. focused example and regression coverage for the library surface.

Out of scope:

1. literal one-to-one FTXUI parity planning,
2. raw C ABI surface expansion unless the library surface needs it,
3. generic language/runtime work unrelated to the UI library,
4. non-TUI backends beyond keeping the public surface backend-agnostic.

## Current Library Checkpoint

The repo already has a first working high-level surface under:

- `examples/libraries/ftxui/ui.omni`

and a first live backend path:

- `ui.run`

Current shipped high-level surface already includes:

- node constructors such as `ui.text`, `ui.paragraph`, `ui.graph`, `ui.button`,
  `ui.input`, `ui.checkbox`, `ui.menu`, `ui.hbox`, `ui.vbox`, `ui.window`
- module-owned declarative effect forms such as `ui.open`, `ui.render`,
  `ui.read_event`, `ui.invalidate`, `ui.close`, `ui.post_event`
- the direct convenience runner `ui.run`

The facade split is now landed:

- `examples/libraries/ftxui/ui.omni` is the canonical public facade
- `examples/libraries/ftxui/lib/ui/nodes.omni` holds node/data helpers
- `examples/libraries/ftxui/lib/ui/effects.omni` holds effect declarations
- `examples/libraries/ftxui/lib/ui/layout.omni` holds layout helpers
- `examples/libraries/ftxui/lib/ui/style.omni` holds style helpers
- `examples/libraries/ftxui/lib/ui/runtime.omni` holds the runtime dispatcher
- `examples/libraries/ftxui/lib/ui/ftxui.omni` holds the concrete backend
  bridge

Current checkpoint:

1. the public facade loads the dotted `lib/ui/*` modules as its implementation
   source;
2. direct default-path dotted imports such as `(import ui.nodes)` are covered
   by `module_direct_smoke.omni`;
3. layout/style helpers and backend-owned `ui.ftxui.run` are split from the
   facade;
4. `ui.runtime` owns dispatch only, while static tree evaluation is isolated in
   `lib/ui/evaluate.omni`;
5. the non-interactive runtime-backend lifecycle path is shipped through
   `ui.ftxui.dispatch`;
6. the blocking effect-tree loop is shipped through `ui.ftxui.loop`;
7. session-owned update/render state is shipped through `ui.open_session`,
   `ui.update_session`, `ui.render_session`, `ui.invalidate_session`,
   `ui.post_event_session`, and `ui.close_session`; raw event-payload reads
   remain fail-closed until the backend exposes event values as Omni data.

## 100% Facade Completion

Treat "100% coverage" for the library facade as full coverage of the chosen
Omni public surface, not full upstream FTXUI API parity.

The completion rule is:

1. the public `ui` facade exports the canonical surface only,
2. the internal implementation is split into real module-owned layers,
3. the backend lowering logic lives behind a backend module,
4. every public family has a focused example and regression,
5. any non-goal remains explicitly documented instead of appearing as an
implicit hole in the facade.

The final facade shape should be:

```text
ui
ui.nodes
ui.effects
ui.layout
ui.style
ui.runtime
ui.ftxui
```

Coverage sequence:

1. complete the submodule import story so the helper split becomes real module
   ownership,
2. finish `ui.layout` and `ui.style` so public demos can be shaped without
   backend-specific code,
3. extract the backend lowering path into `ui.ftxui`,
4. make the effect-driven runtime path the normal way to open/render/close,
5. add examples and smoke tests that exercise each public module boundary.

Exit criterion for the facade plan:

- the public facade is stable, backend-agnostic, and fully exercised by
  examples/tests for every shipped public `ui.*` family.

## Canonical Module Structure

Use `ui` as the facade module and keep the rest as implementation-oriented
submodules:

```text
ui
ui.nodes
ui.effects
ui.layout
ui.style
ui.runtime
ui.ftxui
ui.ftxui_ffi
```

Recommended roles:

### `ui`

Canonical user-facing import and re-export surface.

It should own the stable public contract:

- constructors like `ui.text`, `ui.graph`, `ui.window`
- layout helpers like `ui.hbox`, `ui.vbox`
- style helpers like `ui.border`, `ui.height`, `ui.flex`
- effects like `ui.open`, `ui.render`, `ui.close`
- convenience runner(s) like `ui.run`

### `ui.nodes`

Owns declarative node/data constructors and structural helpers.

Examples:

- `node`
- `kind`
- `props`
- `children`
- `text`
- `paragraph`
- `graph`
- `button`
- `input`
- `checkbox`
- `menu`
- `window`

### `ui.effects`

Owns library effects only.

Examples:

- `open`
- `render`
- `read_event`
- `invalidate`
- `close`
- `post_event`

Initial shape:

- effects are tree-shaped declarative forms, not imperative helpers,
- every effect node has a stable symbol tag, an attribute map, and optional
  child effect nodes,
- normalization happens before backend dispatch so the backend only sees a
  small canonical effect vocabulary,
- effect trees compose the same way node trees do: data in, dispatch out.

Construction model:

- public effect trees should be built with ordinary Omni constructors first,
- macros may exist later as optional sugar over the same constructor surface,
- the canonical effect representation remains explicit tree data regardless of
  how a caller authored it.

Proposed grammar:

```lisp
[open {'target 'app
       'mode 'modal}
  [render {'node ui-tree}]
  [close {}]]
```

Grammar rules:

1. the first item is the effect tag symbol,
2. the second item is the attribute map,
3. remaining items are child effect nodes,
4. leaf effects may omit children entirely,
5. normalization may rewrite aliases or lower shorthand, but it must preserve
   the tree shape and the semantic tag.

Dispatch semantics:

1. `ui.runtime` walks the effect tree depth-first,
2. each normalized node is dispatched by tag,
3. parent nodes may establish scope or lifecycle state for child nodes,
4. the backend sees only canonical effect tags and normalized payloads,
5. unsupported effect tags fail explicitly instead of silently falling back.

Formal grammar:

```lisp
effect-tree ::= [tag attrs child*]
tag ::= symbol
attrs ::= dict
child ::= effect-tree
```

Grammar notes:

1. `tag` is preserved as a symbol through normalization and dispatch.
2. `attrs` is a plain Omni dict; payload symbols remain symbols.
3. `child` entries are recursively nested effect trees.
4. ordinary node trees stay separate from effect trees unless the runtime
   explicitly lowers one into the other.
5. the public UI model must not introduce a new `signal` boundary because
   Omni already reserves `signal` for its own effect system.

Normalization contract:

1. rewrite aliases to canonical tags if aliases exist,
2. fill in omitted default attrs where the tag requires them,
3. reject malformed trees before backend dispatch,
4. preserve tag symbols, tree shape, and payload symbol values.

Dispatch boundary:

1. `ui.runtime` is the entrypoint that receives the effect tree,
2. it normalizes tree-shaped UI effects into the canonical runtime form,
3. it lowers or bridges into Omni's existing `signal` effect machinery,
4. `ui.ftxui` consumes the lowered effect shape to reach the backend,
5. UI code does not invent a second competing effect keyword.

Dispatch error contract:

1. unknown tags return an explicit unsupported-tag failure,
2. malformed trees return a structural validation failure,
3. backend failures propagate through the runtime instead of being masked.

### `ui.layout`

Owns layout/composition helpers.

Initial target:

- `hbox`
- `vbox`
- `stack`
- `spacer`

### `ui.style`

Owns high-level styling and sizing wrappers.

Initial target:

- `border`
- `frame`
- `bold`
- `dim`
- `color`
- `bgcolor`
- `flex`
- `width`
- `height`

This is the next important public-surface gap because current live elements,
including `ui.graph`, have minimal high-level layout/styling control.

### `ui.runtime`

Owns backend-agnostic runtime orchestration.

Initial target:

- `run`
- thin helpers around effect-driven render/update loops
- backend handoff helpers for session-owned lifecycle operations:
  `open_session_to`, `update_session_to`, `render_session_to`,
  `read_event_session_to`, `invalidate_session_to`, `post_event_session_to`,
  and `close_session_to`

Longer term:

- model/view/update helpers over the explicit session handle if adopted,
- backend selection glue.

### `ui.ftxui`

Owns the concrete FTXUI backend implementation.

Examples:

- node lowering,
- screen lifecycle,
- effect interpretation,
- backend-specific convenience seams.

This module is implementation-facing, not the canonical public import.

### `ui.ftxui_ffi`

Owns raw FTXUI foreign declarations only if they need to live under the `ui`
tree.

This is not a normal user-facing module.

## Public Export Policy

Use three visibility tiers:

1. Canonical public:
   - `ui.*`
2. Advanced but still human-usable:
   - `ui.nodes.*`
   - `ui.effects.*`
   - `ui.layout.*`
   - `ui.style.*`
   - `ui.runtime.*`
3. Backend/internal:
   - `ui.ftxui.*`
   - `ui.ftxui_ffi.*`

Normal user code should usually need only:

```lisp
(import ui)
```

## Staged Implementation Plan

### Slice 1: Facade Split

Goal:

- split the current single-file scaffold into the real `ui` facade plus
  separated node/effect helper modules without changing the public surface
  contract.

Deliverables:

1. `ui` facade module with canonical re-exports,
2. separated node helper module holding node constructors,
3. separated effect helper module holding effect declarations,
4. existing examples updated to import `ui` as the canonical entrypoint.

Acceptance boundary:

- existing `ui.*` usage still works,
- examples keep running,
- no raw backend names leak into the public module.

Current status:

- shipped as `ui` facade + dotted `lib/ui/*` helper modules;
- direct dotted module import coverage is provided by
  `examples/libraries/ftxui/module_direct_smoke.omni`.

### Slice 2: Layout and Style Surface

Goal:

- add the first real layout/style library surface so the high-level API is
  usable beyond plain widget creation.

Deliverables:

1. `ui.layout` with `hbox`, `vbox`, `stack`, `spacer`,
2. `ui.style` with `border`, `frame`, `flex`, `width`, `height`,
3. at least one example proving graph/window layout can be shaped through the
   high-level library rather than backend-specific code.

Acceptance boundary:

- graph and window demos can control size/layout from `ui.*`,
- style/layout remains backend-agnostic at the library surface.

### Slice 3: Backend Module Extraction

Goal:

- move backend lowering out of the facade/scaffold lane into `ui.ftxui`.

Deliverables:

1. `ui.ftxui` module owning the current runner/lowering path,
2. `ui.run` delegating through that backend module,
3. backend-only helpers removed from the public facade.

Acceptance boundary:

- canonical public usage stays `ui.run`,
- backend logic is no longer mixed into the public surface definition.

### Slice 4: Declarative Effect Forms and Dispatcher

Goal:

- make the library’s runtime story center on declarative Hiccup-style effect
  forms plus a dispatcher, while reusing Omni's existing `signal` effect
  machinery underneath instead of inventing a second effect boundary.

Deliverables:

1. effect vocabulary defined in `ui.effects` as pure data forms,
2. dispatcher/interpreter path in `ui.runtime` that normalizes and executes
   those forms,
3. bridge/lowering in `ui.ftxui` that consumes the dispatched effect shapes
   via Omni's existing `signal` machinery,
4. a focused example using the declarative effect path through the real
   backend,
5. targeted tests for effect normalization, dispatch, and backend execution on
   the UI library path.

Acceptance boundary:

- the declarative effect path is real and documented,
- `ui.run` may stay as a convenience wrapper, but it is no longer the only
  practical runtime entrypoint,
- runtime helpers are consumed through the dispatcher instead of exposed as the
  primary public model,
- the public UI surface does not add a competing `signal` keyword or syntax
  form.

### Slice 5: Library-Focused Examples and Regressions

Goal:

- make the library surface itself testable and demonstrable independently of
  raw backend expansion.

Deliverables:

1. library-focused examples under `examples/libraries/ftxui/`,
2. focused smoke/regression coverage for:
   - facade imports,
   - style/layout helpers,
   - effect-driven runtime usage,
   - public `ui.*` graph/window shaping.

Acceptance boundary:

- the library can be evolved without relying only on manual REPL/demo checks.

## Explicit Non-Goals for This Plan

Do not let this plan drift back into raw backend parity work.

The following stay outside the library-only queue unless they directly block
the public `ui` library:

1. arbitrary `selectionStyle(Pixel&)` callback parity,
2. gradient decorator parity,
3. every FTXUI widget-option overload,
4. captured-mouse and other backend-runtime helper parity,
5. non-TUI backend implementation work.

Those belong in backend-specific plans unless the missing backend support blocks
an explicitly chosen public library feature.

## Next Recommended Execution Order

Completed:

1. Facade split: `ui`, `ui.nodes`, `ui.effects`
2. `ui.style` and `ui.layout` first-class surface
3. Extract backend logic into `ui.ftxui`
4. Add library-focused examples/tests around the public module contract

Completed backend lifecycle item:

1. `UI-LIB-RUNTIME-BACKEND-001`: declarative `open_tree` / `render_tree` /
   `invalidate_tree` / `post_event_tree` / `close_tree` now drive a real
   non-interactive FTXUI backend lifecycle through `ui.ftxui.dispatch`.

Completed blocking loop item:

1. `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`: `ui.loop` / `ui.ftxui.loop` now run
   a root `open_tree` with exactly one `render_tree` through the real FTXUI app
   loop, route through `ui.runtime.loop_to`, apply `invalidate_tree`,
   `post_event_tree`, and explicit pre-loop `close_tree`, and keep
   `read_event_tree` fail-closed in the one-shot blocking helper.

Completed session item:

1. `UI-LIB-RUNTIME-SESSION-001`: `ui.open_session` / `ui.ftxui.open_session`
   now create an owned `ui-ftxui-session` `ForeignHandle` over the FTXUI custom
   loop ABI; `ui.update_session`, `ui.render_session`, `ui.invalidate_session`,
   `ui.post_event_session`, `ui.read_event_session`, and `ui.close_session`
   expose the external update/render/read lifecycle without reusing the
   one-shot `ui.loop` contract.

Completed session event item:

1. `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001`: session event payload reads now
   use an ABI-level capture/read contract. The FTXUI root component is wrapped
   with `CatchEvent`, the shim stores the last captured event payload, and
   `ui.read_event_session` runs one blocking loop step before returning `nil`
   or an Omni dictionary with `kind` and optional `text`. This keeps the
   explicit session contract separate from the one-shot blocking `ui.loop`
   helper and avoids treating `RunOnceBlocking` itself as an event value.
