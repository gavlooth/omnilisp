# Omni UI Reference

This page is the concise, user-facing reference for Omni's shipped FTXUI-backed
`ui` surface.

Use it when you want the current contract without reading the longer planning
docs.

## Import

```lisp
(import "ui.omni")
(import ui)
```

`ui.omni` is the public facade. It re-exports the shipped helper namespaces as
values so callers can use:

- `ui.nodes.*`
- `ui.effects.*`
- `ui.layout.*`
- `ui.style.*`
- `ui.runtime.*`
- `ui.ftxui.*`

Advanced callers may also import the dotted helper modules directly from the
example subtree:

```lisp
(import ui.nodes)
(import ui.layout)
(import ui.style)
(import ui.effects)
(import ui.runtime)
(import ui.ftxui)
```

Use selective imports from those modules when you do not also import the public
`ui` facade namespace.

## Current Surface

### Isolated Static Evaluation

`ui.evaluate` is currently isolated as an implementation helper and is not part
of the shipped `ui` facade.

The evaluator logic lives in the example subtree as an internal helper module
under `examples/libraries/ftxui/lib/ui/evaluate.omni`.

It accepts:

- quoted node/effect forms,
- explicit `list` / `List` child groups,
- array child groups (`[]`),
- already-built node/effect values.

This keeps the mostly-static tree authoring experiment available without
committing it to the public surface yet.

### Nodes

Node constructors build plain Omni data.

Available helpers:

- `ui.nodes.node`
- `ui.nodes.kind`
- `ui.nodes.props`
- `ui.nodes.children`
- `ui.nodes.text`
- `ui.nodes.paragraph`
- `ui.nodes.button`
- `ui.nodes.input`
- `ui.nodes.checkbox`
- `ui.nodes.menu`
- `ui.nodes.hbox`
- `ui.nodes.vbox`
- `ui.nodes.window`
- `ui.nodes.graph`

Example:

```lisp
(define view
  (ui.nodes.window
    "Counter"
    (ui.nodes.vbox
      (list
        (ui.nodes.text "count: 0")
        (ui.nodes.button "increment" 'increment)
        (ui.nodes.graph [0 1 1 2 3 5 8])))))
```

### Layout

Layout helpers compose node trees:

- `ui.layout.hbox`
- `ui.layout.vbox`
- `ui.layout.stack`
- `ui.layout.spacer`

Example:

```lisp
(ui.layout.hbox
  (list
    (ui.layout.spacer)
    (ui.layout.vbox
      (list
        (ui.nodes.text "layout")
        (ui.layout.stack
          (list
            (ui.nodes.text "one")
            (ui.nodes.text "two")))))
    (ui.layout.spacer)))
```

### Style

Style helpers are backend-agnostic node wrappers:

- `ui.style.border`
- `ui.style.frame`
- `ui.style.flex`
- `ui.style.width`
- `ui.style.height`

Example:

```lisp
(ui.style.border
  (ui.style.width
    40
    (ui.style.frame
      (ui.style.flex
        (ui.nodes.text "styled")))))
```

### Effects

Effects are tree-shaped declarative data, not imperative calls.

Available constructors:

- `ui.effects.effect_node`
- `ui.effects.effect_tag`
- `ui.effects.effect_attrs`
- `ui.effects.effect_children`
- `ui.effects.open_tree`
- `ui.effects.render_tree`
- `ui.effects.read_event_tree`
- `ui.effects.invalidate_tree`
- `ui.effects.close_tree`
- `ui.effects.post_event_tree`

Available effect tags:

- `ui.effects.open`
- `ui.effects.render`
- `ui.effects.read_event`
- `ui.effects.invalidate`
- `ui.effects.close`
- `ui.effects.post_event`

Example:

```lisp
(define effect-tree
  (ui.effects.open_tree {'backend 'ftxui 'title "Counter"}
    (ui.effects.render_tree {'node view})
    (ui.effects.close_tree {'code 'ok})))
```

Effect trees are preserved as symbol-tagged Omni data and then normalized by
`ui.runtime` before they are bridged into Omni's existing `signal` machinery or
handed to a concrete backend.

### Runtime

Runtime helpers interpret normalized effect trees:

- `ui.runtime.dispatch`
- `ui.runtime.dispatch_to`
- `ui.runtime.dispatch_one`
- `ui.runtime.dispatch_children`
- `ui.runtime.loop_to`
- `ui.runtime.open_session_to`
- `ui.runtime.update_session_to`
- `ui.runtime.render_session_to`
- `ui.runtime.read_event_session_to`
- `ui.runtime.invalidate_session_to`
- `ui.runtime.post_event_session_to`
- `ui.runtime.close_session_to`

Example:

```lisp
(ui.runtime.dispatch effect-tree)
```

### Backend

The backend bridge is shipped as:

- `ui.ftxui.run`
- `ui.ftxui.dispatch`
- `ui.ftxui.loop`
- `ui.ftxui.open_session`
- `ui.ftxui.update_session`
- `ui.ftxui.render_session`
- `ui.ftxui.read_event_session`
- `ui.ftxui.invalidate_session`
- `ui.ftxui.post_event_session`
- `ui.ftxui.close_session`

`ui.run` is the public node-tree convenience entrypoint and delegates through
the backend bridge. `ui.loop` is the public effect-tree convenience entrypoint
for the blocking FTXUI loop.

`ui.ftxui.dispatch` consumes a declarative effect tree through
`ui.runtime.dispatch_to` and executes a non-interactive FTXUI lifecycle:

- root `open_tree` with `{'backend 'ftxui}`,
- one or more `render_tree` children,
- optional `invalidate_tree`,
- optional `post_event_tree`,
- optional `close_tree`.

`read_event_tree` is intentionally not supported by this non-interactive
dispatcher. Incremental UI state is handled through explicit session helpers.

`ui.ftxui.loop` consumes a root `open_tree` with `{'backend 'ftxui}` and exactly
one `render_tree` child, lowers it into a real FTXUI app, applies optional
`invalidate_tree` / `post_event_tree` / `close_tree` effects, and then enters the
blocking app loop. `close_tree` requests a pre-loop close and returns without
blocking. `read_event_tree` remains fail-closed in this one-shot blocking helper;
session-owned state is available through the explicit session API.

Session helpers own one `ui-ftxui-session` foreign handle that wraps the FTXUI
custom loop lifecycle. `ui.open_session` consumes a root `open_tree` with
backend `ftxui` and exactly one `render_tree`. `ui.update_session` applies
incremental `invalidate_tree`, `post_event_tree`, and `close_tree` effects.
`ui.render_session` runs one non-blocking loop iteration and returns `true`
while the session remains live. `ui.close_session` releases the handle and is
idempotent. `ui.read_event_session` first drains already captured events in FIFO
order, then runs one blocking loop step when the session queue is empty. It
returns `nil` when no event was captured, or a dictionary with `kind` and
optional `text` fields for the captured event. Character events use
`{'kind 'character 'text "..."}`; known special keys use symbolic kinds such as
`'arrow-left`, `'return`, or `'escape`; FTXUI payload-free custom events use
`{'kind 'custom}`. Activating a high-level `ui.nodes.button` posts the button's
stored message as a special event with the message text in `text`.

Example:

```lisp
(ui.run view)
(ui.ftxui.dispatch effect-tree)
(ui.loop effect-tree)

(define session (ui.open_session effect-tree))
(ui.update_session session update-tree)
(ui.render_session session)
(ui.post_event_session session {'kind 'character 'text "x"})
(ui.read_event_session session)
(ui.close_session session)
```

## Shipped Example Surface

The dedicated library example subtree lives under:

- `examples/libraries/ftxui/`

Useful examples:

- `smoke.omni` - node, layout, style, effect-shape, and dispatch smoke
- `module_value_smoke.omni` - public namespace and dot-access smoke
- `module_effect_smoke.omni` - module-owned effect smoke
- `module_direct_smoke.omni` - direct dotted module import smoke
- `module_backend_smoke.omni` - non-interactive backend lifecycle smoke
- `module_session_smoke.omni` - session-owned update/render lifecycle smoke
- `demo.omni` - live FTXUI-backed runner demo

Validation gate:

- `scripts/run_ftxui_smoke.sh`

## Notes

- Omni already reserves `signal` for its own effect system, so the UI layer
  does not introduce a competing `signal` boundary.
- The public surface is constructor-first and tree-shaped.
- The long-form design rationale lives in
  `docs/plans/ui-library-facade-plan-2026-03-27.md`.
