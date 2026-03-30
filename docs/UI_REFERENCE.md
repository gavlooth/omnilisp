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
- `ui.nodes.stack`
- `ui.nodes.spacer`
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
`ui.runtime` before they are bridged into Omni's existing `signal` machinery.

### Runtime

Runtime helpers interpret normalized effect trees:

- `ui.runtime.dispatch`
- `ui.runtime.dispatch_one`
- `ui.runtime.dispatch_children`

Example:

```lisp
(ui.runtime.dispatch effect-tree)
```

### Backend

The backend bridge is shipped as:

- `ui.ftxui.run`

`ui.run` is the public convenience entrypoint and delegates through the backend
bridge.

Example:

```lisp
(ui.run view)
```

## Shipped Example Surface

The dedicated library example subtree lives under:

- `examples/libraries/ftxui/`

Useful examples:

- `smoke.omni` - node, layout, style, effect-shape, and dispatch smoke
- `module_value_smoke.omni` - public namespace and dot-access smoke
- `module_effect_smoke.omni` - module-owned effect smoke
- `demo.omni` - live FTXUI-backed runner demo

Validation gate:

- `scripts/run_ftxui_smoke.sh`

## Notes

- Omni already reserves `signal` for its own effect system, so the UI layer
  does not introduce a competing `signal` boundary.
- The public surface is constructor-first and tree-shaped.
- The long-form design rationale lives in
  `docs/plans/ui-library-facade-plan-2026-03-27.md`.
