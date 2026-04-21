# FTXUI C ABI Shim Plan (Omni)

Status: `closed for generic-builder-first ABI contract`
Owner: Codex workflow
Date: 2026-03-27

## Purpose

Define a full Omni-facing declaration and integration contract for an FTXUI
backend exposed through a stable C ABI shim.

Scope of this plan:

1. map the practical FTXUI surface (`screen`, `dom`, `component`) into stable
   ABI categories,
2. define what is covered directly versus represented through generic builders,
3. record explicit deferred wrappers and open interface questions.

This plan intentionally targets the practical FTXUI surface rather than a
fragile one-to-one C++ mirror.

## Contract Decisions

1. C ABI boundary only:
   - no C++ types across the boundary,
   - no `std::function` exposure,
   - no template types in exported signatures.
2. Opaque handle ownership:
   - `context`, `screen`, `element`, `component`, `canvas`, `table`,
     `selection`, and `captured-mouse` are opaque.
3. Explicit lifecycle:
   - create/destroy pairs are required for all opaque handles.
4. Callback model:
   - all behavior hooks use `callback + void* user_data`.
5. Versioned options:
   - all options payloads contain `{abi_version, size}` header for
     forward-compatible extension.
6. Error model:
   - integer status codes plus context-local last error string.
7. String ownership:
   - inbound `const char*` fields are borrowed only for the duration of the ABI
     call,
   - the shim must eagerly copy strings into C++ owned storage whenever FTXUI
     state can retain them after the call returns,
   - Omni-side lowering may free temporary C strings immediately after a
     successful or failed call,
   - `omni_ftxui_status_name(...)` returns static storage,
   - `omni_ftxui_context_last_error_message(...)` returns context-owned storage
     valid only until the next context error mutation, context clear, or context
     destroy.
8. Coverage target:
   - the C ABI remains generic-builder-first for DOM, decorators, components,
     canvas, table, events, and screen/app lifecycle,
   - dedicated wrapper entry points are added only where a generic builder
     cannot express the lifetime, callback, or state contract without leaking
     C++ details,
   - one-function-per-upstream-overload parity is explicitly not the target.
9. Custom event payloads:
   - `OMNI_FTXUI_EVENT_CUSTOM` is payload-free in the C ABI,
   - text payloads remain limited to `CHARACTER` and `SPECIAL` events,
   - richer user payloads must flow through callback `user_data` or the
     Omni-facing effect/runtime layer, not through opaque event payload
     ownership.

## Implementation Artifacts

Implemented declaration artifact:

- [src/lisp/ftxui_ffi.c3](/home/heefoo/Documents/code/Omni/src/lisp/ftxui_ffi.c3)

Implemented native shim artifacts:

- `csrc/ftxui_shim.h`
- `csrc/ftxui_shim.cpp`

Reserved example path for this integration:

- [examples/libraries/ftxui/README.md](/home/heefoo/Documents/code/Omni/examples/libraries/ftxui/README.md)

Naming decision for the future high-level wrapper:

- [docs/plans/ui-effects-module-naming-decision-2026-03-27.md](/home/heefoo/Documents/code/Omni/docs/plans/ui-effects-module-naming-decision-2026-03-27.md)

Public wrapper direction:

- backend ABI remains FTXUI-specific and opaque-handle based,
- future Omni-facing wrapper should use dot-qualified `ui.*` names for both
  builders/helpers and module-owned effect tags.
- first runnable wrapper slice is now `examples/libraries/ftxui/ui.omni`,
  where `ui.run` lowers a limited subset of declarative nodes into the shim.

## Current Implemented Surface

The current shim covers the core `screen`, `element`, `component`, `canvas`,
and `table` families through a real C ABI. It does not yet expose literal
one-to-one parity for every FTXUI feature family.

| FTXUI family | ABI category | Coverage in `ftxui_ffi.c3` | Notes |
| --- | --- | --- | --- |
| Screen / loop (`ScreenInteractive`, `Loop`, `PostEvent`, `Exit`) | Opaque handle + lifecycle + control funcs | `omni_ftxui_screen_*`, `omni_ftxui_app_*` | Implemented |
| Events, colors, dimensions | POD option/event structs | `OmniFtxuiEventSpec`, `OmniFtxuiColor`, `OmniFtxuiDimension` | Implemented |
| DOM core (`text`, `vtext`, `paragraph`, `separator`, `spinner`, `gauge`, layout boxes, `window`, `canvas`, `table`, `graph`) | Generic element builder | `omni_ftxui_element_create` | Implemented through versioned options, including graph callbacks |
| Decorators / style chain | Generic decorator apply | `omni_ftxui_element_apply_decorator` | Implemented for current decorator enum set, including hyperlink and color-based selection decorators |
| Component core widgets and containers | Generic component builder | `omni_ftxui_component_create` | Implemented for container/widget/window/modal/collapsible/hoverable/resizable-split families currently encoded in the enum set |
| Callback wrappers | Wrapper helpers | `omni_ftxui_component_wrap_renderer`, `..._wrap_event_handler`, `..._wrap_action`, `..._handle_event`, `..._wrap_maybe` | Implemented; callback exceptions are caught locally and mapped through the context error channel; `handle_event` gives tests and runtime helpers a deterministic non-loop dispatch path |
| Static element-to-component bridge | Wrapper helper | `omni_ftxui_component_from_element` | Implemented for the first `ui.run` slice |
| Built-in quit wrapper | Wrapper helper | `omni_ftxui_component_wrap_quit_keys` | Implemented so the first live demo can exit with `q` / `Esc` without C callbacks; the wrapper retains the screen loop object until the wrapped component is destroyed |
| Table / canvas helpers | Opaque handles + render ops | `omni_ftxui_table_*`, `omni_ftxui_canvas_*` | Implemented |
| Context / error channel | Opaque context + last-error helpers | `omni_ftxui_context_*`, `omni_ftxui_status_name` | Implemented; status-returning C ABI entrypoints use fail-closed exception guards around backend work |

## First High-Level Interpreter Slice

The repo now has a first real Omni-facing interpreter slice on top of the shim:

- module: `examples/libraries/ftxui/ui.omni`
- runtime convenience: `ui.run`
- live demo: `examples/libraries/ftxui/demo.omni`

Current `ui.run` lowering coverage:

1. `text`
2. `paragraph`
3. `graph`
4. `button`
5. `input`
6. `checkbox`
7. `menu`
8. `hbox`
9. `vbox`
10. `window`

Current runner contract:

1. `ui.graph(series)` lowers through a static-series helper on the C ABI side.
   The helper rescales arbitrary numeric Omni samples into FTXUI's graph height
   at render time instead of exposing the raw callback-shaped graph ABI through
   the public `ui` surface.

## Frozen Non-Goal Wrapper List (Explicit)

These are known areas still outside the currently implemented shim surface.
They are intentionally outside the current ABI contract unless a concrete
Omni-facing surface later requires them.

1. Advanced DOM families:
   - arbitrary selection-style pixel callbacks,
   - gradient-specific decorators.
2. Advanced widget option parity:
   - animated button/menu styles,
   - richer input/menu/dropdown/radiobox option structs,
   - widget-specific change/enter callbacks.
3. Runtime helper parity:
   - active-screen access,
   - captured-mouse exposure,
   - `WithRestoredIO`,
   - selection API callbacks.
4. Fine-grained overload parity:
   - styled separators/borders beyond the current enum coverage,
   - one-function-per-overload entry points where Omni callsites benefit from them.

## Contract Coverage Roadmap

Treat "100% coverage" here as 100% of the chosen wrapper contract, not a
byte-for-byte mirror of upstream FTXUI internals.

The completion rule is simple:

1. every family named in the ABI matrix must have either a real wrapper or an
   explicit documented non-goal,
2. the public Omni facade must expose the chosen canonical surface without
   leaking backend-specific details,
3. every shipped family must have at least one focused example and regression,
4. no shipped call path may silently fall back when the contract says it should
   be supported.

Historical execution order:

1. Lock the family matrix:
   - `screen` / loop / event control,
   - DOM core elements,
   - decorators and style-chain helpers,
   - component widgets and containers,
   - canvas and table helpers,
   - runtime helpers and control hooks,
   - overload-specific convenience builders.
2. Finish backend ABI parity family by family:
   - selection-style pixel callbacks,
   - gradient decorators,
   - animated and richer widget option structs,
   - runtime helpers (`active-screen`, `captured-mouse`, `WithRestoredIO`),
   - explicit overload wrappers where the generic builder path is not enough.
3. Finish the Omni-facing surface on top of the ABI:
   - keep `ui.run` working as the convenience entrypoint,
   - complete the facade split into the canonical `ui.*` modules,
   - route backend-specific lowering through backend-owned helpers rather than
     the public facade.
4. Prove the contract:
   - add one focused smoke/regression example per shipped family,
   - add one focused ABI regression per new wrapper family,
   - keep the build and smoke gates green after every family lands.

Exit criterion for this plan:

- the chosen generic-builder-first wrapper contract is implemented and
  validated, and remaining upstream parity gaps are explicitly frozen as
  non-goals instead of being left implicit.

Current result:

- Exit criterion is met for the chosen ABI contract. Advanced upstream parity
  families are intentionally frozen as non-goals, not pending hidden work.

## Resolved Interface Questions

1. String ownership semantics:
   - decision: keep eager C++ copies for retained inbound strings everywhere;
   - no borrow/copy flags are exposed in the current ABI.
2. Coverage target:
   - decision: continue with a generic-builder-first ABI;
   - add dedicated entry points only for families where generic builders cannot
     preserve the correct lifetime, callback, or state semantics.
3. Event/custom payload design:
   - decision: keep custom events payload-free;
   - do not add explicit event payload ownership until a concrete public
     Omni-facing event contract requires it.

## Acceptance Criteria

1. `src/lisp/ftxui_ffi.c3` declares:
   - opaque handles,
   - enum constants for stable ABI categories,
   - POD structs for events/options,
   - callback aliases,
   - create/destroy and helper extern declarations.
2. Native shim implements the declared core surface incrementally while
   preserving ABI stability through options versioning.
3. Any unsupported call returns explicit status code, not silent fallback.

## Next Actions

1. Continue through the Omni-facing UI runtime, not raw upstream parity:
   `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` owns the next interactive
   read/update/render surface.
2. Reopen an ABI wrapper family only when a concrete Omni-facing feature cannot
   be expressed through the existing generic builders or callback wrappers.

## Coverage Checkpoints

### 2026-04-21 Focused ABI Regression Slice

Shipped focused ABI regressions for:

1. context creation, last-error capture/clear, screen lifecycle, animation
   request, event creation, post-event, and exit;
2. table creation, selection helpers, table render-to-element conversion;
3. canvas creation, point/line drawing, canvas render-to-element conversion;
4. widget event callback delivery through `omni_ftxui_component_handle_event`;
5. generic action callback delivery through `omni_ftxui_component_wrap_action`
   plus Return-key dispatch.

The slice added `omni_ftxui_component_handle_event(...)` as a public control
helper instead of relying on an interactive loop for callback regression
coverage. It also landed the previously documented-but-missing action callback
wrapper and tightened event dispatch so callback failures are not cleared after
`OnEvent`.

Validation:

- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- bounded `advanced-ffi-system-surface` group:
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface`
  -> `pass=102 fail=0`
- `scripts/run_ftxui_smoke.sh`

### 2026-04-21 Wrapper Family Freeze

Closed `FTXUI-C-ABI-WRAPPERS-001` by freezing the remaining upstream parity
families as explicit non-goals for the current C ABI contract:

1. arbitrary selection-style pixel callbacks,
2. gradient-specific decorators,
3. animated and richer widget option parity,
4. active-screen access,
5. captured-mouse exposure,
6. `WithRestoredIO`,
7. selection API callbacks,
8. one-function-per-upstream-overload entry points.

The implemented contract remains generic-builder-first. Dedicated wrappers are
only added when a generic builder cannot preserve lifetime, callback, or state
semantics. Added focused fail-closed regression coverage for the unsupported
piped-input screen path so the runtime-helper non-goal is observable:
`screen_create(... handle_piped_input=true ...)` and
`screen_set_handle_piped_input(... true ...)` return
`OMNI_FTXUI_STATUS_NOT_SUPPORTED`.

Validation:

- `c3c build --obj-out obj`
- bounded `advanced-ffi-system-surface` group:
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface`
  -> `pass=105 fail=0`
- `scripts/run_ftxui_smoke.sh`
- targeted `git diff --check`
- `scripts/check_file_size_gate.sh`
