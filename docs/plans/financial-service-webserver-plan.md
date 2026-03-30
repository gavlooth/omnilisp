# Financial-Service Webserver Plan

Status: `active`  
As of: 2026-03-09

## Goal

Use `examples/finwatch/` as the canonical end-to-end product example for
future feature work and regressions.

## Canonical Entry Point

- `examples/finwatch/main.omni`

## Scope

- Keep `examples/deduce_crud_server.omni` as regression coverage only.
- New feature demonstrations should prefer finwatch modules and flows.
- When possible, add/adjust regression tests to mirror finwatch behavior.
- Finwatch should remain idiom-first: prefer native Omni constructs (`match`,
  `|>`, `handle/raise`, typed/union dispatch, schema validation, effectful I/O)
  over imperative patterns.

## Next Steps

- [x] Stabilize request/response paths in `examples/finwatch/server.omni`.
  - Canonicalized malformed conditional branches in routing/request handlers.
  - Introduced explicit route-symbol extraction helper for `/prices/:symbol`.
  - Renamed slash-namespaced server symbols to canonical hyphen names to avoid
    module-load symbol collisions:
    - `server/start` -> `server-start`
    - `server/start-async` -> `server-start-async`
    - `server/dispatch` -> `server-dispatch`
    - `server/handle-client` -> `server-handle-client`
    - `server/accept-loop` -> `server-accept-loop`
    - `server/poll-once!` -> `server-poll-once!`
    - `server/poll-loop` -> `server-poll-loop`
- [x] Expand integration checks around startup, routing, and failure paths.
  - Added bounded route/failure smoke script `examples/finwatch/dispatch_smoke.omni`.
  - Updated runtime HTTP groups to load bounded smoke scripts instead of calling
    nested example module symbols directly from root test context.
- [x] Add a thin smoke test flow that validates the canonical finwatch boot path.
  - Added bounded smoke script `examples/finwatch/boot_smoke.omni`.
  - Added runtime coverage in `src/lisp/tests_runtime_feature_http_groups.c3`: `finwatch canonical boot path smoke (non-blocking)` loading that script.
- [x] Record each behavior-affecting change in `memory/CHANGELOG.md`.
- [x] Keep idiom coverage explicit and up to date in `examples/finwatch/TODO.md`.
  - Added/kept executable idiom checklist entries tied to concrete modules.
