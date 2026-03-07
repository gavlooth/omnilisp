# Financial-Service Webserver Plan

Status: `active`  
As of: 2026-03-07

## Goal

Use `examples/finwatch/` as the canonical end-to-end product example for
future feature work and regressions.

## Canonical Entry Point

- `examples/finwatch/main.omni`

## Scope

- Keep `examples/deduce_crud_server.omni` as legacy compatibility coverage only.
- New feature demonstrations should prefer finwatch modules and flows.
- When possible, add/adjust regression tests to mirror finwatch behavior.

## Next Steps

1. Stabilize request/response paths in `examples/finwatch/server.omni`.
2. Expand integration checks around startup, routing, and failure paths.
3. Add a thin smoke test flow that validates the canonical finwatch boot path.
4. Record each behavior-affecting change in `memory/CHANGELOG.md`.
