# Release Status

As of **2026-04-09**, Omni is in an active stabilization/refactor window.

Language surface decisions remain stable, but implementation work is still
ongoing (runtime hardening, large-file decomposition, and follow-up contract
cleanup).

## Validation Gate Status

Full global validation is **deferred** on local workstations due machine
resource limits. Final release gating must run on CI or a larger host using:

- `scripts/run_validation_container.sh` (preferred), or
- equivalent Docker-bound gate execution on a capable machine.

## Current Operating Policy

- `TODO.md` is the live queue for in-progress work; do not assume it is closed.
- `memory/CHANGELOG.md` remains the implementation truth source.
- New work and regressions should be queued directly in `TODO.md`.

## Syntax Drift Note (Report Layer)

- Report/status docs were re-audited for marker-syntax drift on 2026-04-09.
- Current report docs remain aligned with the quoted-marker rule (`'as`, `'all`);
  no bare `:as` / `:all` report-surface drift was found.
