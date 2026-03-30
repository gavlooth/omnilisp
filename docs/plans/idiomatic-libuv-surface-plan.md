# Idiomatic Omni Surface Plan (Starting From libuv Primitives)

Owner intent:
- Keep libuv integration as the runtime substrate.
- Expose that substrate through Omni-native language features first:
  - typed/dispatched `define` forms,
  - `[effect]` boundaries,
  - thin wrappers (not large wrapper layers).
- Avoid adding ad-hoc "another Lisp" API style when existing Omni features can express the same surface.

Execution path (single sequence):

1. Thin typed wrappers over libuv-facing stdlib API
- [x] Convert core libuv wrappers from untyped lambda aliases to typed `define` wrappers (`tcp-*`, `udp-*`, `pipe-*`, `process-*`, `signal-*`, `dns-resolve`, `async-sleep`, `offload`, `task-*`, `thread-*`, `tls-*`, `http-*`).
  - Status: variadic wrapper fronts are now typed `define` forms with explicit untyped fallback methods to preserve canonical runtime payload errors on invalid inputs.
- [x] Add focused stdlib/runtime smoke tests validating typed wrapper call paths for representative primitives (`tcp-connect`, `task-spawn`, `thread-spawn`, `http-request`).
- Exit condition: libuv-facing public surface is idiomatic typed `define` wrappers, not lambda alias glue.

2. Public docs parity for libuv surface
- [x] Update stdlib appendix to include complete wrapper list (`thread-*`, `http-request`, `tcp-listen`, `tcp-accept`, etc.).
- [x] Update primitive appendix to include all registered raw libuv-backed primitives (`__raw-offload`, `__raw-thread-*`, `__raw-task-*`, `__raw-http-*`).
- [x] Add canonical "layering contract" note: operation-level effects/raw primitives stay canonical; unified facades must remain thin delegators.
- Exit condition: docs match runtime/public syntax exactly.

3. Dispatch-first convenience layer (thin)
- [x] Add narrow dispatch helpers that compose existing wrappers (no new substrate):
  - spawn/join/cancel helpers for pooled jobs vs OS threads,
  - request/stream convenience wrappers for network paths.
- [x] Keep helpers as syntax-level composition only; no duplicate runtime execution logic.
- Exit condition: common flows are expressed through dispatched Omni helpers over existing wrappers.

4. Data-format surface normalization with Omni idioms
- [x] Fix semantic drifts identified in audit before expanding API:
  - TOML boolean false must remain Omni `false` (not `nil`),
  - RFC-4180 strict behavior alignment for CSV strict/default behaviors.
- [x] After semantic fixes, expose thin dispatched format helpers (`parse`/`emit` style) backed by current primitives.
- Exit condition: correctness first, then idiomatic wrappers.

5. Guardrails and CI enforcement
- [x] Add lint/policy checks ensuring new public libuv APIs are introduced via typed wrapper + docs parity.
- [x] Extend parity scripts to fail when raw primitive additions are undocumented or missing wrapper mapping.
- Exit condition: process prevents drift back to ad-hoc surfaces.
