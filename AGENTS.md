# Omni Lisp - Agent Guide

This file is the working guide for coding agents in this repository.
Keep changes aligned with Omni's design and current codebase reality.

## Read First

Before modifying code, read:

1. `docs/LANGUAGE_SPEC.md`
2. `docs/C3_STYLE.md` (mandatory for C3 changes)

`docs/C3_STYLE.md` is the authoritative C3 coding standard for this repo.
It is derived from official C3 language documentation (starting from
`https://c3-lang.org/language-overview/examples/`), and agents should treat it
as the source of truth for feature usage and coding conventions.

For specialized work, also read:

- Docs map (normalized entrypoint): `docs/README.md`
- Plan index and active work queue: `docs/plans/README.md`
- Area status hub + next steps: `docs/areas/README.md`
- Type and dispatch work: `docs/type-system-syntax.md`
- Effects and handlers: `docs/EFFECTS_GUIDE.md`
- Compiler/parser refactor status: `docs/areas/compiler-parser-refactor.md`
- Canonical product example (financial-service webserver): `examples/finwatch/main.omni`
- Memory architecture and status:
  - `memory/CHANGELOG.md` (**primary implementation truth**; if docs disagree, changelog wins for current state)
  - `memory/DESTINATION_ARENA_PLAN.md` (target architecture and design rationale; closure markers must match changelog-backed validation)

## Core Language/Runtime Invariants

When implementing or reviewing features, preserve these constraints:

- Three collection types: list, array, dict
- Do not introduce "vector" terminology or type-specific naming when a generic operation is intended
- Prefer generic operations and dispatch-based extension over adding many type-prefixed primitives
- Multiple dispatch is a core extension mechanism
- Effects are first-class and used for structured control flow and I/O paths
- Lambda arity is strict; partial application should use `_`, `|>`, or explicit `partial`
- Truthiness rule: only `nil` and `false` are falsy
- Memory model is deterministic (scope/region based), so lifetime boundaries must stay explicit
- Runtime memory architecture is dual-lane (`TEMP`/`ESCAPE`) with no stop-the-world GC
- `scope_adopt` is retired from normal return flow; do not reintroduce it in runtime paths
- Boundary promotion/fallback logic must preserve shared promotion-context semantics
- Boundary debug invariant: any committed ESCAPE root must not retain reachable Omni-owned edges into TEMP.
- Hardening priority: stabilize JIT/eval boundary paths (`jit_*`, effect dispatch boundaries, scope handoff points) before adding new runtime wiring.
- Repeated runtime invariants (ownership/lifetime/state guards) should be enforced through shared macros/helpers, not ad-hoc one-off checks.
- Ownership guardrail: default to scope/region ownership (`scope_retain`/`scope_release`) for runtime values.
- Do not introduce per-type refcount lifetimes for language values (`Instance`, `Closure`, etc.).
- Sound exception policy: only rare external-resource wrappers (for example `FFI_HANDLE` boxes over foreign pointers) may use local RC, and only with explicit finalizer policy + boundary tests.
- Any ownership-model exception must include regression tests for:
  - scope return boundary (`let`/function return),
  - closure capture/env copy boundary,
  - destruction path (no UAF/double-free).

## Surface Naming and Alias Policy

Treat pre-alpha language surface cleanup as a design-sharpening phase, not a
compatibility-preservation phase.

- Default to one canonical language-facing name per concept.
- Do not keep compatibility aliases "temporarily" unless the owner explicitly
  asks for a migration window.
- Do not add shorthand spellings unless the owner explicitly approves them.
- Prefer descriptive non-abbreviated names for canonical language-facing
  constructors, types, and operations.
- Do not import borrowed vocabulary from other languages just because it is
  familiar; first justify the capability gap in Omni terms.
- When touching an area with legacy aliases, prefer removing non-canonical
  spellings rather than documenting both.

Current explicit exceptions approved by the owner:

- `Dictionary` is the canonical dictionary constructor/type name.
- `Dict` is an allowed shorthand alias for `Dictionary`.

When naming direction is ambiguous, stop and ask instead of guessing.

## Ownership Drift Guardrails (Required)

Use this as a hard gate before merging memory/lifetime changes:

- Keep Omni value lifetimes region-centric: `ScopeRegion` is the owner of language values; wrappers may cross boundaries, but ownership authority stays with region retain/release.
- Do not add per-type lifetime systems for language objects. If a type stores or embeds Omni `Value`, it must not introduce its own independent RC/GC path.
- Do not use root pinning as a general escape hatch for correctness. Root pinning is allowed only for explicit process-lifetime singletons.
- Treat boundary logic as the source of truth: return-copy, env-copy, mutation copy, and promotion paths must agree on the same ownership model.
- If an exception is unavoidable, it must be rare, local, and explicit:
  - applies only to opaque foreign resources that do not own Omni `Value` graphs,
  - has one clear finalizer authority,
  - includes tests for return/env/destruction boundaries,
  - is recorded in `memory/CHANGELOG.md` with rationale and rollback note.

## Build and Test

Use the commands that match the current repo:

- Build: `c3c build`
- Run main binary: `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Run end-to-end compiler checks: `scripts/run_e2e.sh`

Container-bound validation rule:

- Do not run heavy validation, full-suite runs, global gates, boundary-hardening runs, end-to-end gates, or high-memory Lisp slices directly on the host.
- High-memory or slice-based test execution must run inside the bounded Docker validation path (`scripts/run_validation_container.sh` or the Docker-bound gate scripts).
- Treat `OMNI_LISP_TEST_SLICE=all`, `memory-lifetime-soak`, and `memory-stress` as container-only.
- When a test run could materially spike host memory or CPU, prefer the bounded container path even for targeted validation.
- Keep validation within the repo policy cap: Docker-bound execution with at most 30% host memory and 30% host CPU.

Before finishing significant code changes:

- Run at least targeted tests for touched areas
- Run `c3c build` for integration safety
- When touching memory/lifetime logic, strongly prefer an ASAN pass: `c3c build --sanitize=address`
- When touching memory/lifetime logic, also run full suite:
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## C3 Implementation Rules

Follow `docs/C3_STYLE.md`. Especially:

- Pair resource acquisition with `defer` cleanup
- Use clear preconditions (`@require`) where state assumptions are non-obvious
- Prefer exhaustive `switch` on tagged unions (for example `ValueTag`)
- Prefer `foreach` over manual index loops when practical
- Avoid unsafe type-punning between unrelated handle/value structs

## Change Discipline

- Keep edits small and locally coherent
- Do not silently defer known work; record it in `docs/plans/` with concrete next steps
- If behavior changes, update `memory/CHANGELOG.md` first, then update relevant area/spec docs

## Backlog Shaping and Closure (Required)

Backlog items must be shaped around real semantic/risk boundaries, not broad
umbrella themes.

- Do not keep one backlog item open across multiple materially different
  implementation phases just because they share a topic area.
- When a lane splits into distinct classes of work, split the backlog item
  immediately. Typical split points:
  - admin/visibility work vs execution-semantics changes,
  - parser/surface work vs runtime work,
  - non-recursive support vs recursive support,
  - targeted/local execution vs full optimizer or rewrite semantics.
- Close completed slices as soon as their shipped boundary is real, tested,
  and documented. Do not leave them hanging under one parent item “for later.”
- If one narrow residual blocker remains, promote that residual blocker into
  its own explicit item instead of keeping the whole broader item open.
- When a backlog item has accumulated more than 2-3 landed slices, reassess
  whether it should be split before continuing further work.
- Backlog wording must track the real shipped contract:
  - completed behavior belongs under closed/completed slices,
  - only genuinely unshipped behavior stays under the open item,
  - avoid status text that makes completed work look perpetually partial.
- When work is blocked on a missing language-facing naming or surface-contract
  choice, do not respond only by splitting the backlog:
  - either record an explicit current decision that the surface stays frozen as-is,
  - or add a short decision note under `docs/plans/` with 2-4 concrete
    candidate spellings, their exact semantics, rejected/deferred options,
    and a recommended choice.
- If multiple future trigger/contract families are already visible, choose
  sensible canonical names for all of them in that decision note instead of
  stopping at “pick one later.”
- After such a decision note exists, retitle the remaining implementation item
  so it refers to the chosen/frozen contract directly instead of repeating a
  vague “naming later” umbrella.
- General naming-problem rule:
  - whenever naming is unclear, drifting, or blocking forward progress, do not
    keep moving by hand-waving or repeated backlog reshaping alone
  - resolve it by either:
    - making and recording an explicit current naming decision,
    - producing a short options note with concrete candidate names, meanings,
      rejected/deferred names, and a recommendation,
    - or asking the owner directly when the choice is inherently product-level
      and local repo truth is insufficient
  - do not use undefined phrases like “canonical naming later” or
    “needs naming” as the stopping point for a task; turn them into a concrete
    decision artifact or a concrete owner question

## Owner Workflow Preference (Required)

- Apply safe, non-behavioral, non-hacky patches automatically without asking for
  confirmation.
- When backlog items or code paths are independent, dispatch as much work in
  parallel as is safely possible instead of serializing it. Keep workers scoped
  to disjoint files, lanes, or review slices so they do not trample each other.
- Only pause for explicit confirmation when:
  - the owner explicitly asks for review-before-apply,
  - the action is destructive/high-risk (for example history rewrite or broad
    deletion),
  - requirements are ambiguous enough that applying a patch would likely violate
    intent.
- Default posture: continue execution autonomously and keep momentum unless one
  of the above conditions is true.

## Audit Mode (Important)

If asked to audit:

- Do not apply fixes unless explicitly requested
- Report prioritized findings with `file:line` references
- Focus on correctness, regressions, memory/lifetime risks, and missing tests
- Keep summary short; findings come first

## Refactoring 

- File slitting
  - Always split files top-down.
  - Always split the largest files first, regardless of how hard it seems.
  - Do this before splitting or reorganizing smaller files.
