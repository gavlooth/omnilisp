# Omni Lisp - Agent Guide

This file is the working guide for coding agents in this repository.
Keep changes aligned with Omni's design and current codebase reality.

## Read First

Before modifying code, read:

1. `docs/LANGUAGE_SPEC.md`
2. `docs/C3_STYLE.md` (mandatory for C3 changes)
3. `c3_cheatsheet.md` (quick reference for C3 language syntax and features)

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
preservation phase.

- Default to one canonical language-facing name per concept.
- Do not keep non-canonical aliases "temporarily" unless the owner explicitly
  asks for a migration window.
- Do not add shorthand spellings unless the owner explicitly approves them.
- Prefer descriptive non-abbreviated names for canonical language-facing
  constructors, types, and operations.
- Do not import borrowed vocabulary from other languages just because it is
  familiar; first justify the capability gap in Omni terms.
- When touching an area with non-canonical aliases, prefer removing non-canonical
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
- When ASAN is unsupported or when native memory/lifetime behavior remains
  suspect, run Valgrind if available; the local command is `valgrind`.
  Prefer a targeted invocation first, for example:
  `valgrind --leak-check=full --error-exitcode=99 env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  Use the bounded Docker validation path for broad/high-memory Valgrind runs.
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
- Any deferred or blocked work must also add/update a concrete item in `TODO.md` so the next agent TODO scan can pick it up immediately
- **Plans must be tracked with checkboxes in TODO parts:** Every plan document under `docs/plans/` that contains open or in-flight work must have corresponding line items in `docs/todo_parts/` using `- [ ]` (open) or `- [x]` (complete) checkboxes. Each item must have a unique task ID (e.g., `PHASE-001`, `AUDIT-042`), a one-sentence description, and a file/link reference to the plan it tracks. Do not leave plan documents without checkbox entries in TODO parts. When a plan phase ships or is abandoned, update its checkbox state immediately.
- If behavior changes, update `memory/CHANGELOG.md` first, then update relevant area/spec docs
- When committing, include all non-artifact changes from current and previous
  agent work that are still present in the workspace: source, tests, docs,
  plans, session reports, build manifests, and checked-in configuration. Do not
  leave earlier agent changes behind just because they came from a prior work
  slice. Exclude generated build outputs, binaries, caches, temporary logs, and
  other artifacts unless the owner explicitly asks to commit them.

## Hard Anti-Conservatism Rule (Required)

User intent overrides minimalism. When the owner asks for a capability, design
direction, compatibility, or continued implementation, interpret the request as
permission to implement the complete useful version of the feature, not a
minimal demonstration, scaffold, or safest local slice.

Treat phrases like "continue", "go on", "support X", "add X", "make it work",
"make it compatible", "do the sensible thing", and "stop being conservative" as
authorization to cross files, subsystems, and abstraction boundaries as needed.

Required behavior:

- Start from the user-visible capability and work backward to the implementation
  surface required to make it real.
- Choose the work boundary by semantic completeness, not by diff size.
- If the capability naturally spans parser, runtime representation, dispatch,
  lifecycle, printing, tests, docs, build wiring, or validation, implement
  across those layers in the same work item unless a concrete blocker prevents
  it.
- Prefer a coherent higher-blast-radius change over a low-blast-radius change
  that leaves the requested capability semantically incomplete.
- Do not stop at scaffolding, constructor-only wiring, parser-only changes, type
  tags, compatibility stubs, or doc-only updates when behavior is implied.

Conservative choices are not neutral:

- A conservative choice means choosing a narrow, local, partial,
  low-blast-radius, or scaffolding-first path when a broader complete semantic
  path is available.
- The burden of proof is on choosing the conservative path.
- Before making a conservative choice, the agent must write a concrete
  justification naming:
  - the broader capability being deferred,
  - the invariant, blocker, or validation uncertainty that requires deferral,
  - the exact follow-up required to complete the semantic capability,
  - and the validation command that will prove the conservative step was useful.

Valid reasons for conservatism are limited to:

- preventing a known correctness regression,
- protecting a documented runtime invariant,
- isolating an unknown failure mode with a specific validation command,
- avoiding a destructive or irreversible operation,
- a required dependency, tool, or generated artifact being unavailable,
- or honoring an explicit owner request for a narrow change.

Invalid reasons for conservatism:

- "lower risk",
- "smaller diff",
- "incremental",
- "safer",
- "minimal",
- "easier to review",
- "avoids touching many files",
- "can be done later",
- or equivalent phrasing that does not identify a concrete invariant, blocker,
  or validation purpose.

Mandatory conservative-choice tax:

- Any conservative choice, even justified, requires immediate extra compute in
  the same turn.
- The agent must perform all of the following before returning control:
  - revise the active plan to name the deferred semantic boundary,
  - inspect the broader implementation surface that was avoided,
  - implement at least one additional layer beyond the conservative patch or
    explicitly record the blocker preventing it,
  - run one extra validation command beyond the minimum needed for the
    conservative patch,
  - and record negative memory if the conservative path was chosen because a
    broader assumption is unsafe or currently invalidated.

Violation consequence:

- An unjustified conservative choice is failed work.
- Returning control after an unjustified conservative stopping point is not
  allowed.
- The agent must immediately revise toward the nearest complete semantic
  boundary, update the plan or handoff artifact, and run relevant validation
  before reporting completion.

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
- Churn recognition and consolidation rule:
  - before creating a new residual item under an active parent, count the
    recently closed child slices and open residuals for the same semantic
    capability;
  - churn is present when three or more landed slices close one implementation
    case while preserving the same broader residual class, or when two or more
    open residuals differ mainly by mechanism but depend on the same missing
    contract, planner, ownership model, lowering abstraction, or execution
    boundary;
  - when churn is present, do not create another narrow numbered residual or
    continue with another case-specific implementation as the neutral next
    step;
  - immediately name the repeated capability, demote the case-by-case approach
    in the active plan, and consolidate the remaining work into one semantic
    closure boundary with explicit sub-boundaries;
  - the next implementation must target the shared abstraction or prove a
    concrete blocker that prevents it. If blocked, record the blocker and the
    required evidence in TODO, the active plan, the session report, and memory.
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
  - The hard LOC split/gate rule applies only to code files and is now 1000
    LOC from 2026-04-21 onward.
  - Documentation, plans, changelogs, session reports, and other
    agent-operational artifacts are not subject to the hard LOC rule,
    though they should stay readable and locally discoverable.
  - Always split files top-down.
  - Always split the largest files first, regardless of how hard it seems.
  - Do this before splitting or reorganizing smaller files.
