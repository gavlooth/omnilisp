# Project Instructions (C Memory Model)

You are working in a C codebase that implements a complex memory model. Your job is to iteratively improve correctness by finding architectural/implementation issues and writing tests that expose them.

## Primary Goal
Verify that the *architecture is implemented correctly* (invariants, ordering rules, state transitions, ownership/lifetime, API contracts, error paths), not just that the code compiles.

---

## Persistent State: Avoid Re-finding the Same Bugs

### State File (mandatory)
There is a repository file named:

- `buglog.md`

On EVERY iteration you MUST:
1) **Read** `buglog.md` first.
2) **Choose a new issue** that is not already recorded there.
3) **Append** a new entry for the issue you found (even if you only produced a failing test and not a fix).

If `buglog.md` does not exist, create it with the template in the section “Buglog Template”.

### Deduplication Rules
Before you claim a new issue, checkbuglog for:
- same root cause (even if different symptom),
- same invariant violation,
- same function/struct hot spot,
- same failure mode (UAF, double free, freelist corruption, wrong ordering, etc.).

If it’s a repeat, you must pick a different issue.

---

## Operating Mode: Iterative Loop (One Issue per Iteration)
Each iteration MUST produce:
1) **One concrete issue**
2) **One failing test** that demonstrates it (must fail on current code)
3) **A minimal fix proposal** (describe only; implement only if asked)
4) **Update `buglog.md`** with the new issue entry

Stop after one issue+test per iteration unless explicitly asked for more.

---

## What Counts as an “Issue”
Issues can be anything, but prioritize architecture correctness:
- Invariant violations (refcounts, ownership, bounds, alignment, freelist integrity)
- Incorrect lifecycle (init/teardown/reset/reuse, double free, UAF)
- Concurrency semantics (ordering, atomicity, lock discipline, ABA hazards) if applicable
- API contract mismatch (documented vs actual behavior)
- Error handling that breaks invariants (partial init, rollback failures)
- Undefined behavior, integer overflow, aliasing problems
- State machine missing transitions or allowing illegal transitions
- Persistence/corruption risks (metadata inconsistency)

---

## How to Find Issues (You MUST actively use tools)

You are expected to use tooling *as part of issue discovery*, not just as advice.

### Mandatory: Runtime Bug-Finding Builds
Run  :
- **ASan + UBSan** for memory/UB issues
- **TSan** if the project is multi-threaded
Prefer explicit checks in tests, and use sanitizers to turn UB into deterministic test failures.

### Mandatory: Dynamic + Static Checking
Use  :
- **Valgrind (memcheck)** for deep memory diagnostics (optional but strong)
- **clang-tidy** + **clang-analyzer** to surface likely bugs and invariant risks

### Mandatory: Input/State Exploration
Use :
- **Fuzzing**:  AFL++ to explore allocator/state-machine surfaces
- **Coverage**: llvm-cov or gcov/lcov to guide where tests/fuzzing are lacking

### Mandatory: Continuous Detection
Provide a path to integrate:
- **CI (GitHub Actions or similar)** that runs unit tests and sanitizer builds on every push/PR

When you mention a tool, include concrete commands/flags/snippets (CMake/Make/Meson as appropriate).

---

## Approach to Analysis
- Infer the intended architecture from code: key structs, ownership graph, invariants, allowed transitions.
- Identify mismatch points: “Where can this invariant be broken?”
- Prefer issues that can be demonstrated deterministically with a failing test.

---

## Tests: Requirements
Each test MUST:
- Be minimal and focused on one issue.
- Fail on current code (crash/assert/incorrect return/corrupted state).
- Include a short comment header explaining:
  - expected invariant/behavior
  - observed behavior
  - why it violates the architecture
- Avoid flaky timing-based failures. If concurrency is involved, use deterministic orchestration (barriers, controlled scheduling, or systematic testing tools).

---

## Test Framework Preferences (choose best fit)
Default to one of these, in order:
1) CTest + CMake (if project uses CMake)
2) Criterion or Unity
3) A small custom harness if the project is tiny

---

## Output Format (every iteration)
Use this exact structure:

### Pre-check (Buglog)
- Confirm you read `buglog.md`
- Confirm the issue is new (not a duplicate)

### Issue
- Summary:
- Why it violates architecture/invariants:
- Evidence (code references: functions/structs/paths):

### Failing Test
- Test name:
- Files to add/modify:
- Code:

### Fix Proposal (do not implement unless asked)
- Minimal change:
- Why it resolves the invariant violation:

### Tooling Used (for discovery)
- Which tools you used or would run next to validate/extend:
- Exact commands/flags:

###buglog Update
- Provide the exact entry to append to `buglog.md`

---

##buglog Template
If `buglog.md` is missing, create it with:

#buglog (C Memory Model)

Purpose: prevent rediscovering the same issues. Each entry should capture root cause, invariant violated, repro test, and status.

## Index
(Keep a simple numbered list linking to entries.)

## Entries

### BUG-0001: <short title>
- Date:
- Area (module/struct/function):
- Invariant violated:
- Symptom:
- Root cause (hypothesis / confirmed):
- Repro test:
- Tool signals (ASan/UBSan/TSan/Valgrind/clang-tidy/etc):
- Status: (new / test-added / fixed / verified / wontfix)
- Notes:

---

## Constraints
- Keep changes minimal.
- Do not refactor unrelated code during issue discovery.
- Do not “fix” the test to pass; the test must expose the current bug.
- Follow existing project conventions.

## Questions You Can Ask (only if truly necessary)
Only ask for clarification if you cannot locate:
- how to build tests,
- where public APIs are,
- or whether the memory model is single-threaded vs multi-threaded.

Otherwise proceed by inspecting repository structure and code.
