# Pika Regex Parity Plan (vs PikaParser.jl)

Status date: 2026-03-06
Owner: Codex session workflow

Historical completed checklist. Reopen through `TODO.md` if new Pika regex
parity work appears.

Legend:
- `[x]` done
- `[~]` partial / follow-up required
- `[ ]` pending

## Session 1: Parser correctness parity fixes

Goal: eliminate known semantic bugs before perf/limit work.

Files:
- `src/pika/clauses.c3`
- `src/pika/structs.c3`
- `src/pika/grammar.c3`

Checklist:
- [x] Fix `seeded_by` single-child slice bug.
- [x] Align sentinel/docs with runtime truth (`memo_root` and indexing comments).
- [x] Decide and enforce unreachable grammar behavior contract (strict assert vs explicit error).
- [x] Add focused tests for seed behavior + grammar contract.
- [x] Validate:
  - [x] `c3c build`
  - [x] `LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## Session 2: Regex compiler safety/hard-bug cleanup

Goal: remove UB-level hazards in regex compiler internals.

Files:
- `src/pika/regex.c3`

Checklist:
- [x] Harden malformed tokenizer/compile paths and explicit invalid states.
- [x] Fix/audit rule-name generation lifetime ownership.
- [x] Audit compile/tokenizer paths for transient-buffer/slice lifetime hazards.
- [x] Remove remaining silent malformed-rule fallthroughs.
- [x] Add targeted regressions for each failure class.
- [x] Validate:
  - [x] targeted regex tests
  - [x] full build + full suite

## Session 3: Remove sequence/alternation hard caps

Goal: support large patterns reliably.

Files:
- `src/pika/regex.c3`
- `src/lisp/tests_tests.c3`

Checklist:
- [x] Dynamic seq/alt growth (verify no hidden fixed-size paths remain).
- [x] Replace/clarify bounded quantifier expansion cap behavior (currently capped expansion path).
- [x] Add long-pattern regressions:
  - [x] long concatenations
  - [x] long alternations
  - [x] large bounded quantifier shapes
- [x] Validate full suite.

## Session 4: Remove char-class scanner cap (8-slot bottleneck)

Goal: eliminate fixed scan-slot limitations.

Files:
- `src/pika/structs.c3`
- `src/pika/clauses.c3`
- `src/pika/regex.c3`
- `src/pika/lisp_pika.c3`

Checklist:
- [x] Intermediate mitigation landed (8 -> 32 slots).
- [x] Replace slot-table scan binding with context-aware scanner mechanism (no fixed global slot cap).
- [x] Keep public API unchanged.
- [x] Add tests:
  - [x] patterns with `>32` char classes in one compiled regex
  - [x] grammar-defined scanner parity paths
- [x] Validate full suite + ASAN.

## Session 5: Cache policy hardening

Goal: make cache bounded, robust, and diagnosable.

Files:
- `src/pika/regex.c3`

Checklist:
- [x] Add bounded eviction policy (LRU/clock).
- [x] Ensure eviction/reset frees copied patterns + compiled regex objects.
- [x] Add counters (hit/miss/evict/fallback).
- [x] Add cache growth/eviction/reuse tests.
- [x] Validate repeated compile/search loops under ASAN.

## Session 6: Regression matrix + ASAN + docs

Goal: lock behavior and prevent regressions.

Files:
- `src/lisp/tests_tests.c3`
- `docs/LANGUAGE_SPEC.md`
- `memory/CHANGELOG.md`

Checklist:
- [x] Strict invalid-pattern regressions added.
- [x] Add matrix coverage for:
  - [x] large regexes
  - [x] many char classes
  - [x] anchor correctness
  - [x] cache behavior
  - [x] compiled/fallback parity
- [x] Document regex-on-Pika semantics and non-PCRE differences.
- [x] Final validation:
  - [x] `c3c build`
  - [x] `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - [x] `c3c build --sanitize=address`
  - [x] `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## Current progress snapshot

- [x] Strict regex grammar hardening pass landed (`regex.c3`) with focused regressions.
- [x] Session 1 parser parity fixes completed and verified.
- [x] Session 2 regex safety hardening completed and verified.
- [x] Session 3 long-pattern/cap behavior hardening completed and verified.
- [x] Session 4 scanner-context refactor completed and verified (normal + ASAN).
- [x] Session 5 cache hardening completed and verified (normal + ASAN).
- [x] Session 6 docs/matrix validation completed and verified.
