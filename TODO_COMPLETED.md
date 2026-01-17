# OmniLisp Completed Tasks Archive

This file contains completed and review-pending tasks from the TODO list.
Tasks here are either `[DONE]` or `[DONE] (Review Needed)`.

**Last Updated:** 2026-01-17

---

## Session 2026-01-17: Completion Plan Phases 3-5 [DONE] (Review Needed)

### Phase 3.2: Collection Operations [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-3.2-array-ops
  Location: `runtime/src/runtime.c:2757-2900`
  What was done:
  - `prim_array_reverse()` - returns new reversed array
  - `prim_array_reverse_inplace()` - reverses array in place
  - `prim_array_find()` - finds first element matching predicate
  - `prim_array_find_index()` - finds index of first match
  - `prim_array_index_of()` - finds index of element by equality
  - `prim_array_sort()` - returns sorted copy using qsort
  - `prim_array_sort_inplace()` - sorts array in place

- [DONE] (Review Needed) Label: CP-3.2-dict-ops
  Location: `runtime/src/runtime.c:2900-3206`
  What was done:
  - `prim_dict_keys()` - returns array of all keys
  - `prim_dict_values()` - returns array of all values
  - `prim_dict_entries()` - returns array of [key, value] pairs
  - `prim_dict_merge()` - merges two dicts (second wins conflicts)
  - `prim_dict_filter()` - filters entries by predicate
  - `prim_dict_map()` - transforms values via function
  - `prim_dict_has_key()` - checks key existence
  - `prim_dict_size()` - returns entry count
  - `prim_dict_remove()` - removes key (copy without key)
  Code snippet (dict iteration pattern):
  ```c
  typedef struct { Obj** items; int count; int capacity; } CollectCtx;
  static void collect_keys_fn(void* key, void* value, void* ctx) {
      CollectCtx* c = (CollectCtx*)ctx;
      if (c->count < c->capacity) c->items[c->count++] = (Obj*)key;
  }
  hashmap_foreach(&d->map, collect_keys_fn, &ctx);
  ```

### Phase 3.3: I/O Operations [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-3.3-file-io
  Location: `runtime/src/io.c` (NEW FILE)
  What was done:
  - `prim_file_read()` - read entire file to string
  - `prim_file_read_lines()` - read file as array of lines
  - `prim_file_write()` - write string to file (truncate)
  - `prim_file_append()` - append string to file
  - `prim_file_exists()` - check file existence
  - `prim_file_delete()` - delete file
  - `prim_file_size()` - get file size in bytes
  - `prim_file_is_directory()` - check if path is directory

- [DONE] (Review Needed) Label: CP-3.3-dir-io
  Location: `runtime/src/io.c`
  What was done:
  - `prim_directory_list()` - list directory entries
  - `prim_directory_create()` - create directory (with parents)
  - `prim_directory_exists()` - check directory existence
  - `prim_directory_delete()` - delete empty directory

- [DONE] (Review Needed) Label: CP-3.3-stdio
  Location: `runtime/src/io.c`
  What was done:
  - `prim_stdin_read_line()` - read line from stdin
  - `prim_stdin_read_char()` - read single char from stdin
  - `prim_stdout_write()` - write to stdout
  - `prim_stderr_write()` - write to stderr

- [DONE] (Review Needed) Label: CP-3.3-path-ops
  Location: `runtime/src/io.c`
  What was done:
  - `prim_path_join()` - join path components
  - `prim_path_dirname()` - extract directory part
  - `prim_path_basename()` - extract filename part
  - `prim_path_extension()` - extract file extension
  - `prim_getcwd()` - get current working directory
  - `prim_chdir()` - change working directory

### Phase 3.4: Math Operations [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-3.4-random
  Location: `runtime/src/math_numerics.c:422-480`
  What was done:
  - `prim_random()` - returns float in [0, 1)
  - `prim_random_int(n)` - returns int in [0, n)
  - `prim_random_range(a, b)` - returns int in [a, b)
  - `prim_random_seed(seed)` - seed the RNG
  Code snippet:
  ```c
  static int _random_seeded = 0;
  static void ensure_random_seeded(void) {
      if (!_random_seeded) {
          srand((unsigned int)time(NULL) ^ (unsigned int)clock());
          _random_seeded = 1;
      }
  }
  ```

- [DONE] (Review Needed) Label: CP-3.4-variadic
  Location: `runtime/src/math_numerics.c:480-520`
  What was done:
  - `prim_min_variadic(args, argc)` - min of multiple values
  - `prim_max_variadic(args, argc)` - max of multiple values

- [DONE] (Review Needed) Label: CP-3.4-parsing
  Location: `runtime/src/math_numerics.c:520-549`
  What was done:
  - `prim_parse_int(str)` - parse string to int
  - `prim_parse_float(str)` - parse string to float

### Phase 4.1: Channel Operations RC [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-4.1-channel-rc
  Location: `csrc/codegen/codegen.c:3997-4050`
  What was done:
  - `send!`/`chan-send` - inc_ref before send (ownership transfer)
  - `recv!`/`chan-recv`/`take!` - wrapper for channel_recv
  - `make-channel`/`chan` - create channel with capacity
  - `close!`/`chan-close` - close the channel
  Code snippet:
  ```c
  if (strcmp(name, "send!") == 0 || strcmp(name, "chan-send") == 0) {
      omni_codegen_emit(ctx, "Obj* _send_val = "); codegen_expr(ctx, value);
      omni_codegen_emit(ctx, "if (_send_val && !IS_IMMEDIATE(_send_val)) inc_ref(_send_val);\n");
      omni_codegen_emit(ctx, "channel_send("); codegen_expr(ctx, channel);
      omni_codegen_emit_raw(ctx, ", _send_val);\n");
  }
  ```

### Phase 4.2: Update Operators [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-4.2-update-bang
  Location: `csrc/codegen/codegen.c:3423-3490`
  What was done:
  - `(update! var f)` - transform variable in place
  - `(update! coll idx f)` - transform collection element in place
  - RC: dec_ref old, inc_ref new

- [DONE] (Review Needed) Label: CP-4.2-update-functional
  Location: `csrc/codegen/codegen.c:3490-3571`
  What was done:
  - `(update coll idx f)` - returns new collection with transformed element
  - Array: creates shallow copy, transforms element
  - Dict: creates copy, transforms value at key

### Phase 5.1: Timer/Timeout Operations [DONE] (Review Needed)

- [DONE] (Review Needed) Label: CP-5.1-timer-system
  Location: `runtime/src/memory/continuation.c:1562-1771`
  What was done:
  - Timer thread with sorted deadline list
  - `timer_after(ms)` - creates promise resolved after delay
  - `await_timeout(promise, ms)` - races promise vs timeout
  - `sleep_async(ms)` - returns promise resolved after delay
  - `sleep_ms_blocking(ms)` - blocking sleep
  - `timer_system_shutdown()` - cleanup on exit
  Code snippet (timer thread):
  ```c
  typedef struct TimerEntry {
      uint64_t deadline_ns;
      Promise* promise;
      struct TimerEntry* next;
  } TimerEntry;

  static void* timer_thread_main(void* arg) {
      while (g_timer_system.running) {
          while (g_timer_system.timers && g_timer_system.timers->deadline_ns <= now) {
              promise_resolve(entry->promise, NULL);
          }
          pthread_cond_timedwait(&g_timer_system.cond, &g_timer_system.mutex, &ts);
      }
  }
  ```

- [DONE] (Review Needed) Label: CP-5.1-timer-codegen
  Location: `csrc/codegen/codegen.c:4052-4098`
  What was done:
  - `sleep` / `sleep-async` - emits `mk_promise_obj(sleep_async(...))`
  - `sleep!` / `sleep-blocking` - emits `sleep_ms_blocking(...)`
  - `timeout` / `await-timeout` - emits `await_timeout(...)`
  - `timer-after` - emits `timer_after(...)`

- [DONE] (Review Needed) Label: CP-5.1-promise-retain
  Location: `runtime/src/memory/continuation.c:1518-1523`
  What was done:
  - Added `promise_retain(Promise* p)` to increment refcount

---

## Blocking Issues (Resolved)

### Build Errors (All Resolved)

- **[DONE] Block: Dict struct not found (HashMap type not visible)**
  - RESOLVED (2026-01-10): Fixed Dict struct in internal_types.h
  - Added `#include "util/hashmap.h"` and changed `struct HashMap map;` to `HashMap map;`
  - Location: `runtime/src/internal_types.h` line 29-32

- **[DONE] (Review Needed) Block: runtime ASAN target fails to link (toolchain path assumption)**
  - Fix (2026-01-12): `runtime/tests/Makefile` now defaults ASAN builds to gcc via `ASAN_CC ?= gcc`.
  - Repro: `make -C runtime/tests asan`

- **[DONE] Block: language linkage mismatch in arena.h/omni.h**
  - RESOLVED (2026-01-10): Removed duplicate arena_alloc and arena_reset declarations from omni.h
  - Location: `runtime/include/omni.h` line 876, 879 (now removed)

- **[DONE] Block: omni_store_repair() declaration needed but not implemented**
  - RESOLVED (2026-01-10): Function omni_store_repair() is implemented in runtime/src/runtime.c
  - Location: `runtime/src/runtime.c` line 762

---

## Issue 2: Pool/arena practice + region accounting + auto-repair threshold tuning [DONE] (Review Needed)

**Objective:** Implement region accounting counters and store barrier enforcement for CTRR memory model.

### P0: Region accounting doc + required counters [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-region-accounting-doc (P0)
  - Created `runtime/docs/REGION_ACCOUNTING.md` documenting counter types and semantics
  - Defined: alloc_count, live_count, external_ref_count, region_lifetime_rank

### P1: Retention diagnostics plan [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-retention-diagnostics-plan (P1)
  - Documented diagnostic hooks for pool retention analysis
  - Created test harness for retention validation

### P3: Implement region accounting counters [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-impl-region-accounting-counters (P3)
  - Added counters to Region struct in runtime/src/region.h
  - Implemented increment/decrement on alloc/free paths

### P4: Store Barrier Implementation [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-store-barrier-choke-point (P4)
  - Identified store barrier insertion points in codegen
  - Created omni_store_repair() function

- [DONE] (Review Needed) Label: I2-p4-rank-add-field-and-reset (P4.1)
  - Added lifetime_rank field to Region struct
  - Reset rank on region creation

- [DONE] (Review Needed) Label: I2-p4-rank-codegen-assignment (P4.2)
  - Emitting lifetime_rank assignment in generated C code
  - Child regions get parent_rank + 1

- [DONE] (Review Needed) Label: I2-p4-rank-enforce-store-repair (P4.3)
  - Store barrier uses rank comparison for repair decision
  - Implemented omni_store_repair with proper semantics

- [DONE] (Review Needed) Label: I2-p4-outlives-ancestry-metadata (P4.3b)
  - Added parent pointer for ancestry tracking
  - Used by omni_store_repair() for correct repair decisions

- [DONE] (Review Needed) Label: I2-p4-integrate-store-barrier-boundaries (P4.4)
  - Integrated barriers in dict_set, array_set, box_set paths
  - New entry insertion properly barrier-mediated

- [DONE] (Review Needed) Label: I2-p4-dict-new-entry-store-barrier
  - Dict new-entry insertion uses store barrier (not only update path)

- [DONE] (Review Needed) Label: I2-p4-doc-rank-policy (P4.5)
  - Documented rank assignment policy in CTRR.md

### P5: Region Merge Policy [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I2-region-merge-policy (P5)
  - Documented when region merging is safe
  - Defined merge criteria based on lifetime analysis

### RF: Regression Fixes

- [DONE] (Review Needed) Label: I2-wire-and-strengthen-region-accounting-tests (RF-I2-1)
  - Added accounting tests to test_main.c
  - Tests verify counter invariants

- [DONE] (Review Needed) Label: I2-warning-clean-build-gate (RF-I2-2)
  - Build passes with -Wall -Werror
  - No new warnings introduced

---

## Issue 3: Non-lexical regions + splitting ideas as CTRR roadmap [DONE] (Review Needed)

**Objective:** Document the CTRR region inference roadmap for non-lexical region boundaries.

### P0: CTRR inference roadmap doc [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-ctrr-roadmap-doc (P0)
  - Created `docs/CTRR_REGION_INFERENCE_ROADMAP.md`
  - Outlines phases for non-lexical region inference

### P1: Emission Inventory [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-ctrr-emission-inventory (P1)
  - Inventoried all region_create/region_exit emission points
  - Documented current lexical-only emission pattern

### P2: Non-lexical Region End (Straightline) [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I3-nonlexical-region-end-straightline (P2)
  - Implemented non-lexical region exit for straightline code
  - Region exits emitted after last use, not at scope end

---

## Issue 4: Concurrency SMR techniques for internal runtime DS [DONE] (Review Needed)

**Objective:** Evaluate and document Safe Memory Reclamation techniques for concurrent runtime data structures.

### P0: SMR target inventory + decision matrix [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-smr-target-inventory (P0)
  - Identified candidate structures: metadata registry, intern table, global module map
  - Evaluated contention patterns for each

### P1: QSBR mapping to OmniLisp "quiescent points" [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-qsbr-quiescent-points (P1)
  - Mapped QSBR quiescent points to OmniLisp execution model
  - Region boundaries as natural quiescent points

### P2: Alternatives review [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-alternatives-review (P2)
  - Documented hazard pointers, Hyaline, publish-on-ping alternatives
  - Evaluated trade-offs for OmniLisp use case

### P3: Atomic Policy Wrapper [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-atomic-policy-wrapper (P3)
  - Created atomic wrapper macros for policy-based access
  - Abstracts memory ordering requirements

### P4: QSBR First Target Plan [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I4-qsbr-first-target-plan (P4)
  - Selected metadata registry as first QSBR target (low contention)
  - Documented implementation plan

---

## Issue 5: Build/Test Harness Stabilization [DONE] (Review Needed)

**Objective:** Stabilize build system and test harness to prevent agent stalls.

### P0: Define canonical commands [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p0-canonical-commands
  - Documented canonical build/test commands in BUILD_AND_TEST.md
  - `make -C runtime test`, `make -C csrc test`

### P1: Add csrc/tests harness [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p1-csrc-tests-makefile
  - Created Makefile for csrc/tests
  - Tests compile and run via make targets

### P2: Eliminate tracked build artifacts [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p2-untrack-binaries
  - Added .gitignore rules for build artifacts
  - Clean tree after tests

### P3: Make warning policy explicit [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p3-warning-policy
  - Documented warning flags in Makefiles
  - -Wall -Wextra standard, -Werror for release

### P4: Prevent partial compile-breaking edits [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I5-p4-codegen-guardrails
  - Added compile check after codegen changes
  - Guardrails prevent broken generated code

---

## Issue 7: SYNTAX_REVISION.md Strict Character Calculus Implementation [DONE] (Review Needed)

**Objective:** Implement the Strict Character Calculus from `docs/SYNTAX_REVISION.md`.

**Completed (2026-01-14)**

### P1: Slot-Syntax Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p1-slot-syntax-functions
  - Support `(define add [x] [y] body)` slot-syntax function definitions.
  - Where: `csrc/codegen/codegen.c` (first pass detection, codegen_define)
  - What was changed:
    1. Added `first_pass` flag to CodeGenContext
    2. Modified first pass to detect slot-syntax functions
    3. Wrapped initialization code in `if (!ctx->first_pass)` guards
  - Verification: `(define add [x] [y] (+ x y))` compiles and `(add 3 4)` returns 7 ✓

### P2: Traditional Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p2-traditional-functions
  - Continue supporting `(define (f x y) body)` traditional syntax
  - Verification: `(define (mul a b) (* a b))` works ✓

### P3: Shorthand Function Definitions [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p3-shorthand-functions
  - Support `(define f x body)` shorthand syntax
  - Verification: `(define square n (* n n))` works ✓

### P4: Typed Let Bindings [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p4-typed-let
  - Support `(let [x {Int} 10] body)` typed let bindings
  - Where: `csrc/analysis/analysis.c`, `csrc/codegen/codegen.c`
  - Verification: `(let [x {Int} 10] (print x))` outputs 10 ✓

### P5: Sequential Let with Metadata [DONE] (Review Needed)

- [DONE] (Review Needed) Label: I7-p5-sequential-let
  - Support `(let ^:seq [a 5] [b (+ a 1)] body)` sequential bindings
  - Verification: `(let ^:seq [a 5] [b (+ a 1)] (print b))` outputs 6 ✓

### Additional Fixes (2026-01-14)

- [DONE] Lambda, fn, and -> codegen for slot syntax
  - Fixed inline lambda application `((lambda [x] body) arg)`
  - Added `->` as special form in codegen
  - Arrow syntax `(-> [x] [y] body)` works correctly

- [DONE] Typed variable definitions
  - Fixed `(define x-typed {Int} 42)` to correctly emit value (was emitting type)
  - Codegen now skips type annotation and uses actual value

- [DONE] set! codegen fix
  - Fixed bug that generated `return` statement in set! expression
  - Now correctly emits `(var = value)` as expression

### Pre-existing Issues (Not related to Issue 7)

- Multiline string handling in codegen
- Array literal inlining in expressions
- Some tests use variables before definition

---

## Issue 13: SYNTAX_REVISION.md Strict Character Calculus Implementation [N/A - Duplicate of Issue 7]

**Status:** N/A - This is a duplicate of Issue 7. All work tracked under Issue 7.

---

## Summary Statistics

| Status | Count |
|--------|-------|
| DONE (Review Needed) | ~50 |
| DONE | ~5 |
| N/A (Duplicates) | 2 |

**Next Step:** User review of all `[DONE] (Review Needed)` items to approve as `[DONE]`.
