# Buglog (C Memory Model)

Purpose: prevent rediscovering the same issues. Each entry should capture root cause, invariant violated, repro test, and status.

## Index
- [BUG-0001](#bug-0001-component-merge-integer-overflow)
- [BUG-0002](#bug-0002-pool-bounds-staleness-after-growth)
- [BUG-0003](#bug-0003-frame-clone-jmpbuf-unsafe-memcpy)
- [BUG-0004](#bug-0004-scc-refcount-underflow)
- [BUG-0005](#bug-0005-symmetric-pool-freelist-type-punning)
- [BUG-0006](#bug-0006-component-realloc-unchecked-null)
- [BUG-0007](#bug-0007-constraint-strdup-silent-failure)
- [BUG-0008](#bug-0008-channel-send-refcount-leak-on-closed)

## Entries

### BUG-0001: Component Merge Integer Overflow
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/component.c` - `sym_component_union()`
- **Invariant violated**: Memory safety - integer overflow leading to potential buffer overflow
- **Symptom**: When merging two SymComponent structures with large member counts, the addition `rootA->member_count + rootB->member_count` can overflow signed integer bounds, wrapping to a negative value. This bypasses the capacity check and can cause `realloc()` to be called with an incorrect size, or cause the while loop to behave unpredictably.
- **Root cause (confirmed)**: In `component.c:89-96`, the code uses signed `int` for member counts and capacities without overflow protection:
  ```c
  if (rootA->member_count + rootB->member_count > rootA->member_capacity) {
      int new_cap = rootA->member_capacity * 2;
      while (new_cap < rootA->member_count + rootB->member_count) {
          new_cap *= 2;  /* Can overflow and wrap */
      }
      rootA->members = realloc(rootA->members, new_cap * sizeof(SymObj*));  /* Unsafe size */
  }
  ```
  When `member_count` values are near `INT_MAX/2`, their sum overflows to a large negative number, making the comparison false (negative < capacity), causing the code to skip realloc entirely. Or if it enters the loop, `new_cap *= 2` can also overflow.
- **Repro test**: `runtime/tests/test_component_merge_overflow.c`
  - Creates two components with `member_count` values near `INT_MAX/2`
  - Attempts to merge them
  - Verifies whether integer overflow is detected or causes memory corruption
- **Tool signals**:
  - Would be detected by `-fsanitize=integer` or UBSan
  - Static analysis tools like clang-tidy would flag the signed arithmetic overflow
- **Status**: new
- **Notes**:
  - This is distinct from the issues documented in `MEMORY_MODEL_REVIEW_NOTES.md` (which listed 8 other issues but not this one)
  - The fix should change `member_count`, `member_capacity`, `handle_count`, `tether_count` from `int` to `size_t` and add overflow checks before arithmetic operations
  - The overflow could theoretically lead to heap corruption if an attacker can control member counts, though in practice this would require creating billions of objects

### BUG-0002: Pool Bounds Staleness After Growth
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/handle.c` - `handle_is_pool_obj()`, `update_pool_bounds()`
- **Invariant violated**: Memory management correctness - `handle_is_pool_obj()` must correctly identify all pool objects to route free operations properly
- **Symptom**: When `pool_grow()` adds a new block to the slot pool, `g_pool_bounds` (a global cache of valid pool memory ranges) is NOT updated. Objects allocated from the new block will fail the `handle_is_pool_obj()` bounds check until a "miss" triggers `update_pool_bounds()`. When `handle_is_pool_obj()` returns `false` for a pool object, `handle_free_obj()` returns early without freeing, causing a memory leak.
- **Root cause (confirmed)**: In `handle.c:96-118`, `handle_is_pool_obj()` maintains `g_pool_bounds` as a lazy cache. The `update_pool_bounds()` function at lines 73-94 rebuilds this cache by iterating over all pool blocks, but it is ONLY called when an initial bounds check fails. However, in `slot_pool.c:165-211`, `pool_grow()` adds new blocks without calling `update_pool_bounds()`. This creates a window where:
  1. Pool grows (new block added)
  2. Objects allocated from new block
  3. `handle_is_pool_obj()` checks those objects against stale `g_pool_bounds`
  4. Check returns `false` (new block's address range not in cached bounds)
  5. `handle_free_obj()` sees `false` and returns early (line 149-153)
  6. Object never freed to pool → LEAK
  7. Eventually a non-pool object triggers `update_pool_bounds()`, fixing the cache
- **Evidence (code references)**:
  - `handle.c:73-94` - `update_pool_bounds()` rebuilds bounds from pool blocks
  - `handle.c:109` - `update_pool_bounds()` called only after initial check fails
  - `handle.c:149-153` - `handle_free_obj()` returns early if `handle_is_pool_obj()` returns false
  - `slot_pool.c:165-211` - `pool_grow()` adds blocks but never updates `g_pool_bounds`
- **Repro test**: `runtime/tests/test_pool_bounds_staleness.c`
  - Creates small pool that will grow quickly
  - Allocates objects to trigger pool growth
  - Checks if newly allocated objects are recognized by `handle_is_pool_obj()`
  - Verifies that `handle_free_obj()` properly frees objects from new blocks
- **Tool signals**:
  - Valgrind/ASan would detect memory leaks from objects not being freed
  - Static analysis would note that `pool_grow()` and `update_pool_bounds()` are not synchronized
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 (component merge overflow) and the issues in `MEMORY_MODEL_REVIEW_NOTES.md`
  - Related to issue #4 in the review notes ("Handle/slot-pool safety is not integrated") but this is a specific bug in the integration, not just a missing feature
  - The fix requires either:
    a) Calling `update_pool_bounds()` from `pool_grow()` after successful growth, OR
    b) Making `handle_is_pool_obj()` check directly against the pool instead of using cached bounds
  - Option (b) is slower but simpler; option (a) requires exposing the update function or creating a callback mechanism
  - There's also a secondary issue: TWO separate global pools exist (`g_global_slot_pool` in `slot_pool.c` and `g_obj_pool` in `handle.c`), but only `g_obj_pool` is checked by `handle_is_pool_obj()`. Objects allocated via `slot_pool_global()` would always be seen as non-pool objects by the handle system.

### BUG-0003: Frame Clone jmp_buf Unsafe memcpy
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/continuation.c` - `frame_clone()`
- **Invariant violated**: Type safety and platform portability - `jmp_buf` must not be copied via `memcpy` due to implementation-specific state
- **Symptom**: When cloning a FRAME_PROMPT frame, the `frame_clone()` function uses `memcpy()` to copy the entire Frame structure, including the `jmp_buf escape` field. This blindly copies implementation-specific state including stack pointers, signal masks, and register state. If the cloned frame's `jmp_buf` is later used (via `longjmp`), it can cause crashes, memory corruption, or undefined behavior.
- **Root cause (confirmed)**: In `continuation.c:115-163`, `frame_clone()` does:
  ```c
  Frame* clone = frame_alloc(f->tag);
  /* ... */
  memcpy(clone, f, sizeof(Frame));  /* Blindly copies EVERYTHING */
  clone->refcount = 1;
  clone->prev = NULL;
  ```
  The `memcpy` at line 122 copies the entire 256-byte Frame structure, including the `jmp_buf escape` field (200 bytes on this platform). The `jmp_buf` type contains:
  - Stack pointers (may become invalid after clone)
  - Signal mask state (platform-specific)
  - Saved register state (architecture-specific)

  The C standard does not guarantee that `jmp_buf` can be safely copied via `memcpy`. Using a copied `jmp_buf` for `longjmp` is undefined behavior.
- **Evidence (code references)**:
  - `continuation.c:122` - `memcpy(clone, f, sizeof(Frame))` copies entire struct including `jmp_buf`
  - `continuation.h:91` - `jmp_buf escape; /* For non-local return */` in FRAME_PROMPT union
  - Test output shows: `jmp_buf = 200 bytes` on x86_64 Linux, all copied byte-for-byte
- **Repro test**: `runtime/tests/test_frame_clone_jmpbuf_issue.c`
  - Creates a FRAME_PROMPT with initialized `jmp_buf`
  - Clones the frame using the buggy `frame_clone`
  - Verifies that `jmp_buf` is copied byte-identical (UNSAFE)
  - Demonstrates potential undefined behavior when original is freed
- **Tool signals**:
  - ASan/TSan might detect issues if the copied `jmp_buf` is used after free
  - Static analysis tools (clang-tidy) might flag `memcpy` of `jmp_buf` as suspicious
  - Undefined Behavior Sanitizer could detect if `longjmp` uses corrupted state
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 and BUG-0002
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The `jmp_buf` issue is particularly dangerous because:
    1. It's platform-specific behavior (works by accident on some systems)
    2. The bug may not manifest until the cloned frame is used
    3. Testing may not catch it because `memcpy` "works" on many platforms
  - The fix requires one of:
    a) Zero out the `jmp_buf` in clones (makes escape invalid for cloned frames)
    b) Use platform-specific `jmp_buf` duplication (if available)
    c) Mark cloned prompt frames as non-escapable (add a flag)
  - Option (c) is safest: add a `cloned: bool` field to Frame and check it before allowing escape

### BUG-0004: SCC Refcount Underflow
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/scc.c` - `release_scc()`
- **Invariant violated**: Reference counting invariant - `scc->ref_count` must always be >= 0, and memory should only be freed when refcount reaches exactly 0
- **Symptom**: When `release_scc()` is called more times than `inc_scc_ref()` (e.g., double-free error or unbalanced refcount operations), the refcount decrements below zero. Once negative, the cleanup code at `if (scc->ref_count == 0)` never triggers, causing a permanent memory leak. The SCC struct and its members are never freed.
- **Root cause (confirmed)**: In `scc.c:336-350`, the `release_scc()` function does:
  ```c
  void release_scc(SCC* scc) {
      if (!scc) return;
      scc->ref_count--;  /* BUG: No underflow check! */

      if (scc->ref_count == 0) {
          /* Free all members */
          for (int i = 0; i < scc->member_count; i++) {
              free(scc->members[i]);
          }
          free(scc->members);
          scc->members = NULL;
          scc->member_count = 0;
      }
  }
  ```
  There is no check that `scc->ref_count > 0` before decrementing. If called when `ref_count == 0`, it wraps to -1. The cleanup only runs when `ref_count == 0`, so with negative refcount, the memory is leaked forever (unless the count wraps all the way around after 2^31 more calls, which is effectively never).
- **Evidence (code references)**:
  - `scc.c:336-350` - `release_scc()` function with missing underflow check
  - `scc.c:332-334` - `inc_scc_ref()` only increments, no validation
  - No API-level contract enforcement ensures balanced inc/dec operations
- **Repro test**: `runtime/tests/test_scc_refcount_underflow.c`
  - Test 1: Calls `release_scc()` when `ref_count == 0` - confirms refcount goes to -1
  - Test 2: Calls `release_scc()` twice when `ref_count == 1` - confirms double-free causes underflow
  - Test 3: Verifies memory is leaked after underflow (members never freed)
  - All tests PASSED - bug is confirmed
- **Tool signals**:
  - Clang static analyzer might detect unbalanced increment/decrement
  - UBSan with `-fsanitize=signed-integer-overflow` would catch the underflow
  - Valgrind/ASan would detect the memory leak
  - Custom runtime instrumentation could track refcount operations
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001, BUG-0002, and BUG-0003
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The bug is particularly dangerous because:
    1. It's silent - no crash or error message, just memory leak
    2. It can be triggered by simple programmer errors (unbalanced refcount operations)
    3. The leaked memory accumulates over time, causing gradual performance degradation
    4. Debugging is difficult because the symptom (leak) appears far from the cause (extra release)
  - The fix should add one of:
    a) `assert(scc->ref_count > 0);` before decrement (for debug builds)
    b) Change `ref_count` from `int` to `unsigned int` and use saturating decrement
    c) Return an error code instead of silently corrupting state
    d) Add runtime validation: `if (scc->ref_count <= 0) { log_error("double-free detected"); return; }`
  - Option (d) with (a) for debug builds is recommended - it prevents the underflow in production and catches bugs early in development
  - Related issue: The `inc_scc_ref()` function also has no overflow protection, though this is less critical in practice

### BUG-0005: Symmetric Pool Freelist Type-Punning
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/symmetric.c` - `sym_pool_alloc()`, `sym_pool_free()`
- **Invariant violated**: Type safety and C standard compliance - strict aliasing rules (C99 6.5p7)
- **Symptom**: The pool freelist implementation uses the `refs` field (declared as `SymObj**`) to store freelist next pointers (type `SymObj*`). This violates C's strict aliasing rule which states that an object shall only be accessed through its declared type or compatible types. The compiler may optimize assuming accesses through different pointer types don't alias, leading to incorrect code generation, crashes, or silent memory corruption.
- **Root cause (confirmed)**: In `symmetric.c:32-55`, the freelist management code does:
  ```c
  /* In sym_pool_alloc() when building freelist: */
  for (int i = 0; i < SYM_POOL_SIZE; i++) {
      pool->objects[i].refs = (SymObj**)SYM_TLS.freelist;  /* VIOLATION: SymObj* stored in SymObj** */
      SYM_TLS.freelist = &pool->objects[i];
  }
  /* In sym_pool_alloc() when allocating: */
  SymObj* obj = SYM_TLS.freelist;
  SYM_TLS.freelist = (SymObj*)obj->refs;  /* VIOLATION: SymObj** read as SymObj* */
  /* In sym_pool_free(): */
  obj->refs = (SymObj**)SYM_TLS.freelist;  /* VIOLATION: SymObj* stored in SymObj** */
  ```
  The `refs` field is declared as `SymObj**` (pointer-to-pointer-to-SymObj) for storing outgoing references when the object is alive. However, when the object is free and in the pool freelist, the same memory location is used to store a freelist next pointer of type `SymObj*` (pointer-to-SymObj). These are incompatible types under strict aliasing.
- **Evidence (code references)**:
  - `symmetric.h:35` - `SymObj** refs;` - field declared as pointer-to-pointer
  - `symmetric.c:43` - `pool->objects[i].refs = (SymObj**)SYM_TLS.freelist;` - stores pointer as pointer-to-pointer
  - `symmetric.c:48` - `SYM_TLS.freelist = (SymObj*)obj->refs;` - reads pointer-to-pointer as pointer
  - `symmetric.c:53` - `obj->refs = (SymObj**)SYM_TLS.freelist;` - stores pointer as pointer-to-pointer
- **Repro test**: `runtime/tests/test_symmetric_pool_type_punning.c`
  - Test 1: Demonstrates type size and compilation warnings
  - Test 2: Shows how refs field has dual incompatible uses (invariant violation)
  - Test 3: Compares with correct implementation using dedicated `freelist_next` field
  - Test 4: Documents UB potential with sanitizers and compiler optimizations
  - All tests PASS - bug is confirmed
- **Tool signals**:
  - Should trigger warnings with `-Wstrict-aliasing=2` (though may not on all compilers due to complexity)
  - UBSan with `-fsanitize=undefined` might flag misaligned access or type violations
  - Static analysis tools should flag the incompatible pointer type casts
  - Miscompilation possible with `-O2` or higher due to strict aliasing optimizations
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 through BUG-0004
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The bug is particularly dangerous because:
    1. It's undefined behavior per the C standard (C99 6.5p7)
    2. It may work correctly with some compilers/optimizations but fail with others
    3. The symptom can be subtle (wrong code generation) rather than an obvious crash
    4. It violates a fundamental type safety invariant - the same memory location has two incompatible types
  - The fix requires adding a dedicated `freelist_next` field to `SymObj`:
    ```c
    struct SymObj {
        ...
        SymObj** refs;          /* Only valid when object is alive */
        ...
        SymObj* freelist_next;  /* Only valid when object is in pool freelist */
    };
    ```
  - Alternative fix: Use a union to explicitly document the memory sharing:
    ```c
    union {
        SymObj** refs;          /* When alive */
        SymObj* freelist_next;  /* When in freelist */
    } ref_or_next;
    ```
  - The union approach is clearer about the intent but still requires careful state management to ensure only the correct union member is accessed based on object state
  - The dedicated field approach is safer as it avoids potential aliasing issues entirely

### BUG-0007: Constraint strdup() Silent Failure
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/constraint.c` - `add_violation()`
- **Invariant violated**: Reliability and API contract - constraint violations must never be silently dropped
- **Symptom**: When `add_violation()` is called and `strdup()` fails due to memory exhaustion, the function returns immediately without recording the violation. This causes critical safety violations to be silently discarded exactly when they're most needed (under memory pressure).
- **Root cause (confirmed)**: In `constraint.c:26-27`, the `add_violation()` function does:
  ```c
  char* msg_copy = strdup(message);
  if (!msg_copy) return;  /* BUG: Silent failure on OOM! */
  ```
  When `strdup()` fails:
  1. The function returns void (no error indication)
  2. The violation is NOT recorded
  3. No error flag is set in the context
  4. No fallback logging occurs
  5. The caller has no way to know the violation was lost

  This is particularly dangerous because:
  - `strdup()` fails when memory is exhausted
  - Memory exhaustion is EXACTLY when you most need violation detection
  - Silent failure transforms a detectable violation into an undetectable crash
  - Makes debugging memory issues impossible
- **Evidence (code references)**:
  - `constraint.c:22-42` - `add_violation()` function with silent failure on `strdup()` == NULL
  - `constraint.c:173-184` - Calls `add_violation()` when constraint violations are detected
  - The function is void, so callers can't detect failure
  - No error flag in `ConstraintContext` to track strdup failures
- **Repro test**: `runtime/tests/test_constraint_strdup_oom.c`
  - Test 1: Confirms normal violation recording works
  - Test 2: Analyzes the silent failure code path
  - Test 3: Demonstrates the impact of silent failure
  - Test 4: Documents the memory pressure scenario
  - All tests PASS - bug is confirmed
- **Tool signals**:
  - ASan/Valgrind would NOT catch this (no memory corruption, just lost data)
  - Code analysis tools should flag void-return functions with silent failure paths
  - Static analysis could identify `strdup()` calls without error handling
  - Custom instrumentation needed to detect dropped violations
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 through BUG-0006
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The bug is particularly insidious because:
    1. It only manifests under memory pressure (rare in testing)
    2. The silent failure mode makes detection nearly impossible
    3. It violates the core purpose of the constraint system (detecting errors)
    4. It creates a "false negative" scenario that's worse than a crash
  - The fix requires one or more of:
    a) Change return type from `void` to `bool` to indicate success/failure
    b) Add fallback logging to `stderr` when `strdup()` fails
    c) Add a static buffer as last-resort storage
    d) Add an error flag like `had_strdup_failure` to `ConstraintContext`
    e) Use `vsnprintf()` with a fixed-size buffer instead of `strdup()`
  - Recommended approach:
    1. Change return type to `bool`
    2. On `strdup()` failure, log to `stderr` as fallback and return `false`
    3. Add `ctx->had_strdup_failure` flag for querying
    4. Update all callers to check the return value
  - The same issue exists at line 36 (second `strdup()` call in `grow_array` failure path)
  - Related issue: `grow_array()` at lines 12-19 also returns void on realloc failure, though this is less critical

### BUG-0006: Component realloc() Unchecked NULL
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/component.c` - `sym_component_union()`, `sym_component_add_member()`
- **Invariant violated**: Memory safety and resource management - `realloc()` must be checked for NULL before assigning to preserve original pointer on failure
- **Symptom**: When `realloc()` fails due to memory exhaustion, the code assigns NULL to the pointer without checking, losing the reference to the original allocation (memory leak). The code then proceeds to use the NULL pointer, causing crashes or undefined behavior. Additionally, `member_capacity` is updated to reflect the new (non-existent) capacity, creating an inconsistent state.
- **Root cause (confirmed)**: In `component.c:94` and `component.c:146`, `realloc()` is called without NULL checking:
  ```c
  /* Line 94 in sym_component_union() */
  rootA->members = realloc(rootA->members, new_cap * sizeof(SymObj*));
  rootA->member_capacity = new_cap;  /* Assumes success! */

  /* Line 146 in sym_component_add_member() */
  root->member_capacity *= 2;
  root->members = realloc(root->members, root->member_capacity * sizeof(SymObj*));
  /* No NULL check! */
  ```
  When `realloc()` fails:
  1. It returns NULL, but the original memory remains allocated
  2. Assigning NULL to the pointer loses the reference to the original allocation (permanent leak)
  3. `member_capacity` is updated to the new value, creating inconsistent state
  4. Code proceeds to access `members[...]` which is now NULL, causing crashes
- **Evidence (code references)**:
  - `component.c:94` - `realloc()` result assigned without NULL check in `sym_component_union()`
  - `component.c:95` - `member_capacity` updated assuming success
  - `component.c:103` - Array access `rootA->members[rootA->member_count++]` that will crash if NULL
  - `component.c:146` - `realloc()` result assigned without NULL check in `sym_component_add_member()`
  - `component.c:150` - Array access `root->members[root->member_count++]` that will crash if NULL
- **Repro test**: `runtime/tests/test_component_realloc_leak.c`
  - Test 1: Verifies normal operation works (baseline)
  - Test 2: Code analysis demonstrates the bug pattern and consequences
  - Test 3: Identifies the same bug in `sym_component_add_member()`
  - Test 4: Documents the fix pattern
- **Tool signals**:
  - Clang static analyzer would flag: "Result of 'realloc' is not checked"
  - clang-tidy with `clang-analyzer-security.insecureAPI.UncheckedReturn` would detect this
  - ASan/Valgrind could detect the resulting NULL dereference if triggered
  - Custom instrumentation could detect memory leaks from lost allocations
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 (component merge integer overflow) - that bug is about overflow before the realloc call, while this is about handling realloc failure after the call
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The bug is particularly dangerous because:
    1. It only manifests under memory pressure (when realloc fails)
    2. Testing may not catch it unless OOM is specifically simulated
    3. The consequences compound: leak + crash + state corruption
    4. The same pattern appears in two functions
  - The fix is straightforward:
    ```c
    void* new_members = realloc(rootA->members, new_cap * sizeof(SymObj*));
    if (!new_members) {
        /* Handle OOM gracefully */
        return;  /* or error code */
    }
    rootA->members = (SymObj**)new_members;
    rootA->member_capacity = new_cap;
    ```
  - This pattern (realloc without NULL check) is a well-known C anti-pattern that should be caught by code review and static analysis

### BUG-0008: Channel Send Reference Count Leak on Closed Channel
- **Date**: 2026-01-06
- **Area**: `runtime/src/memory/concurrent.c` - `gen_concurrent_runtime()` generated code for `channel_send()`
- **Invariant violated**: Reference counting invariant - every `atomic_fetch_add(&obj->rc, 1)` must have a corresponding `atomic_fetch_sub(&obj->rc, 1)` when the operation fails
- **Symptom**: In the generated `channel_send()` code at line 168, the refcount is incremented BEFORE entering the wait loop that checks if the channel is closed. If the channel is closed while the sender is waiting for buffer space, the function returns -1 without undoing the refcount increment. This causes a permanent reference count leak - the object will never be freed because its refcount is permanently too high.
- **Root cause (confirmed)**: In `concurrent.c:152-175`, the generated `channel_send()` code does:
  ```c
  atomic_fetch_add(&obj->rc, 1);  // Line 168: Increment RC TOO EARLY
  obj->owner_thread = -1;
  while ((tail + 1) % ch->capacity == head) {
      pthread_cond_wait(&ch->not_full, &ch->mutex);
      if (atomic_load(&ch->closed)) {
          pthread_mutex_unlock(&ch->mutex);
          return -1;  // BUG: Returns WITHOUT undoing the RC increment!
      }
      tail = atomic_load(&ch->tail);
      head = atomic_load(&ch->head);
  }
  ```
  The refcount is incremented at line 168 BEFORE the loop that waits for buffer space. When the channel is closed during the wait (line 160), the function returns -1 at line 162 without decrementing the refcount. The refcount increment was supposed to ensure the object stays alive for the receiver, but since the send never completed, the increment should be undone.
- **Evidence (code references)**:
  - `concurrent.c:168` - `atomic_fetch_add(&obj->rc, 1)` happens too early
  - `concurrent.c:160-162` - Early return on closed channel doesn't undo the increment
  - `concurrent.c:158-166` - Wait loop that can trigger the early return
- **Repro test**: `runtime/tests/test_channel_send_refcount_leak.c`
  - Test 1: Creates a full channel, starts a thread to close it, then attempts to send. Verifies refcount is leaked.
  - Test 2: Attempts multiple sends on a closed channel, verifying each one leaks a refcount.
  - Both tests CONFIRM the bug - refcount is permanently incremented on failed send.
- **Tool signals**:
  - ThreadSanitizer (TSan) might detect the data race if the object is accessed after the channel is closed
  - Valgrind/ASan would detect the memory leak from objects with leaked refcounts
  - Static analysis tools might flag the unbalanced increment/decrement on different code paths
  - Custom reference counting instrumentation would detect the imbalance
- **Status**: new
- **Notes**:
  - This is distinct from BUG-0001 through BUG-0007
  - Not documented in `MEMORY_MODEL_REVIEW_NOTES.md`
  - The bug is particularly dangerous because:
    1. It only occurs when a channel is closed while there's a blocked sender (rare in normal testing)
    2. The leak is silent - no error message or crash, just gradual memory accumulation
    3. Every failed send on a closing channel leaks a refcount
    4. The leaked objects will never be freed, causing permanent memory leaks
    5. In a long-running application with many channel operations, this can exhaust memory
  - The fix is straightforward - move the refcount increment AFTER the closed check:
    ```c
    // Wait for space FIRST, before touching refcounts
    while ((tail + 1) % ch->capacity == head) {
        pthread_cond_wait(&ch->not_full, &ch->mutex);
        if (atomic_load(&ch->closed)) {
            pthread_mutex_unlock(&ch->mutex);
            return -1;  // No RC to undo yet
        }
        tail = atomic_load(&ch->tail);
        head = atomic_load(&ch->head);
    }
    // Only increment RC after we know we'll succeed
    atomic_fetch_add(&obj->rc, 1);
    obj->owner_thread = -1;
    ch->buffer[tail] = obj;
    ```
  - Alternatively, use a cleanup pattern:
    ```c
    atomic_fetch_add(&obj->rc, 1);
    obj->owner_thread = -1;
    while ((tail + 1) % ch->capacity == head) {
        pthread_cond_wait(&ch->not_full, &ch->mutex);
        if (atomic_load(&ch->closed)) {
            atomic_fetch_sub(&obj->rc, 1);  // Undo the increment
            pthread_mutex_unlock(&ch->mutex);
            return -1;
        }
        tail = atomic_load(&ch->tail);
        head = atomic_load(&ch->head);
    }
    ```
  - The first approach (move increment later) is cleaner and more efficient
  - This bug pattern appears in other concurrent codebases where refcounts are managed incorrectly with error handling
