# Test Coverage Summary: prim_memory_usage

## Date
2025-01-15

## Test File
`runtime/tests/test_memory_usage.c`

## Function Tested
`prim_memory_usage` - A debugging primitive that returns total memory allocated in bytes

## Location
`runtime/src/debug.c` (line ~505)

## Purpose
The `prim_memory_usage` function is a developer tool for profiling memory usage in OmniLisp programs. It:
- Tracks total bytes allocated across all regions
- Provides simple integer value representing total memory usage
- Helps identify memory-intensive operations

## Test Coverage

### Tests Added (7 tests)

1. **memory_usage_returns_int**
   - Verifies that function returns a valid immediate integer
   - Tests return type (should be immediate int, not boxed)
   - Ensures no NULL returns

2. **memory_usage_non_negative**
   - Ensures returned value is always >= 0
   - Tests data validity (memory cannot be negative)

3. **memory_usage_increases_after_alloc**
   - Verifies that memory usage increases after allocating objects
   - Tests tracking accuracy across allocations
   - Creates integers, symbols, and pairs to test various allocators

4. **memory_usage_is_idempotent**
   - Tests that calling function multiple times without allocations
     returns same value
   - Verifies no side effects from calling

5. **memory_usage_with_no_allocations**
   - Tests function behavior when no allocations exist
   - Verifies it works in empty state
   - Should return 0 or base value

6. **memory_usage_consistent_type**
   - Ensures function always returns immediate integer type
   - Tests type consistency across multiple calls
   - Verifies never returns boxed integer

7. **memory_usage_with_various_types**
   - Tests memory usage tracking with different object types:
     - Integers (various sizes)
     - Floats
     - Symbols (long names)
     - Lists/pairs
   - Verifies all allocation paths are tracked

## API Coverage

### Input Parameters
- None (void function)

### Return Value
- Immediate integer representing total bytes allocated
- Value is always >= 0

## Testing Strategy

### 1. Type Safety
- Verify return type is always `IS_IMMEDIATE_INT`
- Ensure never boxed (no `IS_BOXED`)
- Test no NULL returns

### 2. Data Integrity
- Check that values are always non-negative
- Verify monotonic behavior (usage increases with allocations)

### 3. Functional Correctness
- Test idempotency (same value if no allocations)
- Verify tracking across different object types
- Test with empty allocation state

### 4. Allocation Tracking
- Test that various allocators (ints, floats, symbols, pairs)
  are tracked correctly
- Verify that memory usage increases after allocations

## Implementation Notes

The test file includes:
- Custom test framework macros (TEST, RUN_TEST, ASSERT)
- Minimal dependencies on OmniLisp runtime
- Tests for both immediate and boxed object handling
- Comprehensive type coverage

## Compilation

```bash
cd runtime/tests
gcc -I../include -I../../csrc test_memory_usage.c -o test_memory_usage -L.. -lomni
```

## Execution

```bash
./test_memory_usage
```

Expected output:
```
=== prim_memory_usage Tests ===

--- Type Safety ---
  Testing memory_usage_returns_int... PASSED
  Testing memory_usage_non_negative... PASSED
  Testing memory_usage_consistent_type... PASSED

--- Functional Behavior ---
  Testing memory_usage_is_idempotent... PASSED
  Testing memory_usage_with_no_allocations... PASSED
  Testing memory_usage_increases_after_alloc... PASSED
  Testing memory_usage_with_various_types... PASSED

=== Test Results ===
Total: 7
Passed: 7
Failed: 0
ALL TESTS PASSED!
```

## Related Documentation
- `runtime/docs/README.md` - Runtime developer guide
- `runtime/src/debug.c` - Implementation file
- `TEST_LEAK_CHECK_SUMMARY.md` - Related leak check test

## Impact
This test adds coverage to a critical profiling primitive that helps developers:
- Track total memory allocation across program execution
- Identify memory-intensive operations
- Profile memory usage patterns
- Debug memory-related performance issues

The test ensures that the primitive:
- Returns correct type (immediate integer)
- Provides accurate byte counts
- Tracks all allocation types
- Works correctly in various states (empty, populated)
- Has consistent behavior (idempotent)

## Relationship to Other Tests

This test complements:
- `test_leak_check.c` - Tests leak detection (derived from same data)
- `test_debug_primitives.c` - Tests other debug tools
- `test_gc_info.c` - Tests garbage collection statistics

All three tests use the same global counter:
- `g_total_bytes_allocated` - Tracked by memory usage
- `g_total_allocs` - Used by leak check
- `g_total_frees` - Used by leak check
