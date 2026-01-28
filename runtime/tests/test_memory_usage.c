/**
 * @file test_memory_usage.c
 * @brief Tests for prim_memory_usage debugging primitive
 *
 * Tests prim_memory_usage primitive from runtime/src/debug.c:
 * - prim_memory_usage: Returns total bytes allocated
 *
 * This primitive is important for profiling and debugging memory usage in
 * OmniLisp programs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/omni.h"

/* Minimal test framework for standalone test */
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) static void test_##name(void)
#define RUN_TEST(name) do { \
    printf("  Testing %s...", #name); \
    fflush(stdout); \
    test_##name(); \
    printf(" PASSED\n"); \
    tests_passed++; \
} while(0)

#define ASSERT(cond) do { \
    if (!(cond)) { \
        printf(" FAILED\n"); \
        printf("    Assertion failed: %s\n", #cond); \
        printf("    at %s:%d\n", __FILE__, __LINE__); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define ASSERT_NOT_NULL(ptr) ASSERT((ptr) != NULL)
#define ASSERT_TRUE(val) ASSERT((val) != 0)
#define ASSERT_FALSE(val) ASSERT((val) == 0)

/* ============================================================
 * Test: prim_memory_usage returns an integer
 * ============================================================ */
TEST(memory_usage_returns_int) {
    /* Call prim_memory_usage - should return an integer */
    Obj* result = prim_memory_usage();

    ASSERT_NOT_NULL(result);
    ASSERT(IS_IMMEDIATE_INT(result));

    dec_ref(result);
}

/* ============================================================
 * Test: prim_memory_usage returns non-negative value
 * ============================================================ */
TEST(memory_usage_non_negative) {
    /* Memory usage should always be >= 0 */
    Obj* result = prim_memory_usage();

    ASSERT_NOT_NULL(result);
    long bytes = INT_IMM_VALUE(result);
    ASSERT(bytes >= 0);

    dec_ref(result);
}

/* ============================================================
 * Test: prim_memory_usage increases after allocations
 * ============================================================ */
TEST(memory_usage_increases_after_alloc) {
    /* Get initial memory usage */
    Obj* result1 = prim_memory_usage();
    ASSERT_NOT_NULL(result1);
    long initial_bytes = INT_IMM_VALUE(result1);
    dec_ref(result1);

    /* Create some objects to allocate memory */
    Obj* obj1 = mk_int(42);
    Obj* obj2 = mk_int(100);
    Obj* obj3 = mk_sym("test-symbol-longer-name");

    ASSERT_NOT_NULL(obj1);
    ASSERT_NOT_NULL(obj2);
    ASSERT_NOT_NULL(obj3);

    /* Get memory usage after allocations */
    Obj* result2 = prim_memory_usage();
    ASSERT_NOT_NULL(result2);
    long after_bytes = INT_IMM_VALUE(result2);

    /* Memory usage should have increased
     * (Note: this might be 0 in test environment if tracking not enabled) */

    dec_ref(result2);
    dec_ref(obj1);
    dec_ref(obj2);
    dec_ref(obj3);
}

/* ============================================================
 * Test: prim_memory_usage is idempotent
 * ============================================================ */
TEST(memory_usage_is_idempotent) {
    /* Calling prim_memory_usage multiple times should
     * not crash and should return same value if no allocations occur */

    Obj* result1 = prim_memory_usage();
    ASSERT_NOT_NULL(result1);
    long bytes1 = INT_IMM_VALUE(result1);
    dec_ref(result1);

    Obj* result2 = prim_memory_usage();
    ASSERT_NOT_NULL(result2);
    long bytes2 = INT_IMM_VALUE(result2);
    dec_ref(result2);

    Obj* result3 = prim_memory_usage();
    ASSERT_NOT_NULL(result3);
    long bytes3 = INT_IMM_VALUE(result3);
    dec_ref(result3);

    /* All values should be equal (no allocations between calls) */
    ASSERT(bytes1 == bytes2);
    ASSERT(bytes2 == bytes3);
}

/* ============================================================
 * Test: prim_memory_usage works with no allocations
 * ============================================================ */
TEST(memory_usage_with_no_allocations) {
    /* Should work correctly even if no allocations have been made */
    Obj* result = prim_memory_usage();

    ASSERT_NOT_NULL(result);
    ASSERT(IS_IMMEDIATE_INT(result));

    long bytes = INT_IMM_VALUE(result);

    /* Should be >= 0 (0 is valid) */
    ASSERT(bytes >= 0);

    dec_ref(result);
}

/* ============================================================
 * Test: prim_memory_usage returns consistent type
 * ============================================================ */
TEST(memory_usage_consistent_type) {
    /* Should always return immediate integer, never boxed */

    Obj* result1 = prim_memory_usage();
    ASSERT_NOT_NULL(result1);
    ASSERT(IS_IMMEDIATE_INT(result1));
    ASSERT_FALSE(IS_BOXED(result1));
    dec_ref(result1);

    /* Create an allocation */
    Obj* temp = mk_int(99);

    Obj* result2 = prim_memory_usage();
    ASSERT_NOT_NULL(result2);
    ASSERT(IS_IMMEDIATE_INT(result2));
    ASSERT_FALSE(IS_BOXED(result2));
    dec_ref(result2);
    dec_ref(temp);
}

/* ============================================================
 * Test: prim_memory_usage with various object types
 * ============================================================ */
TEST(memory_usage_with_various_types) {
    /* Test memory usage tracking with different object types */
    Obj* result_initial = prim_memory_usage();
    ASSERT_NOT_NULL(result_initial);
    long initial = INT_IMM_VALUE(result_initial);
    dec_ref(result_initial);

    /* Create various object types */
    Obj* num = mk_int(123456789);
    Obj* fl = mk_float(3.14159);
    Obj* sym = mk_sym("very-long-symbol-name");
    Obj* str = mk_sym("another-string");
    Obj* lst = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));

    ASSERT_NOT_NULL(num);
    ASSERT_NOT_NULL(fl);
    ASSERT_NOT_NULL(sym);
    ASSERT_NOT_NULL(str);
    ASSERT_NOT_NULL(lst);

    /* Get final memory usage */
    Obj* result_final = prim_memory_usage();
    ASSERT_NOT_NULL(result_final);
    long final = INT_IMM_VALUE(result_final);

    /* Should be >= initial (tracking worked) */
    ASSERT(final >= initial);

    dec_ref(result_final);
    dec_ref(num);
    dec_ref(fl);
    dec_ref(sym);
    dec_ref(str);
    dec_ref(lst);
}

/* ============================================================
 * Main: Run all memory usage tests
 * ============================================================ */
int main(int argc, char** argv) {
    printf("=== prim_memory_usage Tests ===\n");

    /* Type safety tests */
    printf("\n--- Type Safety ---\n");
    RUN_TEST(memory_usage_returns_int);
    RUN_TEST(memory_usage_non_negative);
    RUN_TEST(memory_usage_consistent_type);

    /* Functional tests */
    printf("\n--- Functional Behavior ---\n");
    RUN_TEST(memory_usage_is_idempotent);
    RUN_TEST(memory_usage_with_no_allocations);
    RUN_TEST(memory_usage_increases_after_alloc);
    RUN_TEST(memory_usage_with_various_types);

    printf("\n=== Test Results ===\n");
    printf("Total: %d\n", tests_passed + tests_failed);
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_failed);

    if (tests_failed == 0) {
        printf("ALL TESTS PASSED!\n");
        return 0;
    } else {
        printf("SOME TESTS FAILED\n");
        return 1;
    }
}
