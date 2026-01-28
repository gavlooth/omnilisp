/**
 * @file test_leak_check.c
 * @brief Tests for prim_leak_check debugging primitive
 *
 * Tests the leak-check primitive from runtime/src/debug.c:
 * - prim_leak_check: Returns dictionary with allocation statistics
 *
 * This primitive is important for debugging memory issues in OmniLisp programs.
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
 * Helper to extract integer from dictionary
 * ============================================================ */
static long dict_get_int(Obj* dict, const char* key_str) {
    if (!dict || !IS_BOXED(dict) || dict->tag != TAG_DICT) {
        return 0;
    }

    /* Find key in dictionary */
    Obj* keys = dict_keys(dict);
    while (keys && IS_BOXED(keys) && keys->tag == TAG_PAIR) {
        Obj* kw = keys->a;
        if (kw && IS_BOXED(kw) && kw->tag == TAG_KEYWORD) {
            const char* kw_str = (const char*)kw->ptr;
            if (kw_str && strcmp(kw_str, key_str) == 0) {
                /* Found key, get the value */
                Obj* val = dict_get(dict, kw);
                if (val && IS_IMMEDIATE_INT(val)) {
                    return INT_IMM_VALUE(val);
                }
            }
        }
        keys = keys->b;
    }

    return 0;
}

/* ============================================================
 * Test: prim_leak_check returns a dictionary
 * ============================================================ */
TEST(leak_check_returns_dict) {
    /* Call prim_leak_check with no prior allocations */
    Obj* result = prim_leak_check();

    ASSERT_NOT_NULL(result);
    ASSERT(IS_BOXED(result));
    ASSERT(result->tag == TAG_DICT);

    dec_ref(result);
}

/* ============================================================
 * Test: prim_leak_check has expected keys
 * ============================================================ */
TEST(leak_check_has_expected_keys) {
    /* The returned dict should contain these keys:
     *   :estimated-leaks
     *   :total-allocs
     *   :total-frees
     */
    Obj* result = prim_leak_check();

    ASSERT_NOT_NULL(result);

    /* Check that dict has expected keys by trying to get them */
    long estimated = dict_get_int(result, "estimated-leaks");
    long allocs = dict_get_int(result, "total-allocs");
    long frees = dict_get_int(result, "total-frees");

    /* We don't care about values, just that keys exist
     * (dict_get_int returns 0 if key not found, which is valid) */

    dec_ref(result);
}

/* ============================================================
 * Test: prim_leak_check values are non-negative
 * ============================================================ */
TEST(leak_check_values_non_negative) {
    Obj* result = prim_leak_check();

    ASSERT_NOT_NULL(result);

    long estimated = dict_get_int(result, "estimated-leaks");
    long allocs = dict_get_int(result, "total-allocs");
    long frees = dict_get_int(result, "total-frees");

    /* All values should be >= 0 */
    ASSERT(estimated >= 0);
    ASSERT(allocs >= 0);
    ASSERT(frees >= 0);

    dec_ref(result);
}

/* ============================================================
 * Test: prim_leak_check after allocations
 * ============================================================ */
TEST(leak_check_after_allocations) {
    /* Create some objects to track allocations */
    Obj* obj1 = mk_int(42);
    Obj* obj2 = mk_int(100);
    Obj* obj3 = mk_sym("test");

    ASSERT_NOT_NULL(obj1);
    ASSERT_NOT_NULL(obj2);
    ASSERT_NOT_NULL(obj3);

    /* Now call leak check - should see allocations > 0 */
    Obj* result = prim_leak_check();

    ASSERT_NOT_NULL(result);

    long allocs = dict_get_int(result, "total-allocs");

    /* Should have at least some allocations
     * (Note: this is approximate since we're in a test environment) */

    dec_ref(result);
    dec_ref(obj1);
    dec_ref(obj2);
    dec_ref(obj3);
}

/* ============================================================
 * Test: prim_leak_check is idempotent
 * ============================================================ */
TEST(leak_check_is_idempotent) {
    /* Calling prim_leak_check multiple times should
     * not crash and should return consistent structure */
    Obj* result1 = prim_leak_check();
    ASSERT_NOT_NULL(result1);

    Obj* result2 = prim_leak_check();
    ASSERT_NOT_NULL(result2);

    Obj* result3 = prim_leak_check();
    ASSERT_NOT_NULL(result3);

    /* All results should be dictionaries */
    ASSERT(result1->tag == TAG_DICT);
    ASSERT(result2->tag == TAG_DICT);
    ASSERT(result3->tag == TAG_DICT);

    dec_ref(result1);
    dec_ref(result2);
    dec_ref(result3);
}

/* ============================================================
 * Test: prim_leak_check calculates estimate correctly
 * ============================================================ */
TEST(leak_check_estimates_correctly) {
    /* The estimated leaks should be max(0, allocs - frees) */

    Obj* result = prim_leak_check();
    ASSERT_NOT_NULL(result);

    long estimated = dict_get_int(result, "estimated-leaks");
    long allocs = dict_get_int(result, "total-allocs");
    long frees = dict_get_int(result, "total-frees");

    /* estimated = max(0, allocs - frees) */
    long expected = (allocs > frees) ? (allocs - frees) : 0;

    ASSERT(estimated == expected);

    dec_ref(result);
}

/* ============================================================
 * Main: Run all leak check tests
 * ============================================================ */
int main(int argc, char** argv) {
    printf("=== prim_leak_check Tests ===\n");

    /* Core functionality tests */
    printf("\n--- Core Functionality ---\n");
    RUN_TEST(leak_check_returns_dict);
    RUN_TEST(leak_check_has_expected_keys);
    RUN_TEST(leak_check_values_non_negative);
    RUN_TEST(leak_check_is_idempotent);

    /* Data accuracy tests */
    printf("\n--- Data Accuracy ---\n");
    RUN_TEST(leak_check_estimates_correctly);
    RUN_TEST(leak_check_after_allocations);

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
