/*
 * test_alloc_strategy_name.c
 *
 * Tests for omni_alloc_strategy_name function.
 *
 * Purpose:
 *   Verify that omni_alloc_strategy_name correctly maps AllocStrategy enum
 *   values to their canonical string representations.
 *
 * Why this matters:
 *   The alloc_strategy_name function is used throughout the compiler for
 *   debugging, error messages, and code generation comments. Incorrect mappings
 *   could cause confusing debug output or incorrect code generation.
 *
 * Contract:
 *   - ALLOC_HEAP maps to "heap"
 *   - ALLOC_STACK maps to "stack"
 *   - ALLOC_POOL maps to "pool"
 *   - ALLOC_ARENA maps to "arena"
 *   - Invalid enum values map to "unknown"
 */

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../analysis/analysis.h"

/* Test counters */
static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) static void name(void)
#define RUN_TEST(name) do { \
    printf("  %s: ", #name); \
    name(); \
    tests_run++; \
    tests_passed++; \
    printf("\033[32mPASS\033[0m\n"); \
} while(0)

#define ASSERT(cond) do { \
    if (!(cond)) { \
        printf("\033[31mFAIL\033[0m (line %d: %s)\n", __LINE__, #cond); \
        tests_run++; \
        return; \
    } \
} while(0)

/* ========== Test: Basic alloc strategy name mappings ========== */

TEST(test_alloc_strategy_heap) {
    const char* name = omni_alloc_strategy_name(ALLOC_HEAP);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "heap") == 0);
}

TEST(test_alloc_strategy_stack) {
    const char* name = omni_alloc_strategy_name(ALLOC_STACK);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "stack") == 0);
}

TEST(test_alloc_strategy_pool) {
    const char* name = omni_alloc_strategy_name(ALLOC_POOL);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "pool") == 0);
}

TEST(test_alloc_strategy_arena) {
    const char* name = omni_alloc_strategy_name(ALLOC_ARENA);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "arena") == 0);
}

TEST(test_alloc_strategy_invalid) {
    /* Test that invalid enum value returns "unknown" */
    AllocStrategy invalid = (AllocStrategy)999;
    const char* name = omni_alloc_strategy_name(invalid);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "unknown") == 0);
}

/* ========== Test: Edge case - zero value ========== */

TEST(test_alloc_strategy_zero) {
    /* ALLOC_HEAP = 0, should still return "heap" */
    const char* name = omni_alloc_strategy_name((AllocStrategy)0);
    ASSERT(name != NULL);
    ASSERT(strcmp(name, "heap") == 0);
}

/* ========== Test: All distinct values ========== */

TEST(test_alloc_strategy_all_distinct) {
    /* Verify that all strategies return distinct strings */
    const char* heap_name = omni_alloc_strategy_name(ALLOC_HEAP);
    const char* stack_name = omni_alloc_strategy_name(ALLOC_STACK);
    const char* pool_name = omni_alloc_strategy_name(ALLOC_POOL);
    const char* arena_name = omni_alloc_strategy_name(ALLOC_ARENA);

    ASSERT(strcmp(heap_name, stack_name) != 0);
    ASSERT(strcmp(heap_name, pool_name) != 0);
    ASSERT(strcmp(heap_name, arena_name) != 0);
    ASSERT(strcmp(stack_name, pool_name) != 0);
    ASSERT(strcmp(stack_name, arena_name) != 0);
    ASSERT(strcmp(pool_name, arena_name) != 0);
}

/* ========== Main Test Runner ========== */

int main(void) {
    printf("=== Alloc Strategy Name Tests ===\n");

    RUN_TEST(test_alloc_strategy_heap);
    RUN_TEST(test_alloc_strategy_stack);
    RUN_TEST(test_alloc_strategy_pool);
    RUN_TEST(test_alloc_strategy_arena);
    RUN_TEST(test_alloc_strategy_invalid);
    RUN_TEST(test_alloc_strategy_zero);
    RUN_TEST(test_alloc_strategy_all_distinct);

    printf("\n");
    printf("Tests run: %d\n", tests_run);
    printf("Tests passed: %d\n", tests_passed);

    if (tests_run == tests_passed) {
        printf("\033[32mALL TESTS PASSED\033[0m\n");
        return 0;
    } else {
        printf("\033[31mSOME TESTS FAILED\033[0m\n");
        return 1;
    }
}
