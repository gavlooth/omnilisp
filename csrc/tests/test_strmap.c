/*
 * test_strmap.c
 *
 * Tests for string-keyed hash map (StrMap) utility.
 *
 * Purpose:
 *   Verify that StrMap correctly stores, retrieves, updates, and removes
 *   key-value pairs with O(1) average-case performance.
 *
 * Why this matters:
 *   StrMap is a core data structure used throughout the compiler for
 *   symbol tables, type environments, and metadata storage. Incorrect
 *   behavior leads to compilation errors, memory leaks, or crashes.
 *
 * Contract:
 *   - strmap_put: Inserts new keys, updates existing keys
 *   - strmap_get: Returns value for key, NULL if not found
 *   - strmap_remove: Removes key and returns value, NULL if not found
 *   - strmap_size: Returns count of entries
 *   - strmap_clear: Removes all entries
 *   - All functions handle NULL map/NULL key gracefully
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
#include <assert.h>

#include "../util/strmap.h"

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

/* Test values for tracking */
static int val1 = 100;
static int val2 = 200;
static int val3 = 300;

/* ========== Test: Basic put and get operations ========== */

TEST(test_strmap_basic_put_get) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);
    ASSERT(strmap_size(map) == 0);

    /* Insert first key-value pair */
    strmap_put(map, "key1", &val1);
    ASSERT(strmap_size(map) == 1);

    /* Retrieve the value */
    void* result = strmap_get(map, "key1");
    ASSERT(result == &val1);

    /* Verify non-existent key returns NULL */
    result = strmap_get(map, "nonexistent");
    ASSERT(result == NULL);

    strmap_free(map);
}

/* ========== Test: Update existing key ========== */

TEST(test_strmap_update_key) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Insert initial value */
    strmap_put(map, "key", &val1);
    void* result = strmap_get(map, "key");
    ASSERT(result == &val1);
    ASSERT(strmap_size(map) == 1);

    /* Update with new value */
    strmap_put(map, "key", &val2);
    result = strmap_get(map, "key");
    ASSERT(result == &val2);
    /* Size should still be 1 (not incremented) */
    ASSERT(strmap_size(map) == 1);

    strmap_free(map);
}

/* ========== Test: Multiple keys ========== */

TEST(test_strmap_multiple_keys) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Insert multiple keys */
    strmap_put(map, "alpha", &val1);
    strmap_put(map, "beta", &val2);
    strmap_put(map, "gamma", &val3);

    ASSERT(strmap_size(map) == 3);

    /* Verify each key */
    ASSERT(strmap_get(map, "alpha") == &val1);
    ASSERT(strmap_get(map, "beta") == &val2);
    ASSERT(strmap_get(map, "gamma") == &val3);

    strmap_free(map);
}

/* ========== Test: Remove key ========== */

TEST(test_strmap_remove_key) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Insert keys */
    strmap_put(map, "key1", &val1);
    strmap_put(map, "key2", &val2);
    ASSERT(strmap_size(map) == 2);

    /* Remove one key */
    void* removed = strmap_remove(map, "key1");
    ASSERT(removed == &val1);
    ASSERT(strmap_size(map) == 1);

    /* Verify key is gone */
    void* result = strmap_get(map, "key1");
    ASSERT(result == NULL);

    /* Verify other key still exists */
    result = strmap_get(map, "key2");
    ASSERT(result == &val2);

    /* Remove non-existent key returns NULL */
    removed = strmap_remove(map, "nonexistent");
    ASSERT(removed == NULL);
    ASSERT(strmap_size(map) == 1);

    strmap_free(map);
}

/* ========== Test: Clear all entries ========== */

TEST(test_strmap_clear) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Insert multiple keys */
    strmap_put(map, "a", &val1);
    strmap_put(map, "b", &val2);
    strmap_put(map, "c", &val3);
    ASSERT(strmap_size(map) == 3);

    /* Clear all entries */
    strmap_clear(map);
    ASSERT(strmap_size(map) == 0);

    /* Verify all keys are gone */
    ASSERT(strmap_get(map, "a") == NULL);
    ASSERT(strmap_get(map, "b") == NULL);
    ASSERT(strmap_get(map, "c") == NULL);

    /* Can still insert after clear */
    strmap_put(map, "new", &val1);
    ASSERT(strmap_size(map) == 1);
    ASSERT(strmap_get(map, "new") == &val1);

    strmap_free(map);
}

/* ========== Test: Custom capacity ========== */

TEST(test_strmap_custom_capacity) {
    /* Create map with custom capacity */
    StrMap* map = strmap_with_capacity(32);
    ASSERT(map != NULL);
    ASSERT(strmap_size(map) == 0);

    /* Should work like normal map */
    strmap_put(map, "key", &val1);
    ASSERT(strmap_get(map, "key") == &val1);

    strmap_free(map);

    /* Small capacity should be clamped to minimum (16) */
    map = strmap_with_capacity(5);
    ASSERT(map != NULL);

    strmap_put(map, "key", &val1);
    ASSERT(strmap_get(map, "key") == &val1);

    strmap_free(map);
}

/* ========== Test: NULL handling ========== */

TEST(test_strmap_null_handling) {
    /* NULL map should not crash */
    strmap_put(NULL, "key", &val1);
    ASSERT(strmap_get(NULL, "key") == NULL);
    ASSERT(strmap_remove(NULL, "key") == NULL);
    ASSERT(strmap_size(NULL) == 0);
    strmap_clear(NULL);
    ASSERT(strmap_had_alloc_failure(NULL) == 0);

    /* NULL key should not crash */
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    strmap_put(map, NULL, &val1);
    ASSERT(strmap_get(map, NULL) == NULL);
    ASSERT(strmap_remove(map, NULL) == NULL);

    strmap_free(map);
}

/* ========== Test: strmap_contains ========== */

TEST(test_strmap_contains) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Empty map */
    ASSERT(strmap_contains(map, "key") == false);

    /* After insert */
    strmap_put(map, "key", &val1);
    ASSERT(strmap_contains(map, "key") == true);
    ASSERT(strmap_contains(map, "other") == false);

    /* After remove */
    strmap_remove(map, "key");
    ASSERT(strmap_contains(map, "key") == false);

    strmap_free(map);
}

/* ========== Test: Empty strings ========== */

TEST(test_strmap_empty_string_key) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Empty string should be valid key */
    strmap_put(map, "", &val1);
    ASSERT(strmap_size(map) == 1);
    ASSERT(strmap_get(map, "") == &val1);
    ASSERT(strmap_contains(map, "") == true);

    void* removed = strmap_remove(map, "");
    ASSERT(removed == &val1);
    ASSERT(strmap_contains(map, "") == false);

    strmap_free(map);
}

/* ========== Test: String keys with special chars ========== */

TEST(test_strmap_special_chars) {
    StrMap* map = strmap_new();
    ASSERT(map != NULL);

    /* Keys with various characters */
    strmap_put(map, "key-with-dash", &val1);
    strmap_put(map, "key_with_underscore", &val2);
    strmap_put(map, "key.with.dot", &val3);

    ASSERT(strmap_get(map, "key-with-dash") == &val1);
    ASSERT(strmap_get(map, "key_with_underscore") == &val2);
    ASSERT(strmap_get(map, "key.with.dot") == &val3);

    strmap_free(map);
}

/* ========== Main ========== */

int main(void) {
    printf("=== StrMap Tests ===\n");

    RUN_TEST(test_strmap_basic_put_get);
    RUN_TEST(test_strmap_update_key);
    RUN_TEST(test_strmap_multiple_keys);
    RUN_TEST(test_strmap_remove_key);
    RUN_TEST(test_strmap_clear);
    RUN_TEST(test_strmap_custom_capacity);
    RUN_TEST(test_strmap_null_handling);
    RUN_TEST(test_strmap_contains);
    RUN_TEST(test_strmap_empty_string_key);
    RUN_TEST(test_strmap_special_chars);

    printf("\n=== Test Results ===\n");
    printf("Total: %d\n", tests_run);
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_run - tests_passed);

    if (tests_run == tests_passed) {
        printf("\nALL TESTS PASSED!\n");
        return 0;
    } else {
        printf("\nSOME TESTS FAILED!\n");
        return 1;
    }
}
