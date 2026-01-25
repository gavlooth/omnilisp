/*
 * test_strmap.c - Unit tests for string-keyed hash map
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../src/util/strmap.h"

/* Test counters */
static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) do { \
    printf("  Testing %s... ", #name); \
    tests_run++; \
    if (test_##name()) { \
        printf("PASS\n"); \
        tests_passed++; \
    } else { \
        printf("FAIL\n"); \
    } \
} while(0)

/* ========== Test Functions ========== */

static int test_create_free(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;
    if (strmap_size(map) != 0) return 0;
    strmap_free(map);
    return 1;
}

static int test_put_get(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value1 = 42;
    int value2 = 100;

    strmap_put(map, "key1", &value1);
    strmap_put(map, "key2", &value2);

    if (strmap_size(map) != 2) { strmap_free(map); return 0; }

    int* got1 = strmap_get(map, "key1");
    int* got2 = strmap_get(map, "key2");

    if (!got1 || *got1 != 42) { strmap_free(map); return 0; }
    if (!got2 || *got2 != 100) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_update(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value1 = 42;
    int value2 = 100;

    strmap_put(map, "key", &value1);
    strmap_put(map, "key", &value2);  /* Update same key */

    if (strmap_size(map) != 1) { strmap_free(map); return 0; }

    int* got = strmap_get(map, "key");
    if (!got || *got != 100) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_contains(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value = 42;
    strmap_put(map, "exists", &value);

    if (!strmap_contains(map, "exists")) { strmap_free(map); return 0; }
    if (strmap_contains(map, "missing")) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_remove(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value1 = 42;
    int value2 = 100;

    strmap_put(map, "key1", &value1);
    strmap_put(map, "key2", &value2);

    int* removed = strmap_remove(map, "key1");
    if (!removed || *removed != 42) { strmap_free(map); return 0; }
    if (strmap_size(map) != 1) { strmap_free(map); return 0; }
    if (strmap_contains(map, "key1")) { strmap_free(map); return 0; }
    if (!strmap_contains(map, "key2")) { strmap_free(map); return 0; }

    /* Remove non-existent key */
    if (strmap_remove(map, "missing") != NULL) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_clear(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value = 42;
    strmap_put(map, "a", &value);
    strmap_put(map, "b", &value);
    strmap_put(map, "c", &value);

    strmap_clear(map);

    if (strmap_size(map) != 0) { strmap_free(map); return 0; }
    if (strmap_contains(map, "a")) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_many_entries(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    /* Insert 1000 entries to trigger multiple resizes */
    int values[1000];
    char key[32];

    for (int i = 0; i < 1000; i++) {
        values[i] = i * 2;
        snprintf(key, sizeof(key), "key_%d", i);
        strmap_put(map, key, &values[i]);
    }

    if (strmap_size(map) != 1000) { strmap_free(map); return 0; }

    /* Verify all entries */
    for (int i = 0; i < 1000; i++) {
        snprintf(key, sizeof(key), "key_%d", i);
        int* got = strmap_get(map, key);
        if (!got || *got != i * 2) { strmap_free(map); return 0; }
    }

    strmap_free(map);
    return 1;
}

static int foreach_count = 0;
static int foreach_sum = 0;

static void count_callback(const char* key, void* value, void* ctx) {
    (void)key;
    (void)ctx;
    foreach_count++;
    foreach_sum += *(int*)value;
}

static int test_foreach(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int values[] = {1, 2, 3, 4, 5};
    strmap_put(map, "a", &values[0]);
    strmap_put(map, "b", &values[1]);
    strmap_put(map, "c", &values[2]);
    strmap_put(map, "d", &values[3]);
    strmap_put(map, "e", &values[4]);

    foreach_count = 0;
    foreach_sum = 0;
    strmap_foreach(map, count_callback, NULL);

    if (foreach_count != 5) { strmap_free(map); return 0; }
    if (foreach_sum != 15) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_null_safety(void) {
    /* These should not crash */
    strmap_free(NULL);
    strmap_put(NULL, "key", (void*)1);

    StrMap* map = strmap_new();
    strmap_put(map, NULL, (void*)1);
    if (strmap_get(map, NULL) != NULL) { strmap_free(map); return 0; }
    if (strmap_get(NULL, "key") != NULL) { strmap_free(map); return 0; }
    if (strmap_remove(NULL, "key") != NULL) { strmap_free(map); return 0; }
    if (strmap_remove(map, NULL) != NULL) { strmap_free(map); return 0; }
    if (strmap_contains(NULL, "key")) { strmap_free(map); return 0; }
    if (strmap_contains(map, NULL)) { strmap_free(map); return 0; }
    if (strmap_size(NULL) != 0) { strmap_free(map); return 0; }

    strmap_foreach(NULL, count_callback, NULL);
    strmap_foreach(map, NULL, NULL);
    strmap_clear(NULL);

    strmap_free(map);
    return 1;
}

static int test_collision_handling(void) {
    /* Test with keys that might hash to the same bucket */
    StrMap* map = strmap_with_capacity(4);  /* Small capacity to force collisions */
    if (!map) return 0;

    int values[10];
    char key[32];

    for (int i = 0; i < 10; i++) {
        values[i] = i;
        snprintf(key, sizeof(key), "key%d", i);
        strmap_put(map, key, &values[i]);
    }

    /* Verify all entries are accessible */
    for (int i = 0; i < 10; i++) {
        snprintf(key, sizeof(key), "key%d", i);
        int* got = strmap_get(map, key);
        if (!got || *got != i) { strmap_free(map); return 0; }
    }

    strmap_free(map);
    return 1;
}

static int test_empty_string_key(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    int value = 42;
    strmap_put(map, "", &value);

    int* got = strmap_get(map, "");
    if (!got || *got != 42) { strmap_free(map); return 0; }
    if (!strmap_contains(map, "")) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

static int test_long_key(void) {
    StrMap* map = strmap_new();
    if (!map) return 0;

    /* Create a very long key */
    char long_key[1024];
    memset(long_key, 'a', sizeof(long_key) - 1);
    long_key[sizeof(long_key) - 1] = '\0';

    int value = 42;
    strmap_put(map, long_key, &value);

    int* got = strmap_get(map, long_key);
    if (!got || *got != 42) { strmap_free(map); return 0; }

    strmap_free(map);
    return 1;
}

/* ========== Main ========== */

int main(void) {
    printf("Running strmap tests...\n");

    TEST(create_free);
    TEST(put_get);
    TEST(update);
    TEST(contains);
    TEST(remove);
    TEST(clear);
    TEST(many_entries);
    TEST(foreach);
    TEST(null_safety);
    TEST(collision_handling);
    TEST(empty_string_key);
    TEST(long_key);

    printf("\nResults: %d/%d tests passed\n", tests_passed, tests_run);

    return tests_passed == tests_run ? 0 : 1;
}
