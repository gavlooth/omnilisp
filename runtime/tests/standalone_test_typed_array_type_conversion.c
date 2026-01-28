/*
 * standalone_test_typed_array_type_conversion.c - Standalone runner for type conversion tests
 *
 * This is a minimal test runner that exercises just the new typed array
 * type conversion tests to verify they work correctly.
 */

#include "../include/typed_array.h"
#include <stdio.h>
#include <string.h>

/* Simple test framework */
int tests_run = 0;
int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) \
    do { \
        tests_run++; \
        printf("  %s: ", name); \
        fflush(stdout); \
    } while(0)

#define PASS() \
    do { \
        tests_passed++; \
        printf("PASS\n"); \
    } while(0)

#define FAIL(msg) \
    do { \
        tests_failed++; \
        printf("FAIL - %s\n", msg); \
    } while(0)

#define ASSERT(cond) \
    do { \
        if (!(cond)) { \
            FAIL(#cond); \
            return; \
        } \
    } while(0)

#define ASSERT_STR_EQ(a, b) \
    do { \
        if (strcmp((a), (b)) != 0) { \
            char _msg[256]; \
            snprintf(_msg, sizeof(_msg), "'%s' != '%s'", (a), (b)); \
            FAIL(_msg); \
            return; \
        } \
    } while(0)

#define ASSERT_EQ(a, b) \
    do { \
        if ((a) != (b)) { \
            char _msg[256]; \
            snprintf(_msg, sizeof(_msg), "%s != %s", #a, #b); \
            FAIL(_msg); \
            return; \
        } \
    } while(0)

#define TEST_SUMMARY() \
    do { \
        printf("\n=== Summary ===\n"); \
        printf("  Total:  %d\n", tests_run); \
        printf("  Passed: %d\n", tests_passed); \
        printf("  Failed: %d\n", tests_failed); \
        printf("\n"); \
    } while(0)

/* Test functions */
void test_type_name_int64(void) {
    TEST("type_name Int64");
    const char* name = omni_typed_array_type_name(ARRAY_TYPE_INT64);
    ASSERT_STR_EQ(name, "Int64");
    PASS();
}

void test_type_name_float64(void) {
    TEST("type_name Float64");
    const char* name = omni_typed_array_type_name(ARRAY_TYPE_FLOAT64);
    ASSERT_STR_EQ(name, "Float64");
    PASS();
}

void test_type_name_char(void) {
    TEST("type_name Char");
    const char* name = omni_typed_array_type_name(ARRAY_TYPE_CHAR);
    ASSERT_STR_EQ(name, "Char");
    PASS();
}

void test_type_name_bool(void) {
    TEST("type_name Bool");
    const char* name = omni_typed_array_type_name(ARRAY_TYPE_BOOL);
    ASSERT_STR_EQ(name, "Bool");
    PASS();
}

void test_type_from_string_int(void) {
    TEST("type_from_string 'Int'");
    ArrayElementType type = omni_typed_array_type_from_string("Int");
    ASSERT_EQ(type, ARRAY_TYPE_INT64);
    PASS();
}

void test_type_from_string_float(void) {
    TEST("type_from_string 'Float'");
    ArrayElementType type = omni_typed_array_type_from_string("Float");
    ASSERT_EQ(type, ARRAY_TYPE_FLOAT64);
    PASS();
}

void test_type_from_string_char(void) {
    TEST("type_from_string 'Char'");
    ArrayElementType type = omni_typed_array_type_from_string("Char");
    ASSERT_EQ(type, ARRAY_TYPE_CHAR);
    PASS();
}

void test_type_from_string_bool(void) {
    TEST("type_from_string 'Bool'");
    ArrayElementType type = omni_typed_array_type_from_string("Bool");
    ASSERT_EQ(type, ARRAY_TYPE_BOOL);
    PASS();
}

void test_type_from_string_null(void) {
    TEST("type_from_string NULL");
    ArrayElementType type = omni_typed_array_type_from_string(NULL);
    ASSERT_EQ(type, ARRAY_TYPE_UNKNOWN);
    PASS();
}

/* Main */
int main(void) {
    printf("Typed Array Type Conversion Tests\n");
    printf("=================================\n\n");

    printf("=== omni_typed_array_type_name ===\n");
    test_type_name_int64();
    test_type_name_float64();
    test_type_name_char();
    test_type_name_bool();

    printf("\n=== omni_typed_array_type_from_string ===\n");
    test_type_from_string_int();
    test_type_from_string_float();
    test_type_from_string_char();
    test_type_from_string_bool();
    test_type_from_string_null();

    TEST_SUMMARY();

    return tests_failed > 0 ? 1 : 0;
}
