/*
 * test_typed_array_type_conversion.c - Test typed array type conversion functions
 *
 * Tests utility functions for converting between type enums and string names:
 * - omni_typed_array_type_name: Get string name for type enum
 * - omni_typed_array_type_from_string: Parse string name to type enum
 *
 * These functions are critical for user-facing typed array APIs where
 * types are specified as strings and converted internally to enums.
 *
 * Coverage: runtime/src/typed_array.c:42-78
 */

#include "test_framework.h"
#include "../include/typed_array.h"
#include <string.h>

/* Forward declarations - typed_array.c is included in test_main.c */
const char* omni_typed_array_type_name(ArrayElementType type);
ArrayElementType omni_typed_array_type_from_string(const char* name);

/* ============================================================================
 * Test omni_typed_array_type_name
 * ============================================================================ */

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

void test_type_name_unknown(void) {
    TEST("type_name Unknown");
    const char* name = omni_typed_array_type_name(ARRAY_TYPE_UNKNOWN);
    ASSERT_STR_EQ(name, "Unknown");
    PASS();
}

void test_type_name_invalid_enum(void) {
    TEST("type_name invalid enum (999)");
    /* Test behavior with invalid enum value - should return "Unknown" */
    const char* name = omni_typed_array_type_name((ArrayElementType)999);
    ASSERT_STR_EQ(name, "Unknown");
    PASS();
}

/* ============================================================================
 * Test omni_typed_array_type_from_string
 * ============================================================================ */

void test_type_from_string_int(void) {
    TEST("type_from_string 'Int'");
    ArrayElementType type = omni_typed_array_type_from_string("Int");
    ASSERT_EQ(type, ARRAY_TYPE_INT64);
    PASS();
}

void test_type_from_string_int64(void) {
    TEST("type_from_string 'Int64'");
    ArrayElementType type = omni_typed_array_type_from_string("Int64");
    ASSERT_EQ(type, ARRAY_TYPE_INT64);
    PASS();
}

void test_type_from_string_int64_t(void) {
    TEST("type_from_string 'int64_t'");
    ArrayElementType type = omni_typed_array_type_from_string("int64_t");
    ASSERT_EQ(type, ARRAY_TYPE_INT64);
    PASS();
}

void test_type_from_string_float(void) {
    TEST("type_from_string 'Float'");
    ArrayElementType type = omni_typed_array_type_from_string("Float");
    ASSERT_EQ(type, ARRAY_TYPE_FLOAT64);
    PASS();
}

void test_type_from_string_float64(void) {
    TEST("type_from_string 'Float64'");
    ArrayElementType type = omni_typed_array_type_from_string("Float64");
    ASSERT_EQ(type, ARRAY_TYPE_FLOAT64);
    PASS();
}

void test_type_from_string_double(void) {
    TEST("type_from_string 'double'");
    ArrayElementType type = omni_typed_array_type_from_string("double");
    ASSERT_EQ(type, ARRAY_TYPE_FLOAT64);
    PASS();
}

void test_type_from_string_char(void) {
    TEST("type_from_string 'Char'");
    ArrayElementType type = omni_typed_array_type_from_string("Char");
    ASSERT_EQ(type, ARRAY_TYPE_CHAR);
    PASS();
}

void test_type_from_string_char_lower(void) {
    TEST("type_from_string 'char'");
    ArrayElementType type = omni_typed_array_type_from_string("char");
    ASSERT_EQ(type, ARRAY_TYPE_CHAR);
    PASS();
}

void test_type_from_string_bool(void) {
    TEST("type_from_string 'Bool'");
    ArrayElementType type = omni_typed_array_type_from_string("Bool");
    ASSERT_EQ(type, ARRAY_TYPE_BOOL);
    PASS();
}

void test_type_from_string_bool_lower(void) {
    TEST("type_from_string 'bool'");
    ArrayElementType type = omni_typed_array_type_from_string("bool");
    ASSERT_EQ(type, ARRAY_TYPE_BOOL);
    PASS();
}

void test_type_from_string_null(void) {
    TEST("type_from_string NULL");
    ArrayElementType type = omni_typed_array_type_from_string(NULL);
    ASSERT_EQ(type, ARRAY_TYPE_UNKNOWN);
    PASS();
}

void test_type_from_string_empty(void) {
    TEST("type_from_string empty string");
    ArrayElementType type = omni_typed_array_type_from_string("");
    ASSERT_EQ(type, ARRAY_TYPE_UNKNOWN);
    PASS();
}

void test_type_from_string_invalid(void) {
    TEST("type_from_string invalid type");
    ArrayElementType type = omni_typed_array_type_from_string("InvalidType");
    ASSERT_EQ(type, ARRAY_TYPE_UNKNOWN);
    PASS();
}

void test_type_from_string_case_sensitive(void) {
    TEST("type_from_string case sensitivity");
    /* Lowercase 'int' should NOT match (case sensitive) */
    ArrayElementType type = omni_typed_array_type_from_string("int");
    ASSERT_EQ(type, ARRAY_TYPE_UNKNOWN);
    PASS();
}

/* ============================================================================
 * Test suite runner
 * ============================================================================ */

void run_typed_array_type_conversion_tests(void) {
    TEST_SUITE("Typed Array Type Conversion Tests");

    TEST_SECTION("omni_typed_array_type_name");
    test_type_name_int64();
    test_type_name_float64();
    test_type_name_char();
    test_type_name_bool();
    test_type_name_unknown();
    test_type_name_invalid_enum();

    TEST_SECTION("omni_typed_array_type_from_string");
    test_type_from_string_int();
    test_type_from_string_int64();
    test_type_from_string_int64_t();
    test_type_from_string_float();
    test_type_from_string_float64();
    test_type_from_string_double();
    test_type_from_string_char();
    test_type_from_string_char_lower();
    test_type_from_string_bool();
    test_type_from_string_bool_lower();
    test_type_from_string_null();
    test_type_from_string_empty();
    test_type_from_string_invalid();
    test_type_from_string_case_sensitive();
}
