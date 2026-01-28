/*
 * test_typed_array_element_size.c - Test omni_typed_array_element_size function
 *
 * Tests utility function that returns element size for each typed array type.
 * This is critical for correct memory layout calculations.
 *
 * Coverage: runtime/src/typed_array.c:31-39
 */

#include "test_framework.h"
#include <stddef.h>
#include <stdint.h>

/* Test each supported array element type */
void test_int64_element_size(void) {
    TEST("Int64 element size");
    size_t size = omni_typed_array_element_size(ARRAY_TYPE_INT64);
    ASSERT_EQ(size, sizeof(int64_t));
    PASS();
}

void test_float64_element_size(void) {
    TEST("Float64 element size");
    size_t size = omni_typed_array_element_size(ARRAY_TYPE_FLOAT64);
    ASSERT_EQ(size, sizeof(double));
    PASS();
}

void test_char_element_size(void) {
    TEST("Char element size");
    size_t size = omni_typed_array_element_size(ARRAY_TYPE_CHAR);
    ASSERT_EQ(size, sizeof(char));
    PASS();
}

void test_bool_element_size(void) {
    TEST("Bool element size");
    size_t size = omni_typed_array_element_size(ARRAY_TYPE_BOOL);
    ASSERT_EQ(size, sizeof(_Bool));
    PASS();
}

void test_unknown_type_element_size(void) {
    TEST("Unknown type element size (defaults to sizeof(void*))");
    size_t size = omni_typed_array_element_size(ARRAY_TYPE_UNKNOWN);
    ASSERT_EQ(size, sizeof(void*));
    PASS();
}

/* Test suite runner */
void run_typed_array_element_size_tests(void) {
    TEST_SUITE("Typed Array Element Size Tests");

    TEST_SECTION("Basic Type Sizes");
    test_int64_element_size();
    test_float64_element_size();
    test_char_element_size();
    test_bool_element_size();

    TEST_SECTION("Edge Cases");
    test_unknown_type_element_size();
}
