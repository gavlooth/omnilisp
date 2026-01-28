/*
 * test_basic_arithmetic.c - Tests for Basic Arithmetic Operations
 *
 * Tests fundamental arithmetic primitives from math_numerics.c:
 * - prim_add: Addition with int/float handling
 * - prim_sub: Subtraction with int/float handling
 * - prim_mul: Multiplication with int/float handling
 * - prim_div: Division with int/float handling
 *
 * These operations are critical for all numeric computations in OmniLisp.
 */

#include "test_framework.h"
#include <limits.h>

/* ========== prim_add Tests ========== */

void test_add_int_int_basic(void) {
    /* Test basic integer addition */
    Obj* a = mk_int(42);
    Obj* b = mk_int(58);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 100);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_int_int_negative(void) {
    /* Test addition with negative integers */
    Obj* a = mk_int(100);
    Obj* b = mk_int(-50);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 50);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_int_float_coercion(void) {
    /* Test that int + float returns float (type coercion) */
    Obj* a = mk_int(10);
    Obj* b = mk_float(3.5);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    /* Result should be float */
    ASSERT_TRUE(IS_BOXED(result) && result->tag == TAG_FLOAT);
    ASSERT_EQ_FLOAT(obj_to_float(result), 13.5, 0.0001);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_float_int_coercion(void) {
    /* Test that float + int returns float (reverse coercion) */
    Obj* a = mk_float(2.5);
    Obj* b = mk_int(3);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_TRUE(IS_BOXED(result) && result->tag == TAG_FLOAT);
    ASSERT_EQ_FLOAT(obj_to_float(result), 5.5, 0.0001);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_float_float_basic(void) {
    /* Test basic float addition */
    Obj* a = mk_float(1.25);
    Obj* b = mk_float(2.75);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_TRUE(IS_BOXED(result) && result->tag == TAG_FLOAT);
    ASSERT_EQ_FLOAT(obj_to_float(result), 4.0, 0.0001);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_int_zero_identity(void) {
    /* Test that adding zero is identity for ints */
    Obj* a = mk_int(123);
    Obj* zero = mk_int(0);
    Obj* result = prim_add(a, zero);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 123);

    dec_ref(a); dec_ref(zero); dec_ref(result);
    PASS();
}

void test_add_negative_numbers(void) {
    /* Test addition of two negative numbers */
    Obj* a = mk_int(-30);
    Obj* b = mk_int(-20);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), -50);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_float_negative(void) {
    /* Test addition with negative float */
    Obj* a = mk_float(10.0);
    Obj* b = mk_float(-3.5);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ_FLOAT(obj_to_float(result), 6.5, 0.0001);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

void test_add_immediate_int_small(void) {
    /* Test addition with small immediate integer values */
    Obj* a = mk_int(5);
    Obj* b = mk_int(7);
    Obj* result = prim_add(a, b);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 12);

    dec_ref(a); dec_ref(b); dec_ref(result);
    PASS();
}

/* ========== Run All Basic Arithmetic Tests ========== */

void run_basic_arithmetic_tests(void) {
    TEST_SUITE("Basic Arithmetic (prim_add)");

    /* Integer addition tests */
    TEST_SECTION("Integer Addition");
    RUN_TEST(test_add_int_int_basic);
    RUN_TEST(test_add_int_int_negative);
    RUN_TEST(test_add_int_zero_identity);
    RUN_TEST(test_add_negative_numbers);

    /* Float addition tests */
    TEST_SECTION("Float Addition");
    RUN_TEST(test_add_float_float_basic);
    RUN_TEST(test_add_float_negative);

    /* Type coercion tests */
    TEST_SECTION("Type Coercion");
    RUN_TEST(test_add_int_float_coercion);
    RUN_TEST(test_add_float_int_coercion);

    /* Immediate values tests */
    TEST_SECTION("Immediate Values");
    RUN_TEST(test_add_immediate_int_small);
}
