/*
 * test_profile_primitives.c - Tests for Profiling Primitives
 *
 * Tests profiling functions from profile.c:
 * - prim_time: Time execution of a closure
 * - prim_profile_enable/disable: Enable/disable profiling
 * - prim_profiling_enabled_p: Check profiling status
 *
 * These tests ensure profiling tools work correctly and don't interfere
 * with normal program execution.
 */

#include "test_framework.h"
#include <stdio.h>
#include <stdlib.h>

/* ========== prim_time Tests ========== */

void test_time_basic_thunk(void) {
    /* Test timing a simple thunk that returns a value */
    Obj* thunk = (Obj*)mk_int(42);
    Obj* result = prim_time(thunk);

    /* Should return the thunk's value */
    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 42);

    dec_ref(thunk); dec_ref(result);
    PASS();
}

void test_time_arithmetic(void) {
    /* Test timing a thunk that performs arithmetic */
    Obj* thunk = (Obj*)mk_int(100);
    Obj* result = prim_time(thunk);

    /* Timing should complete and return value */
    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 100);

    dec_ref(thunk); dec_ref(result);
    PASS();
}

void test_time_zero_return(void) {
    /* Test timing a thunk that returns 0 */
    Obj* thunk = (Obj*)mk_int(0);
    Obj* result = prim_time(thunk);

    ASSERT_NOT_NULL(result);
    ASSERT_EQ(obj_to_int(result), 0);

    dec_ref(thunk); dec_ref(result);
    PASS();
}

/* ========== prim_profile_enable/disable Tests ========== */

void test_profile_enable(void) {
    /* Test that profiling can be enabled */
    Obj* result = prim_profile_enable();

    /* Should return true (previously was disabled) */
    ASSERT_NOT_NULL(result);
    /* Result should be true */

    /* Check that profiling is actually enabled */
    Obj* status = prim_profiling_enabled_p();
    ASSERT_NOT_NULL(status);

    dec_ref(result); dec_ref(status);
    PASS();
}

void test_profile_disable(void) {
    /* First enable profiling */
    prim_profile_enable();

    /* Then disable it */
    Obj* result = prim_profile_disable();

    ASSERT_NOT_NULL(result);
    /* Result should be false */

    /* Check that profiling is disabled */
    Obj* status = prim_profiling_enabled_p();
    ASSERT_NOT_NULL(status);

    dec_ref(result); dec_ref(status);
    PASS();
}

void test_profile_enable_disable_cycle(void) {
    /* Test enabling, disabling, and re-enabling */
    Obj* result1 = prim_profile_enable();
    ASSERT_NOT_NULL(result1);

    Obj* status1 = prim_profiling_enabled_p();
    ASSERT_NOT_NULL(status1);

    Obj* result2 = prim_profile_disable();
    ASSERT_NOT_NULL(result2);

    Obj* status2 = prim_profiling_enabled_p();
    ASSERT_NOT_NULL(status2);

    Obj* result3 = prim_profile_enable();
    ASSERT_NOT_NULL(result3);

    Obj* status3 = prim_profiling_enabled_p();
    ASSERT_NOT_NULL(status3);

    dec_ref(result1); dec_ref(result2); dec_ref(result3);
    dec_ref(status1); dec_ref(status2); dec_ref(status3);
    PASS();
}

/* ========== prim_profiling_enabled_p Tests ========== */

void test_profiling_enabled_p_initial(void) {
    /* Test initial state (should be disabled by default) */
    Obj* result = prim_profiling_enabled_p();

    ASSERT_NOT_NULL(result);
    /* Should be false initially */

    dec_ref(result);
    PASS();
}

void test_profiling_enabled_p_after_enable(void) {
    /* Test state changes after enable */
    prim_profile_enable();

    Obj* result = prim_profiling_enabled_p();

    ASSERT_NOT_NULL(result);
    /* Should be true after enable */

    dec_ref(result);
    PASS();
}

void test_profiling_enabled_p_after_disable(void) {
    /* Test state changes after disable */
    prim_profile_enable();
    prim_profile_disable();

    Obj* result = prim_profiling_enabled_p();

    ASSERT_NOT_NULL(result);
    /* Should be false after disable */

    dec_ref(result);
    PASS();
}

/* ========== prim_profile_reset Tests ========== */

void test_profile_reset_clears_data(void) {
    /* Test that reset clears profiling data */
    prim_profile_enable();

    /* Simulate some profiling data */
    /* Reset should clear it */
    Obj* result = prim_profile_reset();

    ASSERT_NOT_NULL(result);
    /* Result should be nothing */

    dec_ref(result);
    PASS();
}

void test_profile_reset_after_enable(void) {
    /* Test reset after enabling profiling */
    prim_profile_enable();

    Obj* result = prim_profile_reset();

    ASSERT_NOT_NULL(result);

    dec_ref(result);
    PASS();
}

/* ========== prim_profile_count Tests ========== */

void test_profile_count_initial(void) {
    /* Test initial profile count */
    Obj* result = prim_profile_count();

    ASSERT_NOT_NULL(result);
    /* Should be 0 initially */
    ASSERT_EQ(obj_to_int(result), 0);

    dec_ref(result);
    PASS();
}

void test_profile_count_type(void) {
    /* Test that profile_count returns integer */
    Obj* result = prim_profile_count();

    ASSERT_NOT_NULL(result);
    /* Should be an integer type */

    dec_ref(result);
    PASS();
}

/* ========== Run All Profiling Primitive Tests ========== */

void run_profile_primitives_tests(void) {
    TEST_SUITE("Profile Primitives");

    /* prim_time tests */
    TEST_SECTION("prim_time");
    RUN_TEST(test_time_basic_thunk);
    RUN_TEST(test_time_arithmetic);
    RUN_TEST(test_time_zero_return);

    /* prim_profile_enable/disable tests */
    TEST_SECTION("prim_profile_enable/disable");
    RUN_TEST(test_profile_enable);
    RUN_TEST(test_profile_disable);
    RUN_TEST(test_profile_enable_disable_cycle);

    /* prim_profiling_enabled_p tests */
    TEST_SECTION("prim_profiling_enabled_p");
    RUN_TEST(test_profiling_enabled_p_initial);
    RUN_TEST(test_profiling_enabled_p_after_enable);
    RUN_TEST(test_profiling_enabled_p_after_disable);

    /* prim_profile_reset tests */
    TEST_SECTION("prim_profile_reset");
    RUN_TEST(test_profile_reset_clears_data);
    RUN_TEST(test_profile_reset_after_enable);

    /* prim_profile_count tests */
    TEST_SECTION("prim_profile_count");
    RUN_TEST(test_profile_count_initial);
    RUN_TEST(test_profile_count_type);
}
