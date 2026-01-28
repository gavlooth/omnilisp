/*
 * test_values_equal.c
 *
 * Purpose:
 *   Unit tests for the omni_values_equal() function in ast/ast.c
 *
 * Why this matters:
 *   omni_values_equal() is a core comparison utility used throughout the
 *   codebase for testing value equality. Testing all value types and edge cases
 *   ensures correctness of AST comparisons and value handling.
 *
 * Contract:
 *   - Pointer equality: a == b should return true
 *   - NULL handling: comparing NULL should return false unless both are NULL
 *   - Type mismatch: different tags should return false
 *   - Primitive types (int, char, float, nil, nothing): value comparison
 *   - String-like types (sym, keyword, code, error): string comparison
 *   - Complex types: pointer equality only
 */

#include "../ast/ast.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void test_int_equality(void) {
    OmniValue* a = omni_new_int(42);
    OmniValue* b = omni_new_int(42);
    OmniValue* c = omni_new_int(43);

    assert(omni_values_equal(a, b) && "Equal ints should be equal");
    assert(!omni_values_equal(a, c) && "Different ints should not be equal");
    assert(omni_values_equal(a, a) && "Same int pointer should be equal");

    printf("PASS: int equality\n");
}

static void test_float_equality(void) {
    OmniValue* a = omni_new_float(3.14);
    OmniValue* b = omni_new_float(3.14);
    OmniValue* c = omni_new_float(2.71);

    assert(omni_values_equal(a, b) && "Equal floats should be equal");
    assert(!omni_values_equal(a, c) && "Different floats should not be equal");
    assert(omni_values_equal(a, a) && "Same float pointer should be equal");

    printf("PASS: float equality\n");
}

static void test_char_equality(void) {
    OmniValue* a = omni_new_char('a');
    OmniValue* b = omni_new_char('a');
    OmniValue* c = omni_new_char('z');

    assert(omni_values_equal(a, b) && "Equal chars should be equal");
    assert(!omni_values_equal(a, c) && "Different chars should not be equal");
    assert(omni_values_equal(a, a) && "Same char pointer should be equal");

    printf("PASS: char equality\n");
}

static void test_symbol_equality(void) {
    OmniValue* a = omni_new_sym("foo");
    OmniValue* b = omni_new_sym("foo");
    OmniValue* c = omni_new_sym("bar");

    assert(omni_values_equal(a, b) && "Equal symbols should be equal");
    assert(!omni_values_equal(a, c) && "Different symbols should not be equal");
    assert(omni_values_equal(a, a) && "Same symbol pointer should be equal");

    printf("PASS: symbol equality\n");
}

static void test_nil_equality(void) {
    OmniValue* a = omni_nil;
    OmniValue* b = omni_nil;

    /* nil is a global singleton */
    assert(omni_values_equal(a, b) && "nil values should be equal");
    assert(omni_values_equal(a, a) && "nil pointer identity should be equal");

    printf("PASS: nil equality\n");
}

static void test_nothing_equality(void) {
    OmniValue* a = omni_nothing;
    OmniValue* b = omni_nothing;

    /* nothing is a global singleton */
    assert(omni_values_equal(a, b) && "nothing values should be equal");
    assert(omni_values_equal(a, a) && "nothing pointer identity should be equal");

    printf("PASS: nothing equality\n");
}

static void test_null_comparisons(void) {
    OmniValue* a = omni_new_int(42);

    assert(!omni_values_equal(NULL, NULL) && "Both NULL should return false");
    assert(!omni_values_equal(NULL, a) && "NULL vs value should return false");
    assert(!omni_values_equal(a, NULL) && "value vs NULL should return false");

    printf("PASS: null comparisons\n");
}

static void test_type_mismatch(void) {
    OmniValue* i = omni_new_int(42);
    OmniValue* f = omni_new_float(3.14);
    OmniValue* s = omni_new_sym("test");

    assert(!omni_values_equal(i, f) && "int vs float should not be equal");
    assert(!omni_values_equal(i, s) && "int vs symbol should not be equal");
    assert(!omni_values_equal(f, s) && "float vs symbol should not be equal");

    printf("PASS: type mismatch\n");
}

static void test_pointer_identity(void) {
    /* Create a value and compare it to itself */
    OmniValue* a = omni_new_int(99);

    /* Even though it's a "complex" type in the default case,
       pointer identity should still work for same reference */
    assert(omni_values_equal(a, a) && "Same pointer should be equal");

    printf("PASS: pointer identity\n");
}

int main(void) {
    /* Initialize AST arena */
    omni_ast_arena_init();

    /* Run all tests */
    test_int_equality();
    test_float_equality();
    test_char_equality();
    test_symbol_equality();
    test_nil_equality();
    test_nothing_equality();
    test_null_comparisons();
    test_type_mismatch();
    test_pointer_identity();

    /* Cleanup */
    omni_ast_arena_cleanup();

    printf("PASS: test_values_equal\n");
    return 0;
}
