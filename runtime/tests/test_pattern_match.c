/*
 * test_pattern_match.c - Tests for pattern matching with variable bindings
 *
 * CP-5.2: Pattern Matching Completeness
 *
 * Tests:
 * 1. Simple variable binding: [x y] matches [1 2] → x=1, y=2
 * 2. Nested patterns: [[a b] c] matches [[1 2] 3] → a=1, b=2, c=3
 * 3. As patterns: [inner as pair] binds both inner vars and whole match
 * 4. Rest patterns: [x y & rest] matches [1 2 3 4] → x=1, y=2, rest=[3 4]
 * 5. Wildcard: [_ y] matches [1 2] → y=2 (no binding for _)
 * 6. Literal matching: [1 x] matches [1 2] → x=2
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "../include/omni.h"

/* Test framework macros */
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) static void test_##name(void)
#define RUN_TEST(name) do { \
    printf("  Testing %s... ", #name); \
    test_##name(); \
    printf("PASS\n"); \
    tests_passed++; \
} while(0)

#define ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("FAIL: %s\n", msg); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define ASSERT_EQ(a, b, msg) ASSERT((a) == (b), msg)
#define ASSERT_NOT_NULL(ptr, msg) ASSERT((ptr) != NULL, msg)
#define ASSERT_NULL(ptr, msg) ASSERT((ptr) == NULL, msg)

/* Helper to create symbol */
static Obj* sym(const char* name) {
    return mk_sym(name);
}

/* Helper to create array with elements */
static Obj* arr(int count, ...) {
    Obj* a = mk_array(count);
    va_list args;
    va_start(args, count);
    for (int i = 0; i < count; i++) {
        array_push(a, va_arg(args, Obj*));
    }
    va_end(args);
    return a;
}

/* Helper to get binding value from dict */
static Obj* binding_get(Obj* bindings, const char* name) {
    if (!bindings) return NULL;
    /* Use dict_get_by_name for symbol key lookup by string content */
    return dict_get_by_name(bindings, name);
}

/* Test 1: Simple variable binding */
TEST(simple_variable_binding) {
    /* Pattern: [x y], Value: [1 2] */
    Obj* pattern = arr(2, sym("x"), sym("y"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* x = binding_get(bindings, "x");
    Obj* y = binding_get(bindings, "y");

    ASSERT_NOT_NULL(x, "x should be bound");
    ASSERT_NOT_NULL(y, "y should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(x) ? INT_IMM_VALUE(x) : x->i, 1, "x should be 1");
    ASSERT_EQ(IS_IMMEDIATE_INT(y) ? INT_IMM_VALUE(y) : y->i, 2, "y should be 2");
}

/* Test 2: Nested patterns */
TEST(nested_patterns) {
    /* Pattern: [[a b] c], Value: [[1 2] 3] */
    Obj* inner_pattern = arr(2, sym("a"), sym("b"));
    Obj* pattern = arr(2, inner_pattern, sym("c"));

    Obj* inner_value = arr(2, mk_int(1), mk_int(2));
    Obj* value = arr(2, inner_value, mk_int(3));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* a = binding_get(bindings, "a");
    Obj* b = binding_get(bindings, "b");
    Obj* c = binding_get(bindings, "c");

    ASSERT_NOT_NULL(a, "a should be bound");
    ASSERT_NOT_NULL(b, "b should be bound");
    ASSERT_NOT_NULL(c, "c should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(a) ? INT_IMM_VALUE(a) : a->i, 1, "a should be 1");
    ASSERT_EQ(IS_IMMEDIATE_INT(b) ? INT_IMM_VALUE(b) : b->i, 2, "b should be 2");
    ASSERT_EQ(IS_IMMEDIATE_INT(c) ? INT_IMM_VALUE(c) : c->i, 3, "c should be 3");
}

/* Test 3: As patterns */
TEST(as_patterns) {
    /* Pattern: [[x y] as pair], Value: [1 2] */
    Obj* inner_pattern = arr(2, sym("x"), sym("y"));
    Obj* pattern = arr(3, inner_pattern, sym("as"), sym("pair"));

    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* x = binding_get(bindings, "x");
    Obj* y = binding_get(bindings, "y");
    Obj* pair = binding_get(bindings, "pair");

    ASSERT_NOT_NULL(x, "x should be bound");
    ASSERT_NOT_NULL(y, "y should be bound");
    ASSERT_NOT_NULL(pair, "pair should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(x) ? INT_IMM_VALUE(x) : x->i, 1, "x should be 1");
    ASSERT_EQ(IS_IMMEDIATE_INT(y) ? INT_IMM_VALUE(y) : y->i, 2, "y should be 2");

    /* pair should be the whole array [1 2] */
    ASSERT(IS_BOXED(pair) && pair->tag == TAG_ARRAY, "pair should be an array");
    ASSERT_EQ(array_length(pair), 2, "pair should have 2 elements");
}

/* Test 4: Rest patterns */
TEST(rest_patterns) {
    /* Pattern: [x y & rest], Value: [1 2 3 4 5] */
    Obj* pattern = arr(4, sym("x"), sym("y"), sym("&"), sym("rest"));
    Obj* value = arr(5, mk_int(1), mk_int(2), mk_int(3), mk_int(4), mk_int(5));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* x = binding_get(bindings, "x");
    Obj* y = binding_get(bindings, "y");
    Obj* rest = binding_get(bindings, "rest");

    ASSERT_NOT_NULL(x, "x should be bound");
    ASSERT_NOT_NULL(y, "y should be bound");
    ASSERT_NOT_NULL(rest, "rest should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(x) ? INT_IMM_VALUE(x) : x->i, 1, "x should be 1");
    ASSERT_EQ(IS_IMMEDIATE_INT(y) ? INT_IMM_VALUE(y) : y->i, 2, "y should be 2");

    /* rest should be [3 4 5] */
    ASSERT(IS_BOXED(rest) && rest->tag == TAG_ARRAY, "rest should be an array");
    ASSERT_EQ(array_length(rest), 3, "rest should have 3 elements");

    Obj* rest0 = array_get(rest, 0);
    Obj* rest1 = array_get(rest, 1);
    Obj* rest2 = array_get(rest, 2);
    ASSERT_EQ(IS_IMMEDIATE_INT(rest0) ? INT_IMM_VALUE(rest0) : rest0->i, 3, "rest[0] should be 3");
    ASSERT_EQ(IS_IMMEDIATE_INT(rest1) ? INT_IMM_VALUE(rest1) : rest1->i, 4, "rest[1] should be 4");
    ASSERT_EQ(IS_IMMEDIATE_INT(rest2) ? INT_IMM_VALUE(rest2) : rest2->i, 5, "rest[2] should be 5");
}

/* Test 5: Rest pattern with no rest elements */
TEST(rest_pattern_empty) {
    /* Pattern: [x y & rest], Value: [1 2] */
    Obj* pattern = arr(4, sym("x"), sym("y"), sym("&"), sym("rest"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* rest = binding_get(bindings, "rest");
    ASSERT_NOT_NULL(rest, "rest should be bound");
    ASSERT(IS_BOXED(rest) && rest->tag == TAG_ARRAY, "rest should be an array");
    ASSERT_EQ(array_length(rest), 0, "rest should be empty");
}

/* Test 6: Wildcard pattern */
TEST(wildcard_pattern) {
    /* Pattern: [_ y], Value: [1 2] */
    Obj* pattern = arr(2, sym("_"), sym("y"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    /* _ should NOT be bound */
    Obj* underscore = binding_get(bindings, "_");
    ASSERT_NULL(underscore, "_ should not be bound");

    Obj* y = binding_get(bindings, "y");
    ASSERT_NOT_NULL(y, "y should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(y) ? INT_IMM_VALUE(y) : y->i, 2, "y should be 2");
}

/* Test 7: Literal matching */
TEST(literal_matching) {
    /* Pattern: [1 x], Value: [1 2] - should match */
    Obj* pattern = arr(2, mk_int(1), sym("x"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* x = binding_get(bindings, "x");
    ASSERT_NOT_NULL(x, "x should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(x) ? INT_IMM_VALUE(x) : x->i, 2, "x should be 2");
}

/* Test 8: Literal mismatch */
TEST(literal_mismatch) {
    /* Pattern: [1 x], Value: [2 3] - should NOT match */
    Obj* pattern = arr(2, mk_int(1), sym("x"));
    Obj* value = arr(2, mk_int(2), mk_int(3));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NULL(bindings, "Pattern should not match");
}

/* Test 9: Length mismatch */
TEST(length_mismatch) {
    /* Pattern: [x y z], Value: [1 2] - should NOT match */
    Obj* pattern = arr(3, sym("x"), sym("y"), sym("z"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NULL(bindings, "Pattern should not match");
}

/* Test 10: Rest pattern insufficient elements */
TEST(rest_pattern_insufficient) {
    /* Pattern: [x y z & rest], Value: [1 2] - should NOT match (need 3 fixed) */
    Obj* pattern = arr(5, sym("x"), sym("y"), sym("z"), sym("&"), sym("rest"));
    Obj* value = arr(2, mk_int(1), mk_int(2));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NULL(bindings, "Pattern should not match (insufficient elements)");
}

/* Test 11: Single variable pattern */
TEST(single_variable) {
    /* Pattern: [x], Value: [42] */
    Obj* pattern = arr(1, sym("x"));
    Obj* value = arr(1, mk_int(42));

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    Obj* x = binding_get(bindings, "x");
    ASSERT_NOT_NULL(x, "x should be bound");
    ASSERT_EQ(IS_IMMEDIATE_INT(x) ? INT_IMM_VALUE(x) : x->i, 42, "x should be 42");
}

/* Test 12: Reserved keywords not bound */
TEST(reserved_keywords) {
    /* Pattern: [nil true false], Value: matching - reserved should not bind */
    Obj* pattern = arr(3, sym("nil"), sym("true"), sym("false"));
    Obj* value = arr(3, NULL, OMNI_TRUE, OMNI_FALSE);

    Obj* bindings = prim_pattern_match(pattern, value);

    ASSERT_NOT_NULL(bindings, "Pattern should match");

    /* Reserved keywords should not be bound */
    ASSERT_NULL(binding_get(bindings, "nil"), "nil should not be bound");
    ASSERT_NULL(binding_get(bindings, "true"), "true should not be bound");
    ASSERT_NULL(binding_get(bindings, "false"), "false should not be bound");
}

int main(void) {
    printf("Pattern Matching Tests (CP-5.2)\n");
    printf("================================\n\n");

    RUN_TEST(simple_variable_binding);
    RUN_TEST(nested_patterns);
    RUN_TEST(as_patterns);
    RUN_TEST(rest_patterns);
    RUN_TEST(rest_pattern_empty);
    RUN_TEST(wildcard_pattern);
    RUN_TEST(literal_matching);
    RUN_TEST(literal_mismatch);
    RUN_TEST(length_mismatch);
    RUN_TEST(rest_pattern_insufficient);
    RUN_TEST(single_variable);
    RUN_TEST(reserved_keywords);

    printf("\n================================\n");
    printf("Results: %d passed, %d failed\n", tests_passed, tests_failed);

    return tests_failed > 0 ? 1 : 0;
}
