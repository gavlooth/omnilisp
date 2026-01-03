/* test_lists.c - Comprehensive list operation tests */
#include "test_framework.h"

/* Helper to count list length (returns int instead of Obj*) */
static int count_list_length(Obj* xs) {
    int len = 0;
    while (xs && xs->tag == TAG_PAIR) {
        len++;
        xs = obj_cdr(xs);
    }
    return len;
}

/* ========== obj_car tests (list-specific) ========== */

void test_list_car_basic(void) {
    Obj* pair = mk_pair(mk_int(42), mk_int(99));
    Obj* car = obj_car(pair);
    ASSERT_NOT_NULL(car);
    ASSERT_EQ(obj_to_int(car), 42);
    free_tree(pair);
}

void test_list_car_nested(void) {
    Obj* inner = mk_pair(mk_int(1), mk_int(2));
    Obj* outer = mk_pair(inner, mk_int(3));
    Obj* car = obj_car(outer);
    ASSERT_NOT_NULL(car);
    ASSERT_TRUE(car->tag == TAG_PAIR);
    ASSERT_EQ(obj_to_int(obj_car(car)), 1);
    free_tree(outer);
}

void test_list_car_empty(void) {
    Obj* car = obj_car(NULL);
    ASSERT_NULL(car);
}

void test_list_car_non_pair(void) {
    Obj* num = mk_int(42);
    Obj* car = obj_car(num);
    ASSERT_NULL(car);
    free_obj(num);
}

void test_list_car_null_car(void) {
    Obj* pair = mk_pair(NULL, mk_int(1));
    Obj* car = obj_car(pair);
    ASSERT_NULL(car);
    free_tree(pair);
}

/* ========== obj_cdr tests (list-specific) ========== */

void test_list_cdr_basic(void) {
    Obj* pair = mk_pair(mk_int(42), mk_int(99));
    Obj* cdr = obj_cdr(pair);
    ASSERT_NOT_NULL(cdr);
    ASSERT_EQ(obj_to_int(cdr), 99);
    free_tree(pair);
}

void test_list_cdr_nested(void) {
    Obj* inner = mk_pair(mk_int(1), mk_int(2));
    Obj* outer = mk_pair(mk_int(0), inner);
    Obj* cdr = obj_cdr(outer);
    ASSERT_NOT_NULL(cdr);
    ASSERT_TRUE(cdr->tag == TAG_PAIR);
    ASSERT_EQ(obj_to_int(obj_car(cdr)), 1);
    free_tree(outer);
}

void test_list_cdr_empty(void) {
    Obj* cdr = obj_cdr(NULL);
    ASSERT_NULL(cdr);
}

void test_list_cdr_non_pair(void) {
    Obj* num = mk_int(42);
    Obj* cdr = obj_cdr(num);
    ASSERT_NULL(cdr);
    free_obj(num);
}

void test_list_cdr_null_cdr(void) {
    Obj* pair = mk_pair(mk_int(1), NULL);
    Obj* cdr = obj_cdr(pair);
    ASSERT_NULL(cdr);
    free_tree(pair);
}

/* ========== list_length tests ========== */

void test_list_length_empty(void) {
    ASSERT_EQ(count_list_length(NULL), 0);
}

void test_list_length_single(void) {
    Obj* list = mk_pair(mk_int(1), NULL);
    ASSERT_EQ(count_list_length(list), 1);
    free_tree(list);
}

void test_list_length_multiple(void) {
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    ASSERT_EQ(count_list_length(list), 3);
    free_tree(list);
}

void test_list_length_ten(void) {
    Obj* list = NULL;
    for (int i = 0; i < 10; i++) {
        list = mk_pair(mk_int(i), list);
    }
    ASSERT_EQ(count_list_length(list), 10);
    free_tree(list);
}

void test_list_length_hundred(void) {
    Obj* list = NULL;
    for (int i = 0; i < 100; i++) {
        list = mk_pair(mk_int(i), list);
    }
    ASSERT_EQ(count_list_length(list), 100);
    free_tree(list);
}

void test_list_length_improper(void) {
    /* Improper list (cdr is not a list) */
    Obj* pair = mk_pair(mk_int(1), mk_int(2));
    /* Should return 1 (only counts proper list elements) */
    ASSERT_EQ(count_list_length(pair), 1);
    free_tree(pair);
}

/* ========== list_append tests ========== */

void test_list_append_both_non_empty(void) {
    Obj* a = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));
    Obj* b = mk_pair(mk_int(3), mk_pair(mk_int(4), NULL));
    Obj* result = list_append(a, b);
    ASSERT_EQ(count_list_length(result), 4);
    ASSERT_EQ(obj_to_int(obj_car(result)), 1);
    free_tree(result);
}

void test_list_append_first_empty(void) {
    Obj* b = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));
    Obj* result = list_append(NULL, b);
    ASSERT_EQ(count_list_length(result), 2);
    ASSERT_EQ(obj_to_int(obj_car(result)), 1);
    /* b is returned directly, don't double-free */
}

void test_list_append_second_empty(void) {
    Obj* a = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));
    Obj* result = list_append(a, NULL);
    ASSERT_EQ(count_list_length(result), 2);
    free_tree(result);
}

void test_list_append_both_empty(void) {
    Obj* result = list_append(NULL, NULL);
    ASSERT_NULL(result);
}

void test_list_append_single_elements(void) {
    Obj* a = mk_pair(mk_int(1), NULL);
    Obj* b = mk_pair(mk_int(2), NULL);
    Obj* result = list_append(a, b);
    ASSERT_EQ(count_list_length(result), 2);
    ASSERT_EQ(obj_to_int(obj_car(result)), 1);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 2);
    free_tree(result);
}

void test_list_append_long_lists(void) {
    Obj* a = NULL;
    Obj* b = NULL;
    for (int i = 49; i >= 0; i--) a = mk_pair(mk_int(i), a);
    for (int i = 99; i >= 50; i--) b = mk_pair(mk_int(i), b);
    Obj* result = list_append(a, b);
    ASSERT_EQ(count_list_length(result), 100);
    free_tree(result);
}

/* ========== list_reverse tests ========== */

void test_list_reverse_empty(void) {
    Obj* result = list_reverse(NULL);
    ASSERT_NULL(result);
}

void test_list_reverse_single(void) {
    Obj* list = mk_pair(mk_int(42), NULL);
    Obj* result = list_reverse(list);
    ASSERT_EQ(count_list_length(result), 1);
    ASSERT_EQ(obj_to_int(obj_car(result)), 42);
    free_tree(result);
}

void test_list_reverse_two(void) {
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));
    Obj* result = list_reverse(list);
    ASSERT_EQ(obj_to_int(obj_car(result)), 2);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 1);
    free_tree(result);
}

void test_list_reverse_three(void) {
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* result = list_reverse(list);
    ASSERT_EQ(obj_to_int(obj_car(result)), 3);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 2);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(obj_cdr(result)))), 1);
    free_tree(result);
}

void test_list_reverse_hundred(void) {
    Obj* list = NULL;
    for (int i = 0; i < 100; i++) {
        list = mk_pair(mk_int(i), list);
    }
    /* list is now (99 98 ... 0) */
    Obj* result = list_reverse(list);
    /* result should be (0 1 ... 99) */
    ASSERT_EQ(obj_to_int(obj_car(result)), 0);
    Obj* last = result;
    for (int i = 0; i < 99; i++) last = obj_cdr(last);
    ASSERT_EQ(obj_to_int(obj_car(last)), 99);
    free_tree(result);
}

void test_list_reverse_preserves_elements(void) {
    Obj* list = mk_pair(mk_int(10), mk_pair(mk_int(20), mk_pair(mk_int(30), NULL)));
    Obj* result = list_reverse(list);
    /* Sum should be same */
    long sum = 0;
    for (Obj* p = result; p && p->tag == TAG_PAIR; p = obj_cdr(p)) {
        sum += obj_to_int(obj_car(p));
    }
    ASSERT_EQ(sum, 60);
    free_tree(result);
}

/* ========== list_map tests ========== */

/* Closure-form helper functions */
static Obj* double_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 1 || !args[0]) return mk_int(0);
    return mk_int(obj_to_int(args[0]) * 2);
}

static Obj* square_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 1 || !args[0]) return mk_int(0);
    long x = obj_to_int(args[0]);
    return mk_int(x * x);
}

void test_list_map_empty(void) {
    Obj* fn = mk_closure(double_closure_fn, NULL, NULL, 0, 1);
    Obj* result = list_map(fn, NULL);
    ASSERT_NULL(result);
    free_obj(fn);
}

void test_list_map_single(void) {
    Obj* fn = mk_closure(double_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(5), NULL);
    Obj* result = list_map(fn, list);
    ASSERT_EQ(count_list_length(result), 1);
    ASSERT_EQ(obj_to_int(obj_car(result)), 10);
    free_tree(list);
    free_tree(result);
    free_obj(fn);
}

void test_list_map_multiple(void) {
    Obj* fn = mk_closure(double_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* result = list_map(fn, list);
    ASSERT_EQ(obj_to_int(obj_car(result)), 2);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 4);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(obj_cdr(result)))), 6);
    free_tree(list);
    free_tree(result);
    free_obj(fn);
}

void test_list_map_square(void) {
    Obj* fn = mk_closure(square_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(2), mk_pair(mk_int(3), mk_pair(mk_int(4), NULL)));
    Obj* result = list_map(fn, list);
    ASSERT_EQ(obj_to_int(obj_car(result)), 4);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 9);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(obj_cdr(result)))), 16);
    free_tree(list);
    free_tree(result);
    free_obj(fn);
}

void test_list_map_preserves_length(void) {
    Obj* fn = mk_closure(double_closure_fn, NULL, NULL, 0, 1);
    Obj* list = NULL;
    for (int i = 0; i < 50; i++) {
        list = mk_pair(mk_int(i), list);
    }
    Obj* result = list_map(fn, list);
    ASSERT_EQ(count_list_length(result), 50);
    free_tree(list);
    free_tree(result);
    free_obj(fn);
}

void test_list_map_null_fn(void) {
    Obj* list = mk_pair(mk_int(1), NULL);
    Obj* result = list_map(NULL, list);
    ASSERT_NULL(result);
    free_tree(list);
}

/* ========== list_filter tests ========== */

/* Filter predicate closures - return truthy Obj* for keep */
static Obj* is_even_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 1 || !args[0]) return NULL;
    return (obj_to_int(args[0]) % 2 == 0) ? mk_int(1) : NULL;
}

static Obj* is_positive_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 1 || !args[0]) return NULL;
    return (obj_to_int(args[0]) > 0) ? mk_int(1) : NULL;
}

static Obj* always_true_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps; (void)args; (void)nargs;
    return mk_int(1);
}

static Obj* always_false_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps; (void)args; (void)nargs;
    return NULL;
}

void test_list_filter_empty(void) {
    Obj* fn = mk_closure(is_even_closure_fn, NULL, NULL, 0, 1);
    Obj* result = list_filter(fn, NULL);
    ASSERT_NULL(result);
    free_obj(fn);
}

void test_list_filter_keep_all(void) {
    Obj* fn = mk_closure(is_even_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(2), mk_pair(mk_int(4), mk_pair(mk_int(6), NULL)));
    Obj* result = list_filter(fn, list);
    ASSERT_EQ(count_list_length(result), 3);
    /* Use dec_ref for shared structure - filter shares elements with original */
    dec_ref(list);
    dec_ref(result);
    free_obj(fn);
    PASS();
}

void test_list_filter_keep_none(void) {
    Obj* fn = mk_closure(is_even_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(3), mk_pair(mk_int(5), NULL)));
    Obj* result = list_filter(fn, list);
    ASSERT_NULL(result);
    dec_ref(list);
    free_obj(fn);
    PASS();
}

void test_list_filter_keep_some(void) {
    Obj* fn = mk_closure(is_even_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), mk_pair(mk_int(4), NULL))));
    Obj* result = list_filter(fn, list);
    ASSERT_EQ(count_list_length(result), 2);
    ASSERT_EQ(obj_to_int(obj_car(result)), 2);
    ASSERT_EQ(obj_to_int(obj_car(obj_cdr(result))), 4);
    dec_ref(list);
    dec_ref(result);
    free_obj(fn);
    PASS();
}

void test_list_filter_positive(void) {
    Obj* fn = mk_closure(is_positive_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(-2), mk_pair(mk_int(0), mk_pair(mk_int(3), mk_pair(mk_int(-1), mk_pair(mk_int(5), NULL)))));
    Obj* result = list_filter(fn, list);
    ASSERT_EQ(count_list_length(result), 2);
    dec_ref(list);
    dec_ref(result);
    free_obj(fn);
    PASS();
}

void test_list_filter_always_true(void) {
    Obj* fn = mk_closure(always_true_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* result = list_filter(fn, list);
    ASSERT_EQ(count_list_length(result), 3);
    dec_ref(list);
    dec_ref(result);
    free_obj(fn);
    PASS();
}

void test_list_filter_always_false(void) {
    Obj* fn = mk_closure(always_false_closure_fn, NULL, NULL, 0, 1);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* result = list_filter(fn, list);
    ASSERT_NULL(result);
    dec_ref(list);
    free_obj(fn);
    PASS();
}

void test_list_filter_null_fn(void) {
    Obj* list = mk_pair(mk_int(1), NULL);
    Obj* result = list_filter(NULL, list);
    ASSERT_NULL(result);
    dec_ref(list);
    PASS();
}

/* ========== list_fold tests ========== */

/* Fold function closures - take (acc, elem) return new acc */
static Obj* add_fold_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 2) return mk_int(0);
    return mk_int(obj_to_int(args[0]) + obj_to_int(args[1]));
}

static Obj* mul_fold_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 2) return mk_int(1);
    return mk_int(obj_to_int(args[0]) * obj_to_int(args[1]));
}

static Obj* sub_fold_closure_fn(Obj** caps, Obj** args, int nargs) {
    (void)caps;
    if (nargs < 2) return mk_int(0);
    return mk_int(obj_to_int(args[0]) - obj_to_int(args[1]));
}

void test_list_fold_empty(void) {
    Obj* fn = mk_closure(add_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* init = mk_int(0);
    Obj* result = list_fold(fn, init, NULL);
    ASSERT_EQ(obj_to_int(result), 0);
    free_obj(result);
    free_obj(fn);
}

void test_list_fold_sum(void) {
    Obj* fn = mk_closure(add_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* init = mk_int(0);
    Obj* result = list_fold(fn, init, list);
    ASSERT_EQ(obj_to_int(result), 6);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

void test_list_fold_product(void) {
    Obj* fn = mk_closure(mul_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = mk_pair(mk_int(2), mk_pair(mk_int(3), mk_pair(mk_int(4), NULL)));
    Obj* init = mk_int(1);
    Obj* result = list_fold(fn, init, list);
    ASSERT_EQ(obj_to_int(result), 24);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

void test_list_fold_subtraction(void) {
    Obj* fn = mk_closure(sub_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* init = mk_int(10);
    Obj* result = list_fold(fn, init, list);
    /* 10 - 1 - 2 - 3 = 4 */
    ASSERT_EQ(obj_to_int(result), 4);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

void test_list_fold_large_sum(void) {
    Obj* fn = mk_closure(add_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = NULL;
    for (int i = 1; i <= 100; i++) {
        list = mk_pair(mk_int(i), list);
    }
    Obj* init = mk_int(0);
    Obj* result = list_fold(fn, init, list);
    /* Sum 1..100 = 5050 */
    ASSERT_EQ(obj_to_int(result), 5050);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

void test_list_fold_null_fn(void) {
    Obj* list = mk_pair(mk_int(1), NULL);
    Obj* init = mk_int(0);
    Obj* result = list_fold(NULL, init, list);
    /* Should return init when fn is NULL */
    ASSERT_EQ(obj_to_int(result), 0);
    free_tree(list);
    free_obj(result);
}

/* ========== list_foldr tests ========== */

void test_list_foldr_empty(void) {
    Obj* fn = mk_closure(add_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* init = mk_int(0);
    Obj* result = list_foldr(fn, init, NULL);
    ASSERT_EQ(obj_to_int(result), 0);
    free_obj(result);
    free_obj(fn);
}

void test_list_foldr_sum(void) {
    Obj* fn = mk_closure(add_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* init = mk_int(0);
    Obj* result = list_foldr(fn, init, list);
    ASSERT_EQ(obj_to_int(result), 6);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

void test_list_foldr_subtraction(void) {
    Obj* fn = mk_closure(sub_fold_closure_fn, NULL, NULL, 0, 2);
    Obj* list = mk_pair(mk_int(1), mk_pair(mk_int(2), mk_pair(mk_int(3), NULL)));
    Obj* init = mk_int(0);
    Obj* result = list_foldr(fn, init, list);
    /* (1 - (2 - (3 - 0))) = 1 - (2 - 3) = 1 - (-1) = 2 */
    ASSERT_EQ(obj_to_int(result), 2);
    free_tree(list);
    free_obj(result);
    free_obj(fn);
}

/* ========== list construction helpers ========== */

void test_build_list_10(void) {
    Obj* list = NULL;
    for (int i = 9; i >= 0; i--) {
        list = mk_pair(mk_int(i), list);
    }
    ASSERT_EQ(count_list_length(list), 10);
    ASSERT_EQ(obj_to_int(obj_car(list)), 0);
    free_tree(list);
}

void test_build_list_1000(void) {
    Obj* list = NULL;
    for (int i = 0; i < 1000; i++) {
        list = mk_pair(mk_int(i), list);
    }
    ASSERT_EQ(count_list_length(list), 1000);
    free_tree(list);
}

void test_nested_list(void) {
    Obj* inner1 = mk_pair(mk_int(1), mk_pair(mk_int(2), NULL));
    Obj* inner2 = mk_pair(mk_int(3), mk_pair(mk_int(4), NULL));
    Obj* outer = mk_pair(inner1, mk_pair(inner2, NULL));
    ASSERT_EQ(count_list_length(outer), 2);
    ASSERT_EQ(count_list_length(obj_car(outer)), 2);
    free_tree(outer);
}

void test_deeply_nested_list(void) {
    Obj* list = mk_int(0);
    for (int i = 0; i < 100; i++) {
        list = mk_pair(list, NULL);
    }
    /* 100 levels of nesting */
    Obj* p = list;
    int depth = 0;
    while (p && p->tag == TAG_PAIR) {
        depth++;
        p = obj_car(p);
    }
    ASSERT_EQ(depth, 100);
    free_tree(list);
}

/* ========== Run all list tests ========== */

void run_list_tests(void) {
    TEST_SECTION("List Operations - car/cdr");
    RUN_TEST(test_list_car_basic);
    RUN_TEST(test_list_car_nested);
    RUN_TEST(test_list_car_empty);
    RUN_TEST(test_list_car_non_pair);
    RUN_TEST(test_list_car_null_car);
    RUN_TEST(test_list_cdr_basic);
    RUN_TEST(test_list_cdr_nested);
    RUN_TEST(test_list_cdr_empty);
    RUN_TEST(test_list_cdr_non_pair);
    RUN_TEST(test_list_cdr_null_cdr);

    TEST_SECTION("List Operations - length");
    RUN_TEST(test_list_length_empty);
    RUN_TEST(test_list_length_single);
    RUN_TEST(test_list_length_multiple);
    RUN_TEST(test_list_length_ten);
    RUN_TEST(test_list_length_hundred);
    RUN_TEST(test_list_length_improper);

    TEST_SECTION("List Operations - append");
    RUN_TEST(test_list_append_both_non_empty);
    RUN_TEST(test_list_append_first_empty);
    RUN_TEST(test_list_append_second_empty);
    RUN_TEST(test_list_append_both_empty);
    RUN_TEST(test_list_append_single_elements);
    RUN_TEST(test_list_append_long_lists);

    TEST_SECTION("List Operations - reverse");
    RUN_TEST(test_list_reverse_empty);
    RUN_TEST(test_list_reverse_single);
    RUN_TEST(test_list_reverse_two);
    RUN_TEST(test_list_reverse_three);
    RUN_TEST(test_list_reverse_hundred);
    RUN_TEST(test_list_reverse_preserves_elements);

    TEST_SECTION("List Operations - map");
    RUN_TEST(test_list_map_empty);
    RUN_TEST(test_list_map_single);
    RUN_TEST(test_list_map_multiple);
    RUN_TEST(test_list_map_square);
    RUN_TEST(test_list_map_preserves_length);
    RUN_TEST(test_list_map_null_fn);

    TEST_SECTION("List Operations - filter");
    RUN_TEST(test_list_filter_empty);
    RUN_TEST(test_list_filter_keep_all);
    RUN_TEST(test_list_filter_keep_none);
    RUN_TEST(test_list_filter_keep_some);
    RUN_TEST(test_list_filter_positive);
    RUN_TEST(test_list_filter_always_true);
    RUN_TEST(test_list_filter_always_false);
    RUN_TEST(test_list_filter_null_fn);

    TEST_SECTION("List Operations - fold");
    RUN_TEST(test_list_fold_empty);
    RUN_TEST(test_list_fold_sum);
    RUN_TEST(test_list_fold_product);
    RUN_TEST(test_list_fold_subtraction);
    RUN_TEST(test_list_fold_large_sum);
    RUN_TEST(test_list_fold_null_fn);

    TEST_SECTION("List Operations - foldr");
    RUN_TEST(test_list_foldr_empty);
    RUN_TEST(test_list_foldr_sum);
    RUN_TEST(test_list_foldr_subtraction);

    TEST_SECTION("List Construction");
    RUN_TEST(test_build_list_10);
    RUN_TEST(test_build_list_1000);
    RUN_TEST(test_nested_list);
    RUN_TEST(test_deeply_nested_list);
}
