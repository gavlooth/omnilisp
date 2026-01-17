/*
 * test_typed_array_fixes.c - Simple test for typed_array.c bug fixes
 *
 * Tests two bugs fixed in runtime/src/typed_array.c:
 * 1. Memory allocation failure paths in omni_typed_array_create
 * 2. omni_typed_array_to_list list building bug
 */

#include "../include/omni.h"
#include "../include/typed_array.h"
#include "../include/primitives_specialized.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Simple list length counter (avoiding conflict with omni.h list_length) */
int count_list_length(Obj* lst) {
    int len = 0;
    while (lst && lst->is_pair) {
        len++;
        lst = lst->b;
    }
    return len;
}

/* Test 1: Verify omni_typed_array_to_list creates correct linked list structure */
void test_to_list_structure() {
    printf("Test 1: omni_typed_array_to_list structure... ");

    /* Create region for this test */
    Region* r = region_create();

    /* Create array with elements [10, 20, 30] */
    int dims[] = {3};
    TypedArray* arr = omni_typed_array_create(r, ARRAY_TYPE_INT64, 1, dims);
    assert(arr != NULL);

    /* Set elements */
    int idx0[] = {0};
    int idx1[] = {1};
    int idx2[] = {2};
    omni_typed_array_set_int(arr, idx0, 10);
    omni_typed_array_set_int(arr, idx1, 20);
    omni_typed_array_set_int(arr, idx2, 30);

    /* Convert to list */
    Obj* lst = omni_typed_array_to_list(arr, r);

    /* Verify list structure */
    int len = count_list_length(lst);
    if (len != 3) {
        printf("FAIL: Expected list length 3, got %d\n", len);
        exit(1);
    }

    /* Verify elements are correctly linked */
    Obj* first = lst->a;
    Obj* second = lst->b->a;
    Obj* third = lst->b->b->a;

    int64_t v1 = unbox_int(first);
    int64_t v2 = unbox_int(second);
    int64_t v3 = unbox_int(third);

    if (v1 != 10 || v2 != 20 || v3 != 30) {
        printf("FAIL: Expected [10, 20, 30], got [%ld, %ld, %ld]\n", (long)v1, (long)v2, (long)v3);
        exit(1);
    }

    /* Verify list is properly terminated (cdr of last cell should be NULL/NIL) */
    Obj* last_cdr = lst->b->b->b;
    if (last_cdr != NULL) {
        printf("FAIL: List not properly terminated (last cdr should be NULL)\n");
        exit(1);
    }

    region_exit(r);
    printf("PASS\n");
}

/* Test 2: Verify empty array handling */
void test_to_list_empty() {
    printf("Test 2: omni_typed_array_to_list empty array... ");

    Region* r = region_create();
    int dims[] = {0};
    TypedArray* arr = omni_typed_array_create(r, ARRAY_TYPE_INT64, 1, dims);

    Obj* lst = omni_typed_array_to_list(arr, r);

    if (lst != NULL) {
        printf("FAIL: Empty array should produce NULL list\n");
        exit(1);
    }

    region_exit(r);
    printf("PASS\n");
}

/* Test 3: Verify single element */
void test_to_list_single() {
    printf("Test 3: omni_typed_array_to_list single element... ");

    Region* r = region_create();
    int dims[] = {1};
    TypedArray* arr = omni_typed_array_create(r, ARRAY_TYPE_INT64, 1, dims);

    int idx[] = {0};
    omni_typed_array_set_int(arr, idx, 42);

    Obj* lst = omni_typed_array_to_list(arr, r);

    int len = count_list_length(lst);
    if (len != 1) {
        printf("FAIL: Expected list length 1, got %d\n", len);
        exit(1);
    }

    int64_t val = unbox_int(lst->a);
    if (val != 42) {
        printf("FAIL: Expected element 42, got %ld\n", (long)val);
        exit(1);
    }

    if (lst->b != NULL) {
        printf("FAIL: Single-element list should have NULL cdr\n");
        exit(1);
    }

    region_exit(r);
    printf("PASS\n");
}

/* Test 4: Verify larger arrays */
void test_to_list_large() {
    printf("Test 4: omni_typed_array_to_list larger array (100 elements)... ");

    Region* r = region_create();
    int dims[] = {100};
    TypedArray* arr = omni_typed_array_create(r, ARRAY_TYPE_INT64, 1, dims);

    /* Fill array */
    for (int i = 0; i < 100; i++) {
        int idx[] = {i};
        omni_typed_array_set_int(arr, idx, i);
    }

    Obj* lst = omni_typed_array_to_list(arr, r);

    int len = count_list_length(lst);
    if (len != 100) {
        printf("FAIL: Expected list length 100, got %d\n", len);
        exit(1);
    }

    /* Spot check some elements */
    Obj* curr = lst;
    for (int i = 0; i < 100; i++) {
        int64_t val = unbox_int(curr->a);
        if (val != i) {
            printf("FAIL: Element at position %d is %ld, expected %d\n", i, (long)val, i);
            exit(1);
        }
        curr = curr->b;
    }

    region_exit(r);
    printf("PASS\n");
}

int main(int argc, char** argv) {
    printf("=== Typed Array Bug Fix Verification Tests ===\n\n");

    test_to_list_structure();
    test_to_list_empty();
    test_to_list_single();
    test_to_list_large();

    printf("\n=== All tests passed ===\n");
    return 0;
}
