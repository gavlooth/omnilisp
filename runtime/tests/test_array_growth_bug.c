/*
 * test_array_growth_bug.c - Verify array_push doesn't grow when full
 */

#include "../include/omni.h"
#include "../include/primitives_specialized.h"
#include <stdio.h>
#include <assert.h>

int main() {
    printf("Testing array_push growth behavior...\n");

    Region* r = region_create();

    /* Create array with initial capacity (default is usually 4) */
    Obj* arr = mk_array_region(r, 4);
    printf("Initial array length: %d\n", array_length(arr));

    /* Push more than default capacity elements */
    for (int i = 0; i < 10; i++) {
        array_push(arr, mk_int_region(r, i));
        printf("After push %d: length = %d\n", i, array_length(arr));
    }

    /* Array should have grown and contain all 10 elements */
    int len = array_length(arr);

    if (len != 10) {
        printf("FAIL: Expected array length 10, got %d\n", len);
        printf("This confirms bug: array_push doesn't grow when full!\n");
        region_exit(r);
        return 1;
    }

    /* Verify all elements */
    for (int i = 0; i < 10; i++) {
        Obj* elem = array_get(arr, i);
        int64_t val = unbox_int(elem);
        if (val != i) {
            printf("FAIL: Element at index %d is %ld, expected %d\n", i, (long)val, i);
            region_exit(r);
            return 1;
        }
    }

    printf("PASS: Array grew correctly and contains all elements\n");
    region_exit(r);
    return 0;
}
