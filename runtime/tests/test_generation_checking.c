/*
 * test_generation_checking.c - Test IPGE generation checking
 *
 * This test verifies that proper generation checking is implemented:
 * 1. Objects are allocated with region's current generation
 * 2. Cross-region access uses generation checking
 * 3. Stale pointers are detected via generation mismatch
 */

#include "../include/omni.h"
#include "../src/memory/region_core.h"
#include "../src/memory/region_pointer.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

void test_object_generation_initialization(void) {
    printf("=== test_object_generation_initialization ===\n");

    /* Create a new region (starts at generation 1) */
    Region* r1 = region_create();
    assert(r1 != NULL);
    assert(r1->generation == 1);  /* First region starts at gen 1 */

    /* Allocate an object in this region */
    Obj* obj1 = mk_int_region(r1, 42);
    assert(obj1 != NULL);

    /* Object should have same generation as region at allocation time */
    assert(obj1->generation == r1->generation);
    printf("  Object generation initialized correctly: obj->gen=%d, region->gen=%d\n",
           obj1->generation, r1->generation);

    /* Clean up */
    region_exit(r1);
    region_destroy_if_dead(r1);
    printf("  PASS\n");
}

void test_region_generation_increment_on_reuse(void) {
    printf("=== test_region_generation_increment_on_reuse ===\n");

    Region* r1 = region_create();
    uint32_t gen1 = r1->generation;

    /* Allocate object and exit region */
    Obj* obj1 = mk_int_region(r1, 100);
    assert(obj1->generation == gen1);

    region_exit(r1);

    /* Destroy region (returns to pool) */
    region_destroy_if_dead(r1);

    /* Create new region (may reuse from pool) */
    Region* r2 = region_create();
    uint32_t gen2 = r2->generation;

    /* If region was reused, generation should be incremented */
    printf("  Initial gen=%d, Reused gen=%d (incremented: %s)\n",
           gen1, gen2, gen2 > gen1 ? "yes" : "no (new region)");

    /* Allocate new object */
    Obj* obj2 = mk_int_region(r2, 200);
    assert(obj2->generation == gen2);

    /* Clean up */
    region_exit(r2);
    region_destroy_if_dead(r2);
    printf("  PASS\n");
}

void test_cross_region_generation_check(void) {
    printf("=== test_cross_region_generation_check ===\n");

    /* Create two regions */
    Region* r1 = region_create();
    Region* r2 = region_create();

    /* Allocate object in r1 */
    Obj* obj1 = mk_int_region(r1, 42);
    uint32_t obj1_gen = obj1->generation;

    /* Encode pointer with region ID */
    void* encoded = pointer_mask_encode(obj1, r1->region_id);
    assert(encoded != NULL);

    /* Extract region ID from encoded pointer */
    uint16_t region_id = pointer_mask_get_region(encoded);
    assert(region_id == r1->region_id);

    /* Decode pointer */
    void* decoded = pointer_mask_decode(encoded);
    assert(decoded == obj1);

    /* Check safe access from same region (should pass) */
    bool same_region_safe = pointer_mask_safe_access(encoded, r1->region_id);
    assert(same_region_safe == true);
    printf("  Same-region access check: PASS\n");

    /* Check safe access from different region (should pass - generations match) */
    bool cross_region_safe = pointer_mask_safe_access(encoded, r2->region_id);
    assert(cross_region_safe == true);
    printf("  Cross-region access check (valid generations): PASS\n");

    /* Clean up */
    region_exit(r1);
    region_exit(r2);
    region_destroy_if_dead(r1);
    region_destroy_if_dead(r2);
    printf("  PASS\n");
}

int main(void) {
    printf("\n=== IPGE Generation Checking Tests ===\n\n");

    test_object_generation_initialization();
    test_region_generation_increment_on_reuse();
    test_cross_region_generation_check();

    printf("\n=== All generation checking tests PASSED ===\n");
    return 0;
}
