/*
 * Test for BUG-0005: Symmetric Pool Freelist Initialization Bug
 *
 * Expected invariant: When a new pool is allocated, all SYM_POOL_SIZE objects
 * should be added to the freelist in a valid linked-list structure.
 *
 * Observed behavior: In sym_pool_alloc(), the freelist is built by iterating
 * from i=0 to SYM_POOL_SIZE-1 and storing the current freelist pointer in
 * refs field, then updating freelist to point to the new object. This creates
 * a chain where object[0] -> NULL, object[1] -> object[0], etc., which is
 * backwards from what's needed. The first allocation returns the LAST object
 * allocated (index SYM_POOL_SIZE-1), which is fine for allocation order but
 * causes issues because the refs field is being used as the freelist next
 * pointer, clobbering what should be actual reference data.
 *
 * Why it violates architecture:
 * 1. The `refs` field in SymObj is meant to store an array of SymObj* pointers
 *    representing outgoing references (see symmetric.h:35-44). Using it to
 *    store the freelist "next" pointer corrupts this field.
 * 2. While in the pool, objects are unused so refs is free to be repurposed,
 *    but the initialization logic is backwards: setting refs=(SymObj**)freelist
 *    stores the CURRENT head, then makes the new object the head. This creates
 *    object[i].refs -> object[i-1] -> ... -> object[0] -> NULL.
 * 3. On first allocation, we get the last allocated object (SYM_POOL_SIZE-1),
 *    and its refs field still points to object[SYM_POOL_SIZE-2]. If the caller
 *    doesn't properly initialize the object, this stale pointer can cause
 *    crashes or memory corruption.
 * 4. More critically, sym_obj_new() does NOT clear the refs field after
 *    allocation, relying on memset(obj, 0, sizeof(SymObj)). But by that point,
 *    the freelist pointer has already been extracted and stored in the local
 *    variable. The actual issue is subtle: the freelist uses the refs field
 *    as a next pointer, but sym_obj_new() sets refs = inline_refs immediately.
 *    This means the freelist structure is broken after the first object is
 *    returned from the pool.
 *
 * Actual bug: The freelist construction in sym_pool_alloc() has the order
 * backwards. It builds the list as: [0]->NULL, [1]->[0], [2]->[1], ...
 * But when popping, we take from the head, so we get the LAST element added.
 * On subsequent allocations, we get [N-2], [N-3], etc. This works, but the
 * real bug is that the code COMMENT says "in reverse order so first alloc
 * gets slot 0" which is WRONG - first alloc gets slot 511!
 *
 * This is a documentation/implementation mismatch bug.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Minimal definitions to test the pool */
#define SYM_POOL_SIZE 512
#define SYM_INLINE_REFS 3

typedef struct SymObj {
    int external_rc;
    int internal_rc;
    struct SymObj** refs;      /* Used as freelist next when in pool */
    int ref_count;
    int ref_capacity;
    void* data;
    int freed;
    void (*destructor)(void*);
    struct SymComponent* comp;
    struct SymObj* inline_refs[4];
} SymObj;

typedef struct SymComponent SymComponent;

typedef struct SymPool {
    struct SymObj* objects;
    struct SymPool* next;
} SymPool;

/* Thread-local storage simulation */
static struct {
    SymPool* pools;
    SymObj* freelist;
} SYM_TLS = {NULL, NULL};

/* The buggy implementation from symmetric.c */
static SymObj* sym_pool_alloc_buggy(void) {
    if (!SYM_TLS.freelist) {
        /* Allocate new pool */
        SymPool* pool = malloc(sizeof(SymPool));
        if (!pool) return NULL;
        pool->objects = malloc(SYM_POOL_SIZE * sizeof(SymObj));
        if (!pool->objects) { free(pool); return NULL; }
        pool->next = SYM_TLS.pools;
        SYM_TLS.pools = pool;
        /* Add all objects to freelist */
        for (int i = 0; i < SYM_POOL_SIZE; i++) {
            pool->objects[i].refs = (SymObj**)SYM_TLS.freelist;  /* Store current head */
            SYM_TLS.freelist = &pool->objects[i];                /* Make new object head */
        }
    }
    SymObj* obj = SYM_TLS.freelist;
    SYM_TLS.freelist = (SymObj*)obj->refs;  /* Pop from head */
    return obj;
}

/* Test 1: Verify freelist construction order */
void test_freelist_order(void) {
    printf("Test 1: Verifying freelist construction order...\n");

    /* Initialize pool */
    SymObj* first = sym_pool_alloc_buggy();
    SymObj* second = sym_pool_alloc_buggy();
    SymObj* third = sym_pool_alloc_buggy();

    /* Calculate expected indices based on the buggy implementation */
    /* The loop builds: [0]->NULL, [1]->[0], [2]->[1], ..., [511]->[510]
     * freelist points to [511]
     * First alloc returns [511], freelist becomes [510]
     * Second alloc returns [510], freelist becomes [509]
     * Third alloc returns [509]
     */
    SymPool* pool = SYM_TLS.pools;
    size_t expected_first_idx = SYM_POOL_SIZE - 1;      /* 511 */
    size_t expected_second_idx = SYM_POOL_SIZE - 2;     /* 510 */
    size_t expected_third_idx = SYM_POOL_SIZE - 3;      /* 509 */

    size_t actual_first_idx = first - pool->objects;
    size_t actual_second_idx = second - pool->objects;
    size_t actual_third_idx = third - pool->objects;

    printf("  First alloc: got index %zu, expected %zu %s\n",
           actual_first_idx, expected_first_idx,
           actual_first_idx == expected_first_idx ? "✓" : "✗");
    printf("  Second alloc: got index %zu, expected %zu %s\n",
           actual_second_idx, expected_second_idx,
           actual_second_idx == expected_second_idx ? "✓" : "✗");
    printf("  Third alloc: got index %zu, expected %zu %s\n",
           actual_third_idx, expected_third_idx,
           actual_third_idx == expected_third_idx ? "✓" : "✗");

    /* The comment says "in reverse order so first alloc gets slot 0" */
    /* But first alloc actually gets slot 511! */
    if (actual_first_idx != 0) {
        printf("  ✗ FAIL: Comment says 'first alloc gets slot 0' but got %zu\n",
               actual_first_idx);
        printf("  This is a documentation/implementation mismatch!\n");
    } else {
        printf("  ✓ PASS: First alloc gets slot 0 as documented\n");
    }
}

/* Test 2: Verify refs field corruption potential */
void test_refs_field_corruption(void) {
    printf("\nTest 2: Checking for refs field corruption...\n");

    /* After allocation, before initialization, what does refs contain? */
    SymObj* obj = sym_pool_alloc_buggy();

    /* At this point, obj->refs was used as the freelist next pointer */
    /* It should contain the address of the next object in the pool */
    /* But this is BEFORE memset clears it */

    printf("  After allocation, refs field contains: %p\n", (void*)obj->refs);

    /* If refs != NULL, it's pointing to another pool object */
    /* If the caller forgets to memset, this could be interpreted as valid data */
    if (obj->refs != NULL) {
        printf("  ✗ WARNING: refs field is non-NULL after pool alloc!\n");
        printf("  This could cause memory corruption if not properly cleared.\n");

        /* Check if it points to a valid pool object */
        SymPool* pool = SYM_TLS.pools;
        SymObj* potential_next = (SymObj*)obj->refs;
        if (potential_next >= pool->objects &&
            potential_next < pool->objects + SYM_POOL_SIZE) {
            printf("  Confirmed: refs points into the pool at index %zu\n",
                   potential_next - pool->objects);
        }
    } else {
        printf("  ✓ refs field is NULL (safe)\n");
    }
}

/* Forward declaration */
void sym_pool_cleanup(void);

/* Test 3: Demonstrate the correct implementation */
static SymObj* sym_pool_alloc_correct(void) {
    if (!SYM_TLS.freelist) {
        SymPool* pool = malloc(sizeof(SymPool));
        if (!pool) return NULL;
        pool->objects = calloc(SYM_POOL_SIZE, sizeof(SymObj));  /* Use calloc */
        if (!pool->objects) { free(pool); return NULL; }
        pool->next = SYM_TLS.pools;
        SYM_TLS.pools = pool;
        /* Add all objects to freelist in REVERSE order */
        for (int i = SYM_POOL_SIZE - 1; i >= 0; i--) {
            pool->objects[i].refs = (SymObj**)SYM_TLS.freelist;
            SYM_TLS.freelist = &pool->objects[i];
        }
    }
    SymObj* obj = SYM_TLS.freelist;
    SYM_TLS.freelist = (SymObj*)obj->refs;
    return obj;
}

void test_correct_implementation(void) {
    printf("\nTest 3: Demonstrating correct implementation...\n");

    /* Clear existing pool */
    sym_pool_cleanup();

    /* Test correct version */
    SymObj* first = sym_pool_alloc_correct();
    SymPool* pool = SYM_TLS.pools;
    size_t first_idx = first - pool->objects;

    printf("  With reverse initialization (i from 511 to 0):\n");
    printf("  First alloc gets index %zu %s\n", first_idx,
           first_idx == 0 ? "✓ (as documented)" : "✗");
}

/* Cleanup helper */
void sym_pool_cleanup(void) {
    SymPool* p = SYM_TLS.pools;
    while (p) {
        SymPool* next = p->next;
        free(p->objects);
        free(p);
        p = next;
    }
    SYM_TLS.pools = NULL;
    SYM_TLS.freelist = NULL;
}

int main(void) {
    printf("=== BUG-0005: Symmetric Pool Freelist Bug ===\n\n");

    test_freelist_order();
    test_refs_field_corruption();

    /* Reset for correct implementation test */
    sym_pool_cleanup();
    test_correct_implementation();

    /* Final cleanup */
    sym_pool_cleanup();

    printf("\n=== Summary ===\n");
    printf("Bug: Documentation/implementation mismatch in sym_pool_alloc()\n");
    printf("Location: runtime/src/memory/symmetric.c:42-45\n");
    printf("Issue: Comment says 'in reverse order so first alloc gets slot 0'\n");
    printf("       but implementation actually gives slot 511 first.\n");
    printf("Impact: Minor - allocation order doesn't affect correctness,\n");
    printf("        but the misleading comment could confuse maintainers.\n");
    printf("Fix: Either fix the comment OR fix the implementation.\n");

    return 0;
}
