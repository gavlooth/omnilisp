/*
 * Test: Component Merge Integer Overflow Vulnerability
 *
 * Expected invariant: When merging two components, if the combined
 * member count would overflow integer bounds, the merge should fail
 * safely without corrupting memory.
 *
 * Observed behavior: In sym_component_union() at component.c:89-96,
 * the capacity check does not prevent integer overflow:
 *
 *   if (rootA->member_count + rootB->member_count > rootA->member_capacity) {
 *       int new_cap = rootA->member_capacity * 2;
 *       while (new_cap < rootA->member_count + rootB->member_count) {
 *           new_cap *= 2;
 *       }
 *       ...
 *   }
 *
 * Issues:
 * 1. The condition `rootA->member_count + rootB->member_count` can overflow
 *    to a negative value before comparison, bypassing the check.
 * 2. The while loop condition can also overflow, potentially infinite looping
 *    or wrapping around.
 * 3. realloc() with a wrapped-around value will allocate far less than needed,
 *    causing heap buffer overflow on subsequent member additions.
 *
 * Architecture violation: Memory safety - integer overflow leading to
 * buffer overflow and potential code execution.
 *
 * Root cause: Using signed int for counts/capacities without overflow
 * protection. The check should use size_t and detect overflow before
 * doing arithmetic.
 *
 * This test creates components with large member counts to trigger
 * the overflow path.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

/* Minimal component definitions */
typedef struct SymObj SymObj;
typedef struct SymComponent SymComponent;

#define SYM_INLINE_REFS 3

struct SymObj {
    int external_rc;
    int internal_rc;
    SymObj** refs;
    int ref_count;
    int ref_capacity;
    void* data;
    int freed;
    void (*destructor)(void*);
    SymComponent* comp;
    SymObj* inline_refs[4];
};

struct SymComponent {
    SymComponent* parent;
    int handle_count;
    int tether_count;
    int state;
    int dismantle_scheduled;
    uint32_t id;
    SymObj** members;
    int member_count;
    int member_capacity;
};

/* Forward declarations */
SymComponent* sym_component_new(void);
void sym_component_union(SymComponent* a, SymComponent* b);
SymComponent* sym_component_find(SymComponent* c);
void sym_component_add_member(SymComponent* c, SymObj* obj);

/* Mock implementation for testing */
SymComponent* sym_component_new(void) {
    SymComponent* c = calloc(1, sizeof(SymComponent));
    if (c) {
        c->member_capacity = 8;
        c->members = malloc(c->member_capacity * sizeof(SymObj*));
        c->id = 1;
    }
    return c;
}

SymComponent* sym_component_find(SymComponent* c) {
    if (!c) return NULL;
    if (c->parent == NULL) return c;

    SymComponent* root = c;
    while (root->parent) {
        root = root->parent;
    }

    /* Path compression */
    SymComponent* curr = c;
    while (curr->parent) {
        SymComponent* next = curr->parent;
        curr->parent = root;
        curr = next;
    }

    return root;
}

void sym_component_add_member(SymComponent* c, SymObj* obj) {
    SymComponent* root = sym_component_find(c);
    if (!root || !obj) return;

    if (root->member_count >= root->member_capacity) {
        root->member_capacity *= 2;
        root->members = realloc(root->members, root->member_capacity * sizeof(SymObj*));
    }

    obj->comp = root;
    root->members[root->member_count++] = obj;
}

/* The buggy implementation from component.c:74-121 */
void sym_component_union(SymComponent* a, SymComponent* b) {
    SymComponent* rootA = sym_component_find(a);
    SymComponent* rootB = sym_component_find(b);

    if (rootA == rootB) return;

    /* Merge B into A (arbitrary direction for now) */
    rootB->parent = rootA;

    /* Transfer counts */
    rootA->handle_count += rootB->handle_count;
    rootA->tether_count += rootB->tether_count;

    /* Transfer members */
    /* Ensure capacity */
    if (rootA->member_count + rootB->member_count > rootA->member_capacity) {
        int new_cap = rootA->member_capacity * 2;
        while (new_cap < rootA->member_count + rootB->member_count) {
            new_cap *= 2;
        }
        rootA->members = realloc(rootA->members, new_cap * sizeof(SymObj*));
        rootA->member_capacity = new_cap;
    }

    /* Copy members and update their component pointer */
    for (int i = 0; i < rootB->member_count; i++) {
        SymObj* obj = rootB->members[i];
        if (obj) {
            obj->comp = rootA;
            rootA->members[rootA->member_count++] = obj;
        }
    }

    /* Release B's member list memory and pool slot */
    if (rootB->members) {
        free(rootB->members);
        rootB->members = NULL;
    }
}

void test_component_merge_overflow(void) {
    printf("=== Test: Component Merge Integer Overflow ===\n");

    /* Create component A with large member count */
    SymComponent* compA = sym_component_new();
    if (!compA) {
        printf("FAIL: Could not create component A\n");
        return;
    }

    /* Set up A to have a large member count near INT_MAX/2 */
    compA->member_count = INT_MAX / 2;
    compA->member_capacity = INT_MAX / 2 + 1;

    /* Allocate minimal members array to avoid huge allocation */
    compA->members = malloc(compA->member_capacity * sizeof(SymObj*));
    if (!compA->members) {
        free(compA);
        printf("SKIP: Cannot allocate large array for testing\n");
        return;
    }

    printf("Component A: member_count=%d, member_capacity=%d\n",
           compA->member_count, compA->member_capacity);

    /* Create component B with large member count */
    SymComponent* compB = sym_component_new();
    if (!compB) {
        free(compA->members);
        free(compA);
        printf("FAIL: Could not create component B\n");
        return;
    }

    compB->member_count = INT_MAX / 2 + 10;  /* Will overflow when added to A */
    compB->member_capacity = compB->member_count;
    compB->members = malloc(compB->member_capacity * sizeof(SymObj*));
    if (!compB->members) {
        free(compB);
        free(compA->members);
        free(compA);
        printf("SKIP: Cannot allocate large array for testing\n");
        return;
    }

    printf("Component B: member_count=%d, member_capacity=%d\n",
           compB->member_count, compB->member_capacity);

    /* Calculate what the sum would be (should overflow) */
    int expected_sum = compA->member_count + compB->member_count;
    printf("Expected sum: %d (overflowed if negative)\n", expected_sum);

    if (expected_sum < 0) {
        printf("Confirmed: Integer overflow will occur (%d + %d = %d)\n",
               compA->member_count, compB->member_count, expected_sum);
    }

    /* Now try to merge - this should be detected as an error condition */
    printf("Attempting merge...\n");

    /* Save original state to detect corruption */
    int original_capacity_A = compA->member_capacity;
    void* original_members_A = compA->members;

    sym_component_union(compA, compB);

    /* Check if merge failed safely or caused corruption */
    if (compA->member_capacity != original_capacity_A) {
        printf("FAIL: member_capacity changed from %d to %d\n",
               original_capacity_A, compA->member_capacity);
        printf("This indicates realloc() was called with overflowed size!\n");
        printf("BUG: The condition at component.c:89 does not detect overflow.\n");
        printf("     rootA->member_count + rootB->member_count wrapped to negative,\n");
        printf("     bypassing the capacity check and calling realloc with bad size.\n");

        free(compB->members);
        free(compB);
        free(compA->members);
        free(compA);
        return;
    }

    if (compA->members != original_members_A) {
        printf("FAIL: members pointer changed despite same capacity!\n");
        printf("Heap corruption likely occurred.\n");

        free(compB->members);
        free(compB);
        free(compA->members);
        free(compA);
        return;
    }

    printf("PASS: Merge did not corrupt memory (no realloc attempted)\n");

    free(compB->members);
    free(compB);
    free(compA->members);
    free(compA);
}

void test_component_merge_sensible_overflow(void) {
    printf("\n=== Test: Sensible Overflow Detection ===\n");

    /* Test with smaller values that still demonstrate the issue */
    SymComponent* compA = sym_component_new();
    SymComponent* compB = sym_component_new();

    if (!compA || !compB) {
        printf("FAIL: Could not create components\n");
        if (compA) free(compA->members);
        if (compA) free(compA);
        if (compB) free(compB->members);
        if (compB) free(compB);
        return;
    }

    /* Fill component A */
    for (int i = 0; i < 5; i++) {
        SymObj* obj = calloc(1, sizeof(SymObj));
        if (obj) {
            sym_component_add_member(compA, obj);
        }
    }

    /* Fill component B */
    for (int i = 0; i < 5; i++) {
        SymObj* obj = calloc(1, sizeof(SymObj));
        if (obj) {
            sym_component_add_member(compB, obj);
        }
    }

    printf("Before merge: A.member_count=%d, B.member_count=%d\n",
           compA->member_count, compB->member_count);

    int expected_total = compA->member_count + compB->member_count;
    int capacity_before = compA->member_capacity;

    sym_component_union(compA, compB);

    printf("After merge: rootA.member_count=%d, member_capacity=%d\n",
           sym_component_find(compA)->member_count,
           sym_component_find(compA)->member_capacity);

    if (sym_component_find(compA)->member_count != expected_total) {
        printf("FAIL: Expected member_count=%d, got %d\n",
               expected_total, sym_component_find(compA)->member_count);
    } else if (sym_component_find(compA)->member_capacity < expected_total) {
        printf("FAIL: capacity=%d < member_count=%d (buffer overflow!)\n",
               sym_component_find(compA)->member_capacity,
               sym_component_find(compA)->member_count);
    } else {
        printf("PASS: Merge completed safely\n");
    }

    /* Cleanup */
    SymComponent* rootA = sym_component_find(compA);
    for (int i = 0; i < rootA->member_count; i++) {
        free(rootA->members[i]);
    }
    free(rootA->members);
    free(rootA);
    free(compB);  /* compB was merged into rootA, but we still have the header */
}

int main(void) {
    printf("Testing Component Merge Integer Overflow\n");
    printf("=========================================\n\n");

    test_component_merge_overflow();
    test_component_merge_sensible_overflow();

    printf("\n=========================================\n");
    printf("Tests completed\n");
    return 0;
}
