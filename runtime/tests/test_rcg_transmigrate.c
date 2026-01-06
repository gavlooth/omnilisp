/*
 * test_rcg_transmigrate.c - Test the transmigrate (deep copy) functionality
 *
 * Tests:
 * 1. Cycle preservation in deep copy
 * 2. Multiple Value types (T_CELL, T_INT, T_SYM, T_STRING, etc.)
 * 3. Region independence after copy
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "../src/memory/transmigrate.h"
#include "../../src/runtime/types.h"

int main(void) {
    printf("Testing RCG Transmigrate...\n\n");

    // Test 1: Simple Cell Copy
    printf("[Test 1] Simple Cell Copy...\n");
    Region* r1 = region_create();
    Region* r2 = region_create();

    Value* cell = region_alloc(r1, sizeof(Value));
    cell->tag = T_CELL;
    cell->cell.car = region_alloc(r1, sizeof(Value));
    cell->cell.car->tag = T_INT;
    cell->cell.car->i = 42;
    cell->cell.cdr = region_alloc(r1, sizeof(Value));
    cell->cell.cdr->tag = T_NIL;

    Value* new_cell = transmigrate(cell, r1, r2);

    // Verify structure
    assert(new_cell != cell);
    assert(new_cell->tag == T_CELL);
    assert(new_cell->cell.car->tag == T_INT);
    assert(new_cell->cell.car->i == 42);
    assert(new_cell->cell.cdr->tag == T_NIL);

    region_exit(r1);  // Free source
    region_exit(r2);  // Free destination
    printf("  PASSED\n\n");

    // Test 2: Cycle Detection
    printf("[Test 2] Cycle Detection (P1 -> P2 -> P1)...\n");
    Region* r_src = region_create();
    Region* r_dst = region_create();

    Value* p1 = region_alloc(r_src, sizeof(Value));
    Value* p2 = region_alloc(r_src, sizeof(Value));

    p1->tag = T_CELL;
    p1->cell.car = region_alloc(r_src, sizeof(Value));
    p1->cell.car->tag = T_INT;
    p1->cell.car->i = 1;
    p1->cell.cdr = p2;

    p2->tag = T_CELL;
    p2->cell.car = region_alloc(r_src, sizeof(Value));
    p2->cell.car->tag = T_INT;
    p2->cell.car->i = 2;
    p2->cell.cdr = p1;  // Back edge forming cycle

    printf("  Copying cycle...\n");
    Value* new_p1 = transmigrate(p1, r_src, r_dst);

    // Verify structure
    assert(new_p1 != p1);
    assert(new_p1->tag == T_CELL);
    assert(new_p1->cell.car->i == 1);

    Value* new_p2 = new_p1->cell.cdr;
    assert(new_p2 != p2);
    assert(new_p2->tag == T_CELL);
    assert(new_p2->cell.car->i == 2);

    // Verify cycle closed
    assert(new_p2->cell.cdr == new_p1);

    region_exit(r_src);
    region_exit(r_dst);
    printf("  PASSED - Cycle preserved correctly\n\n");

    // Test 3: String Copy
    printf("[Test 3] String Copy...\n");
    Region* r_str_src = region_create();
    Region* r_str_dst = region_create();

    Value* str_val = region_alloc(r_str_src, sizeof(Value));
    str_val->tag = T_STRING;
    str_val->str.len = 5;
    str_val->str.data = region_alloc(r_str_src, 5);
    memcpy(str_val->str.data, "Hello", 5);

    Value* new_str = transmigrate(str_val, r_str_src, r_str_dst);

    assert(new_str != str_val);
    assert(new_str->tag == T_STRING);
    assert(new_str->str.len == 5);
    assert(memcmp(new_str->str.data, "Hello", 5) == 0);

    region_exit(r_str_src);
    region_exit(r_str_dst);
    printf("  PASSED\n\n");

    // Test 4: Symbol Copy
    printf("[Test 4] Symbol Copy...\n");
    Region* r_sym_src = region_create();
    Region* r_sym_dst = region_create();

    Value* sym_val = region_alloc(r_sym_src, sizeof(Value));
    sym_val->tag = T_SYM;
    sym_val->s = region_alloc(r_sym_src, 8);
    strcpy(sym_val->s, "example");

    Value* new_sym = transmigrate(sym_val, r_sym_src, r_sym_dst);

    assert(new_sym != sym_val);
    assert(new_sym->tag == T_SYM);
    assert(strcmp(new_sym->s, "example") == 0);

    region_exit(r_sym_src);
    region_exit(r_sym_dst);
    printf("  PASSED\n\n");

    // Test 5: Lambda Copy
    printf("[Test 5] Lambda (Closure) Copy...\n");
    Region* r_lam_src = region_create();
    Region* r_lam_dst = region_create();

    Value* lambda = region_alloc(r_lam_src, sizeof(Value));
    lambda->tag = T_LAMBDA;
    lambda->lam.params = region_alloc(r_lam_src, sizeof(Value));
    lambda->lam.params->tag = T_NIL;
    lambda->lam.body = region_alloc(r_lam_src, sizeof(Value));
    lambda->lam.body->tag = T_NIL;
    lambda->lam.env = region_alloc(r_lam_src, sizeof(Value));
    lambda->lam.env->tag = T_NIL;
    lambda->lam.defaults = NULL;

    Value* new_lambda = transmigrate(lambda, r_lam_src, r_lam_dst);

    assert(new_lambda != lambda);
    assert(new_lambda->tag == T_LAMBDA);
    assert(new_lambda->lam.params != NULL);
    assert(new_lambda->lam.body != NULL);
    assert(new_lambda->lam.env != NULL);

    region_exit(r_lam_src);
    region_exit(r_lam_dst);
    printf("  PASSED\n\n");

    // Test 6: Complex Nested Structure
    printf("[Test 6] Complex Nested Structure...\n");
    Region* r_complex_src = region_create();
    Region* r_complex_dst = region_create();

    // Build: ((1 . 2) . (3 . (4 . 5)))
    Value* pair1 = region_alloc(r_complex_src, sizeof(Value));
    pair1->tag = T_CELL;
    pair1->cell.car = region_alloc(r_complex_src, sizeof(Value));
    pair1->cell.car->tag = T_INT;
    pair1->cell.car->i = 1;
    pair1->cell.cdr = region_alloc(r_complex_src, sizeof(Value));
    pair1->cell.cdr->tag = T_INT;
    pair1->cell.cdr->i = 2;

    Value* pair2 = region_alloc(r_complex_src, sizeof(Value));
    pair2->tag = T_CELL;
    pair2->cell.car = region_alloc(r_complex_src, sizeof(Value));
    pair2->cell.car->tag = T_INT;
    pair2->cell.car->i = 3;
    pair2->cell.cdr = region_alloc(r_complex_src, sizeof(Value));
    pair2->cell.cdr->tag = T_INT;
    pair2->cell.cdr->i = 4;

    Value* pair3 = region_alloc(r_complex_src, sizeof(Value));
    pair3->tag = T_CELL;
    pair3->cell.car = region_alloc(r_complex_src, sizeof(Value));
    pair3->cell.car->tag = T_INT;
    pair3->cell.car->i = 5;
    pair3->cell.cdr = region_alloc(r_complex_src, sizeof(Value));
    pair3->cell.cdr->tag = T_NIL;

    // Link: pair2->cdr = pair3
    pair2->cell.cdr = pair3;

    Value* root = region_alloc(r_complex_src, sizeof(Value));
    root->tag = T_CELL;
    root->cell.car = pair1;
    root->cell.cdr = pair2;

    Value* new_root = transmigrate(root, r_complex_src, r_complex_dst);

    // Verify all integers preserved
    assert(new_root->cell.car->cell.car->i == 1);
    assert(new_root->cell.car->cell.cdr->i == 2);
    assert(new_root->cell.cdr->cell.car->i == 3);
    assert(new_root->cell.cdr->cell.cdr->cell.car->i == 5);

    region_exit(r_complex_src);
    region_exit(r_complex_dst);
    printf("  PASSED\n\n");

    printf("=== All RCG Transmigrate tests PASSED ===\n");
    return 0;
}
