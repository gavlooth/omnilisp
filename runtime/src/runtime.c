/* OmniLisp Region-Based Reference Counting (RC-G) Runtime */
/* Primary Strategy: Adaptive Regions + Tethers */
/* Generated ANSI C99 + POSIX Code */

#define _POSIX_C_SOURCE 200809L

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <stdbool.h>

#include "memory/region_core.h"
#include "memory/region_value.h"
// omni.h is included via region_value.h

/* RC-G Runtime: Standard RC is now Region-RC (Coarse-grained) */
void inc_ref(Obj* x) { (void)x; }
void dec_ref(Obj* x) { (void)x; }
void free_obj(Obj* x) { (void)x; }

/* Global Region for Interpreter/Legacy support */
static Region* _global_region = NULL;
static void _ensure_global_region(void) {
    if (!_global_region) _global_region = region_create();
}

/* Helpers */
int is_nil(Obj* x) { return x == NULL; }
static Obj omni_nothing_obj = { .tag = TAG_NOTHING };
int is_nothing(Obj* x) {
    return x == &omni_nothing_obj || (x && IS_BOXED(x) && x->tag == TAG_NOTHING);
}

/* Object Constructors (Shimmed to Global Region) */
Obj* mk_int(long i) { _ensure_global_region(); return mk_int_region(_global_region, i); }
// mk_int_unboxed is static inline in omni.h
Obj* mk_float(double f) { _ensure_global_region(); return mk_float_region(_global_region, f); }
Obj* mk_char(long c) {
    if (c >= 0 && c <= 0x10FFFF) return mk_char_unboxed(c);
    _ensure_global_region(); return mk_char_region(_global_region, c);
}
// mk_bool is static inline in omni.h
Obj* mk_pair(Obj* a, Obj* b) { _ensure_global_region(); return mk_cell_region(_global_region, a, b); }
// mk_cell is alias in omni.h? No, removed alias to avoid conflict or need to add it if needed.
// omni.h defines mk_pair. csrc might emit mk_cell.
// region_value.h defines mk_cell_region.
// Let's add mk_cell wrapper if needed.
Obj* mk_cell(Obj* a, Obj* b) { return mk_pair(a, b); }

Obj* mk_sym(const char* s) { _ensure_global_region(); return mk_sym_region(_global_region, s); }
Obj* mk_nothing(void) { return &omni_nothing_obj; }
Obj* mk_box(Obj* v) { _ensure_global_region(); return mk_box_region(_global_region, v); }
Obj* mk_error(const char* msg) { _ensure_global_region(); return mk_error_region(_global_region, msg); }

/* Region-Resident Operations */
void box_set(Obj* b, Obj* v) { if (b && IS_BOXED(b) && b->tag == TAG_BOX) b->a = v; }
Obj* box_get(Obj* b) { return (b && IS_BOXED(b) && b->tag == TAG_BOX) ? b->a : NULL; }

/* Arithmetic */
Obj* prim_add(Obj* a, Obj* b) { return mk_int(obj_to_int(a) + obj_to_int(b)); }
Obj* prim_sub(Obj* a, Obj* b) { return mk_int(obj_to_int(a) - obj_to_int(b)); }
Obj* prim_mul(Obj* a, Obj* b) { return mk_int(obj_to_int(a) * obj_to_int(b)); }
Obj* prim_div(Obj* a, Obj* b) { 
    long bv = obj_to_int(b); 
    return mk_int(bv ? obj_to_int(a) / bv : 0); 
}

/* Comparisons */
Obj* prim_eq(Obj* a, Obj* b) { return mk_bool(obj_to_int(a) == obj_to_int(b)); }
Obj* prim_lt(Obj* a, Obj* b) { return mk_bool(obj_to_int(a) < obj_to_int(b)); }
Obj* prim_gt(Obj* a, Obj* b) { return mk_bool(obj_to_int(a) > obj_to_int(b)); }

/* List accessors */
// car/cdr macros in omni.h might conflict if we define functions.
// omni.h declares obj_car, obj_cdr.
Obj* obj_car(Obj* p) { return (p && IS_BOXED(p) && p->tag == TAG_PAIR) ? p->a : NULL; }
Obj* obj_cdr(Obj* p) { return (p && IS_BOXED(p) && p->tag == TAG_PAIR) ? p->b : NULL; }

/* I/O */
void print_obj(Obj* x) {
    if (!x) { printf("()\n"); return; }
    if (IS_IMMEDIATE_INT(x)) { printf("%ld", (long)INT_IMM_VALUE(x)); return; }
    if (IS_IMMEDIATE_CHAR(x)) { printf("%c", (char)CHAR_IMM_VALUE(x)); return; }
    if (IS_IMMEDIATE_BOOL(x)) { printf("%s", x == OMNI_TRUE ? "true" : "false"); return; }
    switch (x->tag) {
        case TAG_INT: printf("%ld", x->i); break;
        case TAG_FLOAT: printf("%g", x->f); break;
        case TAG_SYM: printf("%s", (char*)x->ptr);
            break;
        case TAG_PAIR: printf("("); print_obj(x->a); printf(" . "); print_obj(x->b); printf(")"); break;
        default: printf("#<obj:%d>", x->tag); break;
    }
}

Obj* prim_display(Obj* x) { print_obj(x); return mk_nothing(); }
Obj* prim_print(Obj* x) { print_obj(x); printf("\n"); return mk_nothing(); }
Obj* prim_newline(void) { printf("\n"); return mk_nothing(); }

/* Stubs for legacy GC logic */
void safe_point(void) {}
void flush_deferred(void) {}
void process_deferred(void) {}
void sym_init(void) {}
void sym_cleanup(void) {}
void region_init(void) {}
void invalidate_weak_refs_for(void* t) { (void)t; }
// mk_nil alias to NULL handled by is_nil logic or macro if needed.