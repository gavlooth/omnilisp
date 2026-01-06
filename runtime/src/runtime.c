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

/* Forward declarations */
typedef struct Obj Obj;
typedef struct Closure Closure;

/* RC-G Runtime: Standard RC is now Region-RC (Coarse-grained) */
void inc_ref(Obj* x) { (void)x; }
void dec_ref(Obj* x) { (void)x; }
void free_obj(Obj* x) { (void)x; }

/* ========== Tagged Pointers (Multi-Type Immediates) ========== */
#define IMM_TAG_MASK     0x7ULL
#define IMM_TAG_PTR      0x0ULL
#define IMM_TAG_INT      0x1ULL
#define IMM_TAG_CHAR     0x2ULL
#define IMM_TAG_BOOL     0x3ULL

#define GET_IMM_TAG(p)   (((uintptr_t)(p)) & IMM_TAG_MASK)
#define IS_IMMEDIATE(p)      (GET_IMM_TAG(p) != IMM_TAG_PTR)
#define IS_IMMEDIATE_INT(p)  (GET_IMM_TAG(p) == IMM_TAG_INT)
#define IS_IMMEDIATE_CHAR(p) (GET_IMM_TAG(p) == IMM_TAG_CHAR)
#define IS_IMMEDIATE_BOOL(p) (GET_IMM_TAG(p) == IMM_TAG_BOOL)
#define IS_BOXED(p)          (GET_IMM_TAG(p) == IMM_TAG_PTR && (p) != NULL)

#define MAKE_INT_IMM(n)      ((Obj*)(((uintptr_t)(n) << 3) | IMM_TAG_INT))
#define INT_IMM_VALUE(p)     ((long)((intptr_t)(p) >> 3))
#define OMNI_FALSE         ((Obj*)(((uintptr_t)0 << 3) | IMM_TAG_BOOL))
#define OMNI_TRUE          ((Obj*)(((uintptr_t)1 << 3) | IMM_TAG_BOOL))
#define MAKE_CHAR_IMM(c)     ((Obj*)(((uintptr_t)(c) << 3) | IMM_TAG_CHAR))
#define CHAR_IMM_VALUE(p)    ((long)(((uintptr_t)(p)) >> 3))

/* Core tags for runtime values */
typedef enum {
    TAG_INT = 1, TAG_FLOAT, TAG_CHAR, TAG_PAIR, TAG_SYM,
    TAG_BOX, TAG_CLOSURE, TAG_CHANNEL, TAG_ERROR,
    TAG_NOTHING, TAG_FIBER, TAG_CONTINUATION
} ObjTag;

/* Core object type */
typedef struct Obj {
    int mark;               /* Region management */
    int tag;
    int is_pair;
    union {
        long i;
        double f;
        struct { struct Obj *a, *b; };
        void* ptr;
    };
} Obj;

/* Global Region for Interpreter/Legacy support */
static Region* _global_region = NULL;
static void _ensure_global_region(void) {
    if (!_global_region) _global_region = region_create();
}

/* Helpers */
int is_nil(Obj* x) { return x == NULL; }
static Obj omni_nothing_obj = { .mark = -1, .tag = TAG_NOTHING, .is_pair = 0, .ptr = NULL };
int is_nothing(Obj* x) {
    return x == &omni_nothing_obj || (x && !IS_IMMEDIATE(x) && x->tag == TAG_NOTHING);
}

static inline long obj_to_int(Obj* p) {
    if (IS_IMMEDIATE_INT(p)) return INT_IMM_VALUE(p);
    if (IS_IMMEDIATE_BOOL(p)) return p == OMNI_TRUE ? 1 : 0;
    if (IS_IMMEDIATE_CHAR(p)) return CHAR_IMM_VALUE(p);
    return p ? p->i : 0;
}

static inline int obj_to_bool(Obj* p) {
    if (IS_IMMEDIATE_BOOL(p)) return p == OMNI_TRUE;
    if (p == NULL) return 1;
    if (IS_IMMEDIATE_INT(p) || IS_IMMEDIATE_CHAR(p)) return 1;
    if (p->tag == TAG_NOTHING) return 0;
    return 1;
}

static inline int obj_tag(Obj* p) {
    if (p == NULL) return 0;
    if (IS_IMMEDIATE_INT(p)) return TAG_INT;
    if (IS_IMMEDIATE_CHAR(p)) return TAG_CHAR;
    if (IS_IMMEDIATE_BOOL(p)) return TAG_INT;
    return p->tag;
}

/* Object Constructors (Shimmed to Global Region) */
Obj* mk_int(long i) { _ensure_global_region(); return (Obj*)mk_int_region(_global_region, i); }
Obj* mk_int_unboxed(long i) { return MAKE_INT_IMM(i); }
Obj* mk_float(double f) { _ensure_global_region(); return (Obj*)mk_float_region(_global_region, f); }
Obj* mk_char(long c) {
    if (c >= 0 && c <= 0x10FFFF) return MAKE_CHAR_IMM(c);
    _ensure_global_region(); return (Obj*)mk_char_region(_global_region, c);
}
Obj* mk_bool(int b) { return b ? OMNI_TRUE : OMNI_FALSE; }
Obj* mk_pair(Obj* a, Obj* b) { _ensure_global_region(); return (Obj*)mk_cell_region(_global_region, (Value*)a, (Value*)b); }
Obj* mk_sym(const char* s) { _ensure_global_region(); return (Obj*)mk_sym_region(_global_region, s); }
Obj* mk_nothing(void) { return &omni_nothing_obj; }
Obj* mk_box(Obj* v) { _ensure_global_region(); return (Obj*)mk_box_region(_global_region, (Value*)v); }
Obj* mk_error(const char* msg) { _ensure_global_region(); return (Obj*)mk_error_region(_global_region, msg); }

/* Region-Resident Operations */
void box_set(Obj* b, Obj* v) { if (b && b->tag == TAG_BOX) b->ptr = v; }
Obj* box_get(Obj* b) { return (b && b->tag == TAG_BOX) ? (Obj*)b->ptr : NULL; }

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
Obj* car(Obj* p) { return (p && obj_tag(p) == TAG_PAIR) ? p->a : NULL; }
Obj* cdr(Obj* p) { return (p && obj_tag(p) == TAG_PAIR) ? p->b : NULL; }
Obj* obj_car(Obj* p) { return car(p); }
Obj* obj_cdr(Obj* p) { return cdr(p); }

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