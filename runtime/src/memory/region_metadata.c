/*
 * region_metadata.c - Region-Level Type Metadata Implementation
 *
 * Initializes and manages type metadata for regions.
 */

#include "region_core.h"  /* Must be first - pulls in Region, Arena */
#include "omni.h"          /* For struct Obj definition (needed for sizeof/offsetof) */
#include <stdlib.h>
#include <string.h>

/* ========== Metadata Initialization ========== */

/*
 * init_core_type_metadata - Initialize metadata for all core types
 *
 * This function populates the region's type_table with metadata
 * for all core OmniLisp types. Called during region_create().
 */
static void init_core_type_metadata(Region* r) {
    if (!r) return;
    
    /* Allocate type table for all core types */
    r->num_types = TYPE_ID_MAX;
    r->type_table = (TypeMetadata*)calloc(r->num_types, sizeof(TypeMetadata));
    
    if (!r->type_table) {
        /* Memory allocation failed */
        return;
    }
    
    /* TYPE_ID_INT */
    r->type_table[TYPE_ID_INT] = (TypeMetadata){
        .name = "Int",
        .type_id = TYPE_ID_INT,
        .size = sizeof(struct Obj),  /* Must be full Obj size, not just payload */
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 16,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Integer type (stored in immediate or heap)"
    };
    
    /* TYPE_ID_FLOAT */
    r->type_table[TYPE_ID_FLOAT] = (TypeMetadata){
        .name = "Float",
        .type_id = TYPE_ID_FLOAT,
        .size = sizeof(struct Obj),  /* Must be full Obj size, not just payload */
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 16,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Floating-point type"
    };
    
    /* TYPE_ID_CHAR */
    r->type_table[TYPE_ID_CHAR] = (TypeMetadata){
        .name = "Char",
        .type_id = TYPE_ID_CHAR,
        .size = sizeof(struct Obj),  /* Must be full Obj size, not just payload */
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 8,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Character type (often immediate)"
    };
    
    /* TYPE_ID_PAIR (Cons cell) */
    r->type_table[TYPE_ID_PAIR] = (TypeMetadata){
        .name = "Pair",
        .type_id = TYPE_ID_PAIR,
        .size = sizeof(struct Obj),  /* Current size */
        .alignment = 8,
        .num_pointer_fields = 2,
        .pointer_offsets = {offsetof(struct Obj, a), offsetof(struct Obj, b)},
        .can_inline = true,
        .inline_threshold = 56,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Cons cell (pair)"
    };
    
    /* TYPE_ID_ARRAY */
    r->type_table[TYPE_ID_ARRAY] = (TypeMetadata){
        .name = "Array",
        .type_id = TYPE_ID_ARRAY,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,  /* Arrays are usually large */
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Array type"
    };
    
    /* TYPE_ID_STRING */
    r->type_table[TYPE_ID_STRING] = (TypeMetadata){
        .name = "String",
        .type_id = TYPE_ID_STRING,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "String type"
    };
    
    /* TYPE_ID_SYMBOL */
    r->type_table[TYPE_ID_SYMBOL] = (TypeMetadata){
        .name = "Symbol",
        .type_id = TYPE_ID_SYMBOL,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 24,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Symbol type (often interned)"
    };
    
    /* TYPE_ID_DICT */
    r->type_table[TYPE_ID_DICT] = (TypeMetadata){
        .name = "Dict",
        .type_id = TYPE_ID_DICT,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Dictionary (hash map)"
    };
    
    /* TYPE_ID_CLOSURE */
    r->type_table[TYPE_ID_CLOSURE] = (TypeMetadata){
        .name = "Closure",
        .type_id = TYPE_ID_CLOSURE,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Function closure"
    };
    
    /* TYPE_ID_BOX */
    r->type_table[TYPE_ID_BOX] = (TypeMetadata){
        .name = "Box",
        .type_id = TYPE_ID_BOX,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, a)},
        .can_inline = true,
        .inline_threshold = 32,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Boxed value"
    };
    
    /* TYPE_ID_CHANNEL */
    r->type_table[TYPE_ID_CHANNEL] = (TypeMetadata){
        .name = "Channel",
        .type_id = TYPE_ID_CHANNEL,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Communication channel"
    };
    
    /* TYPE_ID_THREAD */
    r->type_table[TYPE_ID_THREAD] = (TypeMetadata){
        .name = "Thread",
        .type_id = TYPE_ID_THREAD,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Thread handle"
    };
    
    /* TYPE_ID_ERROR */
    r->type_table[TYPE_ID_ERROR] = (TypeMetadata){
        .name = "Error",
        .type_id = TYPE_ID_ERROR,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = true,
        .inline_threshold = 32,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Error value"
    };
    
    /* TYPE_ID_ATOM */
    r->type_table[TYPE_ID_ATOM] = (TypeMetadata){
        .name = "Atom",
        .type_id = TYPE_ID_ATOM,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 16,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Atom (symbol-like)"
    };
    
    /* TYPE_ID_TUPLE */
    r->type_table[TYPE_ID_TUPLE] = (TypeMetadata){
        .name = "Tuple",
        .type_id = TYPE_ID_TUPLE,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 2,
        .pointer_offsets = {offsetof(struct Obj, a), offsetof(struct Obj, b)},
        .can_inline = true,
        .inline_threshold = 48,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Tuple (fixed-size pair)"
    };
    
    /* TYPE_ID_NAMED_TUPLE */
    r->type_table[TYPE_ID_NAMED_TUPLE] = (TypeMetadata){
        .name = "NamedTuple",
        .type_id = TYPE_ID_NAMED_TUPLE,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 2,
        .pointer_offsets = {offsetof(struct Obj, a), offsetof(struct Obj, b)},
        .can_inline = true,
        .inline_threshold = 64,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Named tuple (like struct)"
    };
    
    /* TYPE_ID_GENERIC */
    r->type_table[TYPE_ID_GENERIC] = (TypeMetadata){
        .name = "Generic",
        .type_id = TYPE_ID_GENERIC,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Generic type (parametric)"
    };
    
    /* TYPE_ID_KIND */
    r->type_table[TYPE_ID_KIND] = (TypeMetadata){
        .name = "Kind",
        .type_id = TYPE_ID_KIND,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 1,
        .pointer_offsets = {offsetof(struct Obj, ptr)},
        .can_inline = false,
        .inline_threshold = 0,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Kind (type object)"
    };
    
    /* TYPE_ID_NOTHING */
    r->type_table[TYPE_ID_NOTHING] = (TypeMetadata){
        .name = "Nothing",
        .type_id = TYPE_ID_NOTHING,
        .size = sizeof(struct Obj),
        .alignment = 8,
        .num_pointer_fields = 0,
        .pointer_offsets = {0},
        .can_inline = true,
        .inline_threshold = 8,
        .trace = NULL,
        .destroy = NULL,
        .equals = NULL,
        .hash = NULL,
        .debug_info = "Nothing type (unit)"
    };
}

/* ========== Public API ========== */

/*
 * type_metadata_init - Initialize type metadata for a region
 *
 * This function is called during region_create() to set up the
 * type_table with metadata for all core types.
 */
void type_metadata_init(Region* r) {
    if (!r) return;

    init_core_type_metadata(r);
}

/*
 * type_metadata_get - Get metadata for a type_id
 *
 * @param region: The region containing the metadata
 * @param type_id: The type identifier
 * @return: Pointer to TypeMetadata, or NULL if invalid
 */
const TypeMetadata* type_metadata_get(const Region* region, TypeID type_id) {
    if (!region) return NULL;

    if (type_id >= 0 && (uint32_t)type_id < region->num_types) {
        return &region->type_table[type_id];
    }
    return NULL;
}

/*
 * type_metadata_print - Print metadata information for debugging
 */
void type_metadata_print(const TypeMetadata* meta) {
    if (!meta) {
        printf("TypeMetadata: NULL\n");
        return;
    }
    
    printf("TypeMetadata:\n");
    printf("  name: %s\n", meta->name);
    printf("  type_id: %d\n", meta->type_id);
    printf("  size: %zu bytes\n", meta->size);
    printf("  alignment: %zu bytes\n", meta->alignment);
    printf("  pointer_fields: %d\n", meta->num_pointer_fields);
    printf("  can_inline: %s\n", meta->can_inline ? "yes" : "no");
    printf("  inline_threshold: %zu bytes\n", meta->inline_threshold);
    printf("  debug_info: %s\n", meta->debug_info ? meta->debug_info : "none");
}

/*
 * type_metadata_dump - Dump all metadata from a region
 */
void type_metadata_dump(const Region* region) {
    if (!region) {
        printf("Region: NULL\n");
        return;
    }
    
    printf("\n=== Region Metadata Dump ===\n");
    printf("num_types: %u\n", region->num_types);
    printf("type_table: %p\n\n", (void*)region->type_table);
    
    if (!region->type_table) {
        printf("  (no metadata table)\n");
        return;
    }
    
    for (uint32_t i = 0; i < region->num_types; i++) {
        const TypeMetadata* meta = &region->type_table[i];
        if (meta->name) {
            printf("[%2u] %s\n", i, meta->name);
        }
    }
    
    printf("=============================\n\n");
}
