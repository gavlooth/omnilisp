#ifndef OMNI_REGION_METADATA_H
#define OMNI_REGION_METADATA_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

/*
 * region_metadata.h - Region-Level Type Metadata System
 *
 * This module implements centralized type metadata for regions, eliminating
 * per-object tag overhead and enabling compile-time type resolution.
 *
 * Key benefits:
 * - Eliminates per-object tag field (saves 4-8 bytes per object)
 * - Enables inline allocation for small objects
 * - Compile-time type_id constants (aligned with ASAP philosophy)
 * - Centralized metadata per region (not per object)
 */

/* ========== Type ID Constants ========== */

/* Core type IDs (compile-time constants) */
typedef enum {
    TYPE_ID_INT = 0,
    TYPE_ID_FLOAT,
    TYPE_ID_CHAR,
    TYPE_ID_PAIR,      /* Cons cell */
    TYPE_ID_ARRAY,
    TYPE_ID_STRING,
    TYPE_ID_SYMBOL,
    TYPE_ID_DICT,
    TYPE_ID_CLOSURE,
    TYPE_ID_BOX,
    TYPE_ID_CHANNEL,
    TYPE_ID_THREAD,
    TYPE_ID_ERROR,
    TYPE_ID_ATOM,
    TYPE_ID_TUPLE,
    TYPE_ID_NAMED_TUPLE,
    TYPE_ID_GENERIC,
    TYPE_ID_KIND,
    TYPE_ID_NOTHING,
    TYPE_ID_MAX
} TypeID;

/* ========== Type Metadata Structure ========== */

/*
 * TypeMetadata - Complete type information stored once per region
 *
 * Instead of storing tag/size in every object, we store this information
 * once per region and reference it by type_id.
 */
typedef struct TypeMetadata {
    /* Type identification */
    const char* name;               /* "Int", "Pair", "Array", ... */
    TypeID type_id;                 /* Numeric type identifier */
    
    /* Memory layout */
    size_t size;                    /* Object size in bytes */
    size_t alignment;               /* Alignment requirement */
    
    /* GC/RC tracing information */
    uint8_t num_pointer_fields;     /* Number of pointer fields */
    uint8_t pointer_offsets[8];     /* Offsets of pointer fields within object */
    
    /* Inline allocation info */
    bool can_inline;                /* Can this type be inlined in parent? */
    size_t inline_threshold;        /* Max size for inlining */
    
    /* Operations (function pointers) */
    void (*trace)(struct Obj* obj, void (*visit)(struct Obj**));
    void (*destroy)(struct Obj* obj);
    bool (*equals)(struct Obj* a, struct Obj* b);
    size_t (*hash)(struct Obj* obj);
    
    /* Debug info */
    const char* debug_info;         /* Additional debug information */
} TypeMetadata;

/* ========== Forward Declarations ========== */

struct Obj;  /* Forward declaration (defined in omni.h) */

/* ========== Metadata API ========== */

/*
 * type_metadata_init - Initialize type metadata for a region
 *
 * @param region: The region to initialize metadata for
 *
 * Sets up the type_table with metadata for all core types.
 * This must be called once per region during region_create().
 */
void type_metadata_init(Region* region);

/*
 * type_metadata_get - Get metadata for a type_id
 *
 * @param region: The region containing the metadata
 * @param type_id: The type identifier
 * @return: Pointer to TypeMetadata, or NULL if invalid
 *
 * Fast lookup: metadata = &region->type_table[type_id]
 * Implemented in region_metadata.c to avoid circular dependency.
 */
const TypeMetadata* type_metadata_get(const Region* region, TypeID type_id);

/*
 * type_metadata_print - Print metadata information for debugging
 * 
 * @param meta: The metadata to print
 */
void type_metadata_print(const TypeMetadata* meta);

/*
 * type_metadata_dump - Dump all metadata from a region
 *
 * @param region: The region to dump metadata from
 */
void type_metadata_dump(const Region* region);

/* ========== Metadata Initialization Macros ========== */

/* Helper macro to define type metadata */
#define TYPE_METADATA_INIT(name_, id_, size_, align_, num_ptrs_, ptrs_, can_inline_, thresh_) \
    { \
        .name = (name_), \
        .type_id = (id_), \
        .size = (size_), \
        .alignment = (align_), \
        .num_pointer_fields = (num_ptrs_), \
        .pointer_offsets = { ptrs_ }, \
        .can_inline = (can_inline_), \
        .inline_threshold = (thresh_), \
        .trace = NULL, \
        .destroy = NULL, \
        .equals = NULL, \
        .hash = NULL, \
        .debug_info = "Core type" \
    }

#endif /* OMNI_REGION_METADATA_H */
