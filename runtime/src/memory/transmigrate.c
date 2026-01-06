/*
 * transmigrate.c - Adaptive Deep Copy with Arena Promotion
 *
 * Implements "transmigration" - moving an object graph from one region
 * to another. Uses deep copy for small graphs and arena promotion
 * for large graphs.
 */

#include "transmigrate.h"
#include <stdio.h>
#include <string.h>
#include "../../../third_party/uthash/uthash.h"
#include "../../../src/runtime/types.h"

typedef struct PtrMapEntry {
    void* old_ptr;
    void* new_ptr;
    UT_hash_handle hh;
} PtrMapEntry;

typedef struct {
    Region* dest;
    PtrMapEntry* map;
    size_t bytes_copied;
    bool use_promotion;
} TransmigrateCtx;

#define TRANSMIGRATE_PROMOTION_THRESHOLD 4096

static Value* copy_value(TransmigrateCtx* ctx, Value* obj);

void* transmigrate(void* root, Region* src_region, Region* dest_region) {
    if (!root) return NULL;
    if (!dest_region) return NULL;

    TransmigrateCtx ctx;
    ctx.dest = dest_region;
    ctx.map = NULL;
    ctx.bytes_copied = 0;
    ctx.use_promotion = false;

    // Attempt Deep Copy
    Value* result = copy_value(&ctx, (Value*)root);

    // Check if we decided to promote mid-flight
    if (ctx.use_promotion && src_region) {
        // Discard the partial copy (it will be freed with dest eventually)
        // Promote the source arena to dest
        arena_promote(&dest_region->arena, &src_region->arena);
        
        // Return original root (it is now safe in dest)
        result = (Value*)root;
    }

    // Cleanup map
    PtrMapEntry *current, *tmp;
    HASH_ITER(hh, ctx.map, current, tmp) {
        HASH_DEL(ctx.map, current);
        free(current);
    }

    return result;
}

static Value* copy_value(TransmigrateCtx* ctx, Value* obj) {
    if (!obj) return NULL;
    if (ctx->use_promotion) return NULL; // Fast exit if aborted

    PtrMapEntry* entry = NULL;
    HASH_FIND_PTR(ctx->map, &obj, entry);
    if (entry) return (Value*)entry->new_ptr;

    // Check threshold
    if (ctx->bytes_copied > TRANSMIGRATE_PROMOTION_THRESHOLD) {
        ctx->use_promotion = true;
        return NULL; // Abort recursion
    }

    Value* new_obj = region_alloc(ctx->dest, sizeof(Value));
    ctx->bytes_copied += sizeof(Value);

    entry = malloc(sizeof(PtrMapEntry));
    entry->old_ptr = obj;
    entry->new_ptr = new_obj;
    HASH_ADD_PTR(ctx->map, old_ptr, entry);

    new_obj->tag = obj->tag;

    switch (obj->tag) {
        case T_INT: new_obj->i = obj->i; break;
        case T_NIL: break;
        case T_NOTHING: break;
        case T_CHAR: new_obj->codepoint = obj->codepoint; break;
        case T_FLOAT: new_obj->f = obj->f; break;

        case T_CELL:
            new_obj->cell.car = copy_value(ctx, obj->cell.car);
            new_obj->cell.cdr = copy_value(ctx, obj->cell.cdr);
            break;

        case T_SYM:
        case T_CODE:
        case T_ERROR:
            if (obj->s) {
                size_t len = strlen(obj->s);
                new_obj->s = region_alloc(ctx->dest, len + 1);
                strcpy(new_obj->s, obj->s);
                ctx->bytes_copied += len + 1;
            } else {
                new_obj->s = NULL;
            }
            break;

        // ... Add other types as needed ...
        default:
            // For MVP, just shallow copy unknown types
            memcpy(new_obj, obj, sizeof(Value));
            break;
    }

    return new_obj;
}