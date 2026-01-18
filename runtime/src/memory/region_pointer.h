#ifndef OMNI_REGION_POINTER_H
#define OMNI_REGION_POINTER_H

#include <stdint.h>
#include <stdbool.h>
#include "region_core.h"

/*
 * Include omni.h for struct Obj definition (needed for accessing
 * obj->generation field in pointer_mask_safe_access).
 *
 * Note: Must include guard to prevent circular dependencies since
 * region_core.h is included by omni.h.
 */
#ifndef OMNI_RUNTIME_H
#include "../../include/omni.h"
#endif

/*
 * region_pointer.h - Pointer Masking for Cross-Region References
 *
 * OPTIMIZATION (T-opt-region-metadata-pointer-masking): Encodes region
 * information in pointer high bits to enable safe cross-region references
 * without fat pointers.
 *
 * Key ideas:
 * - Uses unused high bits of 64-bit pointers to store region ID
 * - Maintains 8-byte alignment (low 3 bits must be 0)
 * - Enables region-safe access without fat pointer overhead
 * - Compatible with existing tagged pointer system
 *
 * Architecture (64-bit):
 * - Bits 0-2:     Tag (for immediate values)
 * - Bits 3-47:    Actual pointer (up to 8 TB address space)
 * - Bits 48-63:   Region ID (up to 65K regions)
 *
 * This gives us:
 * - 65,536 possible regions (16 bits)
 * - 8 TB addressable memory per region (45 bits)
 * - Zero runtime overhead for encoding/decoding
 */

/* Pointer masking constants */
#define POINTER_MASK_REGION_BITS   16   /* Number of bits for region ID */
#define POINTER_MASK_ADDR_BITS      45   /* Number of bits for address */
#define POINTER_SHIFT               3    /* 8-byte alignment (2^3 = 8) */

#define POINTER_MAX_REGIONS         (1ULL << POINTER_MASK_REGION_BITS)  /* 65,536 */
#define POINTER_MAX_ADDR            ((1ULL << POINTER_MASK_ADDR_BITS) - 1)

/* Encoding/decoding masks */
#define POINTER_ADDR_MASK           ((1ULL << (POINTER_MASK_ADDR_BITS + POINTER_SHIFT)) - 1)
#define POINTER_REGION_MASK         (((1ULL << POINTER_MASK_REGION_BITS) - 1ULL) << (POINTER_MASK_ADDR_BITS + POINTER_SHIFT))
#define POINTER_REGION_SHIFT        (POINTER_MASK_ADDR_BITS + POINTER_SHIFT)

/*
 * pointer_mask_encode - Encode region ID into a pointer
 *
 * @param ptr: The original pointer (must be 8-byte aligned)
 * @param region_id: The region ID (0 to POINTER_MAX_REGIONS-1)
 * @return: Encoded pointer with region information
 *
 * Example:
 *   Obj* obj = allocate_obj();
 *   Obj* encoded = pointer_mask_encode(obj, region_id);
 *   // Later decode:
 *   Obj* original = pointer_mask_decode(encoded);
 *   uint16_t region = pointer_mask_get_region(encoded);
 */
static inline void* pointer_mask_encode(const void* ptr, uint16_t region_id) {
    if (!ptr) return NULL;

    uintptr_t addr = (uintptr_t)ptr;

    /* Verify 8-byte alignment */
    if (addr & 0x7ULL) {
        /* Not aligned, can't encode region info */
        return (void*)addr;
    }

    /* Encode region ID in high bits */
    uintptr_t encoded = addr | ((uintptr_t)region_id << POINTER_REGION_SHIFT);
    return (void*)encoded;
}

/*
 * pointer_mask_decode - Extract original pointer from encoded pointer
 *
 * @param encoded: The encoded pointer
 * @return: Original pointer without region information
 */
static inline void* pointer_mask_decode(const void* encoded) {
    if (!encoded) return NULL;

    uintptr_t ptr = (uintptr_t)encoded;
    return (void*)(ptr & POINTER_ADDR_MASK);
}

/*
 * pointer_mask_get_region - Extract region ID from encoded pointer
 *
 * @param encoded: The encoded pointer
 * @return: Region ID (0 to POINTER_MAX_REGIONS-1)
 */
static inline uint16_t pointer_mask_get_region(const void* encoded) {
    if (!encoded) return 0;

    uintptr_t ptr = (uintptr_t)encoded;
    return (uint16_t)(ptr >> POINTER_REGION_SHIFT);
}

/*
 * pointer_mask_is_same_region - Check if two encoded pointers are from same region
 *
 * @param ptr1: First encoded pointer
 * @param ptr2: Second encoded pointer
 * @return: true if both pointers are from the same region
 */
static inline bool pointer_mask_is_same_region(const void* ptr1, const void* ptr2) {
    if (!ptr1 || !ptr2) return false;

    uint16_t region1 = pointer_mask_get_region(ptr1);
    uint16_t region2 = pointer_mask_get_region(ptr2);
    return region1 == region2;
}

/*
 * pointer_mask_is_cross_region - Check if a reference crosses regions
 *
 * @param from_ptr: Source pointer
 * @param to_ptr: Target pointer
 * @return: true if pointers are from different regions
 */
static inline bool pointer_mask_is_cross_region(const void* from_ptr, const void* to_ptr) {
    return !pointer_mask_is_same_region(from_ptr, to_ptr);
}

/*
 * pointer_mask_safe_access - Access object with generation check if cross-region
 *
 * @param obj_ptr: The object pointer (may be encoded)
 * @param current_region: The current region ID
 * @return: true if access is safe, false otherwise
 *
 * This function implements a safety check for cross-region access using IPGE
 * (Indexed Pointer Generation Epoch).
 *
 * For same-region access: No check needed (fast path)
 * For cross-region access: Decode pointer, get object's generation,
 * compare with region's current generation.
 *
 * IPGE: Each region has a generation number that increments when
 * region is reused. Objects store the generation they were
 * allocated with. If generations don't match, the pointer is stale.
 */
static inline bool pointer_mask_safe_access(const void* obj_ptr, uint16_t current_region) {
    if (!obj_ptr) return false;

    uint16_t obj_region = pointer_mask_get_region(obj_ptr);

    /* Same region: always safe (no generation check needed) */
    if (obj_region == current_region) {
        return true;
    }

    /* Cross-region: Need IPGE generation check */

    /* Region ID 0 is reserved for NULL/unencoded pointers */
    if (obj_region == 0) {
        return false;
    }

    /* Decode pointer to get actual object */
    void* actual_ptr = pointer_mask_decode(obj_ptr);
    if (!actual_ptr) {
        return false;
    }

    /* Look up target region from registry (O(1) lookup) */
    Region* target_region = region_of(obj_ptr);
    if (!target_region) {
        /* Region no longer exists or was destroyed */
        return false;
    }

    /* Compare object's generation with region's current generation */
    /* If they don't match, object was allocated in an older generation */
    /* and the region has been reused - this is a stale pointer */
    Obj* obj = (Obj*)actual_ptr;
    return obj->generation == target_region->generation;
}

/*
 * pointer_mask_encode_with_generation - Encode pointer with generation
 *
 * This is a placeholder for future integration with IPGE generation checking.
 * Currently just encodes the region ID, as the generation is stored
 * inline in the object's header (Obj.generation field).
 *
 * @param ptr: The original pointer
 * @param region_id: The region ID
 * @param generation: The generation number (for future use)
 * @return: Encoded pointer
 */
static inline void* pointer_mask_encode_with_generation(const void* ptr, uint16_t region_id, uint32_t generation) {
    (void)generation;  /* Generation stored in Obj header, not encoded in pointer */
    return pointer_mask_encode(ptr, region_id);
}

#endif /* OMNI_REGION_POINTER_H */
