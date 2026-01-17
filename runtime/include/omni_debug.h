/**
 * @file omni_debug.h
 * @brief Compile-time debug instrumentation for OmniLisp runtime
 *
 * Debug levels controlled by OMNI_DEBUG compile-time flag:
 * - DOMNI_DEBUG=0: No debug output (production)
 * - DOMNI_DEBUG=1: Minimal debug output (errors, warnings)
 * - DOMNI_DEBUG=2: Verbose debug output (detailed tracing)
 * - DOMNI_DEBUG=3: Maximum debug output (all diagnostics)
 */

#ifndef OMNI_DEBUG_H
#define OMNI_DEBUG_H

#include <stdio.h>
#include <stdlib.h>

/* Debug level configuration */
#ifndef OMNI_DEBUG
#define OMNI_DEBUG 0
#endif

/* Debug level check macros */
#if OMNI_DEBUG >= 1
#define DEBUG_ENABLED 1
#else
#define DEBUG_ENABLED 0
#endif

#if OMNI_DEBUG >= 2
#define DEBUG_VERBOSE 1
#else
#define DEBUG_VERBOSE 0
#endif

#if OMNI_DEBUG >= 3
#define DEBUG_MAX 1
#else
#define DEBUG_MAX 0
#endif

/**
 * @brief Minimal debug output (errors, critical warnings)
 * Active when OMNI_DEBUG >= 1
 */
#define DEBUG_ERROR(fmt, ...) \
	do { \
		if (DEBUG_ENABLED) { \
			fprintf(stderr, "[DEBUG ERROR] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Warning-level debug output
 * Active when OMNI_DEBUG >= 1
 */
#define DEBUG_WARN(fmt, ...) \
	do { \
		if (DEBUG_ENABLED) { \
			fprintf(stderr, "[DEBUG WARN] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Standard debug output (general tracing)
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_PRINT(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Verbose debug output (detailed info)
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_VERBOSE_PRINT(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG VERBOSE] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Maximum debug output (all diagnostics)
 * Active when OMNI_DEBUG >= 3
 */
#define DEBUG_MAX_PRINT(fmt, ...) \
	do { \
		if (DEBUG_MAX) { \
			fprintf(stderr, "[DEBUG MAX] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Parser-specific debug output
 * Uses [DEBUG] prefix to match existing parser conventions
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_PARSER(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG] " fmt "\n", ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Memory/region debug output
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_MEMORY(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG MEMORY] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Transmigration debug output
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_TRANSMIGRATE(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG TRANSMIGRATE] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Effect system debug output
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_EFFECT(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG EFFECT] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Continuation debug output
 * Active when OMNI_DEBUG >= 2
 */
#define DEBUG_CONTINUATION(fmt, ...) \
	do { \
		if (DEBUG_VERBOSE) { \
			fprintf(stderr, "[DEBUG CONTINUATION] %s:%d: " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
		} \
	} while (0)

/**
 * @brief Assert macro that always executes (unlike standard assert)
 * For runtime checks that should remain in production
 */
#define OMNI_ASSERT(cond, msg) \
	do { \
		if (!(cond)) { \
			fprintf(stderr, "[FATAL] %s:%d: Assertion failed: %s\n", __FILE__, __LINE__, msg); \
			abort(); \
		} \
	} while (0)

/**
 * @brief Debug-only assert (removed in production builds)
 * Only active when OMNI_DEBUG >= 1
 */
#define DEBUG_ASSERT(cond, msg) \
	do { \
		if (DEBUG_ENABLED && !(cond)) { \
			fprintf(stderr, "[DEBUG ASSERT] %s:%d: Assertion failed: %s\n", __FILE__, __LINE__, msg); \
			abort(); \
		} \
	} while (0)

/* ============================================================
 * Phase 4.4: Memory Profiling Infrastructure
 * ============================================================ */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/* Forward declarations */
struct Region;

/**
 * @brief Region statistics structure
 * Contains detailed metrics about a single region's memory usage
 */
typedef struct RegionStats {
    /* Allocation metrics */
    size_t bytes_allocated;       /* Current bytes allocated */
    size_t bytes_peak;            /* Peak bytes allocated */
    size_t allocation_count;      /* Number of allocations */

    /* Inline buffer usage */
    size_t inline_bytes_used;     /* Bytes used in inline buffer */
    size_t inline_capacity;       /* Total inline buffer capacity */

    /* Arena metrics */
    size_t arena_chunks;          /* Number of arena chunks */

    /* Repair/escape metrics */
    size_t escape_repairs;        /* Transmigrate/repair operations */

    /* Reference counting */
    int external_rc;              /* External reference count */
    int tether_count;             /* Active tethers */

    /* Lifetime info */
    uint64_t lifetime_rank;       /* Region depth in outlives tree */
    bool scope_alive;             /* Is semantic scope still active */
    bool is_thread_local;         /* Single-thread optimized */

    /* Parent chain depth */
    int parent_depth;             /* Depth to root region */
} RegionStats;

/**
 * @brief Global memory statistics structure
 * Aggregate metrics across all regions
 */
typedef struct GlobalMemStats {
    /* Region counts */
    size_t regions_created;       /* Total regions created */
    size_t regions_destroyed;     /* Total regions destroyed */
    size_t regions_active;        /* Currently active regions */
    size_t regions_peak;          /* Peak active regions */

    /* Allocation metrics */
    size_t total_bytes_allocated; /* Total bytes ever allocated */
    size_t current_bytes;         /* Current bytes in use */
    size_t peak_bytes;            /* Peak memory usage */

    /* Operation counts */
    size_t transmigrate_calls;    /* Times transmigrate was called */
    size_t merge_calls;           /* Times region_merge was called */
    size_t store_repairs;         /* Store barrier repairs */
} GlobalMemStats;

/* Get statistics for a region */
void omni_region_get_stats(struct Region* r, RegionStats* stats);

/* Print region statistics to stderr */
void omni_region_print_stats(struct Region* r, const char* label);

/* Get global memory statistics */
void omni_get_global_stats(GlobalMemStats* stats);

/* Print global memory summary to stderr */
void omni_print_memory_summary(void);

/* Reset global statistics (useful for per-test tracking) */
void omni_reset_global_stats(void);

/* Print all currently active regions with their statistics */
void omni_print_active_regions(void);

/* ============================================================
 * Leak Detection (Debug builds only)
 * ============================================================ */

#if OMNI_DEBUG >= 1

/* Track an allocation for leak detection */
void omni_debug_track_alloc(void* ptr, size_t size, const char* file, int line);

/* Track a deallocation */
void omni_debug_track_free(void* ptr, const char* file, int line);

/* Print all unfreed allocations (potential leaks) */
void omni_debug_print_leaks(void);

/* Get count of unfreed allocations */
size_t omni_debug_leak_count(void);

/* Clear all tracking (for test reset) */
void omni_debug_clear_tracking(void);

/* Macros for automatic file/line tracking */
#define OMNI_TRACK_ALLOC(ptr, size) \
    omni_debug_track_alloc((ptr), (size), __FILE__, __LINE__)

#define OMNI_TRACK_FREE(ptr) \
    omni_debug_track_free((ptr), __FILE__, __LINE__)

#else /* OMNI_DEBUG < 1 */

/* No-op macros when debugging is disabled */
#define OMNI_TRACK_ALLOC(ptr, size) ((void)0)
#define OMNI_TRACK_FREE(ptr) ((void)0)

#endif /* OMNI_DEBUG */

/* ============================================================
 * Profiling Callbacks
 * ============================================================ */

/**
 * @brief Callback type for allocation events
 * Called on every region_alloc when profiling is enabled
 */
typedef void (*OmniAllocCallback)(struct Region* r, void* ptr, size_t size);

/**
 * @brief Callback type for region lifecycle events
 * Events: "create", "destroy", "exit", "retain", "release"
 */
typedef void (*OmniRegionCallback)(struct Region* r, const char* event);

/* Set allocation profiling callback (NULL to disable) */
void omni_set_alloc_callback(OmniAllocCallback callback);

/* Set region lifecycle callback (NULL to disable) */
void omni_set_region_callback(OmniRegionCallback callback);

#endif /* OMNI_DEBUG_H */
