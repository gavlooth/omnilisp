/*@semantic
id: file::profile.c
kind: file
name: profile.c
summary: Performance profiling utilities for OmniLisp runtime.
responsibility:
  - Track function call counts and execution times
  - Identify performance hotspots
  - Track memory allocations per call site
  - Generate profiling reports
inputs:
  - Function calls instrumented via profiling macros
outputs:
  - Profiling statistics and reports
side_effects:
  - global_state (maintains profiling registry)
  - io (prints profiling reports)
related_symbols:
  - omni.h
tags:
  - profiling
  - performance
  - developer-tools
  - issue-27-p4
*/

#include "../include/omni.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Forward declarations */
/* apply is prim_apply in piping.c */

/* ============================================================================
 * Profiling Data Structures
 * ============================================================================ */

#define MAX_PROFILE_ENTRIES 1024
#define MAX_NAME_LENGTH 256

/*@semantic
id: struct::ProfileEntry
kind: struct
name: ProfileEntry
summary: Statistics for a single profiled function/expression.
tags:
  - profiling
  - statistics
*/
typedef struct ProfileEntry {
    char name[MAX_NAME_LENGTH];
    uint64_t call_count;
    uint64_t total_ns;
    uint64_t min_ns;
    uint64_t max_ns;
    size_t alloc_bytes;
    size_t alloc_count;
    bool active;
} ProfileEntry;

/*@semantic
id: global::g_profile_entries
kind: global
name: g_profile_entries
summary: Global array of profiling entries.
tags:
  - profiling
  - registry
*/
static ProfileEntry g_profile_entries[MAX_PROFILE_ENTRIES];
static int g_profile_count = 0;
static bool g_profiling_enabled = false;

/* Current profiling context for nested calls (reserved for future use) */
__attribute__((unused)) static const char* g_current_profile_name = NULL;
__attribute__((unused)) static uint64_t g_current_profile_start = 0;

/* ============================================================================
 * Sorting Utilities
 * ============================================================================ */

/*@semantic
id: function::compare_profile_indices_desc
kind: function
name: compare_profile_indices_desc
summary: Compare two profile indices by total_ns in descending order for qsort.
inputs:
  - a: const void* — pointer to first index
  - b: const void* — pointer to second index
outputs:
  - return: int — negative if a > b, positive if a < b, 0 if equal
tags:
  - profiling
  - sorting
  - internal
*/
// REVIEWED:OPTIMIZED - comparison function for O(n log n) qsort
static int compare_profile_indices_desc(const void* a, const void* b) {
    int idx_a = *(const int*)a;
    int idx_b = *(const int*)b;
    uint64_t total_a = g_profile_entries[idx_a].total_ns;
    uint64_t total_b = g_profile_entries[idx_b].total_ns;
    /* Descending order: higher total_ns comes first */
    if (total_a > total_b) return -1;
    if (total_a < total_b) return 1;
    return 0;
}

/* ============================================================================
 * Time Utilities
 * ============================================================================ */

/*@semantic
id: function::get_time_ns
kind: function
name: get_time_ns
summary: Get current time in nanoseconds using clock_gettime.
tags:
  - profiling
  - timing
  - internal
*/
static uint64_t get_time_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

/* ============================================================================
 * Profile Entry Management
 * ============================================================================ */

/*@semantic
id: function::find_or_create_entry
kind: function
name: find_or_create_entry
summary: Find existing profile entry by name or create a new one.
tags:
  - profiling
  - internal
*/
static ProfileEntry* find_or_create_entry(const char* name) {
    /* First, look for existing entry */
    for (int i = 0; i < g_profile_count; i++) {
        if (g_profile_entries[i].active &&
            strcmp(g_profile_entries[i].name, name) == 0) {
            return &g_profile_entries[i];
        }
    }

    /* Create new entry if space available */
    if (g_profile_count >= MAX_PROFILE_ENTRIES) {
        return NULL;
    }

    ProfileEntry* entry = &g_profile_entries[g_profile_count++];
    strncpy(entry->name, name, MAX_NAME_LENGTH - 1);
    entry->name[MAX_NAME_LENGTH - 1] = '\0';
    entry->call_count = 0;
    entry->total_ns = 0;
    entry->min_ns = UINT64_MAX;
    entry->max_ns = 0;
    entry->alloc_bytes = 0;
    entry->alloc_count = 0;
    entry->active = true;

    return entry;
}

/*@semantic
id: function::record_profile_sample
kind: function
name: record_profile_sample
summary: Record a single profiling sample with timing data.
tags:
  - profiling
  - internal
*/
static void record_profile_sample(const char* name, uint64_t elapsed_ns) {
    ProfileEntry* entry = find_or_create_entry(name);
    if (!entry) return;

    entry->call_count++;
    entry->total_ns += elapsed_ns;
    if (elapsed_ns < entry->min_ns) entry->min_ns = elapsed_ns;
    if (elapsed_ns > entry->max_ns) entry->max_ns = elapsed_ns;
}

/* ============================================================================
 * Public Profiling API
 * ============================================================================ */

/*@semantic
id: function::prim_profile_enable
kind: function
name: prim_profile_enable
summary: Enable profiling globally.
tags:
  - profiling
  - control
*/
Obj* prim_profile_enable(void) {
    g_profiling_enabled = true;
    return mk_bool(true);
}

/*@semantic
id: function::prim_profile_disable
kind: function
name: prim_profile_disable
summary: Disable profiling globally.
tags:
  - profiling
  - control
*/
Obj* prim_profile_disable(void) {
    g_profiling_enabled = false;
    return mk_bool(false);
}

/*@semantic
id: function::prim_profile_reset
kind: function
name: prim_profile_reset
summary: Reset all profiling data.
tags:
  - profiling
  - control
*/
Obj* prim_profile_reset(void) {
    for (int i = 0; i < g_profile_count; i++) {
        g_profile_entries[i].active = false;
    }
    g_profile_count = 0;
    return mk_nothing();
}

/*@semantic
id: function::prim_profile
kind: function
name: prim_profile
summary: Profile execution of a closure and return result with timing.
tags:
  - profiling
  - execution
*/
Obj* prim_profile(Obj* name_obj, Obj* thunk) {
    const char* name = "anonymous";

    if (name_obj && !IS_IMMEDIATE(name_obj)) {
        if (name_obj->tag == TAG_STRING || name_obj->tag == TAG_SYM) {
            name = (const char*)name_obj->ptr;
        }
    }

    if (!thunk || IS_IMMEDIATE(thunk) || thunk->tag != TAG_CLOSURE) {
        return mk_error("profile: second argument must be a function");
    }

    uint64_t start = get_time_ns();
    Obj* result = prim_apply(thunk, NULL);
    uint64_t elapsed = get_time_ns() - start;

    record_profile_sample(name, elapsed);

    /* Return result with timing info */
    Obj* timing = mk_dict();
    dict_set(timing, mk_keyword("result"), result);
    dict_set(timing, mk_keyword("elapsed-ns"), mk_int((long)elapsed));
    dict_set(timing, mk_keyword("elapsed-ms"), mk_float((double)elapsed / 1000000.0));
    dict_set(timing, mk_keyword("name"), mk_string(name));

    return timing;
}

/*@semantic
id: function::prim_time
kind: function
name: prim_time
summary: Simple timing of a closure execution, prints result.
tags:
  - profiling
  - execution
*/
Obj* prim_time(Obj* thunk) {
    if (!thunk || IS_IMMEDIATE(thunk) || thunk->tag != TAG_CLOSURE) {
        return mk_error("time: argument must be a function");
    }

    uint64_t start = get_time_ns();
    Obj* result = prim_apply(thunk, NULL);
    uint64_t elapsed = get_time_ns() - start;

    double ms = (double)elapsed / 1000000.0;
    if (ms < 1.0) {
        printf("Elapsed: %.3f us\n", (double)elapsed / 1000.0);
    } else if (ms < 1000.0) {
        printf("Elapsed: %.3f ms\n", ms);
    } else {
        printf("Elapsed: %.3f s\n", ms / 1000.0);
    }

    return result;
}

/*@semantic
id: function::prim_call_counts
kind: function
name: prim_call_counts
summary: Return dict of function call counts.
tags:
  - profiling
  - reporting
*/
Obj* prim_call_counts(void) {
    Obj* counts = mk_dict();

    for (int i = 0; i < g_profile_count; i++) {
        if (g_profile_entries[i].active) {
            dict_set(counts,
                    mk_keyword(g_profile_entries[i].name),
                    mk_int((long)g_profile_entries[i].call_count));
        }
    }

    return counts;
}

/*@semantic
id: function::prim_hot_spots
kind: function
name: prim_hot_spots
summary: Return list of hot spots sorted by total time.
tags:
  - profiling
  - reporting
*/
Obj* prim_hot_spots(Obj* limit_obj) {
    int limit = 10;
    if (limit_obj && IS_IMMEDIATE_INT(limit_obj)) {
        limit = (int)INT_IMM_VALUE(limit_obj);
    }
    if (limit > g_profile_count) limit = g_profile_count;
    if (limit <= 0) limit = 10;

    /* Create sorted array of indices by total_ns */
    int* indices = malloc(sizeof(int) * g_profile_count);
    if (!indices) return mk_array(1);

// REVIEWED:OPTIMIZED - O(n log n) qsort instead of O(n²) bubble sort
    for (int i = 0; i < g_profile_count; i++) {
        indices[i] = i;
    }

    /* Sort indices by total_ns descending */
    qsort(indices, (size_t)g_profile_count, sizeof(int), compare_profile_indices_desc);

    /* Build result array */
    Obj* result = mk_array(limit);
    for (int i = 0; i < limit && i < g_profile_count; i++) {
        ProfileEntry* entry = &g_profile_entries[indices[i]];
        if (!entry->active) continue;

        Obj* item = mk_dict();
        dict_set(item, mk_keyword("name"), mk_string(entry->name));
        dict_set(item, mk_keyword("calls"), mk_int((long)entry->call_count));
        dict_set(item, mk_keyword("total-ms"),
                mk_float((double)entry->total_ns / 1000000.0));
        dict_set(item, mk_keyword("avg-ms"),
                mk_float(entry->call_count > 0 ?
                        (double)entry->total_ns / (double)entry->call_count / 1000000.0 : 0.0));
        dict_set(item, mk_keyword("min-ms"),
                mk_float(entry->min_ns < UINT64_MAX ?
                        (double)entry->min_ns / 1000000.0 : 0.0));
        dict_set(item, mk_keyword("max-ms"),
                mk_float((double)entry->max_ns / 1000000.0));

        array_push(result, item);
    }

    free(indices);
    return result;
}

/*@semantic
id: function::prim_profile_report
kind: function
name: prim_profile_report
summary: Generate and print a detailed profiling report.
tags:
  - profiling
  - reporting
*/
Obj* prim_profile_report(void) {
    printf("\n=== Profiling Report ===\n\n");

    if (g_profile_count == 0) {
        printf("No profiling data collected.\n\n");
        return mk_nothing();
    }

    /* Sort by total time */
    int* indices = malloc(sizeof(int) * g_profile_count);
    if (!indices) {
        printf("Memory allocation failed.\n");
        return mk_nothing();
    }

// REVIEWED:OPTIMIZED - O(n log n) qsort instead of O(n²) bubble sort
    for (int i = 0; i < g_profile_count; i++) {
        indices[i] = i;
    }

    /* Sort indices by total_ns descending */
    qsort(indices, (size_t)g_profile_count, sizeof(int), compare_profile_indices_desc);

    /* Calculate total time */
    uint64_t total_time = 0;
    for (int i = 0; i < g_profile_count; i++) {
        if (g_profile_entries[i].active) {
            total_time += g_profile_entries[i].total_ns;
        }
    }

    /* Print header */
    printf("%-30s %10s %12s %12s %10s\n",
           "Function", "Calls", "Total (ms)", "Avg (ms)", "Pct");
    printf("%-30s %10s %12s %12s %10s\n",
           "--------", "-----", "----------", "--------", "---");

    /* Print entries */
    for (int i = 0; i < g_profile_count; i++) {
        ProfileEntry* entry = &g_profile_entries[indices[i]];
        if (!entry->active) continue;

        double total_ms = (double)entry->total_ns / 1000000.0;
        double avg_ms = entry->call_count > 0 ?
                        total_ms / (double)entry->call_count : 0.0;
        double pct = total_time > 0 ?
                     (double)entry->total_ns * 100.0 / (double)total_time : 0.0;

        printf("%-30.30s %10lu %12.3f %12.3f %9.1f%%\n",
               entry->name,
               (unsigned long)entry->call_count,
               total_ms,
               avg_ms,
               pct);
    }

    printf("\n");
    free(indices);
    return mk_nothing();
}

/*@semantic
id: function::prim_profile_entry
kind: function
name: prim_profile_entry
summary: Get profiling data for a specific function.
tags:
  - profiling
  - introspection
*/
Obj* prim_profile_entry(Obj* name_obj) {
    if (!name_obj || IS_IMMEDIATE(name_obj)) {
        return mk_error("profile-entry: name must be a string");
    }

    const char* name = NULL;
    if (name_obj->tag == TAG_STRING || name_obj->tag == TAG_SYM) {
        name = (const char*)name_obj->ptr;
    } else {
        return mk_error("profile-entry: name must be a string");
    }

    for (int i = 0; i < g_profile_count; i++) {
        if (g_profile_entries[i].active &&
            strcmp(g_profile_entries[i].name, name) == 0) {
            ProfileEntry* entry = &g_profile_entries[i];

            Obj* result = mk_dict();
            dict_set(result, mk_keyword("name"), mk_string(entry->name));
            dict_set(result, mk_keyword("calls"), mk_int((long)entry->call_count));
            dict_set(result, mk_keyword("total-ns"), mk_int((long)entry->total_ns));
            dict_set(result, mk_keyword("total-ms"),
                    mk_float((double)entry->total_ns / 1000000.0));
            dict_set(result, mk_keyword("avg-ns"),
                    mk_int(entry->call_count > 0 ?
                          (long)(entry->total_ns / entry->call_count) : 0));
            dict_set(result, mk_keyword("min-ns"),
                    mk_int(entry->min_ns < UINT64_MAX ? (long)entry->min_ns : 0));
            dict_set(result, mk_keyword("max-ns"), mk_int((long)entry->max_ns));

            return result;
        }
    }

    return mk_nothing();  /* Not found */
}

/*@semantic
id: function::prim_benchmark
kind: function
name: prim_benchmark
summary: Run a function multiple times and return timing statistics.
tags:
  - profiling
  - benchmarking
*/
Obj* prim_benchmark(Obj* iterations_obj, Obj* thunk) {
    if (!iterations_obj || !IS_IMMEDIATE_INT(iterations_obj)) {
        return mk_error("benchmark: first argument must be an integer");
    }
    if (!thunk || IS_IMMEDIATE(thunk) || thunk->tag != TAG_CLOSURE) {
        return mk_error("benchmark: second argument must be a function");
    }

    long iterations = INT_IMM_VALUE(iterations_obj);
    if (iterations <= 0) iterations = 1;

    uint64_t total_ns = 0;
    uint64_t min_ns = UINT64_MAX;
    uint64_t max_ns = 0;

    printf("Running %ld iterations...\n", iterations);

    for (long i = 0; i < iterations; i++) {
        uint64_t start = get_time_ns();
        prim_apply(thunk, NULL);
        uint64_t elapsed = get_time_ns() - start;

        total_ns += elapsed;
        if (elapsed < min_ns) min_ns = elapsed;
        if (elapsed > max_ns) max_ns = elapsed;
    }

    double avg_ns = (double)total_ns / (double)iterations;
    double avg_ms = avg_ns / 1000000.0;

    printf("Benchmark complete:\n");
    printf("  Iterations: %ld\n", iterations);
    printf("  Total: %.3f ms\n", (double)total_ns / 1000000.0);
    printf("  Average: %.3f ms (%.0f ns)\n", avg_ms, avg_ns);
    printf("  Min: %.3f ms\n", (double)min_ns / 1000000.0);
    printf("  Max: %.3f ms\n", (double)max_ns / 1000000.0);

    Obj* result = mk_dict();
    dict_set(result, mk_keyword("iterations"), mk_int(iterations));
    dict_set(result, mk_keyword("total-ms"), mk_float((double)total_ns / 1000000.0));
    dict_set(result, mk_keyword("avg-ms"), mk_float(avg_ms));
    dict_set(result, mk_keyword("avg-ns"), mk_float(avg_ns));
    dict_set(result, mk_keyword("min-ms"), mk_float((double)min_ns / 1000000.0));
    dict_set(result, mk_keyword("max-ms"), mk_float((double)max_ns / 1000000.0));

    return result;
}

/*@semantic
id: function::prim_profile_memory
kind: function
name: prim_profile_memory
summary: Profile memory allocation of a closure execution.
tags:
  - profiling
  - memory
*/
Obj* prim_profile_memory(Obj* thunk) {
    if (!thunk || IS_IMMEDIATE(thunk) || thunk->tag != TAG_CLOSURE) {
        return mk_error("profile-memory: argument must be a function");
    }

    /* Note: This is a placeholder. Full memory profiling would require
     * hooking into the allocator, which is complex. For now, we just
     * time the execution and note that memory profiling is limited. */

    uint64_t start = get_time_ns();
    Obj* result = prim_apply(thunk, NULL);
    uint64_t elapsed = get_time_ns() - start;

    Obj* profile = mk_dict();
    dict_set(profile, mk_keyword("result"), result);
    dict_set(profile, mk_keyword("elapsed-ms"), mk_float((double)elapsed / 1000000.0));
    dict_set(profile, mk_keyword("note"),
            mk_string("Full memory profiling requires allocator hooks"));

    return profile;
}

/*@semantic
id: function::prim_profiling_enabled_p
kind: function
name: prim_profiling_enabled_p
summary: Check if profiling is enabled.
tags:
  - profiling
  - introspection
*/
Obj* prim_profiling_enabled_p(void) {
    return mk_bool(g_profiling_enabled);
}

/*@semantic
id: function::prim_profile_count
kind: function
name: prim_profile_count
summary: Return number of profiled entries.
tags:
  - profiling
  - introspection
*/
Obj* prim_profile_count(void) {
    return mk_int(g_profile_count);
}

/* ============================================================================
 * TRANSMIGRATION OPTIMIZATION: Profile-Guided Region Placement (Phase 4)
 * ============================================================================
 * Allocation site profiling for profile-guided region placement decisions.
 * Tracks escape frequency and transmigration patterns to inform the compiler's
 * allocation strategy.
 */

#include "../include/omni_debug.h"

/* Global profile feedback data */
ProfileFeedback g_profile_feedback = {
    .sites = NULL,
    .site_count = 0,
    .site_capacity = 0,
    .is_enabled = false,
    .data_loaded = false
};

/* Flag to track if profile system has been initialized */
static bool g_profile_feedback_initialized = false;

/*@semantic
id: function::omni_profile_init
kind: function
name: omni_profile_init
summary: Initialize profile feedback system based on environment variables.
tags:
  - profiling
  - initialization
  - transmigration-optimization
*/
void omni_profile_init(void) {
    if (g_profile_feedback_initialized) return;
    g_profile_feedback_initialized = true;

    /* Check for environment variable to enable profiling */
    const char* env_enabled = getenv("OMNILISP_PROFILE_ENABLED");
    if (env_enabled && atoi(env_enabled) == 1) {
        g_profile_feedback.is_enabled = true;

        /* Allocate initial site array */
        g_profile_feedback.site_capacity = 64;
        g_profile_feedback.sites = (AllocationSiteProfile*)calloc(
            g_profile_feedback.site_capacity, sizeof(AllocationSiteProfile));
        if (!g_profile_feedback.sites) {
            g_profile_feedback.is_enabled = false;
            g_profile_feedback.site_capacity = 0;
            return;
        }
    }

    /* Check for profile data file to load */
    const char* env_file = getenv("OMNILISP_PROFILE_FILE");
    if (env_file) {
        omni_load_profile_data(env_file);
    }
}

/*@semantic
id: function::find_or_create_site
kind: function
name: find_or_create_site
summary: Find existing allocation site or create a new entry.
tags:
  - profiling
  - internal
*/
static AllocationSiteProfile* find_or_create_site(uint64_t site_id) {
    if (!g_profile_feedback.is_enabled || !g_profile_feedback.sites) {
        return NULL;
    }

    /* Look for existing entry */
    for (size_t i = 0; i < g_profile_feedback.site_count; i++) {
        if (g_profile_feedback.sites[i].site_id == site_id) {
            return &g_profile_feedback.sites[i];
        }
    }

    /* Check if we need to grow the array */
    if (g_profile_feedback.site_count >= g_profile_feedback.site_capacity) {
        /* Don't grow beyond max */
        if (g_profile_feedback.site_capacity >= OMNI_PROFILE_MAX_SITES) {
            return NULL;
        }

        size_t new_capacity = g_profile_feedback.site_capacity * 2;
        if (new_capacity > OMNI_PROFILE_MAX_SITES) {
            new_capacity = OMNI_PROFILE_MAX_SITES;
        }

        AllocationSiteProfile* new_sites = (AllocationSiteProfile*)realloc(
            g_profile_feedback.sites,
            new_capacity * sizeof(AllocationSiteProfile));
        if (!new_sites) {
            return NULL;
        }

        /* Zero out new entries */
        memset(new_sites + g_profile_feedback.site_capacity, 0,
               (new_capacity - g_profile_feedback.site_capacity) * sizeof(AllocationSiteProfile));

        g_profile_feedback.sites = new_sites;
        g_profile_feedback.site_capacity = new_capacity;
    }

    /* Create new entry */
    AllocationSiteProfile* site = &g_profile_feedback.sites[g_profile_feedback.site_count++];
    site->site_id = site_id;
    site->total_allocations = 0;
    site->escape_count = 0;
    site->transmigrate_count = 0;
    site->escape_ratio = 0.0f;
    site->recommended_region = PROFILE_REGION_LOCAL;

    return site;
}

/*@semantic
id: function::omni_profile_allocation
kind: function
name: omni_profile_allocation
summary: Record an allocation at the given site.
tags:
  - profiling
  - allocation
  - transmigration-optimization
*/
void omni_profile_allocation(uint64_t site_id, Region* region) {
    (void)region;  /* May be used for more detailed tracking in future */

    AllocationSiteProfile* site = find_or_create_site(site_id);
    if (site) {
        site->total_allocations++;
    }
}

/*@semantic
id: function::omni_profile_escape
kind: function
name: omni_profile_escape
summary: Record an escape event (object escapes its original region).
tags:
  - profiling
  - escape-analysis
  - transmigration-optimization
*/
void omni_profile_escape(uint64_t site_id) {
    AllocationSiteProfile* site = find_or_create_site(site_id);
    if (site) {
        site->escape_count++;
    }
}

/*@semantic
id: function::omni_profile_transmigrate
kind: function
name: omni_profile_transmigrate
summary: Record a transmigration event.
tags:
  - profiling
  - transmigration
  - transmigration-optimization
*/
void omni_profile_transmigrate(uint64_t site_id) {
    AllocationSiteProfile* site = find_or_create_site(site_id);
    if (site) {
        site->transmigrate_count++;
    }
}

/*@semantic
id: function::omni_profile_update_recommendations
kind: function
name: omni_profile_update_recommendations
summary: Compute escape ratios and update region recommendations.
tags:
  - profiling
  - analysis
  - transmigration-optimization
*/
void omni_profile_update_recommendations(void) {
    if (!g_profile_feedback.is_enabled || !g_profile_feedback.sites) {
        return;
    }

    for (size_t i = 0; i < g_profile_feedback.site_count; i++) {
        AllocationSiteProfile* site = &g_profile_feedback.sites[i];

        if (site->total_allocations > 0) {
            site->escape_ratio = (float)site->escape_count / (float)site->total_allocations;

            /* Determine recommendation based on escape ratio
             * - >= 0.9: Always escapes, use global region
             * - >= 0.5: Often escapes, use caller's region
             * - < 0.5: Usually local, use local region
             */
            if (site->escape_ratio >= 0.9f) {
                site->recommended_region = PROFILE_REGION_GLOBAL;
            } else if (site->escape_ratio >= 0.5f) {
                site->recommended_region = PROFILE_REGION_CALLER;
            } else {
                site->recommended_region = PROFILE_REGION_LOCAL;
            }
        }
    }
}

/*@semantic
id: function::omni_profile_get_recommendation
kind: function
name: omni_profile_get_recommendation
summary: Query recommended region for an allocation site.
tags:
  - profiling
  - query
  - transmigration-optimization
*/
ProfileRecommendedRegion omni_profile_get_recommendation(uint64_t site_id) {
    if (!g_profile_feedback.is_enabled || !g_profile_feedback.sites) {
        return PROFILE_REGION_LOCAL;
    }

    for (size_t i = 0; i < g_profile_feedback.site_count; i++) {
        if (g_profile_feedback.sites[i].site_id == site_id) {
            return g_profile_feedback.sites[i].recommended_region;
        }
    }

    return PROFILE_REGION_LOCAL;
}

/*@semantic
id: function::omni_save_profile_data
kind: function
name: omni_save_profile_data
summary: Save profile data to a binary file.
tags:
  - profiling
  - persistence
  - transmigration-optimization
*/
void omni_save_profile_data(const char* filename) {
    if (!g_profile_feedback.sites || g_profile_feedback.site_count == 0) {
        return;
    }

    FILE* f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "[WARN] Could not save profile data to %s\n", filename);
        return;
    }

    /* Update recommendations before saving */
    omni_profile_update_recommendations();

    /* Write header: magic + version + count */
    uint32_t magic = 0x4F4D5049;  /* "OMPI" */
    uint32_t version = 1;
    uint32_t count = (uint32_t)g_profile_feedback.site_count;

    fwrite(&magic, sizeof(magic), 1, f);
    fwrite(&version, sizeof(version), 1, f);
    fwrite(&count, sizeof(count), 1, f);

    /* Write site entries */
    for (size_t i = 0; i < g_profile_feedback.site_count; i++) {
        AllocationSiteProfile* site = &g_profile_feedback.sites[i];
        fwrite(&site->site_id, sizeof(site->site_id), 1, f);
        fwrite(&site->total_allocations, sizeof(site->total_allocations), 1, f);
        fwrite(&site->escape_count, sizeof(site->escape_count), 1, f);
        fwrite(&site->transmigrate_count, sizeof(site->transmigrate_count), 1, f);
        fwrite(&site->escape_ratio, sizeof(site->escape_ratio), 1, f);
        uint8_t rec = (uint8_t)site->recommended_region;
        fwrite(&rec, sizeof(rec), 1, f);
    }

    fclose(f);
}

/*@semantic
id: function::omni_load_profile_data
kind: function
name: omni_load_profile_data
summary: Load profile data from a binary file.
tags:
  - profiling
  - persistence
  - transmigration-optimization
*/
void omni_load_profile_data(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) {
        /* File doesn't exist, that's OK - no profile data to load */
        return;
    }

    /* Read and verify header */
    uint32_t magic, version, count;
    if (fread(&magic, sizeof(magic), 1, f) != 1 ||
        fread(&version, sizeof(version), 1, f) != 1 ||
        fread(&count, sizeof(count), 1, f) != 1) {
        fclose(f);
        return;
    }

    if (magic != 0x4F4D5049 || version != 1) {
        fprintf(stderr, "[WARN] Invalid profile data file: %s\n", filename);
        fclose(f);
        return;
    }

    /* Ensure profiling is enabled and we have capacity */
    if (!g_profile_feedback.is_enabled) {
        g_profile_feedback.is_enabled = true;
    }

    if (!g_profile_feedback.sites || g_profile_feedback.site_capacity < count) {
        size_t new_capacity = count > 64 ? count : 64;
        if (new_capacity > OMNI_PROFILE_MAX_SITES) {
            new_capacity = OMNI_PROFILE_MAX_SITES;
        }

        AllocationSiteProfile* new_sites = (AllocationSiteProfile*)realloc(
            g_profile_feedback.sites,
            new_capacity * sizeof(AllocationSiteProfile));
        if (!new_sites) {
            fclose(f);
            return;
        }

        g_profile_feedback.sites = new_sites;
        g_profile_feedback.site_capacity = new_capacity;
    }

    /* Read site entries */
    g_profile_feedback.site_count = 0;
    for (uint32_t i = 0; i < count && i < OMNI_PROFILE_MAX_SITES; i++) {
        AllocationSiteProfile* site = &g_profile_feedback.sites[i];

        if (fread(&site->site_id, sizeof(site->site_id), 1, f) != 1) break;
        if (fread(&site->total_allocations, sizeof(site->total_allocations), 1, f) != 1) break;
        if (fread(&site->escape_count, sizeof(site->escape_count), 1, f) != 1) break;
        if (fread(&site->transmigrate_count, sizeof(site->transmigrate_count), 1, f) != 1) break;
        if (fread(&site->escape_ratio, sizeof(site->escape_ratio), 1, f) != 1) break;

        uint8_t rec;
        if (fread(&rec, sizeof(rec), 1, f) != 1) break;
        site->recommended_region = (ProfileRecommendedRegion)rec;

        g_profile_feedback.site_count++;
    }

    g_profile_feedback.data_loaded = true;
    fclose(f);
}
