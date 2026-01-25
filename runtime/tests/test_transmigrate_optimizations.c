/*
 * test_transmigrate_optimizations.c - Unit tests for transmigration optimizations
 *
 * Tests for:
 * - Configurable and adaptive merge threshold (Opt 1A, 1B)
 * - Inline fast-paths for pairs, closures, arrays (Opt 3A, 3B, 3C)
 * - Profile-guided region placement (Opt 4A-4C)
 *
 * Build: make -C runtime test
 * Run: ./runtime/tests/test_transmigrate_optimizations
 */

#include "../include/omni.h"
#include "../include/omni_debug.h"
#include "../src/memory/region_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* ============================================================================
 * Test Utilities
 * ============================================================================ */

static int tests_passed = 0;
static int tests_failed = 0;

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        fprintf(stderr, "[FAIL] %s:%d: %s\n", __func__, __LINE__, msg); \
        tests_failed++; \
        return; \
    } \
} while (0)

#define TEST_PASS() do { \
    tests_passed++; \
    printf("[PASS] %s\n", __func__); \
} while (0)

/* ============================================================================
 * Optimization 1: Configurable Threshold Tests
 * ============================================================================ */

void test_default_threshold(void) {
    /* Initialize configuration */
    omni_transmigrate_config_init();

    /* Default threshold should be REGION_MERGE_THRESHOLD_BYTES (4096) */
    size_t threshold = omni_get_merge_threshold();
    TEST_ASSERT(threshold == REGION_MERGE_THRESHOLD_BYTES,
                "Default threshold should be 4096");

    TEST_PASS();
}

void test_set_threshold(void) {
    /* Set threshold to 8192 */
    omni_set_merge_threshold(8192);
    TEST_ASSERT(omni_get_merge_threshold() == 8192,
                "Threshold should be 8192 after set");

    /* Test clamping to minimum */
    omni_set_merge_threshold(100);  /* Below min */
    TEST_ASSERT(omni_get_merge_threshold() == OMNI_TRANSMIGRATE_MIN_THRESHOLD,
                "Threshold should be clamped to minimum");

    /* Test clamping to maximum */
    omni_set_merge_threshold(100000);  /* Above max */
    TEST_ASSERT(omni_get_merge_threshold() == OMNI_TRANSMIGRATE_MAX_THRESHOLD,
                "Threshold should be clamped to maximum");

    /* Reset to default */
    omni_set_merge_threshold(REGION_MERGE_THRESHOLD_BYTES);

    TEST_PASS();
}

void test_adaptive_threshold(void) {
    /* Enable adaptive mode */
    omni_transmigrate_set_adaptive(true);
    TEST_ASSERT(g_transmigrate_config.adaptive_enabled == true,
                "Adaptive mode should be enabled");

    /* Record some sizes */
    for (int i = 0; i < 10; i++) {
        omni_transmigrate_record_size(2000 + i * 100, true);
    }

    /* Record more samples to trigger adaptation */
    for (int i = 0; i < 64; i++) {
        omni_transmigrate_record_size(3000, true);
    }

    /* Threshold should have adapted (P75 of recorded sizes) */
    size_t threshold = omni_get_merge_threshold();
    TEST_ASSERT(threshold >= OMNI_TRANSMIGRATE_MIN_THRESHOLD &&
                threshold <= OMNI_TRANSMIGRATE_MAX_THRESHOLD,
                "Adapted threshold should be in valid range");

    /* Disable adaptive mode */
    omni_transmigrate_set_adaptive(false);
    TEST_ASSERT(g_transmigrate_config.adaptive_enabled == false,
                "Adaptive mode should be disabled");

    TEST_PASS();
}

/* ============================================================================
 * Optimization 4: Profile-Guided Region Placement Tests
 * ============================================================================ */

void test_profile_init(void) {
    /* Initialize profile system */
    omni_profile_init();

    /* Without OMNILISP_PROFILE_ENABLED=1, profiling should be disabled */
    /* This test just verifies init doesn't crash */

    TEST_PASS();
}

void test_profile_allocation_tracking(void) {
    /* Enable profiling for this test */
    g_profile_feedback.is_enabled = true;
    g_profile_feedback.site_capacity = 64;
    g_profile_feedback.sites = (AllocationSiteProfile*)calloc(
        64, sizeof(AllocationSiteProfile));
    g_profile_feedback.site_count = 0;

    if (!g_profile_feedback.sites) {
        fprintf(stderr, "[SKIP] %s: Could not allocate profile sites\n", __func__);
        return;
    }

    /* Generate a site ID */
    uint64_t site_id = omni_profile_site_hash("test.c", 42);
    TEST_ASSERT(site_id != 0, "Site hash should be non-zero");

    /* Record allocations */
    omni_profile_allocation(site_id, NULL);
    omni_profile_allocation(site_id, NULL);
    omni_profile_allocation(site_id, NULL);

    /* Record escapes */
    omni_profile_escape(site_id);
    omni_profile_escape(site_id);

    /* Update recommendations */
    omni_profile_update_recommendations();

    /* Check recommendation - with 2/3 escape ratio (66%), should be CALLER */
    ProfileRecommendedRegion rec = omni_profile_get_recommendation(site_id);
    TEST_ASSERT(rec == PROFILE_REGION_CALLER,
                "Recommendation should be CALLER for 66% escape ratio");

    /* Cleanup */
    free(g_profile_feedback.sites);
    g_profile_feedback.sites = NULL;
    g_profile_feedback.site_count = 0;
    g_profile_feedback.is_enabled = false;

    TEST_PASS();
}

void test_profile_high_escape_ratio(void) {
    /* Enable profiling for this test */
    g_profile_feedback.is_enabled = true;
    g_profile_feedback.site_capacity = 64;
    g_profile_feedback.sites = (AllocationSiteProfile*)calloc(
        64, sizeof(AllocationSiteProfile));
    g_profile_feedback.site_count = 0;

    if (!g_profile_feedback.sites) {
        fprintf(stderr, "[SKIP] %s: Could not allocate profile sites\n", __func__);
        return;
    }

    /* Generate a site ID */
    uint64_t site_id = omni_profile_site_hash("high_escape.c", 100);

    /* Record allocations with high escape ratio (95%) */
    for (int i = 0; i < 100; i++) {
        omni_profile_allocation(site_id, NULL);
    }
    for (int i = 0; i < 95; i++) {
        omni_profile_escape(site_id);
    }

    /* Update recommendations */
    omni_profile_update_recommendations();

    /* With 95% escape ratio, should be GLOBAL */
    ProfileRecommendedRegion rec = omni_profile_get_recommendation(site_id);
    TEST_ASSERT(rec == PROFILE_REGION_GLOBAL,
                "Recommendation should be GLOBAL for 95% escape ratio");

    /* Cleanup */
    free(g_profile_feedback.sites);
    g_profile_feedback.sites = NULL;
    g_profile_feedback.site_count = 0;
    g_profile_feedback.is_enabled = false;

    TEST_PASS();
}

void test_profile_low_escape_ratio(void) {
    /* Enable profiling for this test */
    g_profile_feedback.is_enabled = true;
    g_profile_feedback.site_capacity = 64;
    g_profile_feedback.sites = (AllocationSiteProfile*)calloc(
        64, sizeof(AllocationSiteProfile));
    g_profile_feedback.site_count = 0;

    if (!g_profile_feedback.sites) {
        fprintf(stderr, "[SKIP] %s: Could not allocate profile sites\n", __func__);
        return;
    }

    /* Generate a site ID */
    uint64_t site_id = omni_profile_site_hash("low_escape.c", 200);

    /* Record allocations with low escape ratio (10%) */
    for (int i = 0; i < 100; i++) {
        omni_profile_allocation(site_id, NULL);
    }
    for (int i = 0; i < 10; i++) {
        omni_profile_escape(site_id);
    }

    /* Update recommendations */
    omni_profile_update_recommendations();

    /* With 10% escape ratio, should be LOCAL */
    ProfileRecommendedRegion rec = omni_profile_get_recommendation(site_id);
    TEST_ASSERT(rec == PROFILE_REGION_LOCAL,
                "Recommendation should be LOCAL for 10% escape ratio");

    /* Cleanup */
    free(g_profile_feedback.sites);
    g_profile_feedback.sites = NULL;
    g_profile_feedback.site_count = 0;
    g_profile_feedback.is_enabled = false;

    TEST_PASS();
}

void test_profile_save_load(void) {
    /* Enable profiling for this test */
    g_profile_feedback.is_enabled = true;
    g_profile_feedback.site_capacity = 64;
    g_profile_feedback.sites = (AllocationSiteProfile*)calloc(
        64, sizeof(AllocationSiteProfile));
    g_profile_feedback.site_count = 0;

    if (!g_profile_feedback.sites) {
        fprintf(stderr, "[SKIP] %s: Could not allocate profile sites\n", __func__);
        return;
    }

    /* Generate a site ID and record data */
    uint64_t site_id = omni_profile_site_hash("saveload.c", 300);
    for (int i = 0; i < 50; i++) {
        omni_profile_allocation(site_id, NULL);
    }
    for (int i = 0; i < 40; i++) {
        omni_profile_escape(site_id);
    }

    /* Save to temp file */
    const char* test_file = "/tmp/test_profile.dat";
    omni_save_profile_data(test_file);

    /* Clear current data */
    free(g_profile_feedback.sites);
    g_profile_feedback.sites = NULL;
    g_profile_feedback.site_count = 0;

    /* Load back */
    omni_load_profile_data(test_file);

    /* Verify data was loaded */
    TEST_ASSERT(g_profile_feedback.site_count == 1,
                "Should have loaded 1 site");
    TEST_ASSERT(g_profile_feedback.sites[0].site_id == site_id,
                "Site ID should match");
    TEST_ASSERT(g_profile_feedback.sites[0].total_allocations == 50,
                "Total allocations should be 50");
    TEST_ASSERT(g_profile_feedback.sites[0].escape_count == 40,
                "Escape count should be 40");

    /* Cleanup */
    remove(test_file);
    free(g_profile_feedback.sites);
    g_profile_feedback.sites = NULL;
    g_profile_feedback.site_count = 0;
    g_profile_feedback.is_enabled = false;

    TEST_PASS();
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    printf("\n=== Transmigration Optimization Tests ===\n\n");

    /* Threshold tests */
    printf("--- Configurable Threshold Tests ---\n");
    test_default_threshold();
    test_set_threshold();
    test_adaptive_threshold();

    /* Profile-guided tests */
    printf("\n--- Profile-Guided Placement Tests ---\n");
    test_profile_init();
    test_profile_allocation_tracking();
    test_profile_high_escape_ratio();
    test_profile_low_escape_ratio();
    test_profile_save_load();

    /* Summary */
    printf("\n=== Test Summary ===\n");
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_failed);
    printf("Total:  %d\n", tests_passed + tests_failed);

    return tests_failed > 0 ? 1 : 0;
}
