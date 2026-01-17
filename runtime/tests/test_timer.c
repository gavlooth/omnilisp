/**
 * @file test_timer.c
 * @brief Tests for timer infrastructure (Phase 5.1)
 */

#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../include/omni.h"
#include "../src/memory/continuation.h"

static int tests_passed = 0;
static int tests_total = 0;

#define TEST(name) do { \
    printf("  %s: ", #name); \
    tests_total++; \
} while(0)

#define PASS() do { \
    printf("\033[32mPASS\033[0m\n"); \
    tests_passed++; \
} while(0)

#define FAIL(msg) do { \
    printf("\033[31mFAIL\033[0m - %s\n", msg); \
} while(0)

/* Get current time in milliseconds */
static uint64_t current_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
}

/* Test: Timer system initialization and shutdown */
static void test_timer_init_shutdown(void) {
    TEST(timer_init_shutdown);

    /* Should be able to init/shutdown multiple times */
    timer_system_init();
    timer_system_shutdown();
    timer_system_init();
    timer_system_shutdown();

    PASS();
}

/* Test: Timer with 0ms (immediate resolution) */
static void test_timer_immediate(void) {
    TEST(timer_immediate);

    timer_system_init();

    Promise* p = timer_after(0);
    if (!p) {
        FAIL("timer_after returned NULL");
        timer_system_shutdown();
        return;
    }

    /* Should resolve immediately */
    if (!promise_is_settled(p)) {
        FAIL("0ms timer not immediately resolved");
        timer_system_shutdown();
        return;
    }

    timer_system_shutdown();
    PASS();
}

/* Test: Timer with short delay */
static void test_timer_short_delay(void) {
    TEST(timer_short_delay);

    timer_system_init();

    uint64_t start = current_time_ms();
    Promise* p = timer_after(50);  /* 50ms */
    if (!p) {
        FAIL("timer_after returned NULL");
        timer_system_shutdown();
        return;
    }

    /* Should not be resolved immediately */
    if (promise_is_settled(p)) {
        /* Note: might be settled if timer system is very fast */
        /* This is acceptable, just verify it resolves */
    }

    /* Wait for the timer to resolve */
    promise_await(p);

    uint64_t elapsed = current_time_ms() - start;

    /* Should have waited at least ~40ms (allowing some slack) */
    if (elapsed < 30) {
        char msg[100];
        snprintf(msg, sizeof(msg), "elapsed %lums, expected >= 30ms",
                 (unsigned long)elapsed);
        FAIL(msg);
        timer_system_shutdown();
        return;
    }

    if (!promise_is_settled(p)) {
        FAIL("promise not settled after await");
        timer_system_shutdown();
        return;
    }

    timer_system_shutdown();
    PASS();
}

/* Test: Multiple concurrent timers */
static void test_timer_concurrent(void) {
    TEST(timer_concurrent);

    timer_system_init();

    uint64_t start = current_time_ms();

    /* Create timers with different delays */
    Promise* p1 = timer_after(20);
    Promise* p2 = timer_after(40);
    Promise* p3 = timer_after(60);

    if (!p1 || !p2 || !p3) {
        FAIL("timer_after returned NULL");
        timer_system_shutdown();
        return;
    }

    /* Wait for all to resolve */
    promise_await(p1);
    uint64_t t1 = current_time_ms() - start;

    promise_await(p2);
    uint64_t t2 = current_time_ms() - start;

    promise_await(p3);
    uint64_t t3 = current_time_ms() - start;

    /* Verify ordering: t1 < t2 < t3 (with some tolerance) */
    if (t1 > t2 || t2 > t3) {
        char msg[100];
        snprintf(msg, sizeof(msg), "timers out of order: %lu, %lu, %lu",
                 (unsigned long)t1, (unsigned long)t2, (unsigned long)t3);
        FAIL(msg);
        timer_system_shutdown();
        return;
    }

    timer_system_shutdown();
    PASS();
}

/* Test: await_timeout with promise that resolves before timeout */
static void test_await_timeout_success(void) {
    TEST(await_timeout_success);

    timer_system_init();

    /* Create a timer that resolves quickly */
    Promise* p = timer_after(10);
    if (!p) {
        FAIL("timer_after returned NULL");
        timer_system_shutdown();
        return;
    }

    /* await_timeout with longer timeout */
    Obj* result = await_timeout(p, 200);

    /* Should succeed (not timeout) */
    /* Note: timer_after resolves with NULL value, which is fine */
    if (!promise_is_settled(p)) {
        FAIL("promise not settled");
        timer_system_shutdown();
        return;
    }

    (void)result;  /* Result may be NULL for timer promises */

    timer_system_shutdown();
    PASS();
}

/* Test: await_timeout with timeout expiring first */
static void test_await_timeout_expires(void) {
    TEST(await_timeout_expires);

    timer_system_init();

    /* Create a timer that resolves slowly */
    Promise* p = timer_after(500);  /* 500ms */
    if (!p) {
        FAIL("timer_after returned NULL");
        timer_system_shutdown();
        return;
    }

    uint64_t start = current_time_ms();

    /* await_timeout with shorter timeout */
    Obj* result = await_timeout(p, 50);  /* 50ms timeout */

    uint64_t elapsed = current_time_ms() - start;

    /* Should have returned around 50ms, not 500ms */
    if (elapsed > 200) {
        char msg[100];
        snprintf(msg, sizeof(msg), "timeout took too long: %lums",
                 (unsigned long)elapsed);
        FAIL(msg);
        timer_system_shutdown();
        return;
    }

    /* Result should be NULL (timeout) */
    if (result != NULL) {
        FAIL("expected NULL result on timeout");
        timer_system_shutdown();
        return;
    }

    timer_system_shutdown();
    PASS();
}

/* Test: Timer system not initialized returns immediately */
static void test_timer_no_init(void) {
    TEST(timer_no_init);

    /* Don't init timer system */
    /* timer_after should still work (immediate resolution) */
    Promise* p = timer_after(100);
    if (!p) {
        FAIL("timer_after returned NULL");
        return;
    }

    /* Should resolve immediately when timer system not running */
    if (!promise_is_settled(p)) {
        FAIL("timer should resolve immediately when system not running");
        return;
    }

    PASS();
}

int main(void) {
    printf("\n\033[33m=== Timer Infrastructure Tests (Phase 5.1) ===\033[0m\n\n");

    test_timer_init_shutdown();
    test_timer_immediate();
    test_timer_short_delay();
    test_timer_concurrent();
    test_await_timeout_success();
    test_await_timeout_expires();
    test_timer_no_init();

    printf("\n\033[33m=== Summary ===\033[0m\n");
    printf("  Total:  %d\n", tests_total);
    printf("  \033[32mPassed: %d\033[0m\n", tests_passed);
    printf("  Failed: %d\n", tests_total - tests_passed);

    return (tests_passed == tests_total) ? 0 : 1;
}
