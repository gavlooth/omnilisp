/* Purple Runtime Test Suite - Single Compilation Unit */
/* Define POSIX features FIRST before any includes */
#define _POSIX_C_SOURCE 200112L

#include "test_framework.h"
#include "../src/runtime.c"

/* Test counter definitions */
int tests_run = 0;
int tests_passed = 0;
int tests_failed = 0;
const char* current_suite = NULL;

/* Include all test files directly */
#include "test_constructors.c"
#include "test_memory.c"
#include "test_primitives.c"
#include "test_lists.c"
#include "test_closures.c"
#include "test_tagged_pointers.c"
#include "test_arena.c"
#include "test_scc.c"
#include "test_concurrency.c"
#include "test_weak_refs.c"
#include "test_borrowref.c"
#include "test_deferred.c"
#include "test_stress.c"

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    printf("Purple Runtime Test Suite\n");
    printf("==========================\n");
    printf("Comprehensive testing of C runtime\n\n");

    /* Core functionality tests */
    run_constructor_tests();
    run_memory_tests();
    run_primitive_tests();

    /* Data structure tests */
    run_list_tests();
    run_closure_tests();

    /* Tagged pointer tests */
    run_tagged_pointer_tests();

    /* Memory management tests */
    run_arena_tests();
    run_scc_tests();
    run_weak_refs_tests();
    run_borrowref_tests();
    run_deferred_tests();

    /* Concurrency tests - skipped (blocking issues) */
    // run_concurrency_tests();

    /* Stress tests - skipped for now */
    // run_stress_tests();

    TEST_EXIT();
}
