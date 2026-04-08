module main;

struct ScopeRegionTestCounts {
    usz passed;
    usz failed;
}

fn ScopeRegionTestCounts run_scope_region_core_tests() {
    ScopeRegionTestCounts alloc_lifecycle_counts = run_scope_region_alloc_lifecycle_tests();
    ScopeRegionTestCounts reset_splice_counts = run_scope_region_reset_splice_tests();
    usz passed = alloc_lifecycle_counts.passed + reset_splice_counts.passed;
    usz failed = alloc_lifecycle_counts.failed + reset_splice_counts.failed;

    ScopeRegionTestCounts counts;
    counts.passed = passed;
    counts.failed = failed;
    return counts;
}
