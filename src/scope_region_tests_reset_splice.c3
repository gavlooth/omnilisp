module main;

import std::core::mem;
import std::io;

fn ScopeRegionTestCounts run_scope_region_reset_splice_tests() {
    usz passed = 0;
    usz failed = 0;

    // Test 7: Scope reset (loop reuse)
    {
        ScopeRegion* s = scope_create(null);
        // Allocate some data
        void* p1 = s.alloc(64);
        void* p2 = s.alloc(64);

        int* dtor_counter = (int*)mem::malloc(int.sizeof);
        *dtor_counter = 0;
        scope_register_dtor(s, (void*)dtor_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });

        // Reset the scope
        scope_reset(s);
        if (*dtor_counter == 1) { passed++; } else { io::printfn("FAIL: reset dtor counter=%d", *dtor_counter); failed++; }
        if (s.alloc_bytes == 0) { passed++; } else { io::printn("FAIL: reset alloc_bytes"); failed++; }
        if (s.alloc_count == 0) { passed++; } else { io::printn("FAIL: reset alloc_count"); failed++; }
        if (s.dtors == null) { passed++; } else { io::printn("FAIL: reset dtors"); failed++; }

        // Can allocate again after reset
        void* p3 = s.alloc(32);
        if (p3 != null) { passed++; } else { io::printn("FAIL: alloc after reset"); failed++; }

        scope_release(s);
        mem::free(dtor_counter);
    }

    // Test 7null: reset APIs are no-ops on null scopes
    {
        ScopeRegion* null_scope = null;
        scope_reset(null_scope);
        scope_reset_temp_lane(null_scope);
        passed++;
    }

    // Test 7b: scope_reset clears ESCAPE lane
    {
        ScopeRegion* s = scope_create(null);
        void* e = s.alloc_escape(32);
        if (e != null) { passed++; } else { io::printn("FAIL: reset escape alloc null"); failed++; }
        scope_reset(s);
        if (s.escape_chunks == null) { passed++; } else { io::printn("FAIL: reset escape chunks"); failed++; }
        if (s.escape_chunks_tail == null) { passed++; } else { io::printn("FAIL: reset escape chunks tail"); failed++; }
        if (s.escape_dtors == null) { passed++; } else { io::printn("FAIL: reset escape dtors"); failed++; }
        if (s.escape_dtors_tail == null) { passed++; } else { io::printn("FAIL: reset escape dtors tail"); failed++; }
        if (s.escape_bump == null && s.escape_limit == null) { passed++; } else { io::printn("FAIL: reset escape bump/limit"); failed++; }
        scope_release(s);
    }

    // Test 7c: scope_reset_temp_lane preserves ESCAPE lane
    {
        ScopeRegion* s = scope_create(null);
        int* temp_counter = (int*)mem::malloc(int.sizeof);
        int* escape_counter = (int*)mem::malloc(int.sizeof);
        *temp_counter = 0;
        *escape_counter = 0;

        void* temp_ptr = s.alloc(16);
        void* escape_ptr = s.alloc_escape(16);

        scope_register_dtor(s, (void*)temp_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        scope_register_dtor_escape(s, (void*)escape_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });

        scope_reset_temp_lane(s);

        bool ok = *temp_counter == 1 &&
                  *escape_counter == 0 &&
                  s.dtors == null &&
                  s.escape_dtors != null &&
                  s.escape_dtors_tail != null &&
                  s.escape_chunks_tail != null &&
                  s.alloc_bytes == 0 &&
                  s.alloc_count == 0 &&
                  escape_ptr != null &&
                  is_in_scope(escape_ptr, s) &&
                  temp_ptr != null;

        if (ok) { passed++; } else { io::printn("FAIL: reset_temp_lane invariants"); failed++; }

        scope_release(s);
        if (*escape_counter == 1) { passed++; } else { io::printn("FAIL: reset_temp_lane escape dtor on destroy"); failed++; }

        mem::free(temp_counter);
        mem::free(escape_counter);
    }

    // Test 7d: full reset clears ESCAPE tail pointers and allows clean reinit.
    {
        ScopeRegion* s = scope_create(null);
        int* escape_counter = (int*)mem::malloc(int.sizeof);
        *escape_counter = 0;

        s.alloc_escape(24);
        scope_register_dtor_escape(s, (void*)escape_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });

        bool before_reset_ok = s.escape_chunks_tail != null &&
                               s.escape_dtors_tail != null &&
                               scope_escape_chunk_tail_consistent(s) &&
                               scope_escape_dtor_tail_consistent(s);
        scope_reset(s);
        bool after_reset_ok = s.escape_chunks == null &&
                              s.escape_chunks_tail == null &&
                              s.escape_dtors == null &&
                              s.escape_dtors_tail == null &&
                              scope_escape_chunk_tail_consistent(s) &&
                              scope_escape_dtor_tail_consistent(s) &&
                              *escape_counter == 1;

        s.alloc_escape(24);
        scope_register_dtor_escape(s, (void*)escape_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        bool after_reinit_ok = s.escape_chunks_tail != null &&
                               s.escape_dtors_tail != null &&
                               scope_escape_chunk_tail_consistent(s) &&
                               scope_escape_dtor_tail_consistent(s);

        scope_release(s);
        bool destroy_ok = *escape_counter == 2;
        if (before_reset_ok && after_reset_ok && after_reinit_ok && destroy_ok) {
            passed++;
        } else {
            io::printfn(
                "FAIL: reset tail semantics before=%s after=%s reinit=%s destroy=%s count=%d",
                before_reset_ok ? "yes" : "no",
                after_reset_ok ? "yes" : "no",
                after_reinit_ok ? "yes" : "no",
                destroy_ok ? "yes" : "no",
                *escape_counter
            );
            failed++;
        }

        mem::free(escape_counter);
    }

    scope_region_run_topology_alignment_tests(&passed, &failed);
    scope_region_run_is_in_scope_tests(&passed, &failed);
    scope_region_run_splice_escape_tests(&passed, &failed);

    ScopeRegionTestCounts counts;
    counts.passed = passed;
    counts.failed = failed;
    return counts;
}
