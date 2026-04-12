module main;

import std::core::mem;
import std::io;

fn ScopeRegionTestCounts run_scope_region_alloc_lifecycle_tests() {
    usz passed = 0;
    usz failed = 0;

    // Test 1: Create and destroy a scope
    {
        ScopeRegion* s = scope_create(null);
        if (s != null && s.refcount == 1 && s.parent == null && s.chunks != null) {
            passed++;
        } else {
            io::printn("FAIL: scope_create basic");
            failed++;
        }
        scope_release(s);
        passed++; // didn't crash
    }

    // Test 1b: chunk allocation OOM on scope_create returns null cleanly
    {
        g_scope_force_chunk_alloc_oom = true;
        ScopeRegion* s = scope_create(null);
        g_scope_force_chunk_alloc_oom = false;
        if (s == null) {
            passed++;
        } else {
            io::printn("FAIL: scope_create chunk alloc oom");
            failed++;
            scope_release(s);
        }
    }

    // Test 2: Bump allocation
    {
        ScopeRegion* s = scope_create(null);
        void* p1 = s.alloc(24); // Value-sized
        void* p2 = s.alloc(24);
        void* p3 = s.alloc(8);
        // Allocations should be sequential (8-byte aligned)
        if (p1 != null && p2 != null && p3 != null) {
            usz diff = (usz)p2 - (usz)p1;
            if (diff == 24) { passed++; } else { io::printfn("FAIL: bump sequential (diff=%d)", (int)diff); failed++; }
        } else {
            io::printn("FAIL: bump alloc returned null");
            failed++;
        }
        if (s.alloc_count == 3) { passed++; } else { io::printn("FAIL: alloc_count"); failed++; }
        if (s.alloc_bytes == 24 + 24 + 8) { passed++; } else { io::printfn("FAIL: alloc_bytes=%d", (int)s.alloc_bytes); failed++; }
        scope_release(s);
    }

    // Test 2a: TEMP slow-path chunk allocation OOM returns null without corrupting scope
    {
        ScopeRegion* s = scope_create(null);
        void* first = s.alloc(16);
        g_scope_force_chunk_alloc_oom = true;
        void* slow_fail = s.alloc(SCOPE_CHUNK_INITIAL + 8);
        g_scope_force_chunk_alloc_oom = false;
        void* retry = s.alloc(SCOPE_CHUNK_INITIAL + 8);

        usz chunk_count = 0;
        ScopeChunk* c = s.chunks;
        while (c != null) { chunk_count++; c = c.next; }

        bool ok = first != null &&
                  slow_fail == null &&
                  retry != null &&
                  chunk_count >= 2;
        if (ok) {
            passed++;
        } else {
            io::printn("FAIL: temp slow-path chunk alloc oom");
            failed++;
        }
        scope_release(s);
    }

    // Test 2b: Escape-lane allocation + generation separation
    {
        ScopeRegion* s = scope_create(null);
        void* e1 = s.alloc_escape(24);
        void* e2 = s.alloc_escape(24);
        if (e1 != null && e2 != null && ((usz)e2 - (usz)e1) == 24) {
            passed++;
        } else {
            io::printn("FAIL: escape alloc sequential");
            failed++;
        }
        if (s.escape_generation != s.generation) { passed++; } else { io::printn("FAIL: escape_generation separation"); failed++; }
        if (s.escape_chunks != null) { passed++; } else { io::printn("FAIL: escape chunks missing"); failed++; }
        scope_release(s);
    }

    // Test 2d: ESCAPE slow-path chunk allocation OOM returns null without corrupting scope
    {
        ScopeRegion* s = scope_create(null);
        g_scope_force_chunk_alloc_oom = true;
        void* slow_fail = s.alloc_escape(SCOPE_CHUNK_INITIAL + 8);
        g_scope_force_chunk_alloc_oom = false;
        void* retry = s.alloc_escape(SCOPE_CHUNK_INITIAL + 8);

        usz chunk_count = 0;
        ScopeChunk* c = s.escape_chunks;
        while (c != null) { chunk_count++; c = c.next; }

        bool ok = slow_fail == null &&
                  retry != null &&
                  chunk_count == 1 &&
                  s.escape_chunks_tail == s.escape_chunks;
        if (ok) {
            passed++;
        } else {
            io::printn("FAIL: escape slow-path chunk alloc oom");
            failed++;
        }
        scope_release(s);
    }

    // Test 2e: Overflow-sized requests fail closed before pointer arithmetic
    {
        ScopeRegion* s = scope_create(null);
        usz bad = (usz)-6;
        void* fail = s.alloc(bad);
        void* recover = s.alloc(16);
        void* fail_escape = s.alloc_escape(bad);
        void* recover_escape = s.alloc_escape(16);

        bool ok = fail == null && recover != null && fail_escape == null && recover_escape != null;
        if (ok) {
            passed++;
        } else {
            io::printn("FAIL: overflow-sized scope allocs");
            failed++;
        }
        scope_release(s);
    }

    // Test 2c: Escape-lane destructor execution
    {
        ScopeRegion* s = scope_create(null);
        int* counter = (int*)mem::malloc(int.sizeof);
        *counter = 0;
        scope_register_dtor_escape(s, (void*)counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        scope_register_dtor_escape(s, (void*)counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 10;
        });
        scope_release(s);
        if (*counter == 11) { passed++; } else { io::printfn("FAIL: escape dtor counter=%d", *counter); failed++; }
        mem::free(counter);
    }

    // Test 3: Chunk overflow (allocate more than initial chunk)
    {
        ScopeRegion* s = scope_create(null);
        // Fill initial chunk (512 bytes) and overflow
        for (usz i = 0; i < 100; i++) {
            void* p = s.alloc(64); // 100 * 64 = 6400 bytes
            if (p == null) { io::printn("FAIL: overflow alloc null"); failed++; break; }
        }
        // Should have multiple chunks
        usz chunk_count = 0;
        ScopeChunk* c = s.chunks;
        while (c != null) { chunk_count++; c = c.next; }
        if (chunk_count > 1) { passed++; } else { io::printn("FAIL: no overflow chunks"); failed++; }
        scope_release(s);
        passed++;
    }

    // Test 3a: Raw chunk allocator fails closed on max-size overflow
    {
        bool ok = scope_chunk_alloc_raw((usz)-1) == null;
        if (ok) {
            passed++;
        } else {
            io::printn("FAIL: raw chunk allocator overflow");
            failed++;
        }
    }

    scope_region_run_refcount_destructor_tests(&passed, &failed);

    ScopeRegionTestCounts counts;
    counts.passed = passed;
    counts.failed = failed;
    return counts;
}
