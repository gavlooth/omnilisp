module main;

import std::io;

fn void scope_region_run_topology_alignment_tests(usz* passed, usz* failed) {
    // Test 10: Large allocation (bigger than initial chunk)
    {
        ScopeRegion* s = scope_create(null);
        void* big = s.alloc(2048);  // Bigger than initial 512-byte chunk
        if (big != null) { (*passed)++; } else { io::printn("FAIL: large alloc"); (*failed)++; }
        scope_release(s);
    }

    // Test 11: Deep parent chain
    {
        ScopeRegion* root = scope_create(null);
        ScopeRegion* prev = root;
        for (usz i = 0; i < 10; i++) {
            ScopeRegion* child = scope_create(prev);
            if (i > 0) scope_release(prev);  // Release our reference (child holds one)
            prev = child;
        }
        // Release the leaf — should cascade all the way up
        if (root.refcount == 2) { (*passed)++; } else { io::printfn("FAIL: deep chain root RC=%d", (int)root.refcount); (*failed)++; }
        scope_release(prev);
        // Root should now have RC=1 (our original reference)
        if (root.refcount == 1) { (*passed)++; } else { io::printfn("FAIL: deep chain root RC after cascade=%d", (int)root.refcount); (*failed)++; }
        scope_release(root);
        (*passed)++;
    }

    // Test 12: Retain/release symmetry
    {
        ScopeRegion* s = scope_create(null);
        scope_retain(s);
        scope_retain(s);
        if (s.refcount == 3) { (*passed)++; } else { io::printfn("FAIL: triple retain RC=%d", (int)s.refcount); (*failed)++; }
        scope_release(s);
        scope_release(s);
        if (s.refcount == 1) { (*passed)++; } else { io::printfn("FAIL: after double release RC=%d", (int)s.refcount); (*failed)++; }
        scope_release(s);
        (*passed)++;
    }

    // Test 13: Alignment check
    {
        ScopeRegion* s = scope_create(null);
        void* p1 = s.alloc(1);   // 1 byte -> 8-aligned
        void* p2 = s.alloc(3);   // 3 bytes -> 8-aligned
        void* p3 = s.alloc(16);  // 16 bytes -> 16-aligned (already 8-aligned)
        if ((usz)p1 % 8 == 0) { (*passed)++; } else { io::printn("FAIL: align p1"); (*failed)++; }
        if ((usz)p2 % 8 == 0) { (*passed)++; } else { io::printn("FAIL: align p2"); (*failed)++; }
        if ((usz)p3 % 8 == 0) { (*passed)++; } else { io::printn("FAIL: align p3"); (*failed)++; }
        // p2 should be 8 bytes after p1 (1 byte rounded up to 8)
        usz gap = (usz)p2 - (usz)p1;
        if (gap == 8) { (*passed)++; } else { io::printfn("FAIL: align gap=%d", (int)gap); (*failed)++; }
        scope_release(s);
    }
}
