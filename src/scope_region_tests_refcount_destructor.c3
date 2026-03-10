module main;

import std::core::mem;
import std::io;

fn void scope_region_run_refcount_destructor_tests(usz* passed, usz* failed) {
    // Test 4: Reference counting (parent-child)
    {
        ScopeRegion* parent = scope_create(null);
        ScopeRegion* child = scope_create(parent);
        // Parent RC should be 2 (original + child's reference)
        if (parent.refcount == 2) { (*passed)++; } else { io::printfn("FAIL: parent RC=%d", (int)parent.refcount); (*failed)++; }
        if (child.refcount == 1) { (*passed)++; } else { io::printfn("FAIL: child RC=%d", (int)child.refcount); (*failed)++; }
        if (child.parent == parent) { (*passed)++; } else { io::printn("FAIL: child.parent"); (*failed)++; }
        // Release child -> parent RC drops to 1
        scope_release(child);
        if (parent.refcount == 1) { (*passed)++; } else { io::printfn("FAIL: parent RC after child release=%d", (int)parent.refcount); (*failed)++; }
        scope_release(parent);
        (*passed)++;
    }

    // Test 5: Destructor execution
    {
        ScopeRegion* s = scope_create(null);
        int* counter = (int*)mem::malloc(int.sizeof);
        *counter = 0;
        scope_register_dtor(s, (void*)counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        scope_register_dtor(s, (void*)counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 10;
        });
        scope_release(s);
        if (*counter == 11) { (*passed)++; } else { io::printfn("FAIL: dtor counter=%d", *counter); (*failed)++; }
        mem::free(counter);
    }

    // Test 6: Freelist recycling (generation increment)
    {
        ScopeRegion* s = scope_create(null);
        uint gen_before = s.generation;
        scope_release(s);
        ScopeRegion* s2 = scope_create(null);
        if (s2.generation > gen_before) {
            (*passed)++;
        } else {
            io::printfn("FAIL: gen=%d expected > %d", (int)s2.generation, (int)gen_before);
            (*failed)++;
        }
        scope_release(s2);
    }
}
