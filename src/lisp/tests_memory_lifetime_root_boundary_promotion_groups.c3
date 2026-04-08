module lisp;

import std::io;
import main;

fn void run_memory_lifetime_root_boundary_promotion_test(Interp* interp, int* pass, int* fail) {
    // Invariant: root-boundary promotion handles disjoint scope values through
    // explicit destination routing without fallback copy-site usage.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);

        interp.current_scope = scope_b;
        Value* foreign = make_cons(interp, make_int(interp, 21), make_nil(interp));

        interp.current_scope = scope_a;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(foreign, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted != foreign &&
                      promoted.tag == CONS &&
                      promoted.cons_val.car != null &&
                      promoted.cons_val.car.tag == INT &&
                      promoted.cons_val.car.int_val == 21 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)promoted, scope_b);

        // Releasing source scope must not invalidate promoted value.
        main::scope_release(scope_b);

        bool post_ok = promoted != null &&
                       promoted.tag == CONS &&
                       promoted.cons_val.car != null &&
                       promoted.cons_val.car.tag == INT &&
                       promoted.cons_val.car.int_val == 21;

        interp.current_scope = saved;
        main::scope_release(scope_a);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary disjoint destination route");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary disjoint destination route (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == foreign) ? "yes" : "no",
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_direct_promotion_test(Interp* interp, int* pass, int* fail) {
    // Invariant: releasing-owned root-store values use direct destination
    // promotion and do not increment copy-site fallback counters.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary direct destination promotion (setup)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        Value* local = make_cons(interp, make_int(interp, 37), make_nil(interp));
        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted != local &&
                      promoted.tag == CONS &&
                      boundary_ptr_in_scope((void*)promoted, interp.root_scope) &&
                      promoted.cons_val.car != null &&
                      promoted.cons_val.car.tag == INT &&
                      promoted.cons_val.car.int_val == 37 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)promoted, child);

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == CONS &&
                       promoted.cons_val.car != null &&
                       promoted.cons_val.car.tag == INT &&
                       promoted.cons_val.car.int_val == 37;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary direct destination promotion");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary direct destination promotion (site_delta=%d same=%s pre_ok=%s post_ok=%s)",
                (long)site_delta,
                (promoted == local) ? "yes" : "no",
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_array_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned ARRAY payload crossing root-store boundary is
    // cloned into root lifetime (wrapper + payload), and source wrapper remains
    // valid until source scope release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = ARRAY;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);

        Array* arr = (Array*)mem::malloc(Array.sizeof);
        if (arr == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup array)");
            (*fail)++;
            return;
        }
        arr.capacity = 4;
        arr.length = 1;
        arr.items = (Value**)mem::malloc(Value*.sizeof * arr.capacity);
        if (arr.items == null) {
            mem::free(arr);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary ARRAY clone path (setup items)");
            (*fail)++;
            return;
        }
        arr.items[0] = make_int(interp, 73);
        local.array_val = arr;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == ARRAY &&
                      promoted != local &&
                      promoted.array_val != null &&
                      local.array_val != null &&
                      promoted.array_val != local.array_val &&
                      promoted.array_val.length == 1 &&
                      promoted.array_val.items[0] != null &&
                      promoted.array_val.items[0].tag == INT &&
                      promoted.array_val.items[0].int_val == 73 &&
                      local.array_val.length == 1 &&
                      local.array_val.items[0] != null &&
                      local.array_val.items[0].tag == INT &&
                      local.array_val.items[0].int_val == 73 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == ARRAY &&
                       promoted.array_val != null &&
                       promoted.array_val.length == 1 &&
                       promoted.array_val.items[0] != null &&
                       promoted.array_val.items[0].tag == INT &&
                       promoted.array_val.items[0].int_val == 73;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary ARRAY clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary ARRAY clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}
