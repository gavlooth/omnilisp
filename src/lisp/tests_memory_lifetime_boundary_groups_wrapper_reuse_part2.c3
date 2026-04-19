module lisp;

import std::io;
import std::core::mem;
import main;

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_nested_cons_wrapper_defensive_copy(Interp* interp, int* pass, int* fail) {
    // Invariant: a target-chain CONS wrapper is defensively copied when a
    // nested CONS shell still belongs to the releasing scope, even if the
    // nested payload leaves are only scalars.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        if (target == null || releasing == null) {
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested cons wrapper defensive clone policy");
            (*fail)++;
            return false;
        }

        interp.current_scope = target;
        Value* outer_cons = make_cons(interp, make_int(interp, 11), make_nil(interp));

        interp.current_scope = releasing;
        Value* nested_cons = make_cons(interp, make_int(interp, 22), make_nil(interp));
        bool setup_ok = outer_cons != null && nested_cons != null;
        if (setup_ok) {
            outer_cons.cons_val.cdr = nested_cons;
        }

        interp.current_scope = target;
        interp.releasing_scope = releasing;
        Value* copied_cons = setup_ok ? copy_to_parent(outer_cons, interp) : null;
        Value* escaped_cons = setup_ok ? promote_to_escape(outer_cons, interp) : null;

        bool copy_ok = copied_cons != null &&
                       copied_cons.tag == CONS &&
                       copied_cons != outer_cons &&
                       copied_cons.cons_val.car != null &&
                       copied_cons.cons_val.car.tag == INT &&
                       copied_cons.cons_val.car.int_val == 11 &&
                       copied_cons.cons_val.cdr != null &&
                       copied_cons.cons_val.cdr.tag == CONS &&
                       copied_cons.cons_val.cdr != nested_cons &&
                       copied_cons.cons_val.cdr.cons_val.car != null &&
                       copied_cons.cons_val.cdr.cons_val.car.tag == INT &&
                       copied_cons.cons_val.cdr.cons_val.car.int_val == 22;

        bool escape_ok = escaped_cons != null &&
                         escaped_cons.tag == CONS &&
                         escaped_cons != outer_cons &&
                         escaped_cons.scope_gen == target.escape_generation &&
                         escaped_cons.cons_val.cdr != null &&
                         escaped_cons.cons_val.cdr.tag == CONS &&
                         escaped_cons.cons_val.cdr != nested_cons &&
                         escaped_cons.cons_val.cdr.scope_gen == target.escape_generation &&
                         escaped_cons.cons_val.cdr.cons_val.car != null &&
                         escaped_cons.cons_val.cdr.cons_val.car.tag == INT &&
                         escaped_cons.cons_val.cdr.cons_val.car.int_val == 22;

        main::scope_release(releasing);
        bool post_release_ok = copy_ok &&
                               copied_cons.cons_val.cdr != null &&
                               copied_cons.cons_val.cdr.tag == CONS &&
                               copied_cons.cons_val.cdr.cons_val.car != null &&
                               copied_cons.cons_val.cdr.cons_val.car.tag == INT &&
                               copied_cons.cons_val.cdr.cons_val.car.int_val == 22 &&
                               escape_ok &&
                               escaped_cons.cons_val.cdr != null &&
                               escaped_cons.cons_val.cdr.tag == CONS &&
                               escaped_cons.cons_val.cdr.cons_val.car != null &&
                               escaped_cons.cons_val.cdr.cons_val.car.tag == INT &&
                               escaped_cons.cons_val.cdr.cons_val.car.int_val == 22;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = setup_ok && copy_ok && escape_ok && post_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested cons wrapper defensive clone policy");
            (*pass)++;
            return true;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested cons wrapper defensive clone policy (setup_ok=%s copy_ok=%s escape_ok=%s post_release_ok=%s)",
                setup_ok ? "yes" : "no",
                copy_ok ? "yes" : "no",
                escape_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no"
            );
            (*fail)++;
            return false;
        }
    }
}

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_primitive_wrapper_copy_policy(Interp* interp, int* pass, int* fail) {
    // Invariant: primitive wrappers already in target chain are reused, while
    // disjoint plain primitive wrappers are cloned and opaque payloads fail as
    // typed boundary-copy faults.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy");
            (*fail)++;
            return false;
        }

        interp.current_scope = target;
        Value* reusable = interp.alloc_value();
        reusable.tag = PRIMITIVE;
        main::scope_register_dtor(target, (void*)reusable, &scope_dtor_value);
        Primitive* reusable_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (reusable_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup reusable)");
            (*fail)++;
            return false;
        }
        *reusable_prim = {};
        reusable_prim.name[0] = 'r';
        reusable_prim.name[1] = 0;
        reusable_prim.arity = 1;
        reusable_prim.tag = 601;
        reusable.prim_val = reusable_prim;

        interp.current_scope = foreign;
        Value* disjoint = interp.alloc_value();
        disjoint.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)disjoint, &scope_dtor_value);
        Primitive* disjoint_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (disjoint_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup disjoint)");
            (*fail)++;
            return false;
        }
        *disjoint_prim = {};
        disjoint_prim.name[0] = 'd';
        disjoint_prim.name[1] = 0;
        disjoint_prim.arity = 2;
        disjoint_prim.tag = 602;
        disjoint.prim_val = disjoint_prim;

        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup opaque)");
            (*fail)++;
            return false;
        }
        *opaque_prim = {};
        *opaque_payload = 603;
        opaque_prim.name[0] = 'o';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 3;
        opaque_prim.tag = 603;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);
        BoundaryCopyResult rejected = boundary_copy_to_parent_site_ctx_checked(
            opaque,
            interp,
            COPY_SITE_GENERIC,
            null
        );

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == PRIMITIVE &&
                         copied != disjoint &&
                         copied.prim_val != null &&
                         copied.prim_val != disjoint.prim_val &&
                         copied.prim_val.arity == 2 &&
                         copied.prim_val.tag == 602 &&
                         copied.prim_val.name[0] == 'd' &&
                         copied.prim_val.name[1] == 0;
        bool reject_ok = !boundary_copy_result_ok(rejected) &&
                         rejected.value == null &&
                         rejected.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == PRIMITIVE &&
                       copied.prim_val != null &&
                       copied.prim_val.arity == 2 &&
                       copied.prim_val.tag == 602 &&
                       copied.prim_val.name[0] == 'd' &&
                       copied.prim_val.name[1] == 0;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reuse_ok && copied_ok && reject_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive wrapper copy policy");
            (*pass)++;
            return true;
        } else {
            io::printfn("[FAIL] lifetime: primitive wrapper copy policy (reuse_ok=%s copied_ok=%s reject_ok=%s post_ok=%s fault=%s)",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                reject_ok ? "yes" : "no",
                post_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(rejected.fault_code));
            (*fail)++;
            return false;
        }
    }
}

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_shared_wrapper_slot_abort(Interp* interp, int* pass, int* fail) {
    // Invariant: failed shared-wrapper copy does not consume target wrapper
    // allocation slots before the wrapper is actually committed.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = target == null ? null : main::scope_create(target);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper copy abort does not leak wrapper slots");
            (*fail)++;
            return false;
        }

        interp.current_scope = foreign;
        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(foreign, (void*)mt_src, &scope_dtor_value);

        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper copy abort does not leak wrapper slots (setup table)");
            (*fail)++;
            return false;
        }
        mt_payload.name = interp.symbols.intern("__mt_wrapper_slot_abort");
        mt_payload.capacity = 1;
        mt_payload.entry_count = 1;
        mt_payload.fallback = null;
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper copy abort does not leak wrapper slots (setup entries)");
            (*fail)++;
            return false;
        }
        method_signature_init_empty(&mt_payload.entries[0].sig);
        MethodSignature sig_src;
        method_signature_init_empty(&sig_src);
        if (!method_signature_alloc_arrays_in_scope(&sig_src, foreign, 1, 0) ||
            !method_signature_copy_to_heap(&mt_payload.entries[0].sig, &sig_src)) {
            method_table_free_partial_heap(mt_payload, mt_payload.entry_count);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper copy abort does not leak wrapper slots (setup signature)");
            (*fail)++;
            return false;
        }
        mt_payload.entries[0].implementation = make_int(interp, 1901);
        mt_src.method_table_val = mt_payload;

        interp.current_scope = target;
        interp.releasing_scope = releasing;
        usz alloc_before = target.alloc_count;

        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 1;
        BoundaryCopyResult first_result = boundary_copy_to_parent_site_ctx_checked(
            mt_src,
            interp,
            COPY_SITE_GENERIC,
            null
        );
        usz alloc_after_first = target.alloc_count;

        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 1;
        BoundaryCopyResult second_result = boundary_copy_to_parent_site_ctx_checked(
            mt_src,
            interp,
            COPY_SITE_GENERIC,
            null
        );
        usz alloc_after_second = target.alloc_count;
        g_method_signature_force_heap_copy_fail_on_call = 0;
        interp.releasing_scope = null;

        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(releasing);
        main::scope_release(target);

        bool first_ok = !boundary_copy_result_ok(first_result) &&
                        first_result.value == null &&
                        first_result.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT;
        bool second_ok = !boundary_copy_result_ok(second_result) &&
                         second_result.value == null &&
                         second_result.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT;
        bool alloc_ok = alloc_after_first == alloc_before &&
                        alloc_after_second == alloc_before;

        if (first_ok && second_ok && alloc_ok) {
            emit_pass_literal("[PASS] lifetime: shared-wrapper copy abort does not leak wrapper slots");
            (*pass)++;
            return true;
        } else {
            io::printfn(
                "[FAIL] lifetime: shared-wrapper copy abort does not leak wrapper slots (first_ok=%s second_ok=%s alloc_before=%d alloc_after_first=%d alloc_after_second=%d first_fault=%s second_fault=%s)",
                first_ok ? "yes" : "no",
                second_ok ? "yes" : "no",
                (long)alloc_before,
                (long)alloc_after_first,
                (long)alloc_after_second,
                (ZString)boundary_copy_fault_name(first_result.fault_code),
                (ZString)boundary_copy_fault_name(second_result.fault_code)
            );
            (*fail)++;
            return false;
        }
    }
}
