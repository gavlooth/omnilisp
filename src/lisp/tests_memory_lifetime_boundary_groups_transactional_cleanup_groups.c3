module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_partial_boundary_copy_transactional_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: partial-boundary copy rolls back already-copied child retains
    // when a later arg copy fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: partial boundary copy transactional cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__boundary_partial_txn_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 611), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 611;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'p';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 611;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* partial = interp.alloc_value();
        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque_ok &&
                        partial != null;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.remaining = 1;
            partial.partial_val.first_arg = shared_closure;
            partial.partial_val.second_arg = opaque;
        }

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = setup_ok ? copy_to_parent_with_fault(partial, interp, &copy_fault) : null;
        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);

        bool fault_ok = copied == null &&
                        copy_fault == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool rollback_ok = closure_rc_after_fail == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: partial boundary copy transactional cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: partial boundary copy transactional cleanup (setup_ok=%s fault=%s closure_rc=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_fault),
                closure_rc_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_cons_boundary_copy_transactional_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: cons-boundary copy rolls back already-copied child retains
    // when a later cdr copy fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: cons boundary copy transactional cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__boundary_cons_txn_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 615), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 615;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'c';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 615;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* cons = make_cons(interp, shared_closure, opaque);
        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque_ok &&
                        cons != null &&
                        cons.tag == CONS;

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = setup_ok ? copy_to_parent_with_fault(cons, interp, &copy_fault) : null;
        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);

        bool fault_ok = copied == null &&
                        copy_fault == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool rollback_ok = closure_rc_after_fail == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: cons boundary copy transactional cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: cons boundary copy transactional cleanup (setup_ok=%s fault=%s closure_rc=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_fault),
                closure_rc_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_partial_iterator_boundary_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: shared-wrapper rollback recursively unwinds nested
    // PARTIAL_PRIM / ITERATOR payload retains before returning a boundary fault.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* owner = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (owner == null || foreign == null || target == null) {
            if (owner != null) main::scope_release(owner);
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested partial boundary cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__boundary_nested_partial_cleanup_payload");

        interp.current_scope = owner;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 612), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
            if (closure_env_scope != null) {
                main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                    int* counter = (int*)ptr;
                    if (counter != null) (*counter)++;
                });
            }
        }

        interp.current_scope = foreign;
        Value* partial = interp.alloc_value();
        if (partial != null) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.remaining = 1;
            partial.partial_val.first_arg = shared_closure;
            partial.partial_val.second_arg = make_int(interp, 613);
        }

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 612;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'q';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 612;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* array_src = interp.alloc_value();
        Array* array_payload = null;
        bool array_ok = array_src != null;
        if (array_ok) {
            array_src.tag = ARRAY;
            main::scope_register_dtor(foreign, (void*)array_src, &scope_dtor_value);
            array_payload = (Array*)mem::malloc(Array.sizeof);
            array_ok = array_payload != null;
        }
        if (array_ok) {
            array_payload.capacity = 4;
            array_payload.length = 2;
            array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
            array_ok = array_payload.items != null;
        }
        if (array_ok) {
            for (usz i = 0; i < array_payload.capacity; i++) {
                array_payload.items[i] = null;
            }
            array_payload.items[0] = partial;
            array_payload.items[1] = opaque;
            array_src.array_val = array_payload;
        } else if (array_payload != null) {
            mem::free(array_payload);
            array_payload = null;
        }

        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        partial != null &&
                        partial.partial_val.second_arg != null &&
                        opaque_ok &&
                        array_ok;

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = setup_ok ? copy_to_parent_with_fault(array_src, interp, &copy_fault) : null;
        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        main::scope_release(owner);
        int destroy_count_after_owner_release = closure_env_destroy_count;

        bool fault_ok = copied == null &&
                        copy_fault == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool rollback_ok = rc_after_fail == 1 &&
                           rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0 &&
                           destroy_count_after_owner_release == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested partial boundary cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested partial boundary cleanup (setup_ok=%s fault=%s rc_after_fail=%d rc_after_target_release=%d destroy_target=%d destroy_owner=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_fault),
                rc_after_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_owner_release
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* owner = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (owner == null || foreign == null || target == null) {
            if (owner != null) main::scope_release(owner);
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested iterator boundary cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__boundary_nested_iterator_cleanup_payload");

        interp.current_scope = owner;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 614), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
            if (closure_env_scope != null) {
                main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                    int* counter = (int*)ptr;
                    if (counter != null) (*counter)++;
                });
            }
        }

        interp.current_scope = foreign;
        Value* iter = shared_closure != null ? make_iterator(interp, shared_closure) : null;

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 614;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'r';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 614;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* array_src = interp.alloc_value();
        Array* array_payload = null;
        bool array_ok = array_src != null;
        if (array_ok) {
            array_src.tag = ARRAY;
            main::scope_register_dtor(foreign, (void*)array_src, &scope_dtor_value);
            array_payload = (Array*)mem::malloc(Array.sizeof);
            array_ok = array_payload != null;
        }
        if (array_ok) {
            array_payload.capacity = 4;
            array_payload.length = 2;
            array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
            array_ok = array_payload.items != null;
        }
        if (array_ok) {
            for (usz i = 0; i < array_payload.capacity; i++) {
                array_payload.items[i] = null;
            }
            array_payload.items[0] = iter;
            array_payload.items[1] = opaque;
            array_src.array_val = array_payload;
        } else if (array_payload != null) {
            mem::free(array_payload);
            array_payload = null;
        }

        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        iter != null &&
                        iter.tag == ITERATOR &&
                        opaque_ok &&
                        array_ok;

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* copied = setup_ok ? copy_to_parent_with_fault(array_src, interp, &copy_fault) : null;
        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        main::scope_release(owner);
        int destroy_count_after_owner_release = closure_env_destroy_count;

        bool fault_ok = copied == null &&
                        copy_fault == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool rollback_ok = rc_after_fail == 1 &&
                           rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0 &&
                           destroy_count_after_owner_release == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested iterator boundary cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested iterator boundary cleanup (setup_ok=%s fault=%s rc_after_fail=%d rc_after_target_release=%d destroy_target=%d destroy_owner=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_fault),
                rc_after_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_owner_release
            );
            (*fail)++;
        }
    }
}
