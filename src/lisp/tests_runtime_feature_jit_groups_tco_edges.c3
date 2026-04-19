module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_jit_policy_tco_cons_releasing_scalar_edge_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must copy a surviving CONS wrapper when one
    // direct child still belongs to the releasing scope.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco cons wrapper copies releasing scalar edge");
        (*fail)++;
        return;
    }

    SymbolId cons_sym = interp.symbols.intern("__jit_policy_tco_cons_scalar_edge");

    interp.current_scope = target;
    Value* cons_value = make_cons(interp, make_int(interp, 1), make_nil(interp));
    if (cons_value == null) {
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco cons wrapper copies releasing scalar edge (setup cons)");
        (*fail)++;
        return;
    }

    interp.current_scope = source;
    Value* source_child = make_int(interp, 2702);
    Env* source_env = make_env(interp, interp.global_env);
    bool setup_ok = source_env != null;
    if (setup_ok) {
        cons_value.cons_val.car = source_child;
        setup_ok = source_env.define(cons_sym, cons_value);
    }

    interp.current_scope = target;
    Env* copied_env = setup_ok
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    Value* copied_cons = copied_env != null ? copied_env.lookup(cons_sym) : null;
    bool copy_ok = copied_cons != null &&
        copied_cons != cons_value &&
        copied_cons.tag == CONS &&
        copied_cons.cons_val.car != null &&
        copied_cons.cons_val.car != source_child &&
        copied_cons.cons_val.car.tag == INT &&
        copied_cons.cons_val.car.int_val == 2702;

    main::scope_release(source);

    bool post_release_ok = copy_ok &&
        copied_cons != null &&
        copied_cons.cons_val.car != null &&
        copied_cons.cons_val.car.tag == INT &&
        copied_cons.cons_val.car.int_val == 2702;

    interp.current_scope = saved;
    main::scope_release(target);

    bool ok = setup_ok && copy_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco cons wrapper copies releasing scalar edge");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco cons wrapper copies releasing scalar edge (setup_ok=%s copy_ok=%s post_release_ok=%s)",
            setup_ok ? "yes" : "no",
            copy_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_partial_shared_wrapper_edge_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must not reuse a surviving PARTIAL_PRIM / ITERATOR
    // wrapper whose shared-wrapper arg still points at a child in the releasing scope.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco partial wrapper copies nested shared wrapper edge");
        (*fail)++;
        return;
    }

    SymbolId partial_sym = interp.symbols.intern("__jit_policy_tco_partial_with_array");
    SymbolId iter_sym = interp.symbols.intern("__jit_policy_tco_iter_with_partial_array");

    interp.current_scope = target;
    Value* array_value = interp.alloc_value();
    array_value.tag = ARRAY;
    main::scope_register_dtor(target, (void*)array_value, &scope_dtor_value);
    Array* array_payload = (Array*)mem::malloc(Array.sizeof);
    if (array_payload == null) {
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco partial wrapper copies nested shared wrapper edge (setup array)");
        (*fail)++;
        return;
    }
    array_payload.capacity = 4;
    array_payload.length = 1;
    array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
    if (array_payload.items == null) {
        mem::free(array_payload);
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco partial wrapper copies nested shared wrapper edge (setup array items)");
        (*fail)++;
        return;
    }
    for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
    array_value.array_val = array_payload;

    Value* partial_value = interp.alloc_value();
    partial_value.tag = PARTIAL_PRIM;
    partial_value.partial_val.func = &prim_add;
    partial_value.partial_val.remaining = 1;
    partial_value.partial_val.first_arg = array_value;
    partial_value.partial_val.second_arg = make_int(interp, 44);

    Value* iter_value = make_iterator(interp, partial_value);
    if (iter_value == null) {
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco partial wrapper copies nested shared wrapper edge (setup iterator)");
        (*fail)++;
        return;
    }

    interp.current_scope = source;
    Value* source_child = make_int(interp, 1701);
    array_payload.items[0] = source_child;
    Env* source_env = make_env(interp, interp.global_env);
    if (source_env != null) {
        env_define_with_barrier(source_env, partial_sym, partial_value, interp);
        env_define_with_barrier(source_env, iter_sym, iter_value, interp);
    }

    interp.current_scope = target;
    Env* copied_env = source_env != null
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    Value* copied_partial = copied_env != null ? copied_env.lookup(partial_sym) : null;
    Value* copied_iter = copied_env != null ? copied_env.lookup(iter_sym) : null;
    Value* copied_partial_first = copied_partial != null && copied_partial.tag == PARTIAL_PRIM
        ? copied_partial.partial_val.first_arg
        : null;
    Value* copied_iter_partial = copied_iter != null && copied_iter.tag == ITERATOR
        ? copied_iter.iterator_val
        : null;
    Value* copied_iter_first = copied_iter_partial != null && copied_iter_partial.tag == PARTIAL_PRIM
        ? copied_iter_partial.partial_val.first_arg
        : null;

    bool partial_copy_ok = copied_partial != null &&
        copied_partial != partial_value &&
        copied_partial.tag == PARTIAL_PRIM &&
        copied_partial_first != null &&
        copied_partial_first != array_value &&
        copied_partial_first.tag == ARRAY &&
        copied_partial_first.array_val != null &&
        copied_partial_first.array_val.items != null &&
        copied_partial_first.array_val.items[0] != null &&
        copied_partial_first.array_val.items[0] != source_child &&
        copied_partial_first.array_val.items[0].tag == INT &&
        copied_partial_first.array_val.items[0].int_val == 1701;

    bool iter_copy_ok = copied_iter != null &&
        copied_iter != iter_value &&
        copied_iter.tag == ITERATOR &&
        copied_iter_partial != null &&
        copied_iter_partial.tag == PARTIAL_PRIM &&
        copied_iter_partial != partial_value &&
        copied_iter_first != null &&
        copied_iter_first != array_value &&
        copied_iter_first.tag == ARRAY &&
        copied_iter_first.array_val != null &&
        copied_iter_first.array_val.items != null &&
        copied_iter_first.array_val.items[0] != null &&
        copied_iter_first.array_val.items[0] != source_child &&
        copied_iter_first.array_val.items[0].tag == INT &&
        copied_iter_first.array_val.items[0].int_val == 1701;

    main::scope_release(source);

    bool post_release_ok = partial_copy_ok &&
        iter_copy_ok &&
        copied_partial_first != null &&
        copied_partial_first.array_val != null &&
        copied_partial_first.array_val.items != null &&
        copied_partial_first.array_val.items[0] != null &&
        copied_partial_first.array_val.items[0].tag == INT &&
        copied_partial_first.array_val.items[0].int_val == 1701 &&
        copied_iter_first != null &&
        copied_iter_first.array_val != null &&
        copied_iter_first.array_val.items != null &&
        copied_iter_first.array_val.items[0] != null &&
        copied_iter_first.array_val.items[0].tag == INT &&
        copied_iter_first.array_val.items[0].int_val == 1701;

    interp.current_scope = saved;
    main::scope_release(target);

    bool ok = source_env != null && partial_copy_ok && iter_copy_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco partial wrapper copies nested shared wrapper edge");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco partial wrapper copies nested shared wrapper edge (source_env=%s partial_copy_ok=%s iter_copy_ok=%s post_release_ok=%s)",
            source_env != null ? "yes" : "no",
            partial_copy_ok ? "yes" : "no",
            iter_copy_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_iterator_shared_wrapper_edge_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must copy an ITERATOR binding when its direct
    // shared-wrapper payload still points at a child in the releasing scope.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco iterator wrapper copies nested shared wrapper edge");
        (*fail)++;
        return;
    }

    SymbolId iter_sym = interp.symbols.intern("__jit_policy_tco_iter_with_array");

    interp.current_scope = target;
    Value* array_value = interp.alloc_value();
    array_value.tag = ARRAY;
    main::scope_register_dtor(target, (void*)array_value, &scope_dtor_value);
    Array* array_payload = (Array*)mem::malloc(Array.sizeof);
    if (array_payload == null) {
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco iterator wrapper copies nested shared wrapper edge (setup array)");
        (*fail)++;
        return;
    }
    array_payload.capacity = 4;
    array_payload.length = 1;
    array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
    if (array_payload.items == null) {
        mem::free(array_payload);
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco iterator wrapper copies nested shared wrapper edge (setup array items)");
        (*fail)++;
        return;
    }
    for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
    array_value.array_val = array_payload;

    Value* iter_value = make_iterator(interp, array_value);
    if (iter_value == null) {
        interp.current_scope = saved;
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco iterator wrapper copies nested shared wrapper edge (setup iterator)");
        (*fail)++;
        return;
    }

    interp.current_scope = source;
    Value* source_child = make_int(interp, 3701);
    array_payload.items[0] = source_child;
    Env* source_env = make_env(interp, interp.global_env);
    if (source_env != null) {
        env_define_with_barrier(source_env, iter_sym, iter_value, interp);
    }

    interp.current_scope = target;
    Env* copied_env = source_env != null
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    Value* copied_iter = copied_env != null ? copied_env.lookup(iter_sym) : null;
    Value* copied_array = copied_iter != null && copied_iter.tag == ITERATOR
        ? copied_iter.iterator_val
        : null;

    bool copy_ok = copied_iter != null &&
        copied_iter != iter_value &&
        copied_iter.tag == ITERATOR &&
        copied_array != null &&
        copied_array != array_value &&
        copied_array.tag == ARRAY &&
        copied_array.array_val != null &&
        copied_array.array_val.items != null &&
        copied_array.array_val.items[0] != null &&
        copied_array.array_val.items[0] != source_child &&
        copied_array.array_val.items[0].tag == INT &&
        copied_array.array_val.items[0].int_val == 3701;

    main::scope_release(source);

    bool post_release_ok = copy_ok &&
        copied_array != null &&
        copied_array.array_val != null &&
        copied_array.array_val.items != null &&
        copied_array.array_val.items[0] != null &&
        copied_array.array_val.items[0].tag == INT &&
        copied_array.array_val.items[0].int_val == 3701;

    interp.current_scope = saved;
    main::scope_release(target);

    bool ok = source_env != null && copy_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco iterator wrapper copies nested shared wrapper edge");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco iterator wrapper copies nested shared wrapper edge (source_env=%s copy_ok=%s post_release_ok=%s)",
            source_env != null ? "yes" : "no",
            copy_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_foreign_partial_shared_wrapper_edge_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must copy a foreign PARTIAL_PRIM binding when its
    // shared-wrapper arg still points at a child in the releasing scope.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    main::ScopeRegion* foreign = main::scope_create(saved);
    if (source == null || target == null || foreign == null) {
        if (foreign != null) main::scope_release(foreign);
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign partial wrapper copies nested shared wrapper edge");
        (*fail)++;
        return;
    }

    SymbolId partial_sym = interp.symbols.intern("__jit_policy_tco_foreign_partial_with_array");

    interp.current_scope = foreign;
    Value* array_value = interp.alloc_value();
    array_value.tag = ARRAY;
    main::scope_register_dtor(foreign, (void*)array_value, &scope_dtor_value);
    Array* array_payload = (Array*)mem::malloc(Array.sizeof);
    if (array_payload == null) {
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign partial wrapper copies nested shared wrapper edge (setup array)");
        (*fail)++;
        return;
    }
    array_payload.capacity = 4;
    array_payload.length = 1;
    array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
    if (array_payload.items == null) {
        mem::free(array_payload);
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign partial wrapper copies nested shared wrapper edge (setup array items)");
        (*fail)++;
        return;
    }
    for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
    array_value.array_val = array_payload;

    Value* partial_value = interp.alloc_value();
    partial_value.tag = PARTIAL_PRIM;
    partial_value.partial_val.func = &prim_add;
    partial_value.partial_val.remaining = 1;
    partial_value.partial_val.first_arg = array_value;
    partial_value.partial_val.second_arg = make_int(interp, 55);

    interp.current_scope = source;
    Value* source_child = make_int(interp, 2701);
    array_payload.items[0] = source_child;
    Env* source_env = make_env(interp, interp.global_env);
    if (source_env != null) {
        env_define_with_barrier(source_env, partial_sym, partial_value, interp);
    }

    interp.current_scope = target;
    Env* copied_env = source_env != null
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    Value* copied_partial = copied_env != null ? copied_env.lookup(partial_sym) : null;
    Value* copied_first = copied_partial != null && copied_partial.tag == PARTIAL_PRIM
        ? copied_partial.partial_val.first_arg
        : null;

    bool copy_ok = copied_partial != null &&
        copied_partial != partial_value &&
        copied_partial.tag == PARTIAL_PRIM &&
        copied_first != null &&
        copied_first != array_value &&
        copied_first.tag == ARRAY &&
        copied_first.array_val != null &&
        copied_first.array_val.items != null &&
        copied_first.array_val.items[0] != null &&
        copied_first.array_val.items[0] != source_child &&
        copied_first.array_val.items[0].tag == INT &&
        copied_first.array_val.items[0].int_val == 2701;

    main::scope_release(source);

    bool post_release_ok = copy_ok &&
        copied_first != null &&
        copied_first.array_val != null &&
        copied_first.array_val.items != null &&
        copied_first.array_val.items[0] != null &&
        copied_first.array_val.items[0].tag == INT &&
        copied_first.array_val.items[0].int_val == 2701;

    interp.current_scope = saved;
    main::scope_release(target);
    main::scope_release(foreign);

    bool ok = source_env != null && copy_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco foreign partial wrapper copies nested shared wrapper edge");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco foreign partial wrapper copies nested shared wrapper edge (source_env=%s copy_ok=%s post_release_ok=%s)",
            source_env != null ? "yes" : "no",
            copy_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_foreign_shared_wrapper_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must not reuse a foreign shared wrapper by
    // identity; disjoint graph-carrying bindings must be copied into target.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    main::ScopeRegion* foreign = main::scope_create(saved);
    if (source == null || target == null || foreign == null) {
        if (foreign != null) main::scope_release(foreign);
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign shared wrapper copies by value");
        (*fail)++;
        return;
    }

    SymbolId array_sym = interp.symbols.intern("__jit_policy_tco_foreign_array");

    interp.current_scope = foreign;
    Value* array_value = interp.alloc_value();
    array_value.tag = ARRAY;
    main::scope_register_dtor(foreign, (void*)array_value, &scope_dtor_value);
    Array* array_payload = (Array*)mem::malloc(Array.sizeof);
    if (array_payload == null) {
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign shared wrapper copies by value (setup array)");
        (*fail)++;
        return;
    }
    array_payload.capacity = 4;
    array_payload.length = 1;
    array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
    if (array_payload.items == null) {
        mem::free(array_payload);
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(source);
        main::scope_release(target);
        io::printn("[FAIL] jit policy: tco foreign shared wrapper copies by value (setup array items)");
        (*fail)++;
        return;
    }
    for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
    array_payload.items[0] = make_int(interp, 314);
    array_value.array_val = array_payload;

    interp.current_scope = source;
    Env* source_env = make_env(interp, interp.global_env);
    if (source_env != null) {
        env_define_with_barrier(source_env, array_sym, array_value, interp);
    }

    interp.current_scope = target;
    Env* copied_env = source_env != null
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    Value* copied_array = copied_env != null ? copied_env.lookup(array_sym) : null;
    bool copy_ok = copied_array != null &&
        copied_array.tag == ARRAY &&
        copied_array != array_value &&
        copied_array.array_val != null &&
        copied_array.array_val != array_payload &&
        copied_array.array_val.items != null &&
        copied_array.array_val.items[0] != null &&
        copied_array.array_val.items[0].tag == INT &&
        copied_array.array_val.items[0].int_val == 314;

    main::scope_release(foreign);

    bool post_release_ok = copy_ok &&
        copied_array != null &&
        copied_array.array_val != null &&
        copied_array.array_val.items != null &&
        copied_array.array_val.items[0] != null &&
        copied_array.array_val.items[0].tag == INT &&
        copied_array.array_val.items[0].int_val == 314;

    interp.current_scope = saved;
    main::scope_release(source);
    main::scope_release(target);

    bool ok = source_env != null && copy_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco foreign shared wrapper copies by value");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco foreign shared wrapper copies by value (source_env=%s copy_ok=%s post_release_ok=%s)",
            source_env != null ? "yes" : "no",
            copy_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}
