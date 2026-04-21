module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_jit_policy_tco_stale_generation_moved_binding_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: JIT TCO env-copy treats bindings as copy-required whenever
    // wrapper/payload physically resides in releasing scope, even with stale
    // scope-generation metadata.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco stale-generation moved binding forces copy");
        (*fail)++;
        return;
    }

    SymbolId closure_sym = interp.symbols.intern("__jit_policy_stale_tco_closure");

    interp.current_scope = source;
    Closure* source_payload = (Closure*)source.alloc(Closure.sizeof);
    bool setup_ok = source_payload != null;
    if (setup_ok) {
        source_payload.param = (SymbolId)0;
        source_payload.params = null;
        source_payload.param_count = 0;
        source_payload.has_rest = false;
        source_payload.rest_param = (SymbolId)0;
        source_payload.body = null;
        source_payload.env = interp.global_env;
        source_payload.has_param = false;
        source_payload.has_typed_params = false;
        source_payload.name = (SymbolId)0;
        source_payload.env_scope = null;
        source_payload.scope_gen = interp.root_scope.generation; // stale on purpose
        source_payload.type_sig = null;
    }

    interp.current_scope = target;
    Value* moved_wrapper = setup_ok ? interp.alloc_value() : null;
    setup_ok = setup_ok && moved_wrapper != null;
    if (setup_ok) {
        moved_wrapper.tag = CLOSURE;
        moved_wrapper.closure_val = source_payload;
    }

    interp.current_scope = source;
    Env* source_env = setup_ok ? make_env(interp, interp.global_env) : null;
    setup_ok = setup_ok && source_env != null;
    if (setup_ok) {
        env_define_with_barrier(source_env, closure_sym, moved_wrapper, interp);
    }

    interp.current_scope = target;
    Env* copied_env = setup_ok ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source) : null;
    Value* copied_value = copied_env != null ? copied_env.lookup(closure_sym) : null;
    bool copied_ok = copied_value != null && copied_value.tag == CLOSURE;
    bool wrapper_copied = copied_ok && copied_value != moved_wrapper;
    bool payload_copied = copied_ok &&
                          copied_value.closure_val != null &&
                          copied_value.closure_val != source_payload &&
                          main::is_in_scope((void*)copied_value.closure_val, target);

    main::scope_release(source);
    Value* after_source_release = copied_env != null ? copied_env.lookup(closure_sym) : null;
    bool post_release_ok = after_source_release != null &&
                           after_source_release.tag == CLOSURE &&
                           after_source_release.closure_val != null;

    interp.current_scope = saved;
    main::scope_release(target);

    bool ok = setup_ok && copied_ok && wrapper_copied && payload_copied && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco stale-generation moved binding forces copy");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco stale-generation moved binding forces copy (setup_ok=%s copied_ok=%s wrapper_copied=%s payload_copied=%s post_release_ok=%s)",
            setup_ok ? "yes" : "no",
            copied_ok ? "yes" : "no",
            wrapper_copied ? "yes" : "no",
            payload_copied ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_persistent_parent_root_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: JIT TCO env-copy keeps root-persistent mutable-box identity but
    // rewrites transient parents into root-owned lifetime, not short-lived call scopes.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco persistent parent rewrite lands in root scope");
        (*fail)++;
        return;
    }

    SymbolId payload_sym = interp.symbols.intern("__jit_policy_tco_parent_payload");
    SymbolId box_sym = interp.symbols.intern("__jit_policy_tco_parent_box");

    interp.current_scope = source;
    Env* transient_parent = make_env(interp, interp.global_env);
    env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 313), interp);

    Env* persistent_box = boundary_env_extend_in_root(
        interp,
        transient_parent,
        box_sym,
        make_root_int_for_test(interp, 5)
    );
    Env* original_parent = persistent_box != null ? persistent_box.parent : null;

    interp.current_scope = target;
    Env* copied = jit_copy_tco_env_chain_for_recycle(persistent_box, interp, source);

    bool copied_ok = copied != null &&
                     copied == persistent_box &&
                     copied.is_root_persistent_box();
    bool rewritten_ok = copied_ok &&
                       copied.parent != null &&
                       copied.parent != original_parent &&
                       main::is_in_scope((void*)copied.parent, interp.root_scope) &&
                       !main::is_in_scope((void*)copied.parent, target);

    Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
    main::scope_release(source);
    Value* after_source_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    interp.current_scope = saved;
    main::scope_release(target);
    Value* after_target_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    bool value_ok = before_release != null &&
                    before_release.tag == INT &&
                    before_release.int_val == 313 &&
                    after_source_release != null &&
                    after_source_release.tag == INT &&
                    after_source_release.int_val == 313 &&
                    after_target_release != null &&
                    after_target_release.tag == INT &&
                    after_target_release.int_val == 313;
    bool ok = copied_ok && rewritten_ok && value_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco persistent parent rewrite lands in root scope");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco persistent parent rewrite lands in root scope (copied_ok=%s rewritten_ok=%s value_ok=%s)",
            copied_ok ? "yes" : "no",
            rewritten_ok ? "yes" : "no",
            value_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: parent rewrite for root-persistent boxes keys off physical
    // releasing-scope membership, not potentially stale parent.scope_gen stamps.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco stale-generation persistent parent rewrite");
        (*fail)++;
        return;
    }

    SymbolId payload_sym = interp.symbols.intern("__jit_policy_tco_stale_parent_payload");
    SymbolId box_sym = interp.symbols.intern("__jit_policy_tco_stale_parent_box");

    interp.current_scope = source;
    Env* transient_parent = make_env(interp, interp.global_env);
    env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 404), interp);
    transient_parent.scope_gen = interp.root_scope.generation; // stale on purpose

    Env* persistent_box = boundary_env_extend_in_root(
        interp,
        transient_parent,
        box_sym,
        make_root_int_for_test(interp, 9)
    );
    Env* original_parent = persistent_box != null ? persistent_box.parent : null;

    interp.current_scope = target;
    Env* copied = jit_copy_tco_env_chain_for_recycle(persistent_box, interp, source);

    bool copied_ok = copied != null &&
                     copied == persistent_box &&
                     copied.is_root_persistent_box();
    bool rewritten_ok = copied_ok &&
                       copied.parent != null &&
                       copied.parent != original_parent &&
                       main::is_in_scope((void*)copied.parent, interp.root_scope) &&
                       !main::is_in_scope((void*)copied.parent, target);

    Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
    main::scope_release(source);
    Value* after_source_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    interp.current_scope = saved;
    main::scope_release(target);
    Value* after_target_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    bool value_ok = before_release != null &&
                    before_release.tag == INT &&
                    before_release.int_val == 404 &&
                    after_source_release != null &&
                    after_source_release.tag == INT &&
                    after_source_release.int_val == 404 &&
                    after_target_release != null &&
                    after_target_release.tag == INT &&
                    after_target_release.int_val == 404;
    bool ok = copied_ok && rewritten_ok && value_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco stale-generation persistent parent rewrite");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco stale-generation persistent parent rewrite (copied_ok=%s rewritten_ok=%s value_ok=%s)",
            copied_ok ? "yes" : "no",
            rewritten_ok ? "yes" : "no",
            value_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_root_box_extend_copies_cross_scope_closure_value_test(Interp* interp, int* pass, int* fail) {
    // Invariant: root-owned mutable-box extension must route cross-scope closure
    // values through the write barrier instead of storing transient wrappers.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    if (source == null) {
        io::printn("[FAIL] jit policy: root box extend copies cross-scope closure value");
        (*fail)++;
        return;
    }

    SymbolId closure_sym = interp.symbols.intern("__jit_policy_root_box_closure");

    interp.current_scope = source;
    Value* source_closure = make_closure_no_param(interp, null, interp.global_env);
    Env* boxed = source_closure != null
        ? boundary_env_extend_in_root(interp, interp.global_env, closure_sym, source_closure)
        : null;
    Value* boxed_before_release = boxed != null ? boxed.lookup(closure_sym) : null;

    bool setup_ok = source_closure != null &&
        boxed != null &&
        boxed.is_root_persistent_box() &&
        boxed_before_release != null &&
        boxed_before_release.tag == CLOSURE &&
        boxed_before_release.closure_val != null;
    bool copied_ok = setup_ok &&
        boxed_before_release != source_closure &&
        main::is_in_scope((void*)boxed_before_release, interp.root_scope) &&
        main::is_in_scope((void*)boxed_before_release.closure_val, interp.root_scope);

    main::scope_release(source);
    Value* boxed_after_release = boxed != null ? boxed.lookup(closure_sym) : null;

    interp.current_scope = saved;

    bool post_release_ok = boxed_after_release != null &&
        boxed_after_release.tag == CLOSURE &&
        boxed_after_release.closure_val != null &&
        boxed_after_release == boxed_before_release;
    bool ok = setup_ok && copied_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: root box extend copies cross-scope closure value");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: root box extend copies cross-scope closure value (setup_ok=%s copied_ok=%s post_release_ok=%s same_ptr=%s)",
            setup_ok ? "yes" : "no",
            copied_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no",
            boxed_before_release == source_closure ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_closure_persistent_env_copy_scope_contract_test(Interp* interp, int* pass, int* fail) {
    // Invariant: closure env-copy may reuse a root-persistent mutable box by
    // identity, but must not attach a foreign detached env_scope to that box.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    if (source == null) {
        io::printn("[FAIL] jit policy: closure env-copy root box does not keep foreign env_scope");
        (*fail)++;
        return;
    }

    SymbolId payload_sym = interp.symbols.intern("__jit_policy_closure_root_box_payload");
    SymbolId box_sym = interp.symbols.intern("__jit_policy_closure_root_box");

    interp.current_scope = source;
    Env* transient_parent = make_env(interp, interp.global_env);
    env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 515), interp);
    Env* persistent_box = boundary_env_extend_in_root(
        interp,
        transient_parent,
        box_sym,
        make_root_int_for_test(interp, 8)
    );
    interp.current_scope = saved;
    Value* closure = persistent_box != null ? make_closure_no_param(interp, null, persistent_box) : null;
    bool setup_ok = persistent_box != null &&
        persistent_box.is_root_persistent_box() &&
        closure != null &&
        closure.tag == CLOSURE &&
        closure.closure_val != null &&
        closure.closure_val.env_scope == null;
    int root_rc_before = interp.root_scope != null ? (int)interp.root_scope.refcount : -1;

    BoundaryEnvCopyFault copy_fault = BOUNDARY_ENV_COPY_FAULT_NONE;
    interp.current_scope = source;
    bool copied_ok = setup_ok &&
        jit_copy_closure_env_if_needed(interp, closure, persistent_box, true, &copy_fault);
    int root_rc_after_copy = interp.root_scope != null ? (int)interp.root_scope.refcount : -1;
    bool env_ok = copied_ok &&
        closure.closure_val.env == persistent_box &&
        closure.closure_val.env_scope == null &&
        root_rc_before == root_rc_after_copy;

    main::scope_release(source);
    bool post_release_ok = env_ok &&
        closure.closure_val.env != null &&
        closure.closure_val.env.lookup(payload_sym) != null &&
        closure.closure_val.env.lookup(payload_sym).tag == INT &&
        closure.closure_val.env.lookup(payload_sym).int_val == 515;

    interp.current_scope = saved;

    bool ok = setup_ok && copied_ok && env_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: closure env-copy root box does not keep foreign env_scope");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: closure env-copy root box does not keep foreign env_scope (setup_ok=%s copied_ok=%s env_ok=%s post_release_ok=%s root_rc_before=%d root_rc_after_copy=%d fault=%s)",
            setup_ok ? "yes" : "no",
            copied_ok ? "yes" : "no",
            env_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no",
            root_rc_before,
            root_rc_after_copy,
            (ZString)boundary_env_copy_fault_name(copy_fault)
        );
        (*fail)++;
    }
}

fn void run_jit_policy_cross_scope_alias_policy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary copy/promotion fast-reuse never preserves undelimited
    // closure or iterator-thunk aliases from foreign/disjoint scopes.
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
    main::ScopeRegion* target = main::scope_create(saved_scope);
    main::ScopeRegion* releasing = main::scope_create(saved_scope);
    main::ScopeRegion* foreign = main::scope_create(saved_scope);
    if (target == null || releasing == null || foreign == null) {
        if (target != null) main::scope_release(target);
        if (releasing != null) main::scope_release(releasing);
        if (foreign != null) main::scope_release(foreign);
        io::printn("[FAIL] jit policy: cross-scope alias policy for copy/promotion");
        (*fail)++;
        return;
    }

    interp.current_scope = foreign;
    Closure* foreign_payload = (Closure*)foreign.alloc(Closure.sizeof);
    bool setup_ok = foreign_payload != null;
    if (setup_ok) {
        foreign_payload.param = (SymbolId)0;
        foreign_payload.params = null;
        foreign_payload.param_count = 0;
        foreign_payload.has_rest = false;
        foreign_payload.rest_param = (SymbolId)0;
        foreign_payload.body = null;
        foreign_payload.env = interp.global_env;
        foreign_payload.has_param = false;
        foreign_payload.has_typed_params = false;
        foreign_payload.name = (SymbolId)0;
        foreign_payload.env_scope = null;
        foreign_payload.scope_gen = foreign.generation;
        foreign_payload.type_sig = null;
    }
    Value* foreign_thunk = make_int(interp, 909);

    interp.current_scope = target;
    Value* moved_closure = setup_ok ? interp.alloc_value() : null;
    Value* moved_iterator = setup_ok ? make_iterator(interp, foreign_thunk) : null;
    setup_ok = setup_ok && moved_closure != null && moved_iterator != null;
    if (setup_ok) {
        moved_closure.tag = CLOSURE;
        moved_closure.closure_val = foreign_payload;
    }

    interp.releasing_scope = releasing;
    Value* copied_closure = setup_ok ? test_copy_to_parent(moved_closure, interp) : null;
    Value* copied_iterator = setup_ok ? test_copy_to_parent(moved_iterator, interp) : null;

    bool closure_copy_ok = copied_closure != null &&
                           copied_closure.tag == CLOSURE &&
                           copied_closure != moved_closure &&
                           copied_closure.closure_val != null &&
                           copied_closure.closure_val != foreign_payload &&
                           main::is_in_scope((void*)copied_closure, target) &&
                           main::is_in_scope((void*)copied_closure.closure_val, target);
    bool iterator_copy_ok = copied_iterator != null &&
                            copied_iterator.tag == ITERATOR &&
                            copied_iterator != moved_iterator &&
                            copied_iterator.iterator_val != null &&
                            copied_iterator.iterator_val != foreign_thunk &&
                            copied_iterator.iterator_val.tag == INT &&
                            copied_iterator.iterator_val.int_val == 909;

    interp.current_scope = saved_scope;
    Value* ancestor_iterator = make_iterator(interp, foreign_thunk);
    interp.current_scope = target;
    Value* promoted_iterator = ancestor_iterator != null ? promote_to_escape(ancestor_iterator, interp) : null;

    bool promotion_ok = promoted_iterator != null &&
                        promoted_iterator.tag == ITERATOR &&
                        promoted_iterator != ancestor_iterator &&
                        promoted_iterator.scope_gen == target.escape_generation &&
                        promoted_iterator.iterator_val != null &&
                        promoted_iterator.iterator_val != foreign_thunk &&
                        promoted_iterator.iterator_val.tag == INT &&
                        promoted_iterator.iterator_val.int_val == 909;

    main::scope_release(foreign);
    bool post_release_ok = copied_closure != null &&
                           copied_closure.tag == CLOSURE &&
                           copied_closure.closure_val != null &&
                           copied_iterator != null &&
                           copied_iterator.tag == ITERATOR &&
                           copied_iterator.iterator_val != null &&
                           copied_iterator.iterator_val.tag == INT &&
                           promoted_iterator != null &&
                           promoted_iterator.tag == ITERATOR &&
                           promoted_iterator.iterator_val != null &&
                           promoted_iterator.iterator_val.tag == INT;

    interp.current_scope = saved_scope;
    interp.releasing_scope = saved_releasing_scope;
    main::scope_release(target);
    main::scope_release(releasing);

    bool ok = setup_ok && closure_copy_ok && iterator_copy_ok && promotion_ok && post_release_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: cross-scope alias policy for copy/promotion");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: cross-scope alias policy for copy/promotion (setup_ok=%s closure_copy_ok=%s iterator_copy_ok=%s promotion_ok=%s post_release_ok=%s)",
            setup_ok ? "yes" : "no",
            closure_copy_ok ? "yes" : "no",
            iterator_copy_ok ? "yes" : "no",
            promotion_ok ? "yes" : "no",
            post_release_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_iterator_array_thunk_boundary_test(Interp* interp, int* pass, int* fail) {
    // Invariant: JIT-created iterator thunks that capture array iteration state
    // must survive boundary return without leaving a dangling env lookup path.
    test_eq(
        interp,
        "jit iterator list length survives array boundary",
        "(length (List (Iterator [1 2 3])))",
        3,
        pass,
        fail
    );
}

fn void run_jit_policy_tco_cons_iterator_edge_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: TCO env-copy must not reuse a surviving cons cell whose cdr
    // still points at an iterator wrapper in the releasing scope.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco cons binding copies nested iterator edge");
        (*fail)++;
        return;
    }

    SymbolId pair_sym = interp.symbols.intern("__jit_policy_tco_pair_with_iter");

    interp.current_scope = source;
    Value* list_value = make_cons(
        interp,
        make_int(interp, 1),
        make_cons(
            interp,
            make_int(interp, 2),
            make_cons(interp, make_int(interp, 3), make_nil(interp))
        )
    );
    Value* iterator = iterator_from_list_value(list_value, interp);
    Value*[1] next_args = { iterator };
    Value* first_pair = prim_next(next_args[0:1], null, interp);
    Env* source_env = make_env(interp, interp.global_env);
    if (source_env != null && first_pair != null) {
        env_define_with_barrier(source_env, pair_sym, first_pair, interp);
    }

    interp.current_scope = target;
    Env* copied_env = source_env != null
        ? jit_copy_tco_env_chain_for_recycle(source_env, interp, source)
        : null;

    main::scope_release(source);

    Value* copied_pair = copied_env != null ? copied_env.lookup(pair_sym) : null;
    bool copied_ok = copied_pair != null &&
        copied_pair.tag == CONS &&
        copied_pair.cons_val.car != null &&
        copied_pair.cons_val.car.tag == INT &&
        copied_pair.cons_val.car.int_val == 1 &&
        copied_pair.cons_val.cdr != null &&
        copied_pair.cons_val.cdr.tag == ITERATOR;

    Value* second_pair = null;
    if (copied_ok) {
        Value*[1] second_args = { copied_pair.cons_val.cdr };
        second_pair = prim_next(second_args[0:1], null, interp);
    }
    bool second_ok = second_pair != null &&
        second_pair.tag == CONS &&
        second_pair.cons_val.car != null &&
        second_pair.cons_val.car.tag == INT &&
        second_pair.cons_val.car.int_val == 2;

    interp.current_scope = saved;
    main::scope_release(target);

    bool ok = source_env != null && copied_ok && second_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco cons binding copies nested iterator edge");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco cons binding copies nested iterator edge (source_env=%s copied_ok=%s second_ok=%s)",
            source_env != null ? "yes" : "no",
            copied_ok ? "yes" : "no",
            second_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}
