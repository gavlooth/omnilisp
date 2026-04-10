module lisp;


import std::io;
import std::core::mem;
import main;

fn bool jit_policy_str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

alias JitPolicyTestFn = fn void(Interp* interp, int* pass, int* fail);

fn bool jit_policy_filter_name_exact(char* filter, char[] name) {
    usz len = 0;
    while (filter[len] != 0) len++;
    if (len != name.len) return false;
    for (usz i = 0; i < len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return true;
}

fn bool jit_policy_filter_matches(char[] name) {
    char* filter = test_c_getenv("OMNI_JIT_POLICY_FILTER");
    if (filter == null) return true;
    usz start = 0;
    while (true) {
        usz end = start;
        while (filter[end] != 0 && filter[end] != ',') end++;
        usz len = end - start;
        if (len == name.len) {
            bool same = true;
            for (usz i = 0; i < len; i++) {
                if (filter[start + i] != name[i]) {
                    same = false;
                    break;
                }
            }
            if (same) return true;
        }
        if (filter[end] == 0) break;
        start = end + 1;
    }
    return false;
}

fn void run_jit_policy_case(char[] name, JitPolicyTestFn test_fn, Interp* interp, int* pass, int* fail) {
    if (!jit_policy_filter_matches(name)) {
        return;
    }
    test_fn(interp, pass, fail);
}

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
        source_payload.param = 0;
        source_payload.params = null;
        source_payload.param_count = 0;
        source_payload.has_rest = false;
        source_payload.rest_param = 0;
        source_payload.body = null;
        source_payload.env = interp.global_env;
        source_payload.has_param = false;
        source_payload.has_typed_params = false;
        source_payload.name = 0;
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
        foreign_payload.param = 0;
        foreign_payload.params = null;
        foreign_payload.param_count = 0;
        foreign_payload.has_rest = false;
        foreign_payload.rest_param = 0;
        foreign_payload.body = null;
        foreign_payload.env = interp.global_env;
        foreign_payload.has_param = false;
        foreign_payload.has_typed_params = false;
        foreign_payload.name = 0;
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
    Value* copied_closure = setup_ok ? copy_to_parent(moved_closure, interp) : null;
    Value* copied_iterator = setup_ok ? copy_to_parent(moved_iterator, interp) : null;

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

fn void run_jit_policy_closure_method_iterator_boundary_test(Interp* interp, int* pass, int* fail) {
    // Invariant: typed method wrappers that return iterators must detach a thunk
    // payload from the callee TEMP lane before handing the iterator back.
    test_eq(
        interp,
        "jit closure method iterator wrapper detaches thunk boundary",
        "(block (define (wrap (^Closure thunk)) (Iterator thunk)) (define (wrap (^Array arr)) (let (len (length arr)) (let make-it (i 0) (if (= i len) (wrap (lambda () nil)) (wrap (lambda () (cons (ref arr i) (make-it (+ i 1))))))))) (car (next (wrap [1 2 3]))))",
        1,
        pass,
        fail
    );
}

fn void run_jit_policy_rec_closure_type_sig_copy_failure_test(Interp* interp, int* pass, int* fail) {
    // Invariant: detached recursive closure publication fails closed if
    // type-signature cloning into the detached env scope fails.
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved_scope);
    if (source == null) {
        io::printn("[FAIL] jit policy: rec closure type signature copy failure returns null");
        (*fail)++;
        return;
    }

    SymbolId rec_sym = interp.symbols.intern("__jit_policy_rec_type_sig");
    Value* placeholder = null;
    main::ScopeRegion* detached_env_scope = null;
    Value* closure = null;
    MethodSignature* sig = null;

    interp.current_scope = source;
    Env* captured_env = make_env(interp, interp.global_env);
    if (captured_env != null) {
        placeholder = make_nil(interp);
        env_define_with_barrier(captured_env, rec_sym, placeholder, interp);
        closure = make_env_scope_closure_for_test(interp, captured_env, &detached_env_scope);
    }
    if (closure != null) {
        sig = (MethodSignature*)source.alloc(MethodSignature.sizeof);
        if (sig != null && method_signature_alloc_arrays_in_scope(sig, source, 1, 0)) {
            sig.param_types[0] = interp.tid_Integer;
            sig.param_type_vars[0] = (SymbolId)0;
            sig.has_val_literal[0] = false;
            sig.val_literals[0].tag = NIL;
            sig.val_literals[0].int_value = 0;
            sig.val_literals[0].sym_value = 0;
            closure.closure_val.has_typed_params = true;
            closure.closure_val.type_sig = sig;
        } else {
            sig = null;
        }
    }

    Value* binding_before = closure != null && closure.closure_val.env != null
        ? closure.closure_val.env.lookup(rec_sym)
        : null;
    g_method_signature_scope_copy_call_count = 0;
    g_method_signature_force_scope_copy_fail_on_call = 1;
    Value* published = closure != null ? jit_patch_rec_closure(closure, null, rec_sym, interp) : null;
    g_method_signature_force_scope_copy_fail_on_call = 0;
    Value* binding_after = closure != null && closure.closure_val.env != null
        ? closure.closure_val.env.lookup(rec_sym)
        : null;

    bool setup_ok = captured_env != null &&
                    closure != null &&
                    closure.tag == CLOSURE &&
                    closure.closure_val != null &&
                    detached_env_scope != null &&
                    closure.closure_val.env_scope == detached_env_scope &&
                    sig != null &&
                    closure.closure_val.type_sig == sig &&
                    binding_before != null &&
                    binding_before.tag == NIL;
    bool fail_closed_ok = published == null &&
                          g_method_signature_scope_copy_call_count == 1 &&
                          binding_after == binding_before &&
                          binding_after != null &&
                          binding_after.tag == NIL &&
                          closure.closure_val.type_sig == sig;
    int scope_copy_calls = (int)g_method_signature_scope_copy_call_count;
    g_method_signature_scope_copy_call_count = 0;

    interp.current_scope = saved_scope;
    main::scope_release(source);

    bool ok = setup_ok && fail_closed_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: rec closure type signature copy failure returns null");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: rec closure type signature copy failure returns null (setup_ok=%s fail_closed_ok=%s call_count=%d published=%s binding_after=%s)",
            setup_ok ? "yes" : "no",
            fail_closed_ok ? "yes" : "no",
            scope_copy_calls,
            published == null ? "null" : "non-null",
            binding_after == null ? "null" : (binding_after.tag == NIL ? "nil" : "non-nil")
        );
        (*fail)++;
    }
}

fn void run_jit_policy_stdlib_teardown_redefinition_test(Interp* interp, int* pass, int* fail) {
    // Regression: stdlib-loaded typed method redefinition must still tear down
    // cleanly after interpreter destroy.
    (void)interp;

    Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
    if (tmp == null) {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly (alloc)");
        (*fail)++;
        return;
    }

    tmp.init();
    if (!bootstrap_runtime_interp(tmp)) {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly (bootstrap)");
        (*fail)++;
        tmp.destroy();
        mem::free(tmp);
        return;
    }

    EvalResult r = run(
        "(block "
        "  (define (dispatch-teardown (^Integer x)) x) "
        "  (define (dispatch-teardown x) x) "
        "  (define (dispatch-teardown (^Integer x)) (+ x 1)) "
        "  (dispatch-teardown 1))",
        tmp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == INT &&
              r.value.int_val == 2;

    tmp.destroy();
    mem::free(tmp);

    if (ok) {
        emit_pass_literal("[PASS] jit policy: stdlib teardown redefinition destroys cleanly");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly");
        (*fail)++;
    }
}

fn void run_jit_policy_match_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: match binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(match '(1 2 3 4 5) ([a b c d e] (+ a e)) (_ 0))";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding match clause");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: match binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: match binding growth failure returns error");
        print_jit_failure_debug(interp, "jit match binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "((lambda (a b c d e) (+ a e)) 1 2 3 4 5)";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding argument");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg binding growth failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_primitive_buffer_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg primitive buffer failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(+ 1 2 3)";
    g_jit_force_multi_arg_buffer_alloc_fail = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_jit_force_multi_arg_buffer_alloc_fail = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while preparing call arguments");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg primitive buffer failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg primitive buffer failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg primitive buffer failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_method_table_buffer_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg method-table buffer failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source =
        "(block "
        "  (define (dispatch (^Integer a) (^Integer b)) (+ a b)) "
        "  (dispatch 20 22))";
    g_jit_force_multi_arg_buffer_alloc_fail = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_jit_force_multi_arg_buffer_alloc_fail = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while preparing call arguments");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg method-table buffer failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg method-table buffer failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg method-table buffer failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_match_guard_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: match guard binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(match [1 2 3 4 5] ((? true [a b c d e]) (+ a e)) (_ 0))";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while defining binding");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: match guard binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: match guard binding growth failure returns error");
        print_jit_failure_debug(interp, "jit match guard binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_invalid_primitive_state_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* arg = make_int(interp, 9);
    Value* invalid = interp.alloc_value();
    bool setup_ok = arg != null && invalid != null;
    if (setup_ok) {
        *invalid = {};
        invalid.tag = PRIMITIVE;
        invalid.prim_val = null;
    }

    Value* result = setup_ok ? jit_apply_value_primitive(invalid, arg, interp) : raise_error(interp, "setup failed");
    bool ok = setup_ok &&
        result != null &&
        result.tag == ERROR &&
        jit_policy_str_contains(result.str_chars[:result.str_len], "invalid primitive application state");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: invalid primitive state fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: invalid primitive state fails closed (setup_ok=%s tag=%d)",
            setup_ok ? "yes" : "no",
            result != null ? (int)result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_pending_raise_string_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force = g_value_force_make_string_alloc_fail;
    g_value_force_make_string_alloc_fail = true;
    EvalResult r = run("(handle (/ 1 0) (raise msg 42))", interp);
    g_value_force_make_string_alloc_fail = saved_force;

    char[] msg = r.error.message[:256];
    bool ok = r.error.has_error &&
        jit_policy_str_contains(msg, "out of memory while allocating string");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: pending raise string materialization fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: pending raise string materialization fails closed (run_error=%s msg=%s)",
            r.error.has_error ? "yes" : "no",
            r.error.has_error ? (ZString)msg : (ZString)"<none>"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handler_arg_list_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    main::StackCtx* ctx = null;
    HandleEffectState* state = null;
    Value* handler_closure = null;
    Value* signal_result = null;
    Value* raise_result = null;
    if (ok) {
        EvalResult closure_r = run("(lambda (k x) x)", other);
        ok = !closure_r.error.has_error && closure_r.value != null && closure_r.value.tag == CLOSURE;
        if (ok) handler_closure = closure_r.value;
    }
    if (ok) {
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = state != null;
    }
    if (ok) {
        handle_effect_state_init(state, other);
        state.handler_copy.clause_count = 1;
        state.handler_copy.tags = (SymbolId*)mem::malloc(SymbolId.sizeof);
        state.handler_copy.closures = (Value**)mem::malloc(Value*.sizeof);
        ok = state.handler_copy.tags != null && state.handler_copy.closures != null;
    }
    if (ok) {
        ctx = main::stack_ctx_create(&other.stack_ctx_pool);
        ok = ctx != null;
    }
    if (ok) {
        int saved_force = g_value_force_make_list2_alloc_fail_nth;

        state.signal_tag = other.symbols.intern("ask");
        state.signal_arg = make_int(other, 0);
        state.handler_copy.tags[0] = state.signal_tag;
        state.handler_copy.closures[0] = handler_closure;
        state.signaled = true;
        g_value_force_make_list2_alloc_fail_nth = 1;
        signal_result = jit_handle_run_signals(other, ctx, state, 0);

        other.flags.raise_pending = true;
        other.raise_payload = make_int(other, 42);
        other.raise_msg_len = 0;
        other.raise_msg[0] = 0;
        state.handler_copy.tags[0] = other.sym_raise;
        g_value_force_make_list2_alloc_fail_nth = 1;
        raise_result = jit_handle_dispatch_pending_raise_value(other, state);

        g_value_force_make_list2_alloc_fail_nth = saved_force;
    }

    ok = ok &&
        signal_result != null &&
        signal_result.tag == ERROR &&
        jit_policy_str_contains(signal_result.str_chars[:signal_result.str_len], "handle: failed to construct handler args") &&
        raise_result != null &&
        raise_result.tag == ERROR &&
        jit_policy_str_contains(raise_result.str_chars[:raise_result.str_len], "handle: failed to construct raise handler args");

    if (ctx != null) main::stack_ctx_destroy(ctx, &other.stack_ctx_pool);
    if (state != null) handle_effect_state_release(state);
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: handler arg list construction fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handler arg list construction fails closed (signal_tag=%d raise_tag=%d)",
            signal_result != null ? (int)signal_result.tag : -1,
            raise_result != null ? (int)raise_result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("JIT Policy Tests");

    bool saved_jit_enabled = interp.flags.jit_enabled;
    bool run_stdlib_teardown_redefinition_only =
        test_c_getenv("OMNI_JIT_POLICY_STDLIB_TEARDOWN_REDEFINITION") != null;
    if (run_stdlib_teardown_redefinition_only) {
        run_jit_policy_stdlib_teardown_redefinition_test(interp, pass, fail);
        interp.flags.jit_enabled = saved_jit_enabled;
        jit_cache_clear();
        return;
    }

    bool parsed = true;
    if (jit_policy_filter_matches("warm-cache")) {
        parsed = run_jit_policy_warm_cache_tests(interp, pass, fail);
    }
    if (parsed) {
        run_jit_policy_case("cache-overwrite-count", &run_jit_policy_cache_overwrite_count_test, interp, pass, fail);
        run_jit_policy_case("cache-probe-saturation-recovery", &run_jit_policy_cache_probe_saturation_recovery_test, interp, pass, fail);
        run_jit_policy_case("retired-code-tombstone-table", &run_jit_policy_retired_code_tombstone_table_test, interp, pass, fail);
        run_jit_policy_case("gc-safe-point", &run_jit_policy_gc_safe_point_test, interp, pass, fail);
        run_jit_policy_case("stale-compiled-fn-guard", &run_jit_policy_stale_compiled_fn_guard_test, interp, pass, fail);
        run_jit_policy_case("foreign-compiled-fn-owner", &run_jit_policy_foreign_compiled_fn_owner_test, interp, pass, fail);
        run_jit_policy_case("boundary-reset", &run_jit_policy_boundary_reset_test, interp, pass, fail);
        run_jit_policy_case("stale-raise-scrub", &run_jit_policy_stale_raise_scrub_test, interp, pass, fail);
        run_jit_policy_case("variadic-no-fallback", &run_jit_policy_variadic_no_fallback_test, interp, pass, fail);
        run_jit_policy_case("match-binding-growth-failure", &run_jit_policy_match_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-binding-growth-failure", &run_jit_policy_multi_arg_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-primitive-buffer-failure", &run_jit_policy_multi_arg_primitive_buffer_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-method-table-buffer-failure", &run_jit_policy_multi_arg_method_table_buffer_failure_test, interp, pass, fail);
        run_jit_policy_case("match-guard-binding-growth-failure", &run_jit_policy_match_guard_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-interp-lifetime", &run_jit_policy_multi_interp_lifetime_test, interp, pass, fail);
        run_jit_policy_case("continuation-teardown", &run_jit_policy_continuation_teardown_test, interp, pass, fail);
        run_jit_policy_case("shared-handle-state-teardown", &run_jit_policy_shared_handle_state_teardown_test, interp, pass, fail);
        run_jit_policy_case("cross-interp-continuation-guard", &run_jit_policy_cross_interp_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("escaped-handle-continuation-guard", &run_jit_policy_escaped_handle_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("side-effect-escaped-handle-continuation-guard", &run_jit_policy_side_effect_escaped_handle_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("multishot-capture-scope-guard-clone", &run_jit_policy_multishot_capture_scope_guard_clone_test, interp, pass, fail);
        run_jit_policy_case("shadowed-primitive-call", &run_jit_policy_shadowed_primitive_call_test, interp, pass, fail);
        run_jit_policy_case("capture-boundary-reset", &run_jit_policy_capture_boundary_reset_test, interp, pass, fail);
        run_jit_policy_case("tco-stale-generation-moved-binding-copy", &run_jit_policy_tco_stale_generation_moved_binding_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-persistent-parent-root-rewrite", &run_jit_policy_tco_persistent_parent_root_rewrite_test, interp, pass, fail);
        run_jit_policy_case("tco-persistent-parent-stale-generation-rewrite", &run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test, interp, pass, fail);
        run_jit_policy_case("root-box-extend-copies-cross-scope-closure-value", &run_jit_policy_root_box_extend_copies_cross_scope_closure_value_test, interp, pass, fail);
        run_jit_policy_case("closure-persistent-env-copy-scope-contract", &run_jit_policy_closure_persistent_env_copy_scope_contract_test, interp, pass, fail);
        run_jit_policy_case("cross-scope-alias-policy", &run_jit_policy_cross_scope_alias_policy_test, interp, pass, fail);
        run_jit_policy_case("invalid-primitive-state-fails-closed", &run_jit_policy_invalid_primitive_state_fails_closed_test, interp, pass, fail);
        run_jit_policy_case("pending-raise-string-alloc-failure", &run_jit_policy_pending_raise_string_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("handler-arg-list-alloc-failure", &run_jit_policy_handler_arg_list_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("iterator-array-thunk-boundary", &run_jit_policy_iterator_array_thunk_boundary_test, interp, pass, fail);
        run_jit_policy_case("tco-cons-iterator-edge-copy", &run_jit_policy_tco_cons_iterator_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-cons-releasing-scalar-edge-copy", &run_jit_policy_tco_cons_releasing_scalar_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-partial-shared-wrapper-edge-copy", &run_jit_policy_tco_partial_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-iterator-shared-wrapper-edge-copy", &run_jit_policy_tco_iterator_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-foreign-partial-shared-wrapper-edge-copy", &run_jit_policy_tco_foreign_partial_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-foreign-shared-wrapper-copy", &run_jit_policy_tco_foreign_shared_wrapper_copy_test, interp, pass, fail);
        run_jit_policy_case("closure-method-iterator-boundary", &run_jit_policy_closure_method_iterator_boundary_test, interp, pass, fail);
        run_jit_policy_case("rec-closure-type-sig-copy-failure", &run_jit_policy_rec_closure_type_sig_copy_failure_test, interp, pass, fail);
        run_jit_policy_case("stdlib-teardown-redefinition", &run_jit_policy_stdlib_teardown_redefinition_test, interp, pass, fail);
    }

    interp.flags.jit_enabled = saved_jit_enabled;
    jit_cache_clear();
}

fn void run_jit_policy_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_jit_policy_tests, pass, fail);
}
