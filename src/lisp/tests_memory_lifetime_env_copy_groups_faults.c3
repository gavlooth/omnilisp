module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_env_copy_fault_surface_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy failure paths return typed boundary faults (not silent null-only failures).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy typed fault surfacing");
            (*fail)++;
            return;
        }

        interp.current_scope = source;
        Env* deep_leaf = interp.global_env;
        bool setup_ok = true;
        for (usz i = 0; i < 300; i++) {
            deep_leaf = make_env(interp, deep_leaf);
            if (deep_leaf == null) {
                setup_ok = false;
                break;
            }
        }

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(deep_leaf, interp, target);
        bool depth_fault = copied.fault_code == BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT;
        bool parent_fault_with_depth = copied.fault_code == BOUNDARY_ENV_COPY_FAULT_PARENT_COPY &&
                                       copied.depth >= 256;
        bool ok = setup_ok &&
                  copied.env == null &&
                  (depth_fault || parent_fault_with_depth) &&
                  copied.depth >= 256;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy typed fault surfacing");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: env-copy typed fault surfacing (fault=%s depth=%d env=%s)",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth,
                copied.env == null ? "null" : "non-null");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy destructor registration OOM fails closed");
            (*fail)++;
            return;
        }

        interp.current_scope = source;
        Env* source_frame = make_env(interp, interp.global_env);
        bool setup_ok = source_frame != null;

        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(source_frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;

        bool ok = setup_ok &&
                  copied.env == null &&
                  copied.fault_code == BOUNDARY_ENV_COPY_FAULT_DTOR_REGISTRATION &&
                  copied.depth == 0;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy destructor registration OOM fails closed (setup_ok=%s fault=%s depth=%d env=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth,
                copied.env == null ? "null" : "non-null");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_invalid_persistent_in_place_guard_test(Interp* interp, int* pass, int* fail) {
    // Invariant: in-place rewrite is rejected for malformed persistent boxes that are
    // not actually root-owned (fail closed instead of mutating transient env frames).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy invalid persistent in-place guard");
            (*fail)++;
            return;
        }

        interp.current_scope = source;
        Env* bogus = make_env(interp, interp.global_env);
        bool setup_ok = bogus != null && !main::is_in_scope((void*)bogus, interp.root_scope);
        Env* parent_before = setup_ok ? bogus.parent : null;
        if (setup_ok) {
            // Deliberately malformed lifetime tag: local-scope frame claiming persistent semantics.
            bogus.lifetime_kind = ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX;
        }

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(bogus, interp, target);
        bool ok = setup_ok &&
                  copied.env == null &&
                  copied.fault_code == BOUNDARY_ENV_COPY_FAULT_PARENT_COPY &&
                  copied.depth == 0 &&
                  bogus.parent == parent_before &&
                  boundary_ptr_in_scope((void*)bogus, source);

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy invalid persistent in-place guard");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: env-copy invalid persistent in-place guard (fault=%s depth=%d env=%s)",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth,
                copied.env == null ? "null" : "non-null");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_stale_generation_persistent_parent_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy parent rewrite keys off physical releasing-scope
    // membership, not potentially stale parent.scope_gen stamps.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy stale-generation persistent parent rewrite");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_stale_parent_payload");
        SymbolId box_sym = interp.symbols.intern("__copy_env_stale_parent_box");

        interp.current_scope = source;
        Env* transient_parent = make_env(interp, interp.global_env);
        bool setup_ok = transient_parent != null;
        if (setup_ok) {
            env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 404), interp);
            transient_parent.scope_gen = interp.root_scope.generation; // stale on purpose
        }

        Env* persistent_box = setup_ok
            ? boundary_env_extend_in_root(
                interp,
                transient_parent,
                box_sym,
                make_root_int_for_test(interp, 9)
            )
            : null;
        Env* original_parent = persistent_box != null ? persistent_box.parent : null;

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(
            persistent_box,
            interp,
            target
        );

        bool copied_ok = boundary_env_copy_result_ok(copied) &&
                         copied.env != null &&
                         copied.env == persistent_box &&
                         copied.env.is_root_persistent_box();
        bool rewritten_ok = copied_ok &&
                            copied.env.parent != null &&
                            copied.env.parent != original_parent &&
                            main::is_in_scope((void*)copied.env.parent, interp.root_scope) &&
                            !main::is_in_scope((void*)copied.env.parent, target);

        Value* before_release = rewritten_ok ? copied.env.lookup(payload_sym) : null;
        main::scope_release(source);
        Value* after_source_release = rewritten_ok ? copied.env.lookup(payload_sym) : null;

        interp.current_scope = saved_scope;
        main::scope_release(target);
        Value* after_target_release = rewritten_ok ? copied.env.lookup(payload_sym) : null;

        bool value_ok = before_release != null &&
                        before_release.tag == INT &&
                        before_release.int_val == 404 &&
                        after_source_release != null &&
                        after_source_release.tag == INT &&
                        after_source_release.int_val == 404 &&
                        after_target_release != null &&
                        after_target_release.tag == INT &&
                        after_target_release.int_val == 404;
        bool ok = setup_ok && copied_ok && rewritten_ok && value_ok;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy stale-generation persistent parent rewrite");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy stale-generation persistent parent rewrite (setup_ok=%s copied_ok=%s rewritten_ok=%s value_ok=%s fault=%s depth=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                rewritten_ok ? "yes" : "no",
                value_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_rejects_undelimited_closure_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy rejects releasing-scope closure/iterator wrappers that
    // have no detached env_scope ownership envelope.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy rejects undelimited closure alias");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_bad_closure");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_bad_iterator");

        interp.current_scope = source;
        Env* captured = make_env(interp, interp.global_env);
        Value* bad_closure = captured != null ? make_closure_no_param(interp, null, captured) : null;
        Value* bad_iter = bad_closure != null ? make_iterator(interp, bad_closure) : null;

        Env* closure_frame = make_env(interp, interp.global_env);
        if (closure_frame != null && bad_closure != null) {
            env_define_with_barrier(closure_frame, closure_sym, bad_closure, interp);
        }

        Env* iter_frame = make_env(interp, interp.global_env);
        if (iter_frame != null && bad_iter != null) {
            env_define_with_barrier(iter_frame, iter_sym, bad_iter, interp);
        }

        BoundaryEnvCopyResult closure_copy = boundary_copy_env_to_target_scope_checked(closure_frame, interp, target);
        BoundaryEnvCopyResult iter_copy = boundary_copy_env_to_target_scope_checked(iter_frame, interp, target);

        bool setup_ok = captured != null &&
                        bad_closure != null &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null &&
                        bad_iter != null &&
                        closure_frame != null &&
                        iter_frame != null;
        bool closure_fault_ok = closure_copy.env == null &&
                                closure_copy.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                                closure_copy.depth == 0;
        bool iter_fault_ok = iter_copy.env == null &&
                             iter_copy.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                             iter_copy.depth == 0;
        bool ok = setup_ok && closure_fault_ok && iter_fault_ok;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy rejects undelimited closure alias");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: env-copy rejects undelimited closure alias (setup=%s closure_fault=%s iter_fault=%s closure=%s iter=%s)",
                setup_ok ? "yes" : "no",
                closure_fault_ok ? "yes" : "no",
                iter_fault_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(closure_copy.fault_code),
                (ZString)boundary_env_copy_fault_name(iter_copy.fault_code));
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_rejects_cross_scope_undelimited_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy fail-closes undelimited closure and iterator-thunk aliases
    // even when wrappers originate from a non-releasing foreign scope.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* foreign = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || foreign == null || target == null) {
            if (source != null) main::scope_release(source);
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy rejects cross-scope undelimited aliases");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_cross_scope_bad_closure");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_cross_scope_bad_iterator");

        interp.current_scope = foreign;
        Env* captured = make_env(interp, interp.global_env);
        Value* foreign_bad_closure = captured != null ? make_closure_no_param(interp, null, captured) : null;
        Value* foreign_bad_thunk = make_int(interp, 77);
        Value* foreign_bad_iter = foreign_bad_thunk != null ? make_iterator(interp, foreign_bad_thunk) : null;

        interp.current_scope = source;
        Env* closure_frame = make_env(interp, interp.global_env);
        if (closure_frame != null && foreign_bad_closure != null) {
            env_define_with_barrier(closure_frame, closure_sym, foreign_bad_closure, interp);
        }

        Env* iter_frame = make_env(interp, interp.global_env);
        if (iter_frame != null && foreign_bad_iter != null) {
            env_define_with_barrier(iter_frame, iter_sym, foreign_bad_iter, interp);
        }

        BoundaryEnvCopyResult closure_copy = boundary_copy_env_to_target_scope_checked(closure_frame, interp, target);
        BoundaryEnvCopyResult iter_copy = boundary_copy_env_to_target_scope_checked(iter_frame, interp, target);

        bool setup_ok = captured != null &&
                        foreign_bad_closure != null &&
                        foreign_bad_closure.closure_val != null &&
                        foreign_bad_closure.closure_val.env_scope == null &&
                        foreign_bad_iter != null &&
                        foreign_bad_thunk != null &&
                        foreign_bad_thunk.tag == INT &&
                        closure_frame != null &&
                        iter_frame != null;
        bool closure_fault_ok = closure_copy.env == null &&
                                closure_copy.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                                closure_copy.depth == 0;
        bool iter_fault_ok = iter_copy.env == null &&
                             iter_copy.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                             iter_copy.depth == 0;
        bool ok = setup_ok && closure_fault_ok && iter_fault_ok;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(foreign);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy rejects cross-scope undelimited aliases");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy rejects cross-scope undelimited aliases (setup=%s closure_fault=%s iter_fault=%s closure=%s iter=%s)",
                setup_ok ? "yes" : "no",
                closure_fault_ok ? "yes" : "no",
                iter_fault_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(closure_copy.fault_code),
                (ZString)boundary_env_copy_fault_name(iter_copy.fault_code)
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_clones_global_iterator_closure_test(Interp* interp, int* pass, int* fail) {
    // Invariant: iterator payload closures that are safe undelimited global-env
    // clones follow the same env-copy contract as plain closures.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy clones global iterator closure payload");
            (*fail)++;
            return;
        }

        SymbolId iter_sym = interp.symbols.intern("__copy_env_global_iterator");

        interp.current_scope = source;
        Value* source_closure = make_closure_no_param(interp, null, interp.global_env);
        Value* source_iter = source_closure != null ? make_iterator(interp, source_closure) : null;
        Env* source_frame = make_env(interp, interp.global_env);
        if (source_frame != null && source_iter != null) {
            env_define_with_barrier(source_frame, iter_sym, source_iter, interp);
        }

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(source_frame, interp, target);
        Value* copied_iter = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(iter_sym)
            : null;
        Value* copied_inner = copied_iter != null && copied_iter.tag == ITERATOR
            ? copied_iter.iterator_val
            : null;

        bool setup_ok = source_frame != null &&
                        source_closure != null &&
                        source_closure.tag == CLOSURE &&
                        source_closure.closure_val != null &&
                        source_closure.closure_val.env == interp.global_env &&
                        source_closure.closure_val.env_scope == null &&
                        source_iter != null;
        bool copied_ok = boundary_env_copy_result_ok(copied) &&
                         copied.env != null &&
                         copied_iter != null &&
                         copied_iter.tag == ITERATOR &&
                         copied_iter != source_iter &&
                         copied_inner != null &&
                         copied_inner.tag == CLOSURE &&
                         copied_inner != source_closure &&
                         copied_inner.closure_val != null &&
                         copied_inner.closure_val.env == interp.global_env &&
                         copied_inner.closure_val.env_scope == null &&
                         main::is_in_scope((void*)copied_iter, target) &&
                         main::is_in_scope((void*)copied_inner, target);

        main::scope_release(source);
        Value* copied_after_release = copied.env != null ? copied.env.lookup(iter_sym) : null;
        Value* copied_inner_after_release = copied_after_release != null && copied_after_release.tag == ITERATOR
            ? copied_after_release.iterator_val
            : null;
        bool post_release_ok = copied_after_release != null &&
                               copied_after_release.tag == ITERATOR &&
                               copied_inner_after_release != null &&
                               copied_inner_after_release.tag == CLOSURE &&
                               copied_inner_after_release.closure_val != null &&
                               copied_inner_after_release.closure_val.env == interp.global_env &&
                               copied_inner_after_release.closure_val.env_scope == null;

        interp.current_scope = saved_scope;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy clones global iterator closure payload");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy clones global iterator closure payload (setup_ok=%s copied_ok=%s post_release_ok=%s fault=%s depth=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_memoizes_iterator_closure_payload_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy keeps one memoized clone when the same source closure
    // is bound directly and also reached through an iterator wrapper.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy memoizes iterator closure payload clone");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_iter_closure_memo_closure");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_iter_closure_memo_iter");

        interp.current_scope = source;
        Value* source_closure = make_closure_no_param(interp, null, interp.global_env);
        Value* source_iter = source_closure != null ? make_iterator(interp, source_closure) : null;
        Env* source_frame = make_env(interp, interp.global_env);
        bool setup_ok = source_frame != null &&
                        source_closure != null &&
                        source_closure.tag == CLOSURE &&
                        source_closure.closure_val != null &&
                        source_closure.closure_val.env == interp.global_env &&
                        source_closure.closure_val.env_scope == null &&
                        source_iter != null;
        if (setup_ok) {
            env_define_with_barrier(source_frame, closure_sym, source_closure, interp);
            env_define_with_barrier(source_frame, iter_sym, source_iter, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(source_frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        Value* copied_closure = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(closure_sym)
            : null;
        Value* copied_iter = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(iter_sym)
            : null;
        Value* copied_inner = copied_iter != null && copied_iter.tag == ITERATOR
            ? copied_iter.iterator_val
            : null;

        bool copied_ok = boundary_env_copy_result_ok(copied) &&
                         copied.env != null &&
                         copied_closure != null &&
                         copied_closure.tag == CLOSURE &&
                         copied_closure != source_closure &&
                         copied_iter != null &&
                         copied_iter.tag == ITERATOR &&
                         copied_iter != source_iter &&
                         copied_inner == copied_closure &&
                         copied_inner.closure_val != null &&
                         copied_inner.closure_val.env == interp.global_env &&
                         copied_inner.closure_val.env_scope == null &&
                         main::is_in_scope((void*)copied_closure, target) &&
                         main::is_in_scope((void*)copied_iter, target);

        main::scope_release(source);
        Value* copied_closure_after_release = copied.env != null ? copied.env.lookup(closure_sym) : null;
        Value* copied_iter_after_release = copied.env != null ? copied.env.lookup(iter_sym) : null;
        Value* copied_inner_after_release =
            copied_iter_after_release != null && copied_iter_after_release.tag == ITERATOR
            ? copied_iter_after_release.iterator_val
            : null;
        bool post_release_ok = copied_closure_after_release != null &&
                               copied_closure_after_release.tag == CLOSURE &&
                               copied_iter_after_release != null &&
                               copied_iter_after_release.tag == ITERATOR &&
                               copied_inner_after_release == copied_closure_after_release;

        interp.current_scope = saved_scope;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy memoizes iterator closure payload clone");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy memoizes iterator closure payload clone (setup_ok=%s copied_ok=%s post_release_ok=%s fault=%s depth=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth
            );
            (*fail)++;
        }
    }
}
