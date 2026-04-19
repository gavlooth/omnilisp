module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_destination_fault_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: destination-built structured wrappers fail closed when a
    // nested boundary child copy raises an opaque-primitive fault.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* foreign_scope = main::scope_create(root_scope);
        if (target_scope == null || releasing_scope == null || foreign_scope == null) {
            if (foreign_scope != null) main::scope_release(foreign_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape destination nested fault fail-closed");
            (*fail)++;
        } else {
            interp.current_scope = foreign_scope;
            Value* opaque = interp.alloc_value();
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign_scope, (void*)opaque, &scope_dtor_value);
            Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            int* opaque_payload = (int*)mem::malloc(int.sizeof);
            bool setup_ok = opaque_prim != null && opaque_payload != null;
            if (!setup_ok) {
                if (opaque_prim != null) mem::free(opaque_prim);
                if (opaque_payload != null) mem::free(opaque_payload);
            } else {
                *opaque_prim = {};
                *opaque_payload = 1401;
                opaque_prim.name[0] = 'c';
                opaque_prim.name[1] = 0;
                opaque_prim.arity = 1;
                opaque_prim.tag = 1401;
                opaque_prim.user_data = (void*)opaque_payload;
                opaque.prim_val = opaque_prim;
            }

            Value* cons = null;
            Value* partial = null;
            Value* iter = null;
            if (setup_ok) {
                interp.current_scope = releasing_scope;
                partial = interp.alloc_value();
                partial.tag = PARTIAL_PRIM;
                partial.partial_val.func = &prim_add;
                partial.partial_val.remaining = 1;
                partial.partial_val.first_arg = opaque;
                partial.partial_val.second_arg = make_int(interp, 44);
                iter = make_iterator(interp, partial);
                cons = make_cons(interp, iter, make_nil(interp));
                setup_ok = cons != null && partial != null && iter != null;
            }

            BoundaryCommitEscapeResult committed_cons = {};
            BoundaryCommitEscapeResult committed_partial = {};
            BoundaryCommitEscapeResult committed_iter = {};
            if (setup_ok) {
                PromotionContext ctx = {};
                boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
                committed_cons = boundary_commit_escape(
                    cons,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                committed_partial = boundary_commit_escape(
                    partial,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                committed_iter = boundary_commit_escape(
                    iter,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                boundary_promotion_context_end(interp, &ctx);
            }

            bool cons_ok = setup_ok &&
                committed_cons.value != null &&
                committed_cons.value.tag == ERROR &&
                committed_cons.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                !committed_cons.releasing_scope_spliced &&
                !committed_cons.reused_in_target_chain &&
                main::is_in_scope((void*)committed_cons.value, target_scope);
            bool partial_ok = setup_ok &&
                committed_partial.value != null &&
                committed_partial.value.tag == ERROR &&
                committed_partial.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                !committed_partial.releasing_scope_spliced &&
                !committed_partial.reused_in_target_chain &&
                main::is_in_scope((void*)committed_partial.value, target_scope);
            bool iter_ok = setup_ok &&
                committed_iter.value != null &&
                committed_iter.value.tag == ERROR &&
                committed_iter.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                !committed_iter.releasing_scope_spliced &&
                !committed_iter.reused_in_target_chain &&
                main::is_in_scope((void*)committed_iter.value, target_scope);

            interp.current_scope = root_scope;
            main::scope_release(foreign_scope);
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (cons_ok && partial_ok && iter_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape destination nested fault fail-closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary_commit_escape destination nested fault fail-closed (setup_ok=%s cons_tag=%d cons_outcome=%d partial_tag=%d partial_outcome=%d iter_tag=%d iter_outcome=%d)",
                    setup_ok ? "yes" : "no",
                    committed_cons.value != null ? (int)committed_cons.value.tag : -1,
                    (int)committed_cons.outcome,
                    committed_partial.value != null ? (int)committed_partial.value.tag : -1,
                    (int)committed_partial.outcome,
                    committed_iter.value != null ? (int)committed_iter.value.tag : -1,
                    (int)committed_iter.outcome
                );
                (*fail)++;
            }
        }
    }

    // Invariant: destination-builder memo entries are builder-local and do not
    // survive after the temporary build scope returns.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape destination builder memo is local");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* nested = make_cons(
                interp,
                make_int(interp, 1501),
                make_cons(interp, make_int(interp, 1502), make_nil(interp))
            );
            Value* src = interp.alloc_value();
            bool setup_ok = nested != null && src != null;
            if (setup_ok) {
                src.tag = PARTIAL_PRIM;
                src.partial_val.func = &prim_add;
                src.partial_val.remaining = 1;
                src.partial_val.first_arg = nested;
                src.partial_val.second_arg = make_int(interp, 1503);
                setup_ok = src.partial_val.second_arg != null;
            }

            PromotionContext ctx = {};
            Value* built_first = null;
            Value* built_second = null;
            bool memo_empty_before = false;
            bool memo_empty_after_first = false;
            bool memo_empty_after_second = false;
            bool child_cache_empty_after_first = false;
            bool child_cache_empty_after_second = false;
            bool chain_cache_empty_after_first = false;
            bool chain_cache_empty_after_second = false;
            if (setup_ok) {
                boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
                memo_empty_before = ctx.memo_head == null;
                built_first = boundary_build_destination_partial_escape(
                    src,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                memo_empty_after_first = ctx.memo_head == null;
                child_cache_empty_after_first = promotion_context_lookup(&ctx, nested) == null;
                bool cached_first = false;
                chain_cache_empty_after_first =
                    !promotion_context_lookup_scope_chain_cache(
                        &ctx,
                        (void*)nested,
                        target_scope,
                        nested.scope_gen,
                        &cached_first
                    );
                built_second = boundary_build_destination_partial_escape(
                    src,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                memo_empty_after_second = ctx.memo_head == null;
                child_cache_empty_after_second = promotion_context_lookup(&ctx, nested) == null;
                bool cached_second = false;
                chain_cache_empty_after_second =
                    !promotion_context_lookup_scope_chain_cache(
                        &ctx,
                        (void*)nested,
                        target_scope,
                        nested.scope_gen,
                        &cached_second
                    );
                boundary_promotion_context_end(interp, &ctx);
            }

            bool first_ok = setup_ok &&
                built_first != null &&
                built_first.tag == PARTIAL_PRIM &&
                built_first != src &&
                built_first.scope_gen == target_scope.escape_generation &&
                built_first.partial_val.first_arg != null &&
                built_first.partial_val.first_arg.tag == CONS &&
                built_first.partial_val.first_arg != nested &&
                main::is_in_scope((void*)built_first, target_scope);
            bool second_ok = setup_ok &&
                built_second != null &&
                built_second.tag == PARTIAL_PRIM &&
                built_second != built_first &&
                built_second.scope_gen == target_scope.escape_generation &&
                built_second.partial_val.first_arg != null &&
                built_second.partial_val.first_arg.tag == CONS &&
                built_second.partial_val.first_arg != built_first.partial_val.first_arg &&
                main::is_in_scope((void*)built_second, target_scope);
            bool memo_ok = memo_empty_before &&
                memo_empty_after_first &&
                memo_empty_after_second &&
                child_cache_empty_after_first &&
                child_cache_empty_after_second &&
                chain_cache_empty_after_first &&
                chain_cache_empty_after_second;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (first_ok && second_ok && memo_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape destination builder memo is local");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary_commit_escape destination builder memo is local (setup_ok=%s first_ok=%s second_ok=%s memo_ok=%s memo_after_first=%s memo_after_second=%s child_cached_first=%s child_cached_second=%s chain_cached_first=%s chain_cached_second=%s)",
                    setup_ok ? "yes" : "no",
                    first_ok ? "yes" : "no",
                    second_ok ? "yes" : "no",
                    memo_ok ? "yes" : "no",
                    memo_empty_after_first ? "empty" : "live",
                    memo_empty_after_second ? "empty" : "live",
                    child_cache_empty_after_first ? "no" : "yes",
                    child_cache_empty_after_second ? "no" : "yes",
                    chain_cache_empty_after_first ? "no" : "yes",
                    chain_cache_empty_after_second ? "no" : "yes"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: destination iterator detachment must participate in the
    // caller's promotion context so repeated detach of the same target child in
    // one builder epoch reuses the same copied value.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* build_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || build_scope == null) {
            if (build_scope != null) main::scope_release(build_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape iterator detach shares promotion ctx");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            SymbolId payload_sym = interp.symbols.intern("__boundary_destination_iter_detach_payload");
            Env* captured_env = make_env(interp, interp.global_env);
            main::ScopeRegion* closure_env_scope = null;
            int closure_env_destroy_count = 0;
            Value* shared_closure = null;
            if (captured_env != null) {
                env_define_with_barrier(captured_env, payload_sym, make_int(interp, 1511), interp);
                shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
                if (closure_env_scope != null) {
                    main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                        int* counter = (int*)ptr;
                        if (counter != null) (*counter)++;
                    });
                }
            }

            PromotionContext ctx = {};
            Value* detached_direct = null;
            Value* detached_again = null;
            bool setup_ok = shared_closure != null &&
                            shared_closure.tag == CLOSURE &&
                            closure_env_scope != null &&
                            closure_env_scope.refcount == 1;
            main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
            if (setup_ok) {
                interp.current_scope = build_scope;
                interp.releasing_scope = target_scope;
                boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
                detached_direct = boundary_copy_to_parent_site_ctx(
                    shared_closure,
                    interp,
                    COPY_SITE_GENERIC,
                    &ctx
                );
                detached_again = boundary_destination_detach_iterator_inner(
                    shared_closure,
                    interp,
                    target_scope,
                    &ctx
                );
                boundary_promotion_context_end(interp, &ctx);
            }
            interp.releasing_scope = saved_releasing_scope;

            bool direct_ok = setup_ok &&
                             detached_direct != null &&
                             detached_direct.tag == CLOSURE &&
                             detached_direct != shared_closure &&
                             main::is_in_scope((void*)detached_direct, build_scope);
            bool shared_ok = direct_ok && detached_again == detached_direct;

            interp.current_scope = root_scope;
            main::scope_release(build_scope);
            int rc_after_build_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
            int destroy_count_after_build_release = closure_env_destroy_count;
            bool teardown_ok = rc_after_build_release == 1 &&
                               destroy_count_after_build_release == 0;
            main::scope_release(target_scope);

            if (direct_ok && shared_ok && teardown_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape iterator detach shares promotion ctx");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary_commit_escape iterator detach shares promotion ctx (setup_ok=%s direct_ok=%s shared_ok=%s teardown_ok=%s rc_after_build_release=%d destroy_count=%d)",
                    setup_ok ? "yes" : "no",
                    direct_ok ? "yes" : "no",
                    shared_ok ? "yes" : "no",
                    teardown_ok ? "yes" : "no",
                    rc_after_build_release,
                    destroy_count_after_build_release
                );
                (*fail)++;
            }
        }
    }

}
