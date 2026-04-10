module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_commit_escape_primary_tests(interp, pass, fail);

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

    // Invariant: mixed/uncertain provenance can commit through destination ESCAPE
    // promotion before falling back to deep boundary copy.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* disjoint_scope = main::scope_create(root_scope);
        if (target_scope == null || releasing_scope == null || disjoint_scope == null) {
            if (disjoint_scope != null) main::scope_release(disjoint_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape mixed uncertain destination commit");
            (*fail)++;
        } else {
            interp.current_scope = disjoint_scope;
            Value* src = make_int(interp, 909);

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 909 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta == 0 &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope) &&
                      !main::is_in_scope((void*)committed.value, disjoint_scope);

            interp.current_scope = root_scope;
            main::scope_release(disjoint_scope);
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape mixed uncertain destination commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape mixed uncertain destination commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: direct destination promotion must honor the caller-provided
    // promotion context even when a different context is currently active.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* disjoint_scope = main::scope_create(root_scope);
        if (target_scope == null || releasing_scope == null || disjoint_scope == null) {
            if (disjoint_scope != null) main::scope_release(disjoint_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape direct promotion honors caller ctx");
            (*fail)++;
        } else {
            interp.current_scope = disjoint_scope;
            Value* src = make_int(interp, 1701);

            PromotionContext outer_ctx = {};
            PromotionContext caller_ctx = {};
            boundary_promotion_context_begin(interp, &outer_ctx, PROMOTION_CTX_BUDGET_RETURN);
            caller_ctx.epoch = outer_ctx.epoch + 1;
            caller_ctx.budget = 0;
            caller_ctx.aborted = false;
            caller_ctx.abort_reason = PROMOTION_ABORT_NONE;
            caller_ctx.memo_head = null;
            caller_ctx.prev = &outer_ctx;
            promotion_context_reset_scope_chain_cache(&caller_ctx);

            BoundaryCommitDestinationPromotion promotion =
                boundary_commit_try_destination_promotion(
                    src,
                    interp,
                    target_scope,
                    releasing_scope,
                    &caller_ctx,
                    true
                );

            bool ok = !promotion.committed &&
                      promotion.value == src &&
                      promotion.fault_code == BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION &&
                      caller_ctx.aborted &&
                      caller_ctx.abort_reason == PROMOTION_ABORT_BUDGET_EXHAUSTED &&
                      !outer_ctx.aborted &&
                      outer_ctx.abort_reason == PROMOTION_ABORT_NONE &&
                      interp.active_promotion_ctx == &outer_ctx;

            boundary_promotion_context_end(interp, &outer_ctx);

            interp.current_scope = root_scope;
            main::scope_release(disjoint_scope);
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape direct promotion honors caller ctx");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary_commit_escape direct promotion honors caller ctx (committed=%s fault=%s caller_aborted=%s caller_reason=%d outer_aborted=%s active_outer=%s same=%s)",
                    promotion.committed ? "yes" : "no",
                    (ZString)boundary_commit_fault_name(promotion.fault_code),
                    caller_ctx.aborted ? "yes" : "no",
                    (int)caller_ctx.abort_reason,
                    outer_ctx.aborted ? "yes" : "no",
                    interp.active_promotion_ctx == null ? "no" : (interp.active_promotion_ctx == &outer_ctx ? "yes" : "other"),
                    promotion.value == src ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: destination partial build reuses already-target-escape routed args
    // without introducing extra boundary fallback traffic.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape destination partial routed escape reuse");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, 1201), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, 1202), interp);

            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = escaped_first;
            src.partial_val.second_arg = escaped_second;

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = escaped_first != null &&
                      escaped_second != null &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT &&
                      committed.value != null &&
                      committed.value.tag == PARTIAL_PRIM &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape destination partial routed escape reuse");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape destination partial routed escape reuse (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d first_same=%s second_same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.first_arg.scope_gen == target_scope.escape_generation) ? "yes" : "no",
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.second_arg.scope_gen == target_scope.escape_generation) ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: aborted promotion contexts now hard-fail commit fallback paths.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape fallback disallowed path");
            (*fail)++;
        } else {
            usz list_len = PROMOTION_CTX_BUDGET_RETURN + 64;
            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, list_len);

            BoundaryDecisionStats saved_boundary_stats = boundary_decision_stats_snapshot();
            defer boundary_restore_decision_stats(saved_boundary_stats);
            usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                      committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                      committed.value != null &&
                      committed.value.tag == ERROR &&
                      site_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope) &&
                      !main::is_in_scope((void*)committed.value, releasing_scope);

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape fallback disallowed path");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape fallback disallowed path (outcome=%d fault=%s spliced=%s reused=%s site_delta=%d same=%s)",
                    (int)committed.outcome,
                    (ZString)boundary_commit_fault_name(committed.fault_code),
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: failed boundary commits must roll back staged closure-env
    // normalization so aliased closures retain their original env/env_scope.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on failed commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            SymbolId payload_sym = interp.symbols.intern("__boundary_commit_alias_closure_payload");
            Env* captured_env = make_env(interp, interp.global_env);
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 731), interp);

            main::ScopeRegion* detached_env_scope = null;
            Value* closure = make_env_scope_closure_for_test(interp, captured_env, &detached_env_scope);
            bool setup_ok = closure != null &&
                            closure.tag == CLOSURE &&
                            detached_env_scope != null;
            if (setup_ok) {
                Value* promoted = promote_to_escape(closure, interp);
                setup_ok = promoted != null && promoted.tag == CLOSURE;
                closure = promoted;
            }
            if (setup_ok) {
                closure.closure_val.env = captured_env;
                setup_ok = closure.closure_val.env_scope != null;
            }

            Value* alias_closure = closure;
            Env* env_before = setup_ok ? closure.closure_val.env : null;
            main::ScopeRegion* env_scope_before = setup_ok ? closure.closure_val.env_scope : null;
            int rc_before = env_scope_before != null ? (int)env_scope_before.refcount : -1;
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) {
                committed = boundary_commit_escape(
                    closure,
                    interp,
                    target_scope,
                    releasing_scope,
                    null,
                    false
                );
            }

            int rc_after = env_scope_before != null ? (int)env_scope_before.refcount : -1;
            bool unchanged_ok = setup_ok &&
                                alias_closure != null &&
                                alias_closure.tag == CLOSURE &&
                                alias_closure.closure_val != null &&
                                alias_closure.closure_val.env == env_before &&
                                alias_closure.closure_val.env_scope == env_scope_before &&
                                closure_env_lookup_int(alias_closure, payload_sym, 731) &&
                                env_scope_before != null &&
                                rc_before == rc_after;
            bool commit_ok = setup_ok &&
                             !committed.releasing_scope_spliced &&
                             !committed.reused_in_target_chain &&
                             committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                             committed.fault_code == BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED &&
                             committed.value != null &&
                             committed.value.tag == ERROR;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (commit_ok && unchanged_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape aliased closure rollback on failed commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on failed commit (setup_ok=%s outcome=%d fault=%s unchanged=%s env_same=%s env_scope_same=%s rc_before=%d rc_after=%d)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    (ZString)boundary_commit_fault_name(committed.fault_code),
                    unchanged_ok ? "yes" : "no",
                    setup_ok && alias_closure != null && alias_closure.tag == CLOSURE && alias_closure.closure_val != null && alias_closure.closure_val.env == env_before ? "yes" : "no",
                    setup_ok && alias_closure != null && alias_closure.tag == CLOSURE && alias_closure.closure_val != null && alias_closure.closure_val.env_scope == env_scope_before ? "yes" : "no",
                    rc_before,
                    rc_after);
                (*fail)++;
            }
        }
    }

    // Invariant: destination error escape build fails closed when the error
    // string allocation fails mid-build.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary destination error escape OOM fails closed");
            (*fail)++;
        } else {
            uint target_escape_gen = target_scope.escape_generation;
            Value* source_error = make_error(interp, "boundary error escape oom");
            g_boundary_force_destination_error_escape_alloc_fail = true;
            Value* result = boundary_build_destination_error_escape(source_error, interp, target_scope);
            g_boundary_force_destination_error_escape_alloc_fail = false;

            bool ok = result == source_error &&
                      result != null &&
                      result.tag == ERROR &&
                      result.scope_gen != target_escape_gen &&
                      interp.current_scope == saved_scope &&
                      !main::is_in_scope((void*)result, target_scope);

            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary destination error escape OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary destination error escape OOM fails closed (same=%s tag=%d scope_gen=%d target_escape=%d in_target=%s current_restored=%s)",
                    result == source_error ? "yes" : "no",
                    result != null ? (int)result.tag : -1,
                    result != null ? (int)result.scope_gen : -1,
                    (int)target_escape_gen,
                    result != null && main::is_in_scope((void*)result, target_scope) ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: destination builders fail closed when ESCAPE wrapper
    // allocation itself fails.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary destination wrapper alloc OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, 9101), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, 9102), interp);
            Value* escaped_nil = promote_to_escape(make_nil(interp), interp);

            interp.current_scope = releasing_scope;
            Value* cons_tail = make_cons(interp, escaped_second, escaped_nil);
            Value* cons_src = make_cons(interp, escaped_first, cons_tail);

            Value* partial_src = interp.alloc_value();
            partial_src.tag = PARTIAL_PRIM;
            partial_src.partial_val.func = null;
            partial_src.partial_val.remaining = 1;
            partial_src.partial_val.first_arg = escaped_first;
            partial_src.partial_val.second_arg = escaped_second;

            Value* iterator_src = interp.alloc_value();
            iterator_src.tag = ITERATOR;
            iterator_src.iterator_val = escaped_first;

            Value* error_src = make_error(interp, "destination wrapper alloc oom");

            int saved_force = g_boundary_force_alloc_escape_value_fail_nth;

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* error_result = boundary_build_destination_error_escape(error_src, interp, target_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* cons_head_result = boundary_build_destination_cons_escape(
                cons_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 2;
            Value* cons_tail_result = boundary_build_destination_cons_escape(
                cons_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* partial_result = boundary_build_destination_partial_escape(
                partial_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* iterator_result = boundary_build_destination_iterator_escape(
                iterator_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = saved_force;

            bool ok = error_result == error_src &&
                      cons_head_result == cons_src &&
                      cons_tail_result == cons_src &&
                      partial_result == partial_src &&
                      iterator_result == iterator_src &&
                      !main::is_in_scope((void*)error_result, target_scope) &&
                      !main::is_in_scope((void*)cons_head_result, target_scope) &&
                      !main::is_in_scope((void*)cons_tail_result, target_scope) &&
                      !main::is_in_scope((void*)partial_result, target_scope) &&
                      !main::is_in_scope((void*)iterator_result, target_scope) &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary destination wrapper alloc OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary destination wrapper alloc OOM fails closed (error_same=%s cons_head_same=%s cons_tail_same=%s partial_same=%s iterator_same=%s scope_ok=%s)",
                    error_result == error_src ? "yes" : "no",
                    cons_head_result == cons_src ? "yes" : "no",
                    cons_tail_result == cons_src ? "yes" : "no",
                    partial_result == partial_src ? "yes" : "no",
                    iterator_result == iterator_src ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: generic STRING escape promotion fails closed when ESCAPE
    // allocation fails (no destination builder involved).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic STRING escape promotion OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_string(interp, "generic string promotion oom");

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
            Value* promoted = promote_to_escape(src, interp);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
            boundary_promotion_context_end(interp, &ctx);

            bool ok = promoted == src &&
                      promoted != null &&
                      promoted.tag == STRING &&
                      promoted.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic STRING escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic STRING escape promotion OOM fails closed (same=%s tag=%d chars=%s scope_ok=%s)",
                    promoted == src ? "yes" : "no",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic ERROR escape promotion fails closed when ESCAPE
    // allocation fails (no destination builder involved).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic ERROR escape promotion OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_error(interp, "generic error promotion oom");

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
            Value* promoted = promote_to_escape(src, interp);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
            boundary_promotion_context_end(interp, &ctx);

            bool ok = promoted == src &&
                      promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic ERROR escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic ERROR escape promotion OOM fails closed (same=%s tag=%d chars=%s scope_ok=%s)",
                    promoted == src ? "yes" : "no",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic STRING and ERROR escape promotion fail closed when
    // ESCAPE wrapper allocation itself fails after payload allocation.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic STRING/ERROR wrapper escape OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src_string = make_string(interp, "generic string wrapper oom");
            Value* src_error = make_error(interp, "generic error wrapper oom");
            int saved_force = g_boundary_force_alloc_escape_value_fail_nth;

            PromotionContext ctx_string = {};
            boundary_promotion_context_begin(interp, &ctx_string, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* promoted_string = promote_to_escape(src_string, interp);
            boundary_promotion_context_end(interp, &ctx_string);

            PromotionContext ctx_error = {};
            boundary_promotion_context_begin(interp, &ctx_error, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* promoted_error = promote_to_escape(src_error, interp);
            boundary_promotion_context_end(interp, &ctx_error);
            g_boundary_force_alloc_escape_value_fail_nth = saved_force;

            bool ok = promoted_string == src_string &&
                      promoted_error == src_error &&
                      promoted_string != null &&
                      promoted_error != null &&
                      promoted_string.tag == STRING &&
                      promoted_error.tag == ERROR &&
                      promoted_string.str_chars != null &&
                      promoted_error.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic STRING/ERROR wrapper escape OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: generic STRING/ERROR wrapper escape OOM fails closed (string_same=%s error_same=%s scope_ok=%s)",
                    promoted_string == src_string ? "yes" : "no",
                    promoted_error == src_error ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: generic string constructor fails closed under forced allocation
    // failure without writing through null pointers.
    {
        g_value_force_make_string_alloc_fail = true;
        Value* made = make_string(interp, "constructor oom");
        g_value_force_make_string_alloc_fail = false;

        bool ok = made != null &&
                  made.tag == ERROR &&
                  made.str_chars != null &&
                  made.str_len > 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_string OOM fails closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: make_string OOM fails closed (tag=%d chars=%s len=%d)",
                made != null ? (int)made.tag : -1,
                made != null && made.str_chars != null ? "yes" : "no",
                made != null ? (int)made.str_len : -1);
            (*fail)++;
        }
    }
}
