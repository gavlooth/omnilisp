module lisp;

import std::io;
import main;

fn Value* boundary_commit_make_temp_int_list(Interp* interp, usz len) {
    Value* acc = make_nil(interp);
    for (usz i = 0; i < len; i++) {
        acc = make_cons(interp, make_int(interp, (long)(i + 1)), acc);
    }
    return acc;
}

fn bool boundary_commit_cons_head_matches(Value* value, long expected_head) @inline {
    return value != null &&
           value.tag == CONS &&
           value.cons_val.car != null &&
           value.cons_val.car.tag == INT &&
           value.cons_val.car.int_val == expected_head;
}

fn void run_memory_lifetime_boundary_return_classification_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary return classification provides one explicit provenance class.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* foreign_scope = main::scope_create(saved_scope);
        if (target_scope == null || releasing_scope == null || foreign_scope == null) {
            if (foreign_scope != null) main::scope_release(foreign_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary return classification matrix");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* reusable = make_int(interp, 101);

            interp.current_scope = releasing_scope;
            Value* temp_owned = make_int(interp, 202);
            Value* escape_seed = make_int(interp, 303);
            Value* escape_owned = promote_to_escape(escape_seed, interp);

            interp.current_scope = foreign_scope;
            Value* mixed_uncertain = make_int(interp, 404);

            interp.current_scope = target_scope;
            interp.releasing_scope = releasing_scope;

            BoundaryReturnClassification reusable_class =
                boundary_classify_return_value(reusable, interp, target_scope, releasing_scope);
            BoundaryReturnClassification temp_class =
                boundary_classify_return_value(temp_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification escape_class =
                boundary_classify_return_value(escape_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification mixed_class =
                boundary_classify_return_value(mixed_uncertain, interp, target_scope, releasing_scope);

            bool reusable_ok =
                reusable_class.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN &&
                reusable_class.in_target_scope_chain &&
                !reusable_class.in_releasing_scope;
            bool temp_ok =
                temp_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP &&
                !temp_class.in_target_scope_chain &&
                temp_class.in_releasing_scope;
            bool escape_ok =
                escape_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                !escape_class.in_target_scope_chain &&
                escape_class.in_releasing_scope;
            bool mixed_ok =
                mixed_class.provenance == BOUNDARY_RETURN_MIXED_UNCERTAIN &&
                !mixed_class.in_target_scope_chain &&
                !mixed_class.in_releasing_scope;

            interp.current_scope = saved_scope;
            interp.releasing_scope = null;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
            main::scope_release(foreign_scope);

            bool ok = reusable_ok && temp_ok && escape_ok && mixed_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary return classification matrix");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary return classification matrix (reusable=%s temp=%s escape=%s mixed=%s tags=%s/%s/%s/%s)",
                    reusable_ok ? "yes" : "no",
                    temp_ok ? "yes" : "no",
                    escape_ok ? "yes" : "no",
                    mixed_ok ? "yes" : "no",
                    boundary_return_provenance_name(reusable_class.provenance),
                    boundary_return_provenance_name(temp_class.provenance),
                    boundary_return_provenance_name(escape_class.provenance),
                    boundary_return_provenance_name(mixed_class.provenance)
                );
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: TEMP cons survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, 17), make_nil(interp));
            usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
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
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT &&
                      site_delta == 0 &&
                      boundary_commit_cons_head_matches(committed.value, 17) &&
                      committed.value != src &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP survivor commit (outcome=%d spliced=%s reused=%s site_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: values already reusable in target chain are returned unchanged.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape already-safe reuse");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, 55), make_nil(interp));
            usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                interp,
                target_scope,
                releasing_scope
            );
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      committed.value == safe &&
                      site_delta == 0;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape already-safe reuse");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape already-safe reuse (spliced=%s reused=%s site_delta=%d same=%s)",
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)site_delta,
                    committed.value == safe ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP partial-primitive survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP partial survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = make_int(interp, 71);
            src.partial_val.second_arg = make_int(interp, 72);

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
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == PARTIAL_PRIM &&
                      committed.value.partial_val.remaining == src.partial_val.remaining &&
                      committed.value.partial_val.first_arg != null &&
                      committed.value.partial_val.second_arg != null &&
                      committed.value.partial_val.first_arg.scope_gen == target_scope.escape_generation &&
                      committed.value.partial_val.second_arg.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 2 &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP partial survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP partial survivor commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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

    // Invariant: TEMP iterator survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP iterator survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = ITERATOR;
            src.iterator_val = make_int(interp, 181);

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
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == ITERATOR &&
                      committed.value.iterator_val != null &&
                      committed.value.iterator_val.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 1 &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP iterator survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP iterator survivor commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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

    // Invariant: TEMP scalar survivor commits through direct destination ESCAPE promotion.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP scalar direct destination commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_int(interp, 777);

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
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 777 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta == 0 &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP scalar direct destination commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP scalar direct destination commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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
                      committed.value.partial_val.first_arg == escaped_first &&
                      committed.value.partial_val.second_arg == escaped_second &&
                      committed.value.partial_val.first_arg.scope_gen == target_scope.escape_generation &&
                      committed.value.partial_val.second_arg.scope_gen == target_scope.escape_generation &&
                      generic_delta == 2 &&
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
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.first_arg == escaped_first) ? "yes" : "no",
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.second_arg == escaped_second) ? "yes" : "no");
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
}
