module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_commit_escape_primary_tests(interp, pass, fail);

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
}
