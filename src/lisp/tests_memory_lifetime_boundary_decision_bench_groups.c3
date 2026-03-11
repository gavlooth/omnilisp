module lisp;

import std::io;
import main;

const usz BOUNDARY_BENCH_DECISION_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER = 64;

fn bool boundary_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_BOUNDARY_BENCH") != null;
}

fn long boundary_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn void run_memory_lifetime_boundary_benchmarks(Interp* interp) {
    if (!boundary_bench_enabled()) return;

    main::ScopeRegion* root_scope = interp.current_scope;

    usz decision_splice_ok = 0;
    usz decision_disallowed_ok = 0;
    usz decision_reuse_ok = 0;
    usz decision_destination_reuse_ok = 0;
    usz splice_tail_ok = 0;

    long decision_splice_ms = 0;
    long decision_disallowed_ms = 0;
    long decision_reuse_ms = 0;
    long decision_destination_reuse_ms = 0;
    long splice_tail_ms = 0;

    // Decision benchmark: splice-commit path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, (long)(i + 1)), make_nil(interp));

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

            bool ok = (committed.releasing_scope_spliced ||
                       committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT) &&
                      committed.value != null;
            if (ok) decision_splice_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        decision_splice_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: budget-abort hard-disallowed path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, 8);

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

            bool ok = !committed.releasing_scope_spliced &&
                      committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                      committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                      committed.value != null &&
                      committed.value.tag == ERROR;
            if (ok) decision_disallowed_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_disallowed_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: target-chain reuse path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, (long)(i + 17)), make_nil(interp));
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                interp,
                target_scope,
                releasing_scope
            );

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      committed.value == safe;
            if (ok) decision_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: destination partial build with target-escape routed args.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, (long)(i + 501)), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, (long)(i + 901)), interp);

            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = escaped_first;
            src.partial_val.second_arg = escaped_second;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* destination_partial = boundary_build_destination_partial_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = destination_partial != null &&
                      destination_partial.tag == PARTIAL_PRIM &&
                      destination_partial.scope_gen == target_scope.escape_generation &&
                      destination_partial.partial_val.first_arg == escaped_first &&
                      destination_partial.partial_val.second_arg == escaped_second;
            if (ok) decision_destination_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_destination_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Splice benchmark: focus on escape-lane concat throughput/tail consistency.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SPLICE_ITERS; i++) {
            main::ScopeRegion* parent = main::scope_create(root_scope);
            main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
            if (parent == null || child == null) {
                if (child != null) main::scope_release(child);
                if (parent != null) main::scope_release(parent);
                continue;
            }

            interp.current_scope = child;
            Value* first_escape = null;
            for (usz j = 0; j < BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER; j++) {
                Value* v = interp.alloc_value_escape();
                if (v != null) {
                    v.tag = INT;
                    v.int_val = (long)(j + 1);
                    if (j == 0) first_escape = v;
                }
            }
            interp.current_scope = root_scope;

            BoundaryScopeTransferResult ready = boundary_try_scope_splice_escapes_result(parent, child);
            bool ok = false;
            if (ready.allowed) {
                ok = first_escape != null &&
                     main::is_in_scope((void*)first_escape, parent) &&
                     main::scope_escape_chunk_tail_consistent(parent) &&
                     main::scope_escape_dtor_tail_consistent(parent);
            }
            if (ok) splice_tail_ok++;
            main::scope_release(parent);
        }
        long end_ms = boundary_bench_now_ms();
        splice_tail_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    interp.current_scope = root_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_decision_cost iters=%d splice_ms=%d disallowed_ms=%d reuse_ms=%d splice_ok=%d disallowed_ok=%d reuse_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_splice_ms,
        decision_disallowed_ms,
        decision_reuse_ms,
        (long)decision_splice_ok,
        (long)decision_disallowed_ok,
        (long)decision_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape iters=%d partial_ms=%d partial_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_destination_reuse_ms,
        (long)decision_destination_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=scope_splice_tail iters=%d escapes_per_iter=%d splice_ms=%d splice_ok=%d",
        (long)BOUNDARY_BENCH_SPLICE_ITERS,
        (long)BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER,
        splice_tail_ms,
        (long)splice_tail_ok
    );
}
