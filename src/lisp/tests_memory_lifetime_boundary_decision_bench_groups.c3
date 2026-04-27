module lisp;

import std::io;
import std::core::mem;
import main;

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
    long equality_list_true_ms = 0;
    long equality_list_false_ms = 0;
    long equality_array_true_ms = 0;
    long equality_array_false_ms = 0;
    long equality_mixed_true_ms = 0;
    long equality_mixed_false_ms = 0;
    long shape_ms = 0;
    long product_ms = 0;
    long closure_iter_ms = 0;
    long tensor_metadata_ms = 0;
    long nested_module_ms = 0;

    usz equality_list_true_ok = 0;
    usz equality_list_false_ok = 0;
    usz equality_array_true_ok = 0;
    usz equality_array_false_ok = 0;
    usz equality_mixed_true_ok = 0;
    usz equality_mixed_false_ok = 0;
    bool equality_fixture_ok = false;
    EqualityWorkspaceStats equality_workspace = {};
    usz shape_ok = 0;
    usz stable_passport_ok = 0;
    usz closure_env_ok = 0;
    usz product_ok = 0;
    usz product_commit_ok = 0;
    usz closure_iter_ok = 0;
    usz closure_iter_count_total = 0;
    long closure_iter_sum_total = 0;
    usz tensor_metadata_ok = 0;
    usz tensor_metadata_commit_ok = 0;
    usz nested_module_setup_ok = 0;
    usz nested_module_batch_ok = 0;
    BoundaryBenchCounterSnapshot shape_before = {};
    BoundaryBenchCounterSnapshot shape_after = {};
    BoundaryBenchCounterSnapshot product_before = {};
    BoundaryBenchCounterSnapshot product_after = {};
    BoundaryBenchCounterSnapshot closure_iter_before = {};
    BoundaryBenchCounterSnapshot closure_iter_after = {};
    BoundaryBenchCounterSnapshot tensor_metadata_before = {};
    BoundaryBenchCounterSnapshot tensor_metadata_after = {};
    BoundaryBenchCounterSnapshot nested_module_before = {};
    BoundaryBenchCounterSnapshot nested_module_after = {};

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
                      destination_partial.partial_val.first_arg.scope_gen == target_scope.escape_generation &&
                      destination_partial.partial_val.second_arg.scope_gen == target_scope.escape_generation;
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

    // Counter-shape benchmark: exercise the allocator/value-shape fields added
    // for the telemetry benchmark lane.
    {
        shape_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            main::ScopeRegion* reset_scope = main::scope_create(root_scope);
            if (target_scope == null || releasing_scope == null || reset_scope == null) {
                if (reset_scope != null) main::scope_release(reset_scope);
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = reset_scope;
            void* reset_temp = reset_scope.alloc(96);
            void* reset_escape = reset_scope.alloc_escape(96);
            (void)reset_temp;
            (void)reset_escape;
            main::scope_reset(reset_scope);
            main::scope_release(reset_scope);

            interp.current_scope = releasing_scope;
            void* slow_temp = releasing_scope.alloc(8192 + i);
            void* slow_escape = releasing_scope.alloc_escape(4096 + i);
            (void)slow_temp;
            (void)slow_escape;

            Value* arr = make_array(interp, 1);
            Value* dict = make_hashmap_for_entry_count_checked(
                interp,
                BOUNDARY_BENCH_SHAPE_ITEMS,
                "boundary bench: dictionary out of memory");
            Value* set = make_set_for_entry_count_checked(
                interp,
                BOUNDARY_BENCH_SHAPE_ITEMS,
                "boundary bench: set out of memory");
            bool collection_ok =
                boundary_bench_fill_and_grow_array(interp, arr, i * 100) &&
                boundary_bench_fill_hashmap(interp, dict, i * 1000) &&
                boundary_bench_fill_set(interp, set, i * 2000);
            bool payload_ok = boundary_bench_exercise_payload_shapes(interp, i);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                arr,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = committed.value != null && committed.value.tag == ARRAY;
            if (collection_ok && payload_ok && commit_ok) shape_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }

        for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITERS / 4; i++) {
            EvalResult r = run("(let (x 41 y 1) (lambda (z) (+ x y z)))", interp);
            if (!r.error.has_error && r.value != null && r.value.tag == CLOSURE) closure_env_ok++;
        }
        if (boundary_bench_exercise_stable_passport(interp)) stable_passport_ok++;
        long end_ms = boundary_bench_now_ms();
        shape_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        shape_after = boundary_bench_counter_snapshot();
    }

    // Product-style benchmark: route-shaped Finwatch portfolio/quote payloads.
    {
        product_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_PRODUCT_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* response = boundary_bench_make_finwatch_response(interp, i);
            bool fixture_ok = response != null && response.tag == HASHMAP;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = fixture_ok
                ? boundary_commit_escape(response, interp, target_scope, releasing_scope, &ctx)
                : (BoundaryCommitEscapeResult){};
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = committed.value != null && committed.value.tag == HASHMAP;
            if (fixture_ok) product_ok++;
            if (commit_ok) product_commit_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        product_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        product_after = boundary_bench_counter_snapshot();
    }

    // Closure-heavy iterator pipeline: return a lazy iterator with captured
    // mapping/filter closures across the top-level boundary, then consume it.
    {
        closure_iter_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_CLOSURE_ITER_ITERS; i++) {
            usz count = 0;
            long sum = 0;
            bool ok = boundary_bench_closure_iterator_pipeline_once(interp, &count, &sum) &&
                      count == BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_COUNT &&
                      sum == BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_SUM;
            if (ok) {
                closure_iter_ok++;
                closure_iter_count_total += count;
                closure_iter_sum_total += sum;
            }
        }
        long end_ms = boundary_bench_now_ms();
        closure_iter_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        closure_iter_after = boundary_bench_counter_snapshot();
    }

    // Tensor metadata crossing: commit tensor-heavy graph payloads with shared
    // layout dictionaries so tensor storage and graph sharing stay visible.
    {
        tensor_metadata_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_TENSOR_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* response = boundary_bench_make_tensor_metadata_response(interp, i);
            bool fixture_ok = boundary_bench_tensor_metadata_response_ok(response, interp);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = fixture_ok
                ? boundary_commit_escape(response, interp, target_scope, releasing_scope, &ctx)
                : (BoundaryCommitEscapeResult){};
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = boundary_bench_tensor_metadata_response_ok(committed.value, interp);
            if (fixture_ok) tensor_metadata_ok++;
            if (commit_ok) tensor_metadata_commit_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        tensor_metadata_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        tensor_metadata_after = boundary_bench_counter_snapshot();
    }

    // Nested module returns: repeatedly return graph payloads produced inside
    // nested module bodies across the boundary.
    {
        bool setup_ok = boundary_bench_setup_nested_module_return_fixture(interp);
        if (setup_ok) nested_module_setup_ok = 1;
        nested_module_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        if (setup_ok) {
            for (usz i = 0; i < BOUNDARY_BENCH_NESTED_MODULE_ITERS; i++) {
                EvalResult batch_result = run("(mem006-return-outer.make-batch 5)", interp);
                if (!batch_result.error.has_error &&
                    boundary_bench_nested_module_batch_ok(batch_result.value, BOUNDARY_BENCH_NESTED_MODULE_INPUT)) {
                    nested_module_batch_ok++;
                }
            }
        }
        long end_ms = boundary_bench_now_ms();
        nested_module_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        nested_module_after = boundary_bench_counter_snapshot();
    }

    // Structural equality benchmark: measure large nested equality throughput
    // and workspace heap-growth churn.
    {
        interp.current_scope = root_scope;
        Value* list_left = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH);
        Value* list_right = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH);
        Value* list_mismatch = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH, 1);

        Value* array_left = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH);
        Value* array_right = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH);
        Value* array_mismatch = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH, 1);
        Value* mixed_left = boundary_bench_make_mixed_nested_fixture(interp);
        Value* mixed_right = boundary_bench_make_mixed_nested_fixture(interp);
        Value* mixed_mismatch = boundary_bench_make_mixed_nested_fixture(interp, 1);

        equality_fixture_ok = list_left != null &&
            list_right != null &&
            list_mismatch != null &&
            array_left != null &&
            array_right != null &&
            array_mismatch != null &&
            mixed_left != null &&
            mixed_right != null &&
            mixed_mismatch != null;

        if (equality_fixture_ok) {
            long start_ms = boundary_bench_now_ms();
            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(list_left, list_right, &stats);
                if (eq) equality_list_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long list_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(list_left, list_mismatch, &stats);
                if (!eq) equality_list_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long list_false_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(array_left, array_right, &stats);
                if (eq) equality_array_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long array_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(array_left, array_mismatch, &stats);
                if (!eq) equality_array_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long mixed_true_start_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(mixed_left, mixed_right, &stats);
                if (eq) equality_mixed_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long mixed_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(mixed_left, mixed_mismatch, &stats);
                if (!eq) equality_mixed_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long end_ms = boundary_bench_now_ms();

            equality_list_true_ms = list_true_done_ms > start_ms ? list_true_done_ms - start_ms : 0;
            equality_list_false_ms = list_false_done_ms > list_true_done_ms ? list_false_done_ms - list_true_done_ms : 0;
            equality_array_true_ms = array_true_done_ms > list_false_done_ms ? array_true_done_ms - list_false_done_ms : 0;
            equality_array_false_ms = mixed_true_start_ms > array_true_done_ms ? mixed_true_start_ms - array_true_done_ms : 0;
            equality_mixed_true_ms = mixed_true_done_ms > mixed_true_start_ms ? mixed_true_done_ms - mixed_true_start_ms : 0;
            equality_mixed_false_ms = end_ms > mixed_true_done_ms ? end_ms - mixed_true_done_ms : 0;
        }
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
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=equality_nested_workspace iters=%d fixture_ok=%d list_depth=%d array_len=%d list_true_ms=%d list_true_ok=%d list_false_ms=%d list_false_ok=%d array_true_ms=%d array_true_ok=%d array_false_ms=%d array_false_ok=%d mixed_true_ms=%d mixed_true_ok=%d mixed_false_ms=%d mixed_false_ok=%d stack_growth=%d seen_growth=%d stack_bytes=%d seen_bytes=%d",
        (long)EQUALITY_BENCH_ITERS,
        equality_fixture_ok ? 1 : 0,
        (long)EQUALITY_BENCH_NESTED_LIST_DEPTH,
        (long)EQUALITY_BENCH_ARRAY_LENGTH,
        equality_list_true_ms,
        (long)equality_list_true_ok,
        equality_list_false_ms,
        (long)equality_list_false_ok,
        equality_array_true_ms,
        (long)equality_array_true_ok,
        equality_array_false_ms,
        (long)equality_array_false_ok,
        equality_mixed_true_ms,
        (long)equality_mixed_true_ok,
        equality_mixed_false_ms,
        (long)equality_mixed_false_ok,
        (long)equality_workspace.stack_heap_growths,
        (long)equality_workspace.seen_heap_growths,
        (long)equality_workspace.stack_heap_bytes,
        (long)equality_workspace.seen_heap_bytes
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=finwatch_product_memory iters=%d quotes_per_iter=%d holdings_per_iter=%d product_ms=%d product_ok=%d commit_ok=%d hashmap_construct_delta=%d set_construct_delta=%d array_construct_delta=%d string_payload_bytes_delta=%d ffi_wrappers_delta=%d ffi_releasable_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_PRODUCT_ITERS,
        (long)BOUNDARY_BENCH_PRODUCT_QUOTES,
        (long)BOUNDARY_BENCH_PRODUCT_HOLDINGS,
        product_ms,
        (long)product_ok,
        (long)product_commit_ok,
        (long)boundary_bench_delta(product_after.value_shape.hashmap_construct_count, product_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.set_construct_count, product_before.value_shape.set_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.array_construct_count, product_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.string_payload_bytes_total, product_before.value_shape.string_payload_bytes_total),
        (long)boundary_bench_delta(product_after.value_shape.ffi_handle_wrapper_count, product_before.value_shape.ffi_handle_wrapper_count),
        (long)boundary_bench_delta(product_after.value_shape.ffi_handle_releasable_count, product_before.value_shape.ffi_handle_releasable_count),
        (long)boundary_bench_delta(
            product_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            product_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(product_after.decisions.stable_materialization_copy_bytes_total, product_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=closure_iterator_pipeline_memory iters=%d expected_count=%d expected_sum=%d pipeline_ms=%d pipeline_ok=%d count_total=%d sum_total=%d iterator_roots_delta=%d partial_roots_delta=%d closure_roots_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_CLOSURE_ITER_ITERS,
        (long)BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_COUNT,
        BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_SUM,
        closure_iter_ms,
        (long)closure_iter_ok,
        (long)closure_iter_count_total,
        closure_iter_sum_total,
        (long)boundary_bench_delta(closure_iter_after.escape_shape.iterator_roots, closure_iter_before.escape_shape.iterator_roots),
        (long)boundary_bench_delta(closure_iter_after.escape_shape.partial_roots, closure_iter_before.escape_shape.partial_roots),
        (long)boundary_bench_delta(closure_iter_after.escape_shape.closure_roots, closure_iter_before.escape_shape.closure_roots),
        (long)boundary_bench_delta(closure_iter_after.value_shape.closure_env_copy_frame_count, closure_iter_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(closure_iter_after.value_shape.closure_env_copy_binding_count_total, closure_iter_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(
            closure_iter_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            closure_iter_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(closure_iter_after.decisions.stable_materialization_copy_bytes_total, closure_iter_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=tensor_metadata_crossing_memory iters=%d tensors_per_iter=%d tensor_ms=%d tensor_ok=%d commit_ok=%d tensor_roots_delta=%d tensor_payload_bytes_delta=%d hashmap_construct_delta=%d array_construct_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_TENSOR_ITERS,
        (long)BOUNDARY_BENCH_TENSORS_PER_ITER,
        tensor_metadata_ms,
        (long)tensor_metadata_ok,
        (long)tensor_metadata_commit_ok,
        (long)boundary_bench_delta(tensor_metadata_after.escape_shape.tensor_roots, tensor_metadata_before.escape_shape.tensor_roots),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.tensor_payload_bytes_total, tensor_metadata_before.value_shape.tensor_payload_bytes_total),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.hashmap_construct_count, tensor_metadata_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.array_construct_count, tensor_metadata_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(
            tensor_metadata_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            tensor_metadata_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(tensor_metadata_after.decisions.stable_materialization_copy_bytes_total, tensor_metadata_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=nested_module_return_memory iters=%d nested_module_ms=%d setup_ok=%d batch_ok=%d cons_roots_delta=%d array_construct_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d selected_transplant_delta=%d selected_stable_materialize_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_NESTED_MODULE_ITERS,
        nested_module_ms,
        (long)nested_module_setup_ok,
        (long)nested_module_batch_ok,
        (long)boundary_bench_delta(nested_module_after.escape_shape.cons_roots, nested_module_before.escape_shape.cons_roots),
        (long)boundary_bench_delta(nested_module_after.value_shape.array_construct_count, nested_module_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(nested_module_after.value_shape.closure_env_copy_frame_count, nested_module_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(nested_module_after.value_shape.closure_env_copy_binding_count_total, nested_module_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(
            nested_module_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            nested_module_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(
            nested_module_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
            nested_module_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION]
        ),
        (long)boundary_bench_delta(nested_module_after.decisions.stable_materialization_copy_bytes_total, nested_module_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters iters=%d shape_ms=%d shape_ok=%d closure_env_ok=%d stable_passport_ok=%d temp_slow_delta=%d escape_slow_delta=%d temp_slow_requested_delta=%d escape_slow_requested_delta=%d temp_selected_chunk_delta=%d escape_selected_chunk_delta=%d temp_slow_slack_exact_delta=%d temp_slow_slack_le512_delta=%d temp_slow_slack_le4096_delta=%d temp_slow_slack_gt4096_delta=%d escape_slow_slack_exact_delta=%d escape_slow_slack_le512_delta=%d escape_slow_slack_le4096_delta=%d escape_slow_slack_gt4096_delta=%d temp_slow_sequence_closed_delta=%d escape_slow_sequence_closed_delta=%d temp_slow_sequence_followup_alloc_delta=%d escape_slow_sequence_followup_alloc_delta=%d temp_slow_sequence_followup_bytes_delta=%d escape_slow_sequence_followup_bytes_delta=%d temp_slow_sequence_unused_close_delta=%d escape_slow_sequence_unused_close_delta=%d temp_slow_sequence_no_followup_delta=%d escape_slow_sequence_no_followup_delta=%d escape_slow_sequence_no_followup_request_le512_delta=%d escape_slow_sequence_no_followup_request_le4096_delta=%d escape_slow_sequence_no_followup_request_gt4096_delta=%d escape_slow_sequence_no_followup_unused_exact_delta=%d escape_slow_sequence_no_followup_unused_le512_delta=%d escape_slow_sequence_no_followup_unused_le4096_delta=%d escape_slow_sequence_no_followup_unused_gt4096_delta=%d escape_slow_sequence_no_followup_source_unknown_delta=%d escape_slow_sequence_no_followup_source_direct_delta=%d escape_slow_sequence_no_followup_source_dtor_delta=%d escape_slow_sequence_no_followup_source_interp_value_delta=%d escape_slow_sequence_no_followup_source_interp_env_delta=%d escape_slow_sequence_no_followup_source_boundary_payload_delta=%d escape_slow_sequence_no_followup_source_promotion_signature_delta=%d escape_slow_sequence_no_followup_source_promotion_closure_delta=%d escape_slow_sequence_no_followup_source_jit_args_delta=%d temp_slow_sequence_large_delta=%d escape_slow_sequence_large_delta=%d temp_slow_sequence_large_followup_bytes_delta=%d escape_slow_sequence_large_followup_bytes_delta=%d temp_slow_sequence_large_unused_close_delta=%d escape_slow_sequence_large_unused_close_delta=%d temp_slow_sequence_large_no_followup_delta=%d escape_slow_sequence_large_no_followup_delta=%d temp_reset_slack_delta=%d escape_reset_slack_delta=%d temp_destroy_slack_delta=%d escape_destroy_slack_delta=%d scope_fresh_delta=%d scope_recycle_hit_delta=%d array_construct_delta=%d array_growth_delta=%d hashmap_construct_delta=%d hashmap_growth_delta=%d set_construct_delta=%d set_growth_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d string_payload_bytes_delta=%d error_payload_bytes_delta=%d big_integer_wrappers_delta=%d big_integer_input_bytes_delta=%d tensor_payload_bytes_delta=%d ffi_wrappers_delta=%d ffi_releasable_delta=%d stable_stale_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_SHAPE_ITERS,
        shape_ms,
        (long)shape_ok,
        (long)closure_env_ok,
        (long)stable_passport_ok,
        (long)boundary_bench_delta(shape_after.scope.temp_slow_alloc_count, shape_before.scope.temp_slow_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_alloc_count, shape_before.scope.escape_slow_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_requested_bytes, shape_before.scope.temp_slow_requested_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_requested_bytes, shape_before.scope.escape_slow_requested_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_selected_chunk_bytes, shape_before.scope.temp_slow_selected_chunk_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_selected_chunk_bytes, shape_before.scope.escape_slow_selected_chunk_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_exact_count, shape_before.scope.temp_slow_slack_exact_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_le_512_count, shape_before.scope.temp_slow_slack_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_le_4096_count, shape_before.scope.temp_slow_slack_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_gt_4096_count, shape_before.scope.temp_slow_slack_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_exact_count, shape_before.scope.escape_slow_slack_exact_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_le_512_count, shape_before.scope.escape_slow_slack_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_le_4096_count, shape_before.scope.escape_slow_slack_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_gt_4096_count, shape_before.scope.escape_slow_slack_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_closed_count, shape_before.scope.temp_slow_sequence_closed_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_closed_count, shape_before.scope.escape_slow_sequence_closed_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_followup_alloc_count, shape_before.scope.temp_slow_sequence_followup_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_followup_alloc_count, shape_before.scope.escape_slow_sequence_followup_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_followup_bytes, shape_before.scope.temp_slow_sequence_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_followup_bytes, shape_before.scope.escape_slow_sequence_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_unused_at_close_bytes, shape_before.scope.temp_slow_sequence_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_unused_at_close_bytes, shape_before.scope.escape_slow_sequence_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_no_followup_count, shape_before.scope.temp_slow_sequence_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_count, shape_before.scope.escape_slow_sequence_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_le_512_count, shape_before.scope.escape_slow_sequence_no_followup_request_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_le_4096_count, shape_before.scope.escape_slow_sequence_no_followup_request_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_gt_4096_count, shape_before.scope.escape_slow_sequence_no_followup_request_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_exact_count, shape_before.scope.escape_slow_sequence_no_followup_unused_exact_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_le_512_count, shape_before.scope.escape_slow_sequence_no_followup_unused_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_le_4096_count, shape_before.scope.escape_slow_sequence_no_followup_unused_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_gt_4096_count, shape_before.scope.escape_slow_sequence_no_followup_unused_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_unknown_count, shape_before.scope.escape_slow_sequence_no_followup_source_unknown_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_direct_count, shape_before.scope.escape_slow_sequence_no_followup_source_direct_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_dtor_count, shape_before.scope.escape_slow_sequence_no_followup_source_dtor_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_interp_value_count, shape_before.scope.escape_slow_sequence_no_followup_source_interp_value_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_interp_env_count, shape_before.scope.escape_slow_sequence_no_followup_source_interp_env_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_boundary_payload_count, shape_before.scope.escape_slow_sequence_no_followup_source_boundary_payload_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_promotion_signature_count, shape_before.scope.escape_slow_sequence_no_followup_source_promotion_signature_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_promotion_closure_count, shape_before.scope.escape_slow_sequence_no_followup_source_promotion_closure_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_jit_args_count, shape_before.scope.escape_slow_sequence_no_followup_source_jit_args_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_count, shape_before.scope.temp_slow_sequence_large_slack_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_count, shape_before.scope.escape_slow_sequence_large_slack_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_followup_bytes, shape_before.scope.temp_slow_sequence_large_slack_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_followup_bytes, shape_before.scope.escape_slow_sequence_large_slack_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_unused_at_close_bytes, shape_before.scope.temp_slow_sequence_large_slack_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_unused_at_close_bytes, shape_before.scope.escape_slow_sequence_large_slack_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_no_followup_count, shape_before.scope.temp_slow_sequence_large_slack_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_no_followup_count, shape_before.scope.escape_slow_sequence_large_slack_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.temp_unused_bytes_at_reset, shape_before.scope.temp_unused_bytes_at_reset),
        (long)boundary_bench_delta(shape_after.scope.escape_unused_bytes_at_reset, shape_before.scope.escape_unused_bytes_at_reset),
        (long)boundary_bench_delta(shape_after.scope.temp_unused_bytes_at_destroy, shape_before.scope.temp_unused_bytes_at_destroy),
        (long)boundary_bench_delta(shape_after.scope.escape_unused_bytes_at_destroy, shape_before.scope.escape_unused_bytes_at_destroy),
        (long)boundary_bench_delta(shape_after.scope.scope_fresh_alloc_count, shape_before.scope.scope_fresh_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.scope_recycle_hit_count, shape_before.scope.scope_recycle_hit_count),
        (long)boundary_bench_delta(shape_after.value_shape.array_construct_count, shape_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.array_growth_count, shape_before.value_shape.array_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.hashmap_construct_count, shape_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.hashmap_growth_count, shape_before.value_shape.hashmap_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.set_construct_count, shape_before.value_shape.set_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.set_growth_count, shape_before.value_shape.set_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.closure_env_copy_frame_count, shape_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(shape_after.value_shape.closure_env_copy_binding_count_total, shape_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(shape_after.value_shape.string_payload_bytes_total, shape_before.value_shape.string_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.error_payload_bytes_total, shape_before.value_shape.error_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.big_integer_wrapper_count, shape_before.value_shape.big_integer_wrapper_count),
        (long)boundary_bench_delta(shape_after.value_shape.big_integer_decimal_input_bytes_total, shape_before.value_shape.big_integer_decimal_input_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.tensor_payload_bytes_total, shape_before.value_shape.tensor_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.ffi_handle_wrapper_count, shape_before.value_shape.ffi_handle_wrapper_count),
        (long)boundary_bench_delta(shape_after.value_shape.ffi_handle_releasable_count, shape_before.value_shape.ffi_handle_releasable_count),
        (long)boundary_bench_delta(shape_after.value_shape.stable_passport_prepared_graph_stale_count, shape_before.value_shape.stable_passport_prepared_graph_stale_count),
        (long)boundary_bench_delta(
            shape_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            shape_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(shape_after.decisions.stable_materialization_copy_bytes_total, shape_before.decisions.stable_materialization_copy_bytes_total)
    );
}
