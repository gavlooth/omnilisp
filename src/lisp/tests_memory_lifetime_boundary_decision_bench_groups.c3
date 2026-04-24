module lisp;

import std::io;
import std::core::mem;
import main;

const usz BOUNDARY_BENCH_DECISION_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER = 64;
const usz BOUNDARY_BENCH_SHAPE_ITERS = 128;
const usz BOUNDARY_BENCH_SHAPE_ITEMS = 24;
const usz EQUALITY_BENCH_ITERS = 2048;
const usz EQUALITY_BENCH_NESTED_LIST_DEPTH = 1024;
const usz EQUALITY_BENCH_ARRAY_LENGTH = 1024;

struct BoundaryBenchCounterSnapshot {
    main::ScopeMemoryTelemetryStats scope;
    main::FiberTempPoolStats fiber;
    BoundaryValueShapeStats value_shape;
    BoundaryDecisionStats decisions;
}

fn bool boundary_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_BOUNDARY_BENCH") != null;
}

fn long boundary_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn BoundaryBenchCounterSnapshot boundary_bench_counter_snapshot() @inline {
    BoundaryBenchCounterSnapshot snapshot = {};
    snapshot.scope = main::g_scope_memory_telemetry_stats;
    snapshot.fiber = main::g_fiber_temp_pool_stats;
    snapshot.value_shape = boundary_value_shape_stats_snapshot();
    snapshot.decisions = boundary_decision_stats_snapshot();
    return snapshot;
}

fn usz boundary_bench_delta(usz after, usz before) @inline {
    return after >= before ? after - before : 0;
}

fn Value* boundary_bench_make_nested_int_list(Interp* interp, usz depth, long tail_delta = 0) {
    Value* list = make_nil(interp);
    for (usz i = depth; i > 0; i--) {
        long v = (long)(i - 1);
        if (tail_delta != 0 && i == depth) v += tail_delta;
        list = make_cons(interp, make_int(interp, v), list);
    }
    return list;
}

fn Value* boundary_bench_make_int_array(Interp* interp, usz len, long tail_delta = 0) {
    Value* arr = make_array(interp, len);
    if (arr == null || arr.array_val == null) return null;
    for (usz i = 0; i < len; i++) {
        long v = (long)i;
        if (tail_delta != 0 && i + 1 == len) v += tail_delta;
        arr.array_val.items[i] = make_int(interp, v);
        arr.array_val.length++;
    }
    return arr;
}

fn Value* boundary_bench_make_mixed_nested_fixture(Interp* interp, long tail_delta = 0) {
    Value* nested_list = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH / 2, tail_delta);
    Value* nested_array = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH / 2, tail_delta);
    if (nested_list == null || nested_array == null) return null;

    Value* pair = make_cons(interp, nested_list, make_cons(interp, nested_array, make_nil(interp)));
    return make_cons(interp, pair, make_cons(interp, make_nil(interp), make_nil(interp)));
}

fn void boundary_bench_accumulate_equality_workspace(
    EqualityWorkspaceStats* total,
    EqualityWorkspaceStats sample
) @inline {
    total.stack_heap_growths += sample.stack_heap_growths;
    total.seen_heap_growths += sample.seen_heap_growths;
    total.stack_heap_bytes += sample.stack_heap_bytes;
    total.seen_heap_bytes += sample.seen_heap_bytes;
}

fn bool boundary_bench_fill_and_grow_array(Interp* interp, Value* arr, usz seed) {
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return false;
    Array* vec = arr.array_val;
    for (usz i = 0; i < vec.capacity; i++) {
        vec.items[i] = make_int(interp, (long)(seed + i));
    }
    vec.length = vec.capacity;
    Value*[2] push_args = { arr, make_int(interp, (long)(seed + vec.capacity + 1)) };
    Value* pushed = prim_array_push(push_args[0:2], null, interp);
    return pushed != null && pushed.tag != ERROR && vec.length == vec.capacity / 2 + 1;
}

fn bool boundary_bench_fill_hashmap(Interp* interp, Value* dict, usz seed) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return false;
    for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITEMS; i++) {
        if (!hashmap_set_checked(
            dict.hashmap_val,
            make_int(interp, (long)(seed + i)),
            make_int(interp, (long)(seed + i + 1000)),
            interp
        )) return false;
    }
    return dict.hashmap_val.count == BOUNDARY_BENCH_SHAPE_ITEMS;
}

fn bool boundary_bench_fill_set(Interp* interp, Value* set, usz seed) {
    if (set == null || set.tag != SET || set.hashmap_val == null) return false;
    for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITEMS; i++) {
        Value*[2] set_args = { set, make_int(interp, (long)(seed + i)) };
        Value* added = prim_set_add(set_args[0:2], null, interp);
        if (added == null || added.tag == ERROR) return false;
    }
    return set.hashmap_val.count == BOUNDARY_BENCH_SHAPE_ITEMS;
}

fn bool boundary_bench_exercise_payload_shapes(Interp* interp, usz seed) {
    (void)seed;
    Value* str = make_string(interp, "memory-boundary-telemetry-payload-shape-benchmark-string");
    Value* err = make_error(interp, "memory-boundary-telemetry-payload-shape-benchmark-error");
    Value* big = make_big_integer_from_string(interp, "12345678901234567890123456789012345678901234567890");
    usz[2] tensor_shape = { 8, 8 };
    Value* tensor = make_tensor_double_uninitialized(interp, tensor_shape[0:2]);
    void* ffi_payload = mem::malloc(32);
    Value* ffi = make_ffi_handle_ex(interp, ffi_payload, "bench-ffi-owned", null, true, false);
    return str != null && str.tag == STRING &&
           err != null && err.tag == ERROR &&
           big != null && big.tag == BIG_INTEGER &&
           tensor != null && tensor.tag == TENSOR &&
           ffi != null && ffi.tag == FFI_HANDLE;
}

fn bool boundary_bench_exercise_stable_passport(Interp* interp) {
    StableEscapeHandle handle = {};
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;

    Value* prepared_root = make_cons(interp, make_int(interp, 7701), make_nil(interp));
    if (prepared_root == null || prepared_root.tag != CONS) return false;
    if (!stable_escape_store_prepare_value(interp, prepared_root, &handle)) return false;
    prepared_root.cons_val.car = make_int(interp, 7702);
    StableGraphPassport passport = {};
    bool snapshot_ok = stable_escape_store_passport_snapshot(interp, &handle, &passport);
    bool invalidated = snapshot_ok &&
        passport.invalidation_reason == STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE;
    bool retire_ok = stable_escape_store_retire(interp, &handle);
    return invalidated && retire_ok;
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
    long equality_list_true_ms = 0;
    long equality_list_false_ms = 0;
    long equality_array_true_ms = 0;
    long equality_array_false_ms = 0;
    long equality_mixed_true_ms = 0;
    long equality_mixed_false_ms = 0;
    long shape_ms = 0;

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
    BoundaryBenchCounterSnapshot shape_before = {};
    BoundaryBenchCounterSnapshot shape_after = {};

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
            Value* dict = make_hashmap(interp, 4);
            Value* set = make_set(interp, 4);
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
        "OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters iters=%d shape_ms=%d shape_ok=%d closure_env_ok=%d stable_passport_ok=%d temp_slow_delta=%d escape_slow_delta=%d temp_slow_requested_delta=%d escape_slow_requested_delta=%d temp_selected_chunk_delta=%d escape_selected_chunk_delta=%d temp_reset_slack_delta=%d escape_reset_slack_delta=%d temp_destroy_slack_delta=%d escape_destroy_slack_delta=%d scope_fresh_delta=%d scope_recycle_hit_delta=%d array_construct_delta=%d array_growth_delta=%d hashmap_construct_delta=%d hashmap_growth_delta=%d set_construct_delta=%d set_growth_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d string_payload_bytes_delta=%d error_payload_bytes_delta=%d big_integer_wrappers_delta=%d big_integer_input_bytes_delta=%d tensor_payload_bytes_delta=%d ffi_wrappers_delta=%d ffi_releasable_delta=%d stable_stale_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
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
