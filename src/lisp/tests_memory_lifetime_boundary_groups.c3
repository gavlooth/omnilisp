module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_boundary_transition_regression_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: return/env/splice boundary transitions produce deterministic outcomes.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary transition regression pack");
            (*fail)++;
            return;
        }

        // Return transition: budget-0 commit must deterministically hard-fail with promotion-aborted fault.
        interp.current_scope = releasing_scope;
        Value* return_src = make_cons(interp, make_int(interp, 11), make_nil(interp));
        PromotionContext return_ctx = {};
        boundary_promotion_context_begin(interp, &return_ctx, 0);
        BoundaryCommitEscapeResult committed = boundary_commit_escape(
            return_src,
            interp,
            target_scope,
            releasing_scope,
            &return_ctx
        );
        boundary_promotion_context_end(interp, &return_ctx);
        bool return_ok = committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                         committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                         committed.value != null &&
                         committed.value.tag == ERROR;

        // Env transition: deep env chains copy successfully without recursion limit.
        interp.current_scope = releasing_scope;
        Env* env_src = make_env(interp, interp.global_env);
        SymbolId env_sym = interp.symbols.intern("__boundary_transition_env");
        env_define_with_barrier(env_src, env_sym, make_int(interp, 22), interp);
        BoundaryEnvCopyResult env_copied = boundary_copy_env_to_target_scope_checked(
            env_src,
            interp,
            target_scope,
            256
        );
        bool env_ok = boundary_env_copy_result_ok(env_copied) &&
                      env_copied.env != null;

        // Splice transition: non-unique child refcount must deterministically reject with explicit reason.
        main::scope_retain(releasing_scope);
        BoundaryScopeTransferResult splice_result =
            boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
        bool splice_ok = !splice_result.allowed &&
                         splice_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        main::scope_release(releasing_scope);

        interp.current_scope = saved_scope;
        main::scope_release(releasing_scope);
        main::scope_release(target_scope);

        bool ok = return_ok && env_ok && splice_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary transition regression pack");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary transition regression pack (return_ok=%s env_ok=%s splice_ok=%s env_fault=%s splice_reason=%s commit_fault=%s)",
                return_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                splice_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(env_copied.fault_code),
                (ZString)boundary_scope_transfer_reason_name(splice_result.reason),
                (ZString)boundary_commit_fault_name(committed.fault_code)
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        Value* root = null;
        Value* leaf = null;
        Value* tail = null;
        bool setup_ok = target_scope != null && releasing_scope != null;
        ulong target_escape_generation = target_scope != null ? target_scope.escape_generation : 0;

        if (setup_ok) {
            interp.current_scope = releasing_scope;
            root = interp.alloc_value_escape();
            leaf = interp.alloc_value_escape();
            tail = interp.alloc_value_escape();
            setup_ok = root != null && leaf != null && tail != null;
            if (setup_ok) {
                leaf.tag = INT;
                leaf.int_val = 42;
                tail.tag = NIL;
                root.tag = CONS;
                root.cons_val.car = leaf;
                root.cons_val.cdr = tail;
            }
            interp.current_scope = saved_scope;
        }

        BoundaryScopeTransferResult splice_result = setup_ok
            ? boundary_try_scope_splice_escapes_root_result(target_scope, releasing_scope, root)
            : (BoundaryScopeTransferResult){};
        bool splice_allowed = setup_ok && splice_result.allowed;
        bool root_adopted = splice_allowed && main::scope_contains(target_scope, (void*)root);
        bool leaf_adopted = splice_allowed && main::scope_contains(target_scope, (void*)leaf);
        bool tail_adopted = splice_allowed && main::scope_contains(target_scope, (void*)tail);
        bool root_stamp_ok = splice_allowed && root.scope_gen == target_escape_generation;
        bool leaf_stamp_ok = splice_allowed && leaf.scope_gen == target_escape_generation;
        bool tail_stamp_ok = splice_allowed && tail.scope_gen == target_escape_generation;
        bool ok = splice_allowed &&
                  root_adopted &&
                  leaf_adopted &&
                  tail_adopted &&
                  root_stamp_ok &&
                  leaf_stamp_ok &&
                  tail_stamp_ok;

        interp.current_scope = saved_scope;
        if (!splice_allowed && releasing_scope != null) main::scope_release(releasing_scope);
        if (target_scope != null) main::scope_release(target_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: root splice rewrites escape generation stamps");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: root splice rewrites escape generation stamps (setup_ok=%s splice_allowed=%s reason=%s root_adopted=%s leaf_adopted=%s tail_adopted=%s root_stamp=%s leaf_stamp=%s tail_stamp=%s)",
                setup_ok ? "yes" : "no",
                splice_allowed ? "yes" : "no",
                (ZString)boundary_scope_transfer_reason_name(splice_result.reason),
                root_adopted ? "yes" : "no",
                leaf_adopted ? "yes" : "no",
                tail_adopted ? "yes" : "no",
                root_stamp_ok ? "yes" : "no",
                leaf_stamp_ok ? "yes" : "no",
                tail_stamp_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        reset_memory_promotion_stats();

        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary escape shape telemetry baseline");
            (*fail)++;
            return;
        }

        interp.current_scope = target_scope;
        interp.releasing_scope = releasing_scope;
        Value* cons_root = make_cons(
            interp,
            make_int(interp, 101),
            make_cons(interp, make_int(interp, 202), make_nil(interp))
        );
        Value* array_root = make_array(interp, 2);
        Value* scalar_root = make_int(interp, 303);
        bool setup_ok = cons_root != null &&
                        array_root != null &&
                        array_root.array_val != null &&
                        scalar_root != null;
        if (setup_ok) {
            array_root.array_val.items[0] = make_int(interp, 404);
            array_root.array_val.items[1] = make_int(interp, 505);
            array_root.array_val.length = 2;
        }

        Value* cons_escaped = setup_ok ? promote_to_escape(cons_root, interp) : null;
        Value* array_escaped = setup_ok ? promote_to_escape(array_root, interp) : null;
        Value* scalar_escaped = setup_ok ? promote_to_escape(scalar_root, interp) : null;
        BoundaryEscapeShapeStats shape_stats = boundary_escape_shape_stats_snapshot();

        bool ok = setup_ok &&
                  cons_escaped != null &&
                  array_escaped != null &&
                  scalar_escaped != null &&
                  shape_stats.candidate_roots >= 3 &&
                  shape_stats.graph_roots >= 2 &&
                  shape_stats.cons_roots >= 1 &&
                  shape_stats.cons_spine_samples >= 1 &&
                  shape_stats.cons_spine_peak_len >= 2 &&
                  shape_stats.array_roots >= 1;

        interp.current_scope = saved_scope;
        interp.releasing_scope = saved_releasing;
        main::scope_release(releasing_scope);
        main::scope_release(target_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary escape shape telemetry baseline");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary escape shape telemetry baseline (setup_ok=%s candidate_roots=%d graph_roots=%d cons_roots=%d array_roots=%d cons_samples=%d cons_peak=%d)",
                setup_ok ? "yes" : "no",
                (long)shape_stats.candidate_roots,
                (long)shape_stats.graph_roots,
                (long)shape_stats.cons_roots,
                (long)shape_stats.array_roots,
                (long)shape_stats.cons_spine_samples,
                (long)shape_stats.cons_spine_peak_len
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        BoundaryRuntimePolicy saved_policy = g_boundary_runtime_policy;
        g_boundary_runtime_policy.initialized = true;
        g_boundary_runtime_policy.graph_audit = true;
        g_boundary_runtime_policy.graph_audit_rate = 1;
        g_boundary_runtime_policy.graph_audit_max_roots = 0;

        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        Value* root = null;
        Value* leaf = null;
        Value* tail = null;
        bool setup_ok = target_scope != null && releasing_scope != null;

        if (setup_ok) {
            interp.current_scope = releasing_scope;
            leaf = make_int(interp, 43);
            root = interp.alloc_value_escape();
            tail = interp.alloc_value_escape();
            setup_ok = root != null && leaf != null && tail != null;
            if (setup_ok) {
                tail.tag = NIL;
                root.tag = CONS;
                root.cons_val.car = leaf;
                root.cons_val.cdr = tail;
            }
            interp.current_scope = saved_scope;
        }

        BoundaryTransplantProof proof = setup_ok
            ? boundary_build_transplant_root_proof(target_scope, releasing_scope, root, interp.global_env)
            : (BoundaryTransplantProof){};
        BoundaryScopeTransferResult splice_result = setup_ok
            ? boundary_try_scope_splice_escapes_root_result(target_scope, releasing_scope, root, interp.global_env)
            : (BoundaryScopeTransferResult){};
        bool ok = setup_ok &&
                  !proof.transfer.allowed &&
                  proof.transfer.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID &&
                  proof.root_graph_required &&
                  proof.root_non_null &&
                  proof.root_owned_by_child_escape &&
                  proof.source_escape_graph_safe &&
                  !proof.reachable_temp_edges_safe &&
                  !proof.closure_env_edges_safe &&
                  proof.stamp_rewrite_required &&
                  proof.stamp_rewrite_safe &&
                  !splice_result.allowed &&
                  splice_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID &&
                  root != null &&
                  root.scope_gen == releasing_scope.escape_generation &&
                  leaf != null &&
                  leaf.scope_gen == releasing_scope.generation &&
                  main::scope_contains(releasing_scope, (void*)root) &&
                  main::scope_contains(releasing_scope, (void*)leaf);

        g_boundary_runtime_policy = saved_policy;
        interp.current_scope = saved_scope;
        if (releasing_scope != null) main::scope_release(releasing_scope);
        if (target_scope != null) main::scope_release(target_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: root splice debug audit rejects releasing temp edge");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: root splice debug audit rejects releasing temp edge (setup_ok=%s allowed=%s reason=%s)",
                setup_ok ? "yes" : "no",
                splice_result.allowed ? "yes" : "no",
                (ZString)boundary_scope_transfer_reason_name(splice_result.reason)
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        bool setup_ok = target_scope != null;
        StableEscapeHandle handle = {};
        Value* resolved = null;
        Value* promoted = null;

        if (setup_ok) {
            interp.current_scope = target_scope;
            Value* seed = make_cons(
                interp,
                make_int(interp, 606),
                make_cons(interp, make_int(interp, 707), make_nil(interp))
            );
            promoted = seed != null ? promote_to_escape(seed, interp) : null;
            setup_ok = promoted != null;
            if (setup_ok) {
                setup_ok = stable_escape_store_publish_value(interp, promoted, &handle);
                resolved = setup_ok ? stable_escape_store_resolve_value(interp, &handle) : null;
            }
            interp.current_scope = saved_scope;
        }

        bool handle_live = setup_ok && stable_escape_store_handle_is_alive(interp, &handle);
        bool resolve_ok = setup_ok && resolved == promoted;
        bool survive_scope_release_ok = false;

        bool retire_ok = false;
        bool stale_ok = false;
        if (setup_ok) {
            interp.current_scope = saved_scope;
            main::scope_release(target_scope);
            target_scope = null;
            survive_scope_release_ok =
                stable_escape_store_handle_is_alive(interp, &handle) &&
                stable_escape_store_resolve_value(interp, &handle) == promoted;
            retire_ok = stable_escape_store_retire(interp, &handle);
            stale_ok = stable_escape_store_resolve_value(interp, &handle) == null;
        }

        interp.current_scope = saved_scope;
        if (target_scope != null) main::scope_release(target_scope);

        bool ok = setup_ok &&
                  handle_live &&
                  resolve_ok &&
                  survive_scope_release_ok &&
                  retire_ok &&
                  stale_ok &&
                  handle.kind == STABLE_ESCAPE_HANDLE_ROOT_VALUE &&
                  handle.slot < STABLE_ESCAPE_STORE_CAPACITY &&
                  handle.generation != 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store publish and stale-handle retirement");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store publish and stale-handle retirement (setup_ok=%s handle_live=%s resolve_ok=%s survive_scope_release_ok=%s retire_ok=%s stale_ok=%s kind=%d slot=%d generation=%d)",
                setup_ok ? "yes" : "no",
                handle_live ? "yes" : "no",
                resolve_ok ? "yes" : "no",
                survive_scope_release_ok ? "yes" : "no",
                retire_ok ? "yes" : "no",
                stale_ok ? "yes" : "no",
                (int)handle.kind,
                (long)handle.slot,
                (long)handle.generation
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        bool setup_ok = target_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;

        if (setup_ok) {
            interp.current_scope = target_scope;
            Value* seed = make_cons(
                interp,
                make_int(interp, 808),
                make_cons(interp, make_int(interp, 909), make_nil(interp))
            );
            prepared_root = seed != null ? promote_to_escape(seed, interp) : null;
            setup_ok = prepared_root != null;
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
            interp.current_scope = saved_scope;
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz first_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz tail_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, second_child) : 0;
        usz tail_car = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, second_child, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz tail_cdr = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, second_child, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        StableGraphPassport passport = {};
        bool passport_ok = setup_ok && stable_escape_store_passport_snapshot(interp, &handle, &passport);

        bool shape_ok = setup_ok &&
                        passport_ok &&
                        passport.owner_scope_generation == target_scope.generation &&
                        passport.owner_escape_generation == target_scope.escape_generation &&
                        passport.prepared_node_count == node_count &&
                        passport.prepared_edge_count == 4 &&
                        passport.prepared_root_index == root_index &&
                        (passport.policy_flags & STABLE_GRAPH_PASSPORT_POLICY_PREPARED_GRAPH) != 0 &&
                        (passport.risk_flags & STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH) != 0 &&
                        passport.invalidation_reason == STABLE_GRAPH_PASSPORT_VALID &&
                        node_count == 5 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == CONS &&
                        root_child_count == 2 &&
                        first_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        second_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, first_child) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, second_child) == CONS &&
                        tail_child_count == 2 &&
                        tail_car != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        tail_cdr != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, tail_car) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, tail_cdr) == NIL;

        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (target_scope != null) main::scope_release(target_scope);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares cons graph metadata");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares cons graph metadata (setup_ok=%s node_count=%d root_index=%d root_child_count=%d first_child=%d second_child=%d tail_child_count=%d tail_car=%d tail_cdr=%d root_tag=%d first_tag=%d second_tag=%d tail_car_tag=%d tail_cdr_tag=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)first_child,
                (long)second_child,
                (long)tail_child_count,
                (long)tail_car,
                (long)tail_cdr,
                (int)stable_escape_store_prepared_node_tag(interp, &handle, root_index),
                (int)stable_escape_store_prepared_node_tag(interp, &handle, first_child),
                (int)stable_escape_store_prepared_node_tag(interp, &handle, second_child),
                (int)stable_escape_store_prepared_node_tag(interp, &handle, tail_car),
                (int)stable_escape_store_prepared_node_tag(interp, &handle, tail_cdr),
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        bool setup_ok = target_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;

        if (setup_ok) {
            interp.current_scope = target_scope;
            Value* shared_seed = make_cons(interp, make_int(interp, 1001), make_nil(interp));
            Value* shared = shared_seed != null ? promote_to_escape(shared_seed, interp) : null;
            Value* array_seed = make_array(interp, 3);
            setup_ok = shared != null &&
                       array_seed != null &&
                       array_seed.tag == ARRAY &&
                       array_seed.array_val != null;
            if (setup_ok) {
                array_seed.array_val.items[0] = shared;
                array_seed.array_val.items[1] = shared;
                array_seed.array_val.items[2] = make_int(interp, 1002);
                array_seed.array_val.length = 3;
                prepared_root = promote_to_escape(array_seed, interp);
                setup_ok = prepared_root != null && prepared_root.tag == ARRAY && prepared_root.array_val != null;
            }
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
            interp.current_scope = saved_scope;
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz first_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz third_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 2) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz shared_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, first_child) : 0;
        usz shared_car = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, first_child, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz shared_cdr = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, first_child, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool shape_ok = setup_ok &&
                        node_count == 5 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == ARRAY &&
                        root_child_count == 3 &&
                        first_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        first_child == second_child &&
                        third_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, first_child) == CONS &&
                        stable_escape_store_prepared_node_tag(interp, &handle, third_child) == INT &&
                        shared_child_count == 2 &&
                        shared_car != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        shared_cdr != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, shared_car) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, shared_cdr) == NIL &&
                        stable_escape_store_prepared_node_child(interp, &handle, root_index, 3) == STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (target_scope != null) main::scope_release(target_scope);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares array graph metadata with shared child reuse");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares array graph metadata with shared child reuse (setup_ok=%s node_count=%d root_index=%d root_child_count=%d first_child=%d second_child=%d third_child=%d shared_child_count=%d shared_car=%d shared_cdr=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)first_child,
                (long)second_child,
                (long)third_child,
                (long)shared_child_count,
                (long)shared_car,
                (long)shared_cdr,
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            Value* map_seed = make_hashmap_checked(interp, 4, "test: stable escape dictionary map allocation failed");
            SymbolId key_a_sym = interp.symbols.intern("stable-dict-a");
            SymbolId key_b_sym = interp.symbols.intern("stable-dict-b");
            Value* key_a = key_a_sym != INVALID_SYMBOL_ID ? make_symbol(interp, key_a_sym) : null;
            Value* key_b = key_b_sym != INVALID_SYMBOL_ID ? make_symbol(interp, key_b_sym) : null;
            Value* shared_seed = make_cons(interp, make_int(interp, 2001), make_nil(interp));
            Value* shared = shared_seed != null ? promote_to_escape(shared_seed, interp) : null;
            setup_ok = map_seed != null && map_seed.tag == HASHMAP && map_seed.hashmap_val != null &&
                       key_a != null && key_b != null && shared != null;
            if (setup_ok) {
                setup_ok = hashmap_set_checked(map_seed.hashmap_val, key_a, shared, interp) &&
                           hashmap_set_checked(map_seed.hashmap_val, key_b, shared, interp);
            }
            prepared_root = setup_ok ? promote_to_escape(map_seed, interp) : null;
            setup_ok = setup_ok && prepared_root != null && prepared_root.tag == HASHMAP;
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz first_key = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz first_value = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_key = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 2) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_value = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 3) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz shared_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, first_value) : 0;
        usz shared_car = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, first_value, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz shared_cdr = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, first_value, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool shape_ok = setup_ok &&
                        node_count == 6 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == HASHMAP &&
                        root_child_count == 4 &&
                        first_key != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        second_key != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        first_value != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        first_value == second_value &&
                        stable_escape_store_prepared_node_tag(interp, &handle, first_key) == SYMBOL &&
                        stable_escape_store_prepared_node_tag(interp, &handle, second_key) == SYMBOL &&
                        stable_escape_store_prepared_node_tag(interp, &handle, first_value) == CONS &&
                        shared_child_count == 2 &&
                        stable_escape_store_prepared_node_tag(interp, &handle, shared_car) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, shared_cdr) == NIL &&
                        stable_escape_store_prepared_node_child(interp, &handle, root_index, 4) == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares dictionary graph metadata with shared value reuse");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares dictionary graph metadata with shared value reuse (setup_ok=%s node_count=%d root_index=%d root_child_count=%d first_key=%d first_value=%d second_key=%d second_value=%d shared_child_count=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)first_key,
                (long)first_value,
                (long)second_key,
                (long)second_value,
                (long)shared_child_count,
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            Value* set_seed = make_set_checked(interp, 4, "test: stable escape set allocation failed");
            Value* member_seed = make_cons(interp, make_int(interp, 4040), make_nil(interp));
            Value* member_cons = member_seed != null ? promote_to_escape(member_seed, interp) : null;
            SymbolId member_sym = interp.symbols.intern("stable-set-member");
            Value* member_symbol = member_sym != INVALID_SYMBOL_ID ? make_symbol(interp, member_sym) : null;
            Value* true_val = make_symbol(interp, interp.sym_true);
            setup_ok = set_seed != null && set_seed.tag == SET && set_seed.hashmap_val != null &&
                       member_cons != null && member_symbol != null && true_val != null;
            if (setup_ok) {
                setup_ok = hashmap_set_checked(set_seed.hashmap_val, member_cons, true_val, interp) &&
                           hashmap_set_checked(set_seed.hashmap_val, member_symbol, true_val, interp);
            }
            prepared_root = setup_ok ? promote_to_escape(set_seed, interp) : null;
            setup_ok = setup_ok && prepared_root != null && prepared_root.tag == SET;
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz first_member = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_member = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        ValueTag first_tag = setup_ok ? stable_escape_store_prepared_node_tag(interp, &handle, first_member) : VOID;
        ValueTag second_tag = setup_ok ? stable_escape_store_prepared_node_tag(interp, &handle, second_member) : VOID;
        usz cons_member = first_tag == CONS ? first_member : (second_tag == CONS ? second_member : STABLE_ESCAPE_PREPARED_INVALID_INDEX);
        usz cons_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, cons_member) : 0;
        usz cons_car = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, cons_member, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz cons_cdr = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, cons_member, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool shape_ok = setup_ok &&
                        node_count == 5 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == SET &&
                        root_child_count == 2 &&
                        first_member != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        second_member != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        first_member != second_member &&
                        ((first_tag == CONS && second_tag == SYMBOL) || (first_tag == SYMBOL && second_tag == CONS)) &&
                        cons_child_count == 2 &&
                        stable_escape_store_prepared_node_tag(interp, &handle, cons_car) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, cons_cdr) == NIL &&
                        stable_escape_store_prepared_node_child(interp, &handle, root_index, 2) == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares set graph metadata with member-only edges");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares set graph metadata with member-only edges (setup_ok=%s node_count=%d root_index=%d root_child_count=%d first_member=%d second_member=%d first_tag=%d second_tag=%d cons_child_count=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)first_member,
                (long)second_member,
                (long)first_tag,
                (long)second_tag,
                (long)cons_child_count,
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            Env* captured_env = make_env(interp, interp.global_env);
            SymbolId cons_sym = interp.symbols.intern("stable-closure-cons");
            SymbolId symbol_sym = interp.symbols.intern("stable-closure-symbol");
            SymbolId captured_symbol_id = interp.symbols.intern("stable-closure-captured-symbol");
            Value* captured_cons = make_cons(interp, make_int(interp, 5050), make_nil(interp));
            Value* captured_symbol = captured_symbol_id != INVALID_SYMBOL_ID ? make_symbol(interp, captured_symbol_id) : null;
            setup_ok = captured_env != null &&
                       cons_sym != INVALID_SYMBOL_ID &&
                       symbol_sym != INVALID_SYMBOL_ID &&
                       captured_cons != null &&
                       captured_symbol != null;
            if (setup_ok) {
                setup_ok = env_define_with_barrier_checked(captured_env, cons_sym, captured_cons, interp) &&
                           env_define_with_barrier_checked(captured_env, symbol_sym, captured_symbol, interp);
            }
            prepared_root = setup_ok ? make_closure_no_param(interp, null, captured_env) : null;
            setup_ok = setup_ok && prepared_root != null && prepared_root.tag == CLOSURE;
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz first_capture = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz second_capture = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        ValueTag first_tag = setup_ok ? stable_escape_store_prepared_node_tag(interp, &handle, first_capture) : VOID;
        ValueTag second_tag = setup_ok ? stable_escape_store_prepared_node_tag(interp, &handle, second_capture) : VOID;
        usz cons_capture = first_tag == CONS ? first_capture : (second_tag == CONS ? second_capture : STABLE_ESCAPE_PREPARED_INVALID_INDEX);
        usz cons_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, cons_capture) : 0;
        usz cons_car = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, cons_capture, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz cons_cdr = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, cons_capture, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool shape_ok = setup_ok &&
                        node_count == 5 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == CLOSURE &&
                        root_child_count == 2 &&
                        first_capture != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        second_capture != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        first_capture != second_capture &&
                        ((first_tag == CONS && second_tag == SYMBOL) || (first_tag == SYMBOL && second_tag == CONS)) &&
                        cons_child_count == 2 &&
                        stable_escape_store_prepared_node_tag(interp, &handle, cons_car) == INT &&
                        stable_escape_store_prepared_node_tag(interp, &handle, cons_cdr) == NIL &&
                        stable_escape_store_prepared_node_child(interp, &handle, root_index, 2) == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares closure env graph metadata");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares closure env graph metadata (setup_ok=%s node_count=%d root_index=%d root_child_count=%d first_capture=%d second_capture=%d first_tag=%d second_tag=%d cons_child_count=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)first_capture,
                (long)second_capture,
                (long)first_tag,
                (long)second_tag,
                (long)cons_child_count,
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            prepared_root = make_cons(interp, make_nil(interp), make_nil(interp));
            setup_ok = prepared_root != null && prepared_root.tag == CONS;
            if (setup_ok) {
                prepared_root.cons_val.car = prepared_root;
                prepared_root.cons_val.cdr = prepared_root;
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
        }

        usz node_count = setup_ok ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz root_child_count = setup_ok ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
        usz car_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        usz cdr_child = setup_ok ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 1) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;

        bool shape_ok = setup_ok &&
                        node_count == 1 &&
                        root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                        stable_escape_store_prepared_node_tag(interp, &handle, root_index) == CONS &&
                        root_child_count == 2 &&
                        car_child == root_index &&
                        cdr_child == root_index &&
                        stable_escape_store_prepared_node_child(interp, &handle, root_index, 2) == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (shape_ok && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape store prepares cyclic cons graph metadata");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape store prepares cyclic cons graph metadata (setup_ok=%s node_count=%d root_index=%d root_child_count=%d car_child=%d cdr_child=%d retire_ok=%s)",
                setup_ok ? "yes" : "no",
                (long)node_count,
                (long)root_index,
                (long)root_child_count,
                (long)car_child,
                (long)cdr_child,
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            prepared_root = make_cons(interp, make_int(interp, 6060), make_nil(interp));
            setup_ok = prepared_root != null && prepared_root.tag == CONS;
            if (setup_ok) setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
        }
        usz root_index = setup_ok ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
        bool before_live = setup_ok && stable_escape_store_handle_is_alive(interp, &handle);
        if (setup_ok) {
            prepared_root.cons_val.car = make_int(interp, 6061);
        }
        StableGraphPassport stale_passport = {};
        bool stale_passport_ok = setup_ok && stable_escape_store_passport_snapshot(interp, &handle, &stale_passport);
        bool mutation_invalidated = setup_ok &&
                                    stale_passport_ok &&
                                    stale_passport.invalidation_reason == STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE &&
                                    !stable_escape_store_handle_is_alive(interp, &handle) &&
                                    stable_escape_store_prepared_node_tag(interp, &handle, root_index) == VOID &&
                                    stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) == STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                                    stable_escape_store_resolve_value(interp, &handle) == null &&
                                    stable_escape_store_prepared_node_count(interp, &handle) == 0;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);
        if (before_live && mutation_invalidated && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape prepared cons invalidates after child mutation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape prepared cons invalidates after child mutation (setup_ok=%s before_live=%s mutation_invalidated=%s retire_ok=%s)",
                setup_ok ? "yes" : "no",
                before_live ? "yes" : "no",
                mutation_invalidated ? "yes" : "no",
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        StableEscapeHandle handle = {};
        Value* prepared_root = null;
        Env* captured_env = null;
        SymbolId captured_sym = INVALID_SYMBOL_ID;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        if (setup_ok) {
            captured_env = make_env(interp, interp.global_env);
            captured_sym = interp.symbols.intern("stable-closure-mutated-capture");
            setup_ok = captured_env != null && captured_sym != INVALID_SYMBOL_ID;
            if (setup_ok) {
                setup_ok = env_define_with_barrier_checked(captured_env, captured_sym, make_int(interp, 7070), interp);
            }
            prepared_root = setup_ok ? make_closure_no_param(interp, null, captured_env) : null;
            setup_ok = setup_ok && prepared_root != null && prepared_root.tag == CLOSURE;
            if (setup_ok) {
                setup_ok = stable_escape_store_prepare_value(interp, prepared_root, &handle);
            }
        }

        bool before_live = setup_ok && stable_escape_store_handle_is_alive(interp, &handle);
        uint epoch_before = setup_ok ? stable_graph_mutation_epoch_snapshot() : 0;
        bool mutation_ok = setup_ok && env_set_with_barrier_checked(captured_env, captured_sym, make_int(interp, 7071), interp);
        uint epoch_after = setup_ok ? stable_graph_mutation_epoch_snapshot() : 0;
        StableGraphPassport stale_passport = {};
        bool stale_passport_ok = mutation_ok && stable_escape_store_passport_snapshot(interp, &handle, &stale_passport);
        bool mutation_invalidated = mutation_ok && stale_passport_ok &&
                                    epoch_after != epoch_before && stale_passport.mutation_epoch == epoch_before &&
                                    stale_passport.invalidation_reason == STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE &&
                                    !stable_escape_store_handle_is_alive(interp, &handle) &&
                                    stable_escape_store_resolve_value(interp, &handle) == null;
        bool retire_ok = setup_ok && stable_escape_store_retire(interp, &handle);

        if (before_live && mutation_invalidated && retire_ok) {
            emit_pass_literal("[PASS] lifetime: stable escape prepared closure invalidates after captured binding mutation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape prepared closure invalidates after captured binding mutation (setup_ok=%s before_live=%s mutation_ok=%s mutation_invalidated=%s retire_ok=%s)",
                setup_ok ? "yes" : "no",
                before_live ? "yes" : "no",
                mutation_ok ? "yes" : "no",
                mutation_invalidated ? "yes" : "no",
                retire_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool setup_ok = interp.root_scope != null;
        Value* prepared_root = null;
        usz raw_before = stable_escape_store_raw_publication_hook_count_snapshot();
        usz fallback_before = stable_escape_store_prepare_failure_fallback_count_snapshot();
        usz prepared_before = stable_escape_store_prepared_publication_hook_count_snapshot();
        usz total_before = stable_escape_store_publication_hook_count_snapshot();

        if (setup_ok) {
            interp.current_scope = interp.root_scope;
            defer interp.current_scope = saved_scope;
            Value* unsupported_thunk = make_closure_no_param(interp, null, interp.global_env);
            prepared_root = unsupported_thunk != null ? make_iterator(interp, unsupported_thunk) : null;
            setup_ok = prepared_root != null && prepared_root.tag == ITERATOR && prepared_root.iterator_val == unsupported_thunk;
            if (setup_ok) {
                Value* resolved = stable_escape_prepare_and_publish_value(interp, prepared_root);
                setup_ok = resolved == prepared_root;
            }
        }

        usz raw_after = stable_escape_store_raw_publication_hook_count_snapshot();
        usz fallback_after = stable_escape_store_prepare_failure_fallback_count_snapshot();
        usz prepared_after = stable_escape_store_prepared_publication_hook_count_snapshot();
        usz total_after = stable_escape_store_publication_hook_count_snapshot();
        bool ok = setup_ok &&
                  raw_after == raw_before + 1 &&
                  fallback_after == fallback_before + 1 &&
                  prepared_after == prepared_before &&
                  total_after == total_before + 1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: stable escape unsupported prepared graph fallback is observable");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: stable escape unsupported prepared graph fallback is observable (setup_ok=%s raw_before=%d raw_after=%d fallback_before=%d fallback_after=%d prepared_before=%d prepared_after=%d total_before=%d total_after=%d)",
                setup_ok ? "yes" : "no",
                (long)raw_before,
                (long)raw_after,
                (long)fallback_before,
                (long)fallback_after,
                (long)prepared_before,
                (long)prepared_after,
                (long)total_before,
                (long)total_after
            );
            (*fail)++;
        }
    }
}
