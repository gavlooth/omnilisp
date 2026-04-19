module lisp;

import std::core::mem;
import std::io;

fn bool deduce_test_component_delta_tuple_matches(DeduceDeltaTupleSet* set, usz idx, char[] expected) {
    if (set == null || idx >= set.count) return false;
    return deduce_bytes_equal(
        set.tuples[idx].key_bytes,
        set.tuples[idx].key_len,
        expected.ptr,
        expected.len
    );
}

fn void run_deduce_capacity_growth_overflow_guard_test(Interp* interp, int* pass, int* fail) {
    Relation rel = {};
    DeduceDb db = {};
    DeduceAggregateGroups aggregate_groups = {};
    DeduceDeltaTupleSet delta_set = {};
    DeduceDeltaTupleSet state_set = {};
    Interp source_dir_interp = {};

    bool relation_ok =
        !deduce_relation_ensure_column_capacity(&rel, usz.max) &&
        !deduce_relation_ensure_unique_column_capacity(&rel, usz.max) &&
        !deduce_relation_ensure_reference_constraint_capacity(&rel, usz.max) &&
        !deduce_relation_ensure_check_constraint_capacity(&rel, usz.max) &&
        rel.columns == null &&
        rel.unique_cols == null &&
        rel.reference_constraints == null &&
        rel.check_constraints == null;

    bool db_ok =
        !deduce_db_ensure_incremental_dirty_capacity(&db, usz.max) &&
        !deduce_db_ensure_rule_signature_capacity(&db, usz.max) &&
        !deduce_db_ensure_relation_schema_capacity(&db, usz.max) &&
        !deduce_db_ensure_persisted_rule_catalog_capacity(&db, usz.max) &&
        db.incremental_dirty_predicates == null &&
        db.rule_signatures == null &&
        db.relation_schemas == null &&
        db.persisted_rule_catalog == null;

    Value* aggregate_err = deduce_aggregate_groups_ensure_capacity(&aggregate_groups, usz.max, interp);
    bool aggregate_ok = aggregate_err != null &&
                        aggregate_err.tag == ERROR &&
                        aggregate_groups.entries == null &&
                        aggregate_groups.capacity == 0;

    delta_set.count = usz.max;
    bool delta_ok = !deduce_delta_set_append_encoded_tuple(&delta_set, "x".ptr, "x".len) &&
                    delta_set.tuples == null &&
                    delta_set.capacity == 0;

    state_set.count = usz.max;
    Value* state_err = deduce_delta_set_insert(&state_set, "x".ptr, "x".len, interp);
    bool state_ok = state_err != null &&
                    state_err.tag == ERROR &&
                    state_set.tuples == null &&
                    state_set.capacity == 0;

    bool source_dir_ok = !ensure_source_dir_capacity(&source_dir_interp, usz.max) &&
                         source_dir_interp.source_dirs == null &&
                         source_dir_interp.source_dir_capacity == 0;

    DeduceRuleSignature signature_overflow = {};
    signature_overflow.body_count = (usz.max / SymbolId.sizeof) + 1;
    usz signature_record_size = 123;
    bool signature_record_ok =
        !deduce_rule_signature_record_size(&signature_overflow, 0, 0, &signature_record_size) &&
        signature_record_size == 123;

    char[4] tiny_record = {};
    usz wrapped_cursor = usz.max - (usz)1;
    char[] slice = {};
    bool cursor_slice_ok =
        !deduce_record_slice(tiny_record[..], &wrapped_cursor, 2, &slice) &&
        wrapped_cursor == usz.max - (usz)1;

    wrapped_cursor = usz.max - (usz)1;
    bool cursor_copy_ok =
        !deduce_copy_bytes_into_record(tiny_record[..], &wrapped_cursor, &tiny_record[0], 2) &&
        wrapped_cursor == usz.max - (usz)1;

    wrapped_cursor = 0;
    bool array_slice_ok =
        !deduce_record_array_slice(tiny_record[..], &wrapped_cursor, usz.max, 2, &slice) &&
        wrapped_cursor == 0;

    bool ok = relation_ok &&
              db_ok &&
              aggregate_ok &&
              delta_ok &&
              state_ok &&
              source_dir_ok &&
              signature_record_ok &&
              cursor_slice_ok &&
              cursor_copy_ok &&
              array_slice_ok;

    deduce_aggregate_groups_release(&aggregate_groups);
    deduce_delta_set_release(&delta_set);
    deduce_delta_set_release(&state_set);

    if (ok) {
        emit_pass_literal("[PASS] deduce capacity growth overflow guards fail closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce capacity growth overflow guards fail closed");
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_delta_payload_roundtrip_test(Interp* interp, int* pass, int* fail) {
    SymbolId[3] predicates = {
        interp.symbols.intern("parallel-component-a"),
        interp.symbols.intern("parallel-component-b"),
        interp.symbols.intern("parallel-component-c")
    };
    int[3] predicate_component = { 0, 0, 1 };
    DeduceSccPlan plan = {
        .predicates = predicates[..],
        .predicate_component = predicate_component[..],
        .component_count = 2
    };

    DeduceSignedDeltaSet[3] delta_sets = {};
    bool ok =
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].additions, "add-a-0".ptr, "add-a-0".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].additions, "add-a-1".ptr, "add-a-1".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[0].removals, "remove-a".ptr, "remove-a".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[1].removals, "remove-b".ptr, "remove-b".len) &&
        deduce_delta_set_append_encoded_tuple(&delta_sets[2].additions, "ignored-c".ptr, "ignored-c".len);
    defer foreach (signed : delta_sets) deduce_signed_delta_set_release(&signed);

    char* encoded = null;
    usz encoded_len = 0;
    if (ok) {
        Value* err = deduce_component_delta_payload_serialize(&plan, 0, delta_sets[..], &encoded, &encoded_len, interp);
        ok = err == null && encoded != null && encoded_len > 0;
    }
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    if (ok) {
        Value* err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
        ok = err == null;
    }
    defer deduce_serialized_component_delta_payload_release(&payload);

    if (ok) {
        ok = payload.component_id == 0 &&
             payload.entry_count == 2 &&
             payload.entries != null &&
             payload.entries[0].predicate_index == 0 &&
             payload.entries[0].predicate == predicates[0] &&
             payload.entries[0].delta_set.additions.count == 2 &&
             payload.entries[0].delta_set.removals.count == 1 &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.additions, 0, "add-a-0") &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.additions, 1, "add-a-1") &&
             deduce_test_component_delta_tuple_matches(&payload.entries[0].delta_set.removals, 0, "remove-a") &&
             payload.entries[1].predicate_index == 1 &&
             payload.entries[1].predicate == predicates[1] &&
             payload.entries[1].delta_set.additions.count == 0 &&
             payload.entries[1].delta_set.removals.count == 1 &&
             deduce_test_component_delta_tuple_matches(&payload.entries[1].delta_set.removals, 0, "remove-b");
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel component delta payload roundtrips worker-scratch deltas");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce parallel component delta payload roundtrips worker-scratch deltas");
        (*fail)++;
    }
}

fn bool deduce_test_component_delta_set_has_tuple(DeduceDeltaTupleSet* set, char[] expected) {
    if (set == null) return false;
    for (usz i = 0; i < set.count; i++) {
        if (deduce_bytes_equal(set.tuples[i].key_bytes, set.tuples[i].key_len, expected.ptr, expected.len)) {
            return true;
        }
    }
    return false;
}

fn void run_deduce_parallel_component_scratch_pass_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-scratch-pass (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-scratch-pass] edge-parallel-scratch-pass (src) (dst)) "
        "  (define [relation ddb-parallel-scratch-pass] path-parallel-scratch-pass (src) (dst)) "
        "  (deduce/rule! ddb-parallel-scratch-pass '(path-parallel-scratch-pass ?x ?y) '(edge-parallel-scratch-pass ?x ?y)) "
        "  (deduce/rule! ddb-parallel-scratch-pass '(path-parallel-scratch-pass ?x ?z) '(edge-parallel-scratch-pass ?x ?y) '(path-parallel-scratch-pass ?y ?z)) "
        "  (deduce 'fact! edge-parallel-scratch-pass 1 2) "
        "  (deduce 'fact! path-parallel-scratch-pass 2 3) "
        "  ddb-parallel-scratch-pass)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    Value* expected_1_2 = null;
    Value* expected_1_3 = null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = null;
        err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-scratch-pass")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_pass_payload(
                db,
                &plan,
                component_id,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            Value*[2] tuple_1_2 = { make_int(interp, 1), make_int(interp, 2) };
            Value*[2] tuple_1_3 = { make_int(interp, 1), make_int(interp, 3) };
            err = deduce_encode_value_vector_to_owned_string(tuple_1_2[..], &expected_1_2, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_1_3[..], &expected_1_3, interp);
            ok = err == null && expected_1_2 != null && expected_1_3 != null;
        }
        if (ok) {
            ok = payload.entry_count == 1 &&
                 payload.entries != null &&
                 payload.entries[0].predicate == interp.symbols.intern("path-parallel-scratch-pass") &&
                 payload.entries[0].delta_set.additions.count == 2 &&
                 payload.entries[0].delta_set.removals.count == 0 &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_2.str_chars[:expected_1_2.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_3.str_chars[:expected_1_3.str_len]
                 );
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch component pass computes serialized recursive deltas");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas (entries=%d adds=%d removes=%d)",
                (int)payload.entry_count,
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.additions.count : 0),
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.removals.count : 0)
            );
            io::printn("[FAIL] deduce parallel worker-scratch component pass computes serialized recursive deltas");
        }
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_scratch_closure_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-closure (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-closure] edge-parallel-closure (src) (dst)) "
        "  (define [relation ddb-parallel-closure] path-parallel-closure (src) (dst)) "
        "  (deduce/rule! ddb-parallel-closure '(path-parallel-closure ?x ?y) '(edge-parallel-closure ?x ?y)) "
        "  (deduce/rule! ddb-parallel-closure '(path-parallel-closure ?x ?z) '(path-parallel-closure ?x ?y) '(edge-parallel-closure ?y ?z)) "
        "  (deduce 'fact! edge-parallel-closure 1 2) "
        "  (deduce 'fact! edge-parallel-closure 2 3) "
        "  ddb-parallel-closure)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    Value* expected_1_2 = null;
    Value* expected_2_3 = null;
    Value* expected_1_3 = null;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-closure")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            Value*[2] tuple_1_2 = { make_int(interp, 1), make_int(interp, 2) };
            Value*[2] tuple_2_3 = { make_int(interp, 2), make_int(interp, 3) };
            Value*[2] tuple_1_3 = { make_int(interp, 1), make_int(interp, 3) };
            err = deduce_encode_value_vector_to_owned_string(tuple_1_2[..], &expected_1_2, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_2_3[..], &expected_2_3, interp);
            if (err == null) err = deduce_encode_value_vector_to_owned_string(tuple_1_3[..], &expected_1_3, interp);
            ok = err == null && expected_1_2 != null && expected_2_3 != null && expected_1_3 != null;
        }
        if (ok) {
            ok = payload.entry_count == 1 &&
                 payload.entries != null &&
                 payload.entries[0].predicate == interp.symbols.intern("path-parallel-closure") &&
                 payload.entries[0].delta_set.additions.count == 3 &&
                 payload.entries[0].delta_set.removals.count == 0 &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_2.str_chars[:expected_1_2.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_2_3.str_chars[:expected_2_3.str_len]
                 ) &&
                 deduce_test_component_delta_set_has_tuple(
                     &payload.entries[0].delta_set.additions,
                     expected_1_3.str_chars[:expected_1_3.str_len]
                 );
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas (entries=%d adds=%d removes=%d)",
                (int)payload.entry_count,
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.additions.count : 0),
                (int)(payload.entry_count > 0 && payload.entries != null ? payload.entries[0].delta_set.removals.count : 0)
            );
            io::printn("[FAIL] deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas");
        }
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_scratch_multi_atom_closure_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-multiatom (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-multiatom] edge-parallel-multiatom (src) (dst)) "
        "  (define [relation ddb-parallel-multiatom] path-parallel-multiatom (src) (dst)) "
        "  (deduce/rule! ddb-parallel-multiatom '(path-parallel-multiatom ?x ?y) '(edge-parallel-multiatom ?x ?y)) "
        "  (deduce/rule! ddb-parallel-multiatom '(path-parallel-multiatom ?x ?z) '(path-parallel-multiatom ?x ?y) '(path-parallel-multiatom ?y ?z)) "
        "  (deduce 'fact! edge-parallel-multiatom 1 2) "
        "  (deduce 'fact! edge-parallel-multiatom 2 3) "
        "  (deduce 'fact! edge-parallel-multiatom 3 4) "
        "  ddb-parallel-multiatom)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-multiatom")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err != null &&
                 err.tag == ERROR;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-scratch closure rejects positive multi-atom recursive SCC rules");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce parallel worker-scratch closure rejects positive multi-atom recursive SCC rules");
        (*fail)++;
    }
}

fn void run_deduce_parallel_component_delta_apply_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define ddb-parallel-apply (deduce 'open 'memory)) "
        "  (define [relation ddb-parallel-apply] edge-parallel-apply (src) (dst)) "
        "  (define [relation ddb-parallel-apply] path-parallel-apply (src) (dst)) "
        "  (deduce/rule! ddb-parallel-apply '(path-parallel-apply ?x ?y) '(edge-parallel-apply ?x ?y)) "
        "  (deduce/rule! ddb-parallel-apply '(path-parallel-apply ?x ?z) '(path-parallel-apply ?x ?y) '(edge-parallel-apply ?y ?z)) "
        "  (deduce 'fact! edge-parallel-apply 1 2) "
        "  (deduce 'fact! edge-parallel-apply 2 3) "
        "  ddb-parallel-apply)",
        interp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == FFI_HANDLE &&
              r.value.ffi_val != null &&
              r.value.ffi_val.lib_handle != null;

    DeduceSccPlan plan = {};
    defer deduce_scc_plan_free(&plan);

    char* encoded = null;
    usz encoded_len = 0;
    defer if (encoded != null) mem::free(encoded);

    DeduceSerializedComponentDeltaPayload payload = {};
    defer deduce_serialized_component_delta_payload_release(&payload);

    long applied = 0;
    bool changed = false;
    long tuple_count = -1;
    if (ok) {
        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* err = deduce_build_scc_plan(db, &plan, interp);
        ok = err == null;
        int path_idx = ok ? deduce_scc_find_predicate_index(plan.predicates, interp.symbols.intern("path-parallel-apply")) : -1;
        ok = ok && path_idx >= 0 && (usz)path_idx < plan.predicate_component.len;
        usz component_id = ok ? (usz)plan.predicate_component[(usz)path_idx] : 0;

        if (ok) {
            err = deduce_seminaive_compute_component_scratch_closure_payload(
                db,
                &plan,
                component_id,
                0,
                &encoded,
                &encoded_len,
                interp
            );
            ok = err == null && encoded != null && encoded_len > 0;
        }
        if (ok) {
            err = deduce_component_delta_payload_deserialize(encoded[0:encoded_len], &payload, interp);
            ok = err == null;
        }
        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(db.env, null, 0, &txn);
            ok = rc == MDB_SUCCESS && txn != null;
            if (ok) {
                err = deduce_apply_serialized_component_delta_payload(
                    db,
                    &plan,
                    txn,
                    &payload,
                    &applied,
                    &changed,
                    interp
                );
                ok = err == null;
            }
            if (ok) {
                rc = mdb_txn_commit(txn);
                ok = rc == MDB_SUCCESS;
                txn = null;
            }
            if (txn != null) mdb_txn_abort(txn);
        }
        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
            ok = rc == MDB_SUCCESS && txn != null;
            MdbCursor* cursor = null;
            if (ok) {
                MdbDbi path_dbi = 0;
                DeduceRelationSchema* schema = null;
                ok = deduce_rule_open_existing_dbi(
                    db,
                    txn,
                    interp.symbols.intern("path-parallel-apply"),
                    &path_dbi,
                    &schema,
                    interp
                ) && schema != null;
                if (ok) {
                    rc = mdb_cursor_open(txn, path_dbi, &cursor);
                    ok = rc == MDB_SUCCESS && cursor != null;
                }
                if (ok) {
                    MdbVal key = {};
                    MdbVal data = {};
                    tuple_count = 0;
                    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
                    while (rc == MDB_SUCCESS) {
                        tuple_count++;
                        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                    }
                    ok = tuple_count == 3 &&
                        schema.cardinality_estimate == 3 &&
                        schema.distinct_estimate == 3;
                }
            }
            if (cursor != null) mdb_cursor_close(cursor);
            if (txn != null) mdb_txn_abort(txn);
        }
        if (ok) {
            ok = payload.entry_count == 1 && applied == 3 && changed;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce parallel worker-computed component deltas apply on the main thread");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce parallel worker-computed component deltas apply on the main thread (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn(
                "[FAIL] deduce parallel worker-computed component deltas apply on the main thread (entries=%d applied=%d changed=%d tuples=%d)",
                (int)payload.entry_count,
                (int)applied,
                changed ? 1 : 0,
                (int)tuple_count
            );
            io::printn("[FAIL] deduce parallel worker-computed component deltas apply on the main thread");
        }
        (*fail)++;
    }
}
