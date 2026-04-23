module lisp;

import std::core::mem;

fn Value* deduce_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    if (data == null) return raise_error_with_payloadless_fallback_names(interp, "deduce", code, message);
    return raise_error_with_payload_names(interp, "deduce", code, message, data);
}

fn bool deduce_ffi_handle_name_equals(Value* v, char[] expected) @inline {
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return false;
    if (v.ffi_val.name_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (v.ffi_val.lib_name[i] != expected[i]) return false;
    }
    return true;
}

fn bool deduce_name_equals(char[] name, char[] expected) @inline {
    if (name.len != expected.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (name[i] != expected[i]) return false;
    }
    return true;
}

fn bool deduce_rule_signature_has_aggregates(DeduceRuleSignature* sig) @inline {
    return sig != null && sig.aggregate_projection_count > 0;
}

fn void deduce_db_finalizer(void* handle) {
    if (handle == null) return;
    DeduceDb* db = (DeduceDb*)handle;
    if (db.rule_signatures != null) {
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (db.rule_signatures[i].body_predicates != null) {
                mem::free(db.rule_signatures[i].body_predicates);
                db.rule_signatures[i].body_predicates = null;
            }
            if (db.rule_signatures[i].body_negated != null) {
                mem::free(db.rule_signatures[i].body_negated);
                db.rule_signatures[i].body_negated = null;
            }
            if (db.rule_signatures[i].body_bound_masks != null) {
                mem::free(db.rule_signatures[i].body_bound_masks);
                db.rule_signatures[i].body_bound_masks = null;
            }
            if (db.rule_signatures[i].body_selected_indexes != null) {
                mem::free(db.rule_signatures[i].body_selected_indexes);
                db.rule_signatures[i].body_selected_indexes = null;
            }
            if (db.rule_signatures[i].body_filter_pushdown != null) {
                mem::free(db.rule_signatures[i].body_filter_pushdown);
                db.rule_signatures[i].body_filter_pushdown = null;
            }
            if (db.rule_signatures[i].body_projection_masks != null) {
                mem::free(db.rule_signatures[i].body_projection_masks);
                db.rule_signatures[i].body_projection_masks = null;
            }
            if (db.rule_signatures[i].head_term_kinds != null) {
                mem::free(db.rule_signatures[i].head_term_kinds);
                db.rule_signatures[i].head_term_kinds = null;
            }
            if (db.rule_signatures[i].head_term_aggregate_ops != null) {
                mem::free(db.rule_signatures[i].head_term_aggregate_ops);
                db.rule_signatures[i].head_term_aggregate_ops = null;
            }
            if (db.rule_signatures[i].head_term_is_variable != null) {
                mem::free(db.rule_signatures[i].head_term_is_variable);
                db.rule_signatures[i].head_term_is_variable = null;
            }
            if (db.rule_signatures[i].head_term_var_ids != null) {
                mem::free(db.rule_signatures[i].head_term_var_ids);
                db.rule_signatures[i].head_term_var_ids = null;
            }
            if (db.rule_signatures[i].head_term_literals != null) {
                mem::free(db.rule_signatures[i].head_term_literals);
                db.rule_signatures[i].head_term_literals = null;
            }
            if (db.rule_signatures[i].body_term_offsets != null) {
                mem::free(db.rule_signatures[i].body_term_offsets);
                db.rule_signatures[i].body_term_offsets = null;
            }
            if (db.rule_signatures[i].body_term_counts != null) {
                mem::free(db.rule_signatures[i].body_term_counts);
                db.rule_signatures[i].body_term_counts = null;
            }
            if (db.rule_signatures[i].body_term_is_variable != null) {
                mem::free(db.rule_signatures[i].body_term_is_variable);
                db.rule_signatures[i].body_term_is_variable = null;
            }
            if (db.rule_signatures[i].body_term_var_ids != null) {
                mem::free(db.rule_signatures[i].body_term_var_ids);
                db.rule_signatures[i].body_term_var_ids = null;
            }
            if (db.rule_signatures[i].body_term_literals != null) {
                mem::free(db.rule_signatures[i].body_term_literals);
                db.rule_signatures[i].body_term_literals = null;
            }
            db.rule_signatures[i].head_term_count = 0;
            db.rule_signatures[i].aggregate_projection_count = 0;
            db.rule_signatures[i].body_term_total_count = 0;
            db.rule_signatures[i].max_var_id = 0;
            db.rule_signatures[i].body_count = 0;
            db.rule_signatures[i].head_predicate = (SymbolId)0;
        }
        mem::free(db.rule_signatures);
        db.rule_signatures = null;
    }
    db.rule_signature_count = 0;
    db.rule_signature_capacity = 0;
    deduce_db_clear_persisted_rule_catalog(db);

    if (db.incremental_dirty_predicates != null) {
        mem::free(db.incremental_dirty_predicates);
        db.incremental_dirty_predicates = null;
    }
    db.incremental_dirty_count = 0;
    db.incremental_dirty_capacity = 0;
    db.incremental_mutation_epoch = 0;
    db.incremental_full_recompute_required = false;
    db.integrity_violation_count = 0;
    db.key_integrity_violation_count = 0;
    db.unique_integrity_violation_count = 0;
    db.reference_integrity_violation_count = 0;
    db.check_integrity_violation_count = 0;
    db.last_integrity_violation_code = (SymbolId)0;
    db.last_integrity_violation_relation = (SymbolId)0;
    db.last_goal_directed_read_fallback_reason = (SymbolId)0;
    if (db.last_goal_directed_read_requested_bound_positions != null) {
        mem::free(db.last_goal_directed_read_requested_bound_positions);
        db.last_goal_directed_read_requested_bound_positions = null;
    }
    if (db.last_goal_directed_read_applied_bound_positions != null) {
        mem::free(db.last_goal_directed_read_applied_bound_positions);
        db.last_goal_directed_read_applied_bound_positions = null;
    }
    if (db.last_goal_directed_read_step_counters != null) {
        mem::free(db.last_goal_directed_read_step_counters);
        db.last_goal_directed_read_step_counters = null;
    }
    db.last_goal_directed_read_requested_bound_count = 0;
    db.last_goal_directed_read_applied_bound_count = 0;
    db.last_goal_directed_read_step_count = 0;
    db.integrity_violation_history_count = 0;
    db.integrity_violation_sequence = 0;

    if (db.relation_schemas != null) {
        for (usz i = 0; i < db.relation_schema_count; i++) {
            DeduceRelationSchema* schema = &db.relation_schemas[i];
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            if (schema.unique_columns != null) {
                mem::free(schema.unique_columns);
                schema.unique_columns = null;
            }
            if (schema.reference_constraints != null) {
                mem::free(schema.reference_constraints);
                schema.reference_constraints = null;
            }
            if (schema.check_constraints != null) {
                mem::free(schema.check_constraints);
                schema.check_constraints = null;
            }
            if (schema.indexes != null) {
                for (usz j = 0; j < schema.index_count; j++) {
                    if (schema.indexes[j].column_positions != null) {
                        mem::free(schema.indexes[j].column_positions);
                        schema.indexes[j].column_positions = null;
                    }
                    schema.indexes[j].column_count = 0;
                    schema.indexes[j].unique = false;
                }
                mem::free(schema.indexes);
                schema.indexes = null;
            }
            schema.index_count = 0;
            schema.cardinality_estimate = 0;
            schema.distinct_estimate = 0;
            schema.integrity_violation_count = 0;
            schema.key_integrity_violation_count = 0;
            schema.unique_integrity_violation_count = 0;
            schema.reference_integrity_violation_count = 0;
            schema.check_integrity_violation_count = 0;
            schema.last_integrity_violation_code = (SymbolId)0;
            schema.last_goal_directed_read_fallback_reason = (SymbolId)0;
            if (schema.last_goal_directed_read_requested_bound_positions != null) {
                mem::free(schema.last_goal_directed_read_requested_bound_positions);
                schema.last_goal_directed_read_requested_bound_positions = null;
            }
            if (schema.last_goal_directed_read_applied_bound_positions != null) {
                mem::free(schema.last_goal_directed_read_applied_bound_positions);
                schema.last_goal_directed_read_applied_bound_positions = null;
            }
            for (usz key_index = 0; key_index < schema.last_goal_directed_read_subject_keys.count; key_index++) {
                if (schema.last_goal_directed_read_subject_keys.keys[key_index] != null) {
                    mem::free(schema.last_goal_directed_read_subject_keys.keys[key_index]);
                    schema.last_goal_directed_read_subject_keys.keys[key_index] = null;
                }
                schema.last_goal_directed_read_subject_keys.key_lens[key_index] = 0;
            }
            if (schema.last_goal_directed_read_step_counters != null) {
                mem::free(schema.last_goal_directed_read_step_counters);
                schema.last_goal_directed_read_step_counters = null;
            }
            deduce_delta_set_release(&schema.maintained_pending_insertions);
            schema.last_goal_directed_read_requested_bound_count = 0;
            schema.last_goal_directed_read_applied_bound_count = 0;
            schema.last_goal_directed_read_subject_keys.count = 0;
            schema.last_goal_directed_read_subject_keys.complete = true;
            schema.last_goal_directed_read_step_count = 0;
            schema.integrity_violation_history_count = 0;
            schema.key_column = (SymbolId)0;
            schema.unique_column = (SymbolId)0;
            schema.unique_column_count = 0;
            schema.reference_constraint_count = 0;
            schema.check_constraint_count = 0;
            schema.index_column = (SymbolId)0;
            schema.arity = 0;
            schema.dropped = false;
            schema.name = (SymbolId)0;
            schema.last_goal_directed_read_fallback_reason = (SymbolId)0;
        }
        mem::free(db.relation_schemas);
        db.relation_schemas = null;
    }
    db.relation_schema_count = 0;
    db.relation_schema_capacity = 0;

    if (db.env != null && db.open) {
        mdb_env_close(db.env);
        db.env = null;
        db.open = false;
    }
    mem::free(db);
}
