module lisp;

import std::core::mem;

fn void deduce_integrity_violation_history_append(
    DeduceIntegrityViolationEntry[DEDUCE_INTEGRITY_HISTORY_LIMIT]* history,
    usz* count,
    DeduceIntegrityViolationEntry entry
) {
    if (history == null || count == null) return;
    if (*count < DEDUCE_INTEGRITY_HISTORY_LIMIT) {
        (*history)[*count] = entry;
        (*count)++;
        return;
    }
    for (usz i = 1; i < DEDUCE_INTEGRITY_HISTORY_LIMIT; i++) {
        (*history)[i - 1] = (*history)[i];
    }
    (*history)[DEDUCE_INTEGRITY_HISTORY_LIMIT - 1] = entry;
}

fn DeduceRelationSchema* deduce_db_lookup_relation_schema(DeduceDb* db, SymbolId name) {
    if (db == null || name == (SymbolId)0) return null;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (db.relation_schemas[i].name != name) continue;
        return &db.relation_schemas[i];
    }
    return null;
}

fn DeduceRelationSchema* deduce_relation_lookup_schema(Relation* rel) {
    if (rel == null || rel.db == null) return null;
    return deduce_db_lookup_relation_schema(rel.db, rel.name);
}

fn void deduce_relation_schema_set_estimates(Relation* rel, ulong cardinality, ulong distinct_estimate) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    schema.cardinality_estimate = cardinality;
    if (distinct_estimate > cardinality) {
        schema.distinct_estimate = cardinality;
    } else {
        schema.distinct_estimate = distinct_estimate;
    }
}

fn void deduce_relation_schema_note_insert(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate < ulong.max) schema.cardinality_estimate++;
    if (schema.distinct_estimate < schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_delete(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate > 0) schema.cardinality_estimate--;
    if (schema.distinct_estimate > schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_clear(Relation* rel) {
    deduce_relation_schema_set_estimates(rel, 0, 0);
}

fn void deduce_relation_schema_note_integrity_violation(
    DeduceRelationSchema* schema,
    DeduceIntegrityViolationClass violation_class,
    SymbolId relation,
    SymbolId code,
    ulong sequence
) {
    if (schema == null) return;
    if (schema.integrity_violation_count < ulong.max) schema.integrity_violation_count++;
    schema.last_integrity_violation_code = code;
    DeduceIntegrityViolationEntry entry = {
        .sequence = sequence,
        .relation = relation,
        .code = code,
        .violation_class = violation_class
    };
    deduce_integrity_violation_history_append(&schema.integrity_violation_history, &schema.integrity_violation_history_count, entry);
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_KEY) {
        if (schema.key_integrity_violation_count < ulong.max) schema.key_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_UNIQUE) {
        if (schema.unique_integrity_violation_count < ulong.max) schema.unique_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_REFERENCE &&
        schema.reference_integrity_violation_count < ulong.max) {
        schema.reference_integrity_violation_count++;
        return;
    }
    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_CHECK &&
        schema.check_integrity_violation_count < ulong.max) {
        schema.check_integrity_violation_count++;
    }
}

fn void deduce_db_note_integrity_violation(
    Relation* rel,
    DeduceIntegrityViolationClass violation_class,
    char[] code_name,
    Interp* interp
) {
    if (rel == null || rel.db == null || interp == null) return;

    DeduceDb* db = rel.db;
    SymbolId code = interp.symbols.intern(code_name);

    if (db.integrity_violation_count < ulong.max) db.integrity_violation_count++;
    if (db.integrity_violation_sequence < ulong.max) db.integrity_violation_sequence++;
    ulong sequence = db.integrity_violation_sequence;
    db.last_integrity_violation_code = code;
    db.last_integrity_violation_relation = rel.name;
    DeduceIntegrityViolationEntry entry = {
        .sequence = sequence,
        .relation = rel.name,
        .code = code,
        .violation_class = violation_class
    };
    deduce_integrity_violation_history_append(&db.integrity_violation_history, &db.integrity_violation_history_count, entry);

    if (violation_class == DEDUCE_INTEGRITY_VIOLATION_KEY) {
        if (db.key_integrity_violation_count < ulong.max) db.key_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_UNIQUE) {
        if (db.unique_integrity_violation_count < ulong.max) db.unique_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_REFERENCE &&
               db.reference_integrity_violation_count < ulong.max) {
        db.reference_integrity_violation_count++;
    } else if (violation_class == DEDUCE_INTEGRITY_VIOLATION_CHECK &&
               db.check_integrity_violation_count < ulong.max) {
        db.check_integrity_violation_count++;
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    deduce_relation_schema_note_integrity_violation(schema, violation_class, rel.name, code, sequence);
}

fn bool deduce_symbol_id_slice_contains(SymbolId[] values, SymbolId target) @inline {
    if (target == (SymbolId)0) return false;
    for (usz i = 0; i < values.len; i++) {
        if (values[i] == target) return true;
    }
    return false;
}

fn bool deduce_db_ensure_incremental_dirty_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.incremental_dirty_capacity) return true;

    usz new_cap = db.incremental_dirty_capacity > 0 ? db.incremental_dirty_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* next = (SymbolId*)mem::realloc(db.incremental_dirty_predicates, SymbolId.sizeof * new_cap);
    if (next == null) return false;
    db.incremental_dirty_predicates = next;
    db.incremental_dirty_capacity = new_cap;
    return true;
}

fn bool deduce_db_mark_incremental_dirty_predicate(DeduceDb* db, SymbolId predicate) {
    if (db == null || predicate == (SymbolId)0) return false;
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates != null && db.incremental_dirty_predicates[i] == predicate) {
            return true;
        }
    }
    usz needed = db.incremental_dirty_count + 1;
    if (!deduce_db_ensure_incremental_dirty_capacity(db, needed)) return false;
    db.incremental_dirty_predicates[db.incremental_dirty_count] = predicate;
    db.incremental_dirty_count++;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
    deduce_relation_schema_note_materialized_stale(db, schema, DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY);
    return true;
}

fn bool deduce_db_is_incremental_dirty_predicate(DeduceDb* db, SymbolId predicate) {
    if (db == null || predicate == (SymbolId)0) return false;
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates != null && db.incremental_dirty_predicates[i] == predicate) {
            return true;
        }
    }
    return false;
}

fn bool deduce_relation_schema_has_derived_rules(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || schema.name == (SymbolId)0) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (db.rule_signatures[i].head_predicate == schema.name) return true;
    }
    return false;
}

fn void deduce_relation_schema_clear_pending_incremental_insertions(DeduceRelationSchema* schema) {
    if (schema == null) return;
    deduce_delta_set_release(&schema.maintained_pending_insertions);
}

fn void deduce_db_clear_pending_incremental_insertions(DeduceDb* db) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        deduce_relation_schema_clear_pending_incremental_insertions(&db.relation_schemas[i]);
    }
}

fn void deduce_db_escalate_incremental_tracking_to_full_recompute(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null) return;
    deduce_db_clear_pending_incremental_insertions(db);
    db.incremental_full_recompute_required = true;
    deduce_db_note_materialized_views_stale(db, reason);
}

fn bool deduce_rule_signature_is_incremental_direct_copy_class(
    DeduceRuleSignature* sig,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig == null || sig.body_count != 1 || deduce_rule_signature_has_aggregates(sig)) return false;
    if (sig.body_predicates == null || sig.body_predicates[0] == (SymbolId)0) return false;
    if (sig.body_negated != null && sig.body_negated[0]) return false;
    if (sig.head_term_count == 0 || sig.head_term_kinds == null || sig.head_term_var_ids == null) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) return false;

    usz body_offset = sig.body_term_offsets[0];
    usz body_count = sig.body_term_counts[0];
    if (body_count != sig.head_term_count) return false;
    if (body_offset + body_count > sig.body_term_total_count) return false;

    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] != DEDUCE_RULE_TERM_VARIABLE) return false;
        usz body_term_index = body_offset + i;
        if (!sig.body_term_is_variable[body_term_index]) return false;
        if (sig.head_term_var_ids[i] != sig.body_term_var_ids[body_term_index]) return false;
    }

    if (source_predicate_out != null) *source_predicate_out = sig.body_predicates[0];
    return true;
}

fn bool deduce_relation_schema_incremental_extensional_copy_source(
    DeduceDb* db,
    DeduceRelationSchema* target_schema,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (db == null || target_schema == null || target_schema.name == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    SymbolId source_predicate = (SymbolId)0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != target_schema.name) continue;

        SymbolId candidate_source = (SymbolId)0;
        if (!deduce_rule_signature_is_incremental_direct_copy_class(sig, &candidate_source)) {
            return false;
        }
        if (matched != null) return false;
        matched = sig;
        source_predicate = candidate_source;
    }
    if (matched == null || source_predicate == (SymbolId)0) return false;

    DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
    if (source_schema == null || deduce_relation_schema_has_derived_rules(db, source_schema)) return false;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    return true;
}

fn bool deduce_relation_schema_incremental_derived_copy_source(
    DeduceDb* db,
    DeduceRelationSchema* target_schema,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (db == null || target_schema == null || target_schema.name == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    SymbolId source_predicate = (SymbolId)0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != target_schema.name) continue;

        SymbolId candidate_source = (SymbolId)0;
        if (!deduce_rule_signature_is_incremental_direct_copy_class(sig, &candidate_source)) {
            return false;
        }
        if (matched != null) return false;
        matched = sig;
        source_predicate = candidate_source;
    }
    if (matched == null || source_predicate == (SymbolId)0) return false;

    DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
    if (source_schema == null || !source_schema.materialized_view ||
        !deduce_relation_schema_is_materialized_ready(db, source_schema)) {
        return false;
    }
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    return true;
}

fn bool deduce_db_queue_supported_incremental_insert_for_source(
    DeduceDb* db,
    SymbolId source_predicate,
    bool derived_source,
    void* key_data,
    usz key_len
) {
    if (db == null || source_predicate == (SymbolId)0 || (key_data == null && key_len > 0)) return false;
    if (db.relation_schemas == null) return true;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* target_schema = &db.relation_schemas[i];
        if (!target_schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, target_schema)) continue;
        if (target_schema.materialized_refresh_count == 0) continue;

        SymbolId candidate_source = (SymbolId)0;
        bool supported = derived_source
            ? deduce_relation_schema_incremental_derived_copy_source(db, target_schema, &candidate_source)
            : deduce_relation_schema_incremental_extensional_copy_source(db, target_schema, &candidate_source);
        if (!supported) continue;
        if (candidate_source != source_predicate) continue;

        if (!deduce_delta_set_append_encoded_tuple(&target_schema.maintained_pending_insertions, key_data, key_len)) {
            return false;
        }
    }
    return true;
}

fn bool deduce_db_queue_supported_incremental_insert(
    Relation* rel,
    void* key_data,
    usz key_len
) {
    if (rel == null || rel.db == null) return false;
    return deduce_db_queue_supported_incremental_insert_for_source(rel.db, rel.name, false, key_data, key_len);
}

fn bool deduce_db_queue_supported_incremental_insert_from_materialized_source(
    DeduceDb* db,
    SymbolId source_predicate,
    void* key_data,
    usz key_len
) {
    return deduce_db_queue_supported_incremental_insert_for_source(db, source_predicate, true, key_data, key_len);
}

fn bool deduce_relation_schema_is_materialized_ready(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null) return false;
    if (!schema.materialized_view || schema.dropped) return false;
    return deduce_relation_schema_has_derived_rules(db, schema);
}

fn Value* deduce_db_note_insert_mutation(Relation* rel, void* key_data, usz key_len, Interp* interp) {
    deduce_db_note_committed_mutation(rel, false);
    if (rel != null && rel.db != null && !deduce_db_queue_supported_incremental_insert(rel, key_data, key_len)) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            rel.db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
    return deduce_db_persist_materialized_view_states(rel != null ? rel.db : null, interp);
}

fn DeduceMaterializedStaleReason deduce_relation_schema_effective_materialized_stale_reason(
    DeduceRelationSchema* schema
) {
    if (schema == null || !schema.materialized_view) return DEDUCE_MATERIALIZED_STALE_NONE;
    if (schema.materialized_refresh_count == 0) return DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED;
    return schema.materialized_last_stale_reason;
}

fn bool deduce_relation_schema_is_materialized_stale(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || !schema.materialized_view) return false;
    if (schema.materialized_refresh_count == 0) return true;
    if (schema.materialized_last_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE) return true;
    if (db.incremental_full_recompute_required) return true;
    return deduce_db_is_incremental_dirty_predicate(db, schema.name);
}

fn void deduce_relation_schema_note_materialized_stale(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || schema == null || !schema.materialized_view) return;
    schema.materialized_last_stale_mutation_epoch = db.incremental_mutation_epoch;
    schema.materialized_last_stale_reason = reason;
}

fn void deduce_db_note_materialized_views_stale(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        deduce_relation_schema_note_materialized_stale(db, &db.relation_schemas[i], reason);
    }
}

fn void deduce_db_note_ready_materialized_views_stale(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        deduce_relation_schema_note_materialized_stale(db, schema, reason);
    }
}

fn void deduce_relation_schema_note_materialized_refresh(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || !schema.materialized_view) return;
    if (schema.materialized_refresh_count < ulong.max) schema.materialized_refresh_count++;
    schema.materialized_last_refresh_mutation_epoch = db.incremental_mutation_epoch;
    schema.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
}

fn Value* deduce_db_persist_materialized_view_states(DeduceDb* db, Interp* interp) {
    if (db == null || interp == null) return null;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            schema.name,
            schema.materialized_view,
            schema.materialized_refresh_policy,
            schema.materialized_refresh_count,
            schema.materialized_last_refresh_mutation_epoch,
            schema.materialized_last_stale_mutation_epoch,
            schema.materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) return meta_err;
    }
    return null;
}

fn ulong deduce_rule_head_demand_bound_count(DeduceRuleHeadDemand* head_demand) {
    if (head_demand == null || head_demand.term_bound == null) return 0;
    ulong count = 0;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (!head_demand.term_bound[i]) continue;
        if (count < ulong.max) count++;
    }
    return count;
}

fn usz* deduce_rule_head_demand_copy_bound_positions(
    DeduceRuleHeadDemand* head_demand,
    ulong* out_count
) {
    if (out_count != null) *out_count = 0;
    ulong count = deduce_rule_head_demand_bound_count(head_demand);
    if (count == 0) return null;

    usz* positions = (usz*)mem::malloc(usz.sizeof * (usz)count);
    if (positions == null) return null;

    usz write_index = 0;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (!head_demand.term_bound[i]) continue;
        positions[write_index++] = i;
    }
    if (out_count != null) *out_count = count;
    return positions;
}

fn DeduceObservedStepCounters* deduce_observed_rule_counters_copy_steps(
    DeduceObservedRuleCounters* observed_rule_counters,
    usz* out_count
) {
    if (out_count != null) *out_count = 0;
    if (observed_rule_counters == null || observed_rule_counters.steps == null ||
        observed_rule_counters.step_count == 0) {
        return null;
    }

    DeduceObservedStepCounters[] steps =
        mem::new_array(DeduceObservedStepCounters, observed_rule_counters.step_count);
    if (steps.ptr == null) return null;
    for (usz i = 0; i < observed_rule_counters.step_count; i++) {
        steps[i] = observed_rule_counters.steps[i];
    }
    if (out_count != null) *out_count = observed_rule_counters.step_count;
    return steps.ptr;
}

fn void deduce_relation_schema_note_goal_directed_read(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    SymbolId execution_path,
    SymbolId surface,
    long selector_rule_index,
    DeduceRuleHeadDemand* requested_head_demand = null,
    DeduceRuleHeadDemand* applied_head_demand = null,
    DeduceObservedRuleCounters* observed_rule_counters = null,
    SymbolId fallback_reason = (SymbolId)0
) {
    if (db == null || schema == null || execution_path == (SymbolId)0) return;
    schema.last_goal_directed_read_execution_path = execution_path;
    schema.last_goal_directed_read_surface = surface;
    schema.last_goal_directed_read_fallback_reason = fallback_reason;
    schema.last_goal_directed_read_selector_rule_index = selector_rule_index;
    schema.last_goal_directed_read_mutation_epoch = db.incremental_mutation_epoch;
    if (schema.last_goal_directed_read_requested_bound_positions != null) {
        mem::free(schema.last_goal_directed_read_requested_bound_positions);
        schema.last_goal_directed_read_requested_bound_positions = null;
    }
    if (schema.last_goal_directed_read_applied_bound_positions != null) {
        mem::free(schema.last_goal_directed_read_applied_bound_positions);
        schema.last_goal_directed_read_applied_bound_positions = null;
    }
    deduce_goal_directed_read_subject_key_capture_release(&schema.last_goal_directed_read_subject_keys);
    if (schema.last_goal_directed_read_step_counters != null) {
        mem::free(schema.last_goal_directed_read_step_counters);
        schema.last_goal_directed_read_step_counters = null;
    }
    schema.last_goal_directed_read_requested_bound_positions =
        deduce_rule_head_demand_copy_bound_positions(
            requested_head_demand,
            &schema.last_goal_directed_read_requested_bound_count
        );
    schema.last_goal_directed_read_applied_bound_positions =
        deduce_rule_head_demand_copy_bound_positions(
            applied_head_demand,
            &schema.last_goal_directed_read_applied_bound_count
        );
    schema.last_goal_directed_read_step_counters =
        deduce_observed_rule_counters_copy_steps(
            observed_rule_counters,
            &schema.last_goal_directed_read_step_count
        );
}

fn void deduce_goal_directed_read_subject_key_capture_release(
    DeduceGoalDirectedReadSubjectKeyCapture* capture
) {
    if (capture == null) return;
    for (usz i = 0; i < capture.count; i++) {
        if (capture.keys[i] != null) {
            mem::free(capture.keys[i]);
            capture.keys[i] = null;
        }
        capture.key_lens[i] = 0;
    }
    capture.count = 0;
    capture.complete = true;
}

fn void deduce_goal_directed_read_subject_key_capture_record(
    DeduceGoalDirectedReadSubjectKeyCapture* capture,
    void* subject_key,
    usz subject_key_len
) {
    if (capture == null || subject_key == null || subject_key_len == 0) {
        if (capture != null) capture.complete = false;
        return;
    }
    if (capture.count >= DEDUCE_GOAL_DIRECTED_READ_SUBJECT_KEY_LIMIT) {
        capture.complete = false;
        return;
    }

    char* copied = (char*)mem::malloc(subject_key_len);
    if (copied == null) {
        capture.complete = false;
        return;
    }
    mem::copy(copied, subject_key, subject_key_len);
    capture.keys[capture.count] = copied;
    capture.key_lens[capture.count] = subject_key_len;
    capture.count++;
}

fn void deduce_relation_schema_note_goal_directed_read_subject_keys(
    DeduceRelationSchema* schema,
    DeduceGoalDirectedReadSubjectKeyCapture* capture
) {
    if (schema == null) return;
    deduce_goal_directed_read_subject_key_capture_release(&schema.last_goal_directed_read_subject_keys);
    if (capture == null) return;

    for (usz i = 0; i < capture.count; i++) {
        schema.last_goal_directed_read_subject_keys.keys[i] = capture.keys[i];
        schema.last_goal_directed_read_subject_keys.key_lens[i] = capture.key_lens[i];
        capture.keys[i] = null;
        capture.key_lens[i] = 0;
    }
    schema.last_goal_directed_read_subject_keys.count = capture.count;
    schema.last_goal_directed_read_subject_keys.complete = capture.complete;
    capture.count = 0;
    capture.complete = true;
}

fn void deduce_db_note_goal_directed_read(
    DeduceDb* db,
    SymbolId relation_name,
    SymbolId execution_path,
    SymbolId surface,
    long selector_rule_index,
    DeduceRuleHeadDemand* requested_head_demand = null,
    DeduceRuleHeadDemand* applied_head_demand = null,
    DeduceObservedRuleCounters* observed_rule_counters = null,
    SymbolId fallback_reason = (SymbolId)0
) {
    if (db == null || relation_name == (SymbolId)0 || execution_path == (SymbolId)0) return;
    db.last_goal_directed_read_execution_path = execution_path;
    db.last_goal_directed_read_surface = surface;
    db.last_goal_directed_read_relation = relation_name;
    db.last_goal_directed_read_fallback_reason = fallback_reason;
    db.last_goal_directed_read_selector_rule_index = selector_rule_index;
    db.last_goal_directed_read_mutation_epoch = db.incremental_mutation_epoch;
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
    db.last_goal_directed_read_requested_bound_positions =
        deduce_rule_head_demand_copy_bound_positions(
            requested_head_demand,
            &db.last_goal_directed_read_requested_bound_count
        );
    db.last_goal_directed_read_applied_bound_positions =
        deduce_rule_head_demand_copy_bound_positions(
            applied_head_demand,
            &db.last_goal_directed_read_applied_bound_count
        );
    db.last_goal_directed_read_step_counters =
        deduce_observed_rule_counters_copy_steps(
            observed_rule_counters,
            &db.last_goal_directed_read_step_count
        );
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, relation_name);
    deduce_relation_schema_note_goal_directed_read(
        db,
        schema,
        execution_path,
        surface,
        selector_rule_index,
        requested_head_demand,
        applied_head_demand,
        observed_rule_counters,
        fallback_reason
    );
}

fn bool deduce_db_mark_dirty_with_rule_dependents(DeduceDb* db, SymbolId seed_predicate) {
    if (db == null || seed_predicate == (SymbolId)0) return false;
    usz queue_cap = db.rule_signature_count + db.relation_schema_count + 1;
    if (queue_cap == 0) queue_cap = 1;
    SymbolId[] queue = mem::new_array(SymbolId, queue_cap);
    if (queue.ptr == null) return false;
    defer mem::free(queue.ptr);

    usz queue_len = 1;
    usz queue_idx = 0;
    queue[0] = seed_predicate;

    while (queue_idx < queue_len) {
        SymbolId current = queue[queue_idx++];
        if (!deduce_db_mark_incremental_dirty_predicate(db, current)) return false;

        for (usz r = 0; r < db.rule_signature_count; r++) {
            DeduceRuleSignature* sig = &db.rule_signatures[r];
            bool depends_on_current = false;
            for (usz b = 0; b < sig.body_count; b++) {
                if (sig.body_predicates != null && sig.body_predicates[b] == current) {
                    depends_on_current = true;
                    break;
                }
            }
            if (!depends_on_current) continue;
            SymbolId head = sig.head_predicate;
            if (head == (SymbolId)0) continue;
            if (deduce_symbol_id_slice_contains(queue[..queue_len], head)) continue;
            if (queue_len >= queue.len) return false;
            queue[queue_len++] = head;
        }
    }
    return true;
}

fn void deduce_db_note_committed_mutation(Relation* rel, bool destructive_change) {
    if (rel == null || rel.db == null) return;
    DeduceDb* db = rel.db;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    if (destructive_change) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
    if (!deduce_db_mark_dirty_with_rule_dependents(db, rel.name)) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
}

fn Value* deduce_db_note_mutation(Relation* rel, bool destructive_change, Interp* interp) {
    deduce_db_note_committed_mutation(rel, destructive_change);
    return deduce_db_persist_materialized_view_states(rel != null ? rel.db : null, interp);
}

fn Value* deduce_db_note_untracked_write_commit(DeduceDb* db, Interp* interp) {
    if (db == null) return null;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    deduce_db_escalate_incremental_tracking_to_full_recompute(
        db,
        DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
    );
    return deduce_db_persist_materialized_view_states(db, interp);
}

fn Value* deduce_db_note_rule_signature_install(DeduceDb* db, Interp* interp) {
    if (db == null) return null;
    bool has_ready_materialized_views = false;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i])) continue;
        has_ready_materialized_views = true;
        break;
    }
    if (!has_ready_materialized_views) return null;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    deduce_db_clear_pending_incremental_insertions(db);
    db.incremental_full_recompute_required = true;
    deduce_db_note_ready_materialized_views_stale(db, DEDUCE_MATERIALIZED_STALE_RULE_SET_CHANGE);
    return deduce_db_persist_materialized_view_states(db, interp);
}

fn void deduce_db_reset_incremental_tracking(DeduceDb* db) {
    if (db == null) return;
    deduce_db_clear_pending_incremental_insertions(db);
    db.incremental_dirty_count = 0;
    db.incremental_full_recompute_required = false;
}

fn void deduce_db_dependency_edge_counts(DeduceDb* db, long* total_out, long* negated_out) {
    if (total_out != null) *total_out = 0;
    if (negated_out != null) *negated_out = 0;
    if (db == null) return;

    long total = 0;
    long negated = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        for (usz b = 0; b < sig.body_count; b++) {
            total++;
            if (sig.body_negated != null && sig.body_negated[b]) negated++;
        }
    }
    if (total_out != null) *total_out = total;
    if (negated_out != null) *negated_out = negated;
}

fn bool deduce_db_lookup_relation_arity(DeduceDb* db, SymbolId name, usz* arity_out) {
    if (arity_out == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, name);
    if (schema == null) return false;
    *arity_out = schema.arity;
    return true;
}

fn bool deduce_index_column_bound(ulong bound_mask, usz col_pos) @inline {
    usz width = (usz)(ulong.sizeof * 8);
    if (col_pos >= width) return false;
    return (bound_mask & ((ulong)1 << col_pos)) != 0;
}

fn int deduce_schema_select_index_for_bound_mask(DeduceRelationSchema* schema, ulong bound_mask) {
    if (schema == null || schema.indexes == null || schema.index_count == 0) return -1;

    int best_index = -1;
    int best_score = -1;
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* idx = &schema.indexes[i];
        if (idx.column_positions == null || idx.column_count == 0) continue;

        usz prefix_len = 0;
        for (usz c = 0; c < idx.column_count; c++) {
            if (!deduce_index_column_bound(bound_mask, idx.column_positions[c])) break;
            prefix_len++;
        }
        if (prefix_len == 0) continue;

        int score = (int)(prefix_len * 128);
        score -= (int)idx.column_count;
        if (idx.unique) score += 4;
        if (score > best_score) {
            best_score = score;
            best_index = (int)i;
        }
    }
    return best_index;
}
