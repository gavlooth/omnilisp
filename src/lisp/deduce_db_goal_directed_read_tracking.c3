module lisp;

import std::core::mem;

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
    if (count > (ulong)usz.max || (usz)count > usz.max / usz.sizeof) return null;

    usz* positions = (usz*)mem::malloc(usz.sizeof * (usz)count);
    if (positions == null) return null;

    usz write_index = 0;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (!head_demand.term_bound[i]) continue;
        if (write_index >= (usz)count) break;
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
