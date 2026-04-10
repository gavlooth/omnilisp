module lisp;

fn bool deduce_why_result_relation_has_positive_self_recursive_rule(Relation* rel) {
    if (rel == null || rel.db == null || rel.name == (SymbolId)0) return false;
    for (usz i = 0; i < rel.db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &rel.db.rule_signatures[i];
        if (sig == null || sig.head_predicate != rel.name || sig.body_count == 0 || sig.body_predicates == null) {
            continue;
        }
        for (usz body_idx = 0; body_idx < sig.body_count; body_idx++) {
            if (sig.body_predicates[body_idx] != rel.name) continue;
            if (sig.body_negated != null && sig.body_negated[body_idx]) continue;
            return true;
        }
    }
    return false;
}

fn Value* deduce_why_result_truncated_value(
    Relation* rel,
    bool truncated,
    Interp* interp
) {
    if (interp == null) return null;
    if (truncated) return make_symbol(interp, interp.sym_true);
    if (deduce_why_result_relation_has_positive_self_recursive_rule(rel)) {
        return make_symbol(interp, interp.sym_false);
    }
    return make_nil(interp);
}

fn Value* deduce_why_result_build_goal_directed_read_context(
    Relation* rel,
    Interp* interp
) {
    if (rel == null || rel.db == null || interp == null) return make_nil(interp);
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null || schema.last_goal_directed_read_execution_path == (SymbolId)0) {
        return make_nil(interp);
    }

    Value* out = make_hashmap_checked(interp, 16, "deduce/why-result: out of memory");
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) return out;
    if (Value* err = explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(out, interp, "execution-path", make_symbol(interp, schema.last_goal_directed_read_execution_path))) return err;
    if (Value* err = explain_dict_set(out, interp, "surface",
        schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "fallback-reason",
        schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "selector-rule-index",
        schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch))) return err;
    if (Value* err = explain_dict_set(out, interp, "requested-bound-count", make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count))) return err;
    if (Value* err = explain_dict_set(out, interp, "requested-bound-positions",
        deduce_bound_positions_payload(
            schema.last_goal_directed_read_requested_bound_positions,
            schema.last_goal_directed_read_requested_bound_count,
            interp
        ))) return err;
    if (Value* err = explain_dict_set(out, interp, "applied-bound-count", make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count))) return err;
    if (Value* err = explain_dict_set(out, interp, "applied-bound-positions",
        deduce_bound_positions_payload(
            schema.last_goal_directed_read_applied_bound_positions,
            schema.last_goal_directed_read_applied_bound_count,
            interp
        ))) return err;
    if (Value* err = explain_dict_set(out, interp, "step-counters",
        schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp))) return err;
    return out;
}

fn Value* deduce_why_result_build_path_goal_directed_read_context(
    Relation* rel,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    if (rel == null || interp == null || subject_key == null || subject_key_len == 0) return make_nil(interp);
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    SymbolId query_surface = interp.symbols.intern("query");
    SymbolId match_surface = interp.symbols.intern("match");
    SymbolId scan_surface = interp.symbols.intern("scan");
    SymbolId scan_range_surface = interp.symbols.intern("scan-range");
    if (schema == null ||
        schema.last_goal_directed_read_execution_path == (SymbolId)0 ||
        (schema.last_goal_directed_read_surface != query_surface &&
            schema.last_goal_directed_read_surface != match_surface &&
            schema.last_goal_directed_read_surface != scan_surface &&
            schema.last_goal_directed_read_surface != scan_range_surface) ||
        !schema.last_goal_directed_read_subject_keys.complete ||
        schema.last_goal_directed_read_subject_keys.count == 0) {
        return make_nil(interp);
    }

    for (usz key_index = 0; key_index < schema.last_goal_directed_read_subject_keys.count; key_index++) {
        if (schema.last_goal_directed_read_subject_keys.key_lens[key_index] != subject_key_len) continue;
        bool match = true;
        for (usz i = 0; i < subject_key_len; i++) {
            if (schema.last_goal_directed_read_subject_keys.keys[key_index][i] != subject_key[i]) {
                match = false;
                break;
            }
        }
        if (match) {
            return deduce_why_result_build_goal_directed_read_context(rel, interp);
        }
    }
    return make_nil(interp);
}

fn Value* deduce_why_result_maybe_attach_path_goal_directed_read_context(
    Value* path,
    Relation* rel,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return null;
    Value* context = deduce_why_result_build_path_goal_directed_read_context(rel, subject_key, subject_key_len, interp);
    if (context != null && context.tag == HASHMAP && context.hashmap_val != null) {
        return explain_dict_set(path, interp, "goal-directed-read-context", context);
    }
    return null;
}

fn Value* deduce_why_result_maybe_attach_subject_goal_directed_read_context(
    Value* node,
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    if (node == null || node.tag != HASHMAP || node.hashmap_val == null ||
        rel == null || tuple.len != rel.col_count || interp == null) {
        return null;
    }

    char[4096] key_buf;
    usz subject_key_len = 0;
    usz? key_len = encode_tuple(tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        (void)err;
        return null;
    }
    subject_key_len = key_len;

    Value* context = deduce_why_result_build_path_goal_directed_read_context(
        rel,
        &key_buf[0],
        subject_key_len,
        interp
    );
    if (context != null && context.tag == HASHMAP && context.hashmap_val != null) {
        return explain_dict_set(node, interp, "goal-directed-read-context", context);
    }
    return null;
}

fn Value* deduce_why_result_build_payload(
    Relation* rel,
    Value*[] tuple,
    SymbolId status,
    Value* paths,
    long path_count,
    long max_depth,
    bool truncated,
    Interp* interp,
    SymbolId code = (SymbolId)0
) {
    Value* out = make_hashmap_checked(interp, 16, "deduce/why-result: out of memory");
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) return out;
    if (Value* err = explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("why-result"))) return err;
    if (Value* err = explain_dict_set_symbol(out, interp, "status", status)) return err;
    if (Value* err = explain_dict_set_symbol(out, interp, "surface-kind", interp.symbols.intern("provenance-snapshot"))) return err;
    if (Value* err = explain_dict_set_symbol(out, interp, "subject-kind", interp.symbols.intern("row"))) return err;
    Value* subject = deduce_why_result_build_row_subject(rel, tuple, interp);
    if (subject != null && subject.tag == ERROR) return subject;
    if (Value* err = explain_dict_set(out, interp, "subject", subject)) return err;
    if (Value* err = explain_dict_set(out, interp, "path-count", make_int(interp, path_count))) return err;
    if (Value* err = explain_dict_set(out, interp, "max-depth", make_int(interp, max_depth))) return err;
    if (Value* err = explain_dict_set(out, interp, "truncated",
        deduce_why_result_truncated_value(rel, truncated, interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "paths", paths)) return err;
    Value* goal_directed_read_context = deduce_why_result_build_goal_directed_read_context(rel, interp);
    if (goal_directed_read_context != null && goal_directed_read_context.tag == ERROR) return goal_directed_read_context;
    if (goal_directed_read_context != null &&
        goal_directed_read_context.tag == HASHMAP &&
        goal_directed_read_context.hashmap_val != null) {
        if (Value* err = explain_dict_set(out, interp, "goal-directed-read-context", goal_directed_read_context)) return err;
    }
    if (code != (SymbolId)0) {
        if (Value* err = explain_dict_set(out, interp, "code", make_symbol(interp, code))) return err;
    }
    return out;
}
