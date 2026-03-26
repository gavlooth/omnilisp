module lisp;

import std::core::mem;

fn Value* deduce_why_result_build_row_subject(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* subject = make_hashmap(interp, 8);
    if (subject == null || subject.tag != HASHMAP || subject.hashmap_val == null) return null;
    explain_dict_set(subject, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(subject, interp, "tuple", deduce_build_tuple_list(tuple, interp));
    return subject;
}

fn Value* deduce_why_result_build_fact_frame(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* frame = make_hashmap(interp, 8);
    if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) return null;
    explain_dict_set_symbol(frame, interp, "kind", interp.symbols.intern("fact"));
    explain_dict_set(frame, interp, "frame-index", make_int(interp, 0));
    explain_dict_set(frame, interp, "predicate", make_symbol(interp, rel.name));
    explain_dict_set(frame, interp, "tuple", deduce_build_tuple_list(tuple, interp));
    deduce_why_result_maybe_attach_subject_goal_directed_read_context(frame, rel, tuple, interp);
    return frame;
}

fn Value* deduce_why_result_build_rule_step_frame(
    Relation* rel,
    Value*[] tuple,
    long rule_index,
    long step_index,
    Interp* interp
) {
    Value* frame = make_hashmap(interp, 12);
    if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) return null;
    explain_dict_set_symbol(frame, interp, "kind", interp.symbols.intern("rule-step"));
    explain_dict_set(frame, interp, "frame-index", make_int(interp, 0));
    explain_dict_set(frame, interp, "predicate", make_symbol(interp, rel.name));
    explain_dict_set(frame, interp, "tuple", deduce_build_tuple_list(tuple, interp));
    explain_dict_set(frame, interp, "rule-index", make_int(interp, rule_index));
    explain_dict_set(frame, interp, "step-index", make_int(interp, step_index));
    deduce_why_result_maybe_attach_subject_goal_directed_read_context(frame, rel, tuple, interp);
    return frame;
}

fn Value* deduce_why_result_build_seed_path(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* path = make_hashmap(interp, 12);
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return null;
    Value* frame = deduce_why_result_build_fact_frame(rel, tuple, interp);
    if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) return null;
    Value* support = make_cons(interp, frame, make_nil(interp));
    explain_dict_set(path, interp, "path-id", make_int(interp, 0));
    explain_dict_set_symbol(path, interp, "path-kind", interp.symbols.intern("seed"));
    explain_dict_set(path, interp, "head-predicate", make_symbol(interp, rel.name));
    explain_dict_set(path, interp, "head-tuple", deduce_build_tuple_list(tuple, interp));
    explain_dict_set(path, interp, "rule-index", make_nil(interp));
    explain_dict_set(path, interp, "support", support);
    explain_dict_set(path, interp, "truncated", explain_bool_value(interp, false));
    return path;
}

fn Value* deduce_why_result_dict_get(
    Value* dict,
    char[] key,
    Interp* interp
) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null || interp == null) {
        return null;
    }
    Value* key_val = make_symbol(interp, interp.symbols.intern(key));
    if (key_val == null) return null;
    return hashmap_get(dict.hashmap_val, key_val);
}

fn Value* deduce_why_result_find_common_support_goal_directed_read_context(
    Value* support,
    Interp* interp
) {
    if (interp == null) return null;

    Value* first_context = null;
    SymbolId context_relation = (SymbolId)0;
    Value* cur = support;
    while (is_cons(cur)) {
        Value* frame = car(cur);
        Value* context = deduce_why_result_dict_get(frame, "goal-directed-read-context", interp);
        if (context != null && !is_nil(context)) {
            Value* relation = deduce_why_result_dict_get(context, "relation", interp);
            if (relation == null || relation.tag != SYMBOL) {
                return make_nil(interp);
            }
            if (first_context == null) {
                first_context = context;
                context_relation = relation.sym_val;
            } else {
                if (relation.sym_val != context_relation) return make_nil(interp);
            }
        }
        cur = cdr(cur);
    }
    return first_context != null ? first_context : make_nil(interp);
}

fn bool deduce_why_result_symbol_list_contains(
    Value* list,
    SymbolId symbol
) {
    Value* cur = list;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        if (entry != null && entry.tag == SYMBOL && entry.sym_val == symbol) return true;
        cur = cdr(cur);
    }
    return false;
}

fn Value* deduce_why_result_build_support_goal_directed_read_contexts(
    Value* support,
    long* unique_count_out,
    Interp* interp
) {
    if (unique_count_out != null) *unique_count_out = 0;
    if (interp == null) return null;

    Value* rev_contexts = make_nil(interp);
    Value* seen_relations = make_nil(interp);
    long unique_count = 0;
    Value* cur = support;
    while (is_cons(cur)) {
        Value* frame = car(cur);
        Value* context = deduce_why_result_dict_get(frame, "goal-directed-read-context", interp);
        if (context != null && !is_nil(context)) {
            Value* relation = deduce_why_result_dict_get(context, "relation", interp);
            if (relation == null || relation.tag != SYMBOL) {
                return make_nil(interp);
            }
            if (!deduce_why_result_symbol_list_contains(seen_relations, relation.sym_val)) {
                seen_relations = make_cons(interp, relation, seen_relations);
                rev_contexts = make_cons(interp, context, rev_contexts);
                unique_count++;
            }
        }
        cur = cdr(cur);
    }

    if (unique_count_out != null) *unique_count_out = unique_count;
    return deduce_why_result_concat_support(rev_contexts, make_nil(interp), interp);
}

fn void deduce_why_result_maybe_attach_common_support_goal_directed_read_context(
    Value* path,
    Value* support,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null || interp == null) return;
    Value* existing = deduce_why_result_dict_get(path, "goal-directed-read-context", interp);
    if (existing != null && !is_nil(existing)) return;

    Value* context = deduce_why_result_find_common_support_goal_directed_read_context(support, interp);
    if (context != null && !is_nil(context)) {
        explain_dict_set(path, interp, "goal-directed-read-context", context);
    }
}

fn void deduce_why_result_maybe_attach_support_goal_directed_read_contexts(
    Value* path,
    Value* support,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null || interp == null) return;
    Value* existing = deduce_why_result_dict_get(path, "goal-directed-read-context", interp);
    if (existing != null && !is_nil(existing)) return;

    long unique_count = 0;
    Value* contexts = deduce_why_result_build_support_goal_directed_read_contexts(
        support,
        &unique_count,
        interp
    );
    if (contexts != null && !is_nil(contexts) && unique_count > 1) {
        explain_dict_set(path, interp, "goal-directed-read-contexts", contexts);
    }
}

fn Value* deduce_why_result_build_derived_path(
    Relation* rel,
    Value*[] tuple,
    long rule_index,
    Value* support,
    Interp* interp
) {
    Value* path = make_hashmap(interp, 12);
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return null;
    explain_dict_set(path, interp, "path-id", make_int(interp, 0));
    explain_dict_set_symbol(path, interp, "path-kind", interp.symbols.intern("derived"));
    explain_dict_set(path, interp, "head-predicate", make_symbol(interp, rel.name));
    explain_dict_set(path, interp, "head-tuple", deduce_build_tuple_list(tuple, interp));
    explain_dict_set(path, interp, "rule-index", make_int(interp, rule_index));
    explain_dict_set(path, interp, "support", support);
    explain_dict_set(path, interp, "truncated", explain_bool_value(interp, false));
    return path;
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

    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) return out;
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "execution-path", make_symbol(interp, schema.last_goal_directed_read_execution_path));
    explain_dict_set(out, interp, "surface",
        schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "fallback-reason",
        schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "selector-rule-index",
        schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch));
    explain_dict_set(out, interp, "requested-bound-count", make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count));
    explain_dict_set(out, interp, "requested-bound-positions",
        deduce_bound_positions_payload(
            schema.last_goal_directed_read_requested_bound_positions,
            schema.last_goal_directed_read_requested_bound_count,
            interp
        ));
    explain_dict_set(out, interp, "applied-bound-count", make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count));
    explain_dict_set(out, interp, "applied-bound-positions",
        deduce_bound_positions_payload(
            schema.last_goal_directed_read_applied_bound_positions,
            schema.last_goal_directed_read_applied_bound_count,
            interp
        ));
    explain_dict_set(out, interp, "step-counters",
        schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp));
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

fn void deduce_why_result_maybe_attach_path_goal_directed_read_context(
    Value* path,
    Relation* rel,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return;
    Value* context = deduce_why_result_build_path_goal_directed_read_context(rel, subject_key, subject_key_len, interp);
    if (context != null && !is_nil(context)) {
        explain_dict_set(path, interp, "goal-directed-read-context", context);
    }
}

fn void deduce_why_result_maybe_attach_subject_goal_directed_read_context(
    Value* node,
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    if (node == null || node.tag != HASHMAP || node.hashmap_val == null ||
        rel == null || tuple.len != rel.col_count || interp == null) {
        return;
    }

    char[4096] key_buf;
    usz subject_key_len = 0;
    usz? key_len = encode_tuple(tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        (void)err;
        return;
    }
    subject_key_len = key_len;

    Value* context = deduce_why_result_build_path_goal_directed_read_context(
        rel,
        &key_buf[0],
        subject_key_len,
        interp
    );
    if (context != null && !is_nil(context)) {
        explain_dict_set(node, interp, "goal-directed-read-context", context);
    }
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
    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) return out;
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("why-result"));
    explain_dict_set_symbol(out, interp, "status", status);
    explain_dict_set_symbol(out, interp, "surface-kind", interp.symbols.intern("provenance-snapshot"));
    explain_dict_set_symbol(out, interp, "subject-kind", interp.symbols.intern("row"));
    explain_dict_set(out, interp, "subject", deduce_why_result_build_row_subject(rel, tuple, interp));
    explain_dict_set(out, interp, "path-count", make_int(interp, path_count));
    explain_dict_set(out, interp, "max-depth", make_int(interp, max_depth));
    explain_dict_set(out, interp, "truncated", explain_bool_value(interp, truncated));
    explain_dict_set(out, interp, "paths", paths);
    Value* goal_directed_read_context = deduce_why_result_build_goal_directed_read_context(rel, interp);
    if (goal_directed_read_context != null &&
        !(is_nil(goal_directed_read_context))) {
        explain_dict_set(out, interp, "goal-directed-read-context", goal_directed_read_context);
    }
    if (code != (SymbolId)0) {
        explain_dict_set(out, interp, "code", make_symbol(interp, code));
    }
    return out;
}

struct DeduceWhyResultVisitedSubject {
    SymbolId                      predicate;
    Value*[]                      tuple;
    DeduceWhyResultVisitedSubject* parent;
}

fn bool deduce_why_result_visited_contains(
    DeduceWhyResultVisitedSubject* visited,
    SymbolId predicate,
    Value*[] tuple
) {
    DeduceWhyResultVisitedSubject* cur = visited;
    while (cur != null) {
        if (cur.predicate == predicate && cur.tuple.len == tuple.len) {
            bool same = true;
            foreach (i, val : tuple) {
                if (deduce_compare_value(cur.tuple[i], val) != 0) {
                    same = false;
                    break;
                }
            }
            if (same) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool deduce_why_result_bind_head_tuple_variables(
    DeduceRuleSignature* sig,
    Value*[] head_tuple,
    bool[]* bound_out,
    Value*[]* bound_values_out
) {
    if (bound_out != null) *bound_out = {};
    if (bound_values_out != null) *bound_values_out = {};
    if (sig == null || bound_out == null || bound_values_out == null) return false;
    if (sig.head_term_count != head_tuple.len) return false;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null) return false;

    usz var_count = (usz)sig.max_var_id + 1;
    bool[] bound = mem::new_array(bool, var_count);
    if (var_count > 0 && bound.ptr == null) return false;
    Value*[] bound_values = mem::new_array(Value*, var_count);
    if (var_count > 0 && bound_values.ptr == null) {
        if (bound.ptr != null) mem::free(bound.ptr);
        return false;
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        switch (sig.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_LITERAL:
                if (sig.head_term_literals == null ||
                    deduce_compare_value(head_tuple[i], sig.head_term_literals[i]) != 0) {
                    if (bound.ptr != null) mem::free(bound.ptr);
                    if (bound_values.ptr != null) mem::free(bound_values.ptr);
                    return false;
                }
                break;
            case DEDUCE_RULE_TERM_VARIABLE:
                uint var_id = sig.head_term_var_ids[i];
                if ((usz)var_id >= var_count) {
                    if (bound.ptr != null) mem::free(bound.ptr);
                    if (bound_values.ptr != null) mem::free(bound_values.ptr);
                    return false;
                }
                if (bound[var_id]) {
                    if (deduce_compare_value(bound_values[var_id], head_tuple[i]) != 0) {
                        if (bound.ptr != null) mem::free(bound.ptr);
                        if (bound_values.ptr != null) mem::free(bound_values.ptr);
                        return false;
                    }
                } else {
                    bound[var_id] = true;
                    bound_values[var_id] = head_tuple[i];
                }
                break;
            case DEDUCE_RULE_TERM_AGGREGATE:
                if (bound.ptr != null) mem::free(bound.ptr);
                if (bound_values.ptr != null) mem::free(bound_values.ptr);
                return false;
        }
    }

    *bound_out = bound;
    *bound_values_out = bound_values;
    return true;
}

fn bool deduce_why_result_find_supported_extensional_rule(
    DeduceDb* db,
    SymbolId head_predicate,
    long* rule_index_out,
    SymbolId* source_predicate_out,
    DeduceRuleSignature** sig_out
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig_out != null) *sig_out = null;
    if (db == null || head_predicate == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    long matched_rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    long head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != head_predicate) continue;
        head_rule_count++;
        if (head_rule_count > 1) return false;
        if (sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) return false;
        if (sig.body_predicates == null) return false;
        if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
            sig.body_term_offsets == null || sig.body_term_counts == null ||
            sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
            return false;
        }
        source_predicate = sig.body_predicates[0];
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_predicates[step] == (SymbolId)0) return false;
            if (sig.body_negated != null && sig.body_negated[step]) return false;
            DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, sig.body_predicates[step]);
            if (source_schema == null || source_schema.dropped ||
                deduce_relation_schema_has_derived_rules(db, source_schema)) {
                return false;
            }
        }
        matched = sig;
        matched_rule_index = (long)i;
    }

    if (matched == null || matched_rule_index < 0 || source_predicate == (SymbolId)0) return false;

    if (rule_index_out != null) *rule_index_out = matched_rule_index;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    if (sig_out != null) *sig_out = matched;
    return true;
}

fn bool deduce_why_result_find_supported_single_rule(
    DeduceDb* db,
    SymbolId head_predicate,
    long* rule_index_out,
    SymbolId* source_predicate_out,
    DeduceRuleSignature** sig_out
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig_out != null) *sig_out = null;
    if (db == null || head_predicate == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    long matched_rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    long head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != head_predicate) continue;
        head_rule_count++;
        if (head_rule_count > 1) return false;
        if (sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) return false;
        if (sig.body_predicates == null) return false;
        if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
            sig.body_term_offsets == null || sig.body_term_counts == null ||
            sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
            return false;
        }
        source_predicate = sig.body_predicates[0];
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_predicates[step] == (SymbolId)0) return false;
            if (sig.body_negated != null && sig.body_negated[step]) return false;
        }
        matched = sig;
        matched_rule_index = (long)i;
    }

    if (matched == null || matched_rule_index < 0 || source_predicate == (SymbolId)0) return false;
    if (rule_index_out != null) *rule_index_out = matched_rule_index;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    if (sig_out != null) *sig_out = matched;
    return true;
}

fn bool deduce_why_result_rule_has_supported_nonrecursive_shape(
    DeduceDb* db,
    DeduceRuleSignature* sig
) {
    (void)db;
    if (sig == null || sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) {
        return false;
    }
    if (sig.body_predicates == null) return false;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
        sig.body_term_offsets == null || sig.body_term_counts == null ||
        sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
        return false;
    }

    foreach (i, predicate : sig.body_predicates[..sig.body_count]) {
        if (predicate == (SymbolId)0) return false;
        if (sig.body_negated != null && sig.body_negated[i]) return false;
    }
    return true;
}

fn bool deduce_why_result_clone_bindings(
    bool[] bound,
    Value*[] bound_values,
    bool[]* bound_out,
    Value*[]* bound_values_out
) {
    if (bound_out != null) *bound_out = {};
    if (bound_values_out != null) *bound_values_out = {};
    if (bound.len != bound_values.len || bound_out == null || bound_values_out == null) return false;

    bool[] next_bound = mem::new_array(bool, bound.len);
    if (bound.len > 0 && next_bound.ptr == null) return false;
    Value*[] next_values = mem::new_array(Value*, bound_values.len);
    if (bound_values.len > 0 && next_values.ptr == null) {
        if (next_bound.ptr != null) mem::free(next_bound.ptr);
        return false;
    }

    if (bound.len > 0) mem::copy(next_bound.ptr, bound.ptr, bool.sizeof * bound.len);
    if (bound_values.len > 0) mem::copy(next_values.ptr, bound_values.ptr, Value*.sizeof * bound_values.len);
    *bound_out = next_bound;
    *bound_values_out = next_values;
    return true;
}

fn Value* deduce_why_result_concat_support(Value* front, Value* tail, Interp* interp) {
    Value* rev = make_nil(interp);
    Value* cur = front;
    while (is_cons(cur)) {
        rev = make_cons(interp, car(cur), rev);
        cur = cdr(cur);
    }
    Value* out = tail == null ? make_nil(interp) : tail;
    while (is_cons(rev)) {
        out = make_cons(interp, car(rev), out);
        rev = cdr(rev);
    }
    return out;
}

fn bool deduce_why_result_try_exact_one_rule_extensional_support(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    Value*[] tuple,
    long* rule_index_out,
    long* path_count_out,
    long* max_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (path_count_out != null) *path_count_out = 0;
    if (max_depth_out != null) *max_depth_out = 0;
    if (first_support_out != null) *first_support_out = make_nil(interp);
    if (db == null || txn == null || predicate == (SymbolId)0 ||
        path_count_out == null || first_support_out == null || interp == null) {
        return false;
    }

    long rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    DeduceRuleSignature* sig = null;
    if (!deduce_why_result_find_supported_extensional_rule(
            db,
            predicate,
            &rule_index,
            &source_predicate,
            &sig
        )) {
        return false;
    }

    bool[] bound = {};
    Value*[] bound_values = {};
    if (!deduce_why_result_bind_head_tuple_variables(sig, tuple, &bound, &bound_values)) {
        return false;
    }
    defer if (bound.ptr != null) mem::free(bound.ptr);
    defer if (bound_values.ptr != null) mem::free(bound_values.ptr);

    Value* first_support = make_nil(interp);
    long path_count = 0;
    long body_depth = 0;
    deduce_why_result_search_extensional_support_path(
        db,
        txn,
        sig,
        0,
        bound,
        bound_values,
        null,
        &path_count,
        &body_depth,
        &first_support,
        interp
    );
    if (path_count == 0) return false;

    if (rule_index_out != null) *rule_index_out = rule_index;
    *path_count_out = path_count;
    if (max_depth_out != null) *max_depth_out = body_depth + 1;
    *first_support_out = first_support;
    return true;
}

fn bool deduce_why_result_try_supported_single_rule_support(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* max_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (path_count_out != null) *path_count_out = 0;
    if (max_depth_out != null) *max_depth_out = 0;
    if (first_support_out != null) *first_support_out = make_nil(interp);
    if (db == null || txn == null || sig == null || path_count_out == null ||
        first_support_out == null || interp == null) {
        return false;
    }

    bool[] bound = {};
    Value*[] bound_values = {};
    if (!deduce_why_result_bind_head_tuple_variables(sig, tuple, &bound, &bound_values)) {
        return false;
    }
    defer if (bound.ptr != null) mem::free(bound.ptr);
    defer if (bound_values.ptr != null) mem::free(bound_values.ptr);

    Value* first_support = make_nil(interp);
    long path_count = 0;
    long body_depth = 0;
    deduce_why_result_search_extensional_support_path(
        db,
        txn,
        sig,
        0,
        bound,
        bound_values,
        visited,
        &path_count,
        &body_depth,
        &first_support,
        interp
    );
    if (path_count == 0) return false;

    *path_count_out = path_count;
    if (max_depth_out != null) *max_depth_out = body_depth + 1;
    *first_support_out = first_support;
    return true;
}

fn bool deduce_why_result_search_extensional_support_path(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    usz step_index,
    bool[] bound,
    Value*[] bound_values,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* first_body_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (db == null || txn == null || sig == null || path_count_out == null ||
        first_body_depth_out == null || first_support_out == null || interp == null) {
        return false;
    }

    if (step_index >= sig.body_count) {
        *path_count_out = *path_count_out + 1;
        if (*path_count_out == 1) {
            *first_support_out = make_nil(interp);
            *first_body_depth_out = 0;
        }
        return *path_count_out >= 2;
    }

    MdbDbi source_dbi = 0;
    DeduceRelationSchema* source_schema = null;
    if (!deduce_rule_open_existing_dbi(
            db,
            txn,
            sig.body_predicates[step_index],
            &source_dbi,
            &source_schema,
            interp
        )) {
        return false;
    }
    if (source_schema == null) return false;

    Relation source_rel = deduce_relation_view_from_schema(db, source_schema, source_dbi);
    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open_in_txn(
            &it,
            &source_rel,
            interp,
            txn,
            null,
            0,
            false,
            {},
            {},
            false
        )) {
        return false;
    }
    defer deduce_tuple_iterator_close(&it);

    while (deduce_tuple_iterator_next(&it)) {
        bool[] next_bound = {};
        Value*[] next_values = {};
        if (!deduce_why_result_clone_bindings(bound, bound_values, &next_bound, &next_values)) {
            return false;
        }
        defer {
            if (next_bound.ptr != null) mem::free(next_bound.ptr);
            if (next_values.ptr != null) mem::free(next_values.ptr);
        }

        if (!deduce_rule_match_positive_tuple(sig, step_index, it.vals, next_bound, next_values)) {
            continue;
        }

        Value* matched_support = make_nil(interp);
        long matched_path_count = 1;
        long matched_depth = 1;
        if (deduce_relation_schema_has_derived_rules(db, source_schema)) {
            if (deduce_why_result_visited_contains(visited, source_rel.name, it.vals)) {
                continue;
            }
            DeduceWhyResultVisitedSubject child_visited = {
                .predicate = source_rel.name,
                .tuple = it.vals,
                .parent = visited
            };
            long child_rule_index = -1;
            if (!deduce_why_result_try_supported_subject_support(
                    db,
                    txn,
                    source_rel.name,
                    it.vals,
                    &child_visited,
                    &child_rule_index,
                    &matched_path_count,
                    &matched_depth,
                    &matched_support,
                    interp
                ) || matched_path_count != 1) {
                continue;
            }
            if (child_rule_index < 0) continue;
            Value* rule_step = deduce_why_result_build_rule_step_frame(
                &source_rel,
                it.vals,
                child_rule_index,
                (long)step_index,
                interp
            );
            if (rule_step == null || rule_step.tag != HASHMAP || rule_step.hashmap_val == null) {
                return false;
            }
            matched_support = deduce_why_result_concat_support(
                matched_support,
                make_cons(interp, rule_step, make_nil(interp)),
                interp
            );
        } else {
            Value* frame = deduce_why_result_build_fact_frame(&source_rel, it.vals, interp);
            if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) {
                return false;
            }
            matched_support = make_cons(interp, frame, make_nil(interp));
        }

        long count_before = *path_count_out;
        long tail_body_depth = 0;
        bool stop = deduce_why_result_search_extensional_support_path(
            db,
            txn,
            sig,
            step_index + 1,
            next_bound,
            next_values,
            visited,
            path_count_out,
            &tail_body_depth,
            first_support_out,
            interp
        );
        if (count_before == 0 && *path_count_out > 0) {
            *first_support_out = deduce_why_result_concat_support(matched_support, *first_support_out, interp);
            *first_body_depth_out = matched_depth > tail_body_depth ? matched_depth : tail_body_depth;
        }
        if (stop) return true;
    }

    return false;
}

fn bool deduce_why_result_try_supported_subject_support(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
    long* rule_index_out,
    long* path_count_out,
    long* max_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (path_count_out != null) *path_count_out = 0;
    if (max_depth_out != null) *max_depth_out = 0;
    if (first_support_out != null) *first_support_out = make_nil(interp);
    if (db == null || txn == null || predicate == (SymbolId)0 ||
        path_count_out == null || first_support_out == null || interp == null) {
        return false;
    }

    usz head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (db.rule_signatures[i].head_predicate == predicate) head_rule_count++;
    }
    if (head_rule_count == 0) return false;

    if (head_rule_count == 1) {
        long direct_rule_index = -1;
        SymbolId source_predicate = (SymbolId)0;
        DeduceRuleSignature* source_sig = null;
        if (deduce_why_result_find_supported_single_rule(
                db,
                predicate,
                &direct_rule_index,
                &source_predicate,
                &source_sig
            )) {
                long path_count = 0;
                long max_depth = 0;
                Value* first_support = make_nil(interp);
                if (deduce_why_result_try_supported_single_rule_support(
                        db,
                        txn,
                        source_sig,
                        tuple,
                        visited,
                        &path_count,
                        &max_depth,
                        &first_support,
                        interp
                    ) && path_count > 0) {
                        if (rule_index_out != null) *rule_index_out = direct_rule_index;
                        *path_count_out = path_count;
                        if (max_depth_out != null) *max_depth_out = max_depth;
                        *first_support_out = first_support;
                        return true;
                }
        }
    }

    long total_path_count = 0;
    long first_rule_index = -1;
    long first_max_depth = 0;
    Value* first_support = make_nil(interp);
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

        long rule_path_count = 0;
        long rule_max_depth = 0;
        Value* rule_support = make_nil(interp);
        if (!deduce_why_result_try_supported_single_rule_support(
                db,
                txn,
                sig,
                tuple,
                visited,
                &rule_path_count,
                &rule_max_depth,
                &rule_support,
                interp
            )) {
            continue;
        }
        if (rule_path_count <= 0) continue;
        if (total_path_count == 0) {
            first_rule_index = (long)i;
            first_max_depth = rule_max_depth;
            first_support = rule_support;
        }
        total_path_count += rule_path_count;
        if (total_path_count >= 2) {
            total_path_count = 2;
            break;
        }
    }

    if (total_path_count == 0 || first_rule_index < 0) return false;
    if (rule_index_out != null) *rule_index_out = first_rule_index;
    *path_count_out = total_path_count;
    if (max_depth_out != null) *max_depth_out = first_max_depth;
    *first_support_out = first_support;
    return true;
}

fn Value* prim_deduce_why_result(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 1) {
        return deduce_raise(interp, "deduce/why-result-expected-relation-values",
            "deduce/why-result: expected (deduce 'why-result relation val...)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/why-result-relation-not-handle",
            "deduce/why-result: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/why-result-relation-handle-closed",
            "deduce/why-result: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/why-result-relation-not-deduce-relation",
            "deduce/why-result: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/why-result-database-not-open",
            "deduce/why-result: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/why-result-relation-not-found",
            "deduce/why-result: relation schema is not registered");
    }
    if (rel.dropped || schema.dropped) {
        return deduce_raise(interp, "deduce/why-result-relation-dropped",
            "deduce/why-result: relation is dropped");
    }

    Value*[] tuple = args[1..];
    if (tuple.len != rel.col_count) {
        return deduce_raise(interp, "deduce/why-result-arity-mismatch",
            "deduce/why-result: wrong number of values for relation");
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(interp, "deduce/why-result-tuple-too-large",
            "deduce/why-result: tuple too large for buffer");
    }
    usz encoded_key_len = key_len;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/why-result-transaction-failed",
            "deduce/why-result: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    MdbVal key = { .mv_size = encoded_key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    bool found = mdb_get(txn, rel.dbi, &key, &data) == MDB_SUCCESS;
    if (!found) {
        return deduce_why_result_build_payload(
            rel,
            tuple,
            interp.symbols.intern("missing"),
            make_nil(interp),
            0,
            0,
            false,
            interp
        );
    }

    if (deduce_relation_schema_has_derived_rules(rel.db, schema)) {
        DeduceWhyResultVisitedSubject root_subject = {
            .predicate = rel.name,
            .tuple = tuple,
            .parent = null
        };
        long rule_index = -1;
        long path_count = 0;
        long max_depth = 0;
        Value* first_support = make_nil(interp);
        if (deduce_why_result_try_supported_subject_support(
                rel.db,
                txn,
                rel.name,
                tuple,
                &root_subject,
                &rule_index,
                &path_count,
                &max_depth,
                &first_support,
                interp
            ) && path_count > 0 && rule_index >= 0) {
            Value* path = deduce_why_result_build_derived_path(
                rel,
                tuple,
                rule_index,
                first_support,
                interp
            );
            deduce_why_result_maybe_attach_path_goal_directed_read_context(
                path,
                rel,
                &key_buf[0],
                encoded_key_len,
                interp
            );
            deduce_why_result_maybe_attach_common_support_goal_directed_read_context(
                path,
                first_support,
                interp
            );
            deduce_why_result_maybe_attach_support_goal_directed_read_contexts(
                path,
                first_support,
                interp
            );
            Value* paths = make_cons(interp, path, make_nil(interp));
            Value* out = deduce_why_result_build_payload(
                rel,
                tuple,
                interp.symbols.intern(path_count > 1 ? "partial" : "ok"),
                paths,
                1,
                max_depth,
                path_count > 1,
                interp
            );
            return out;
        }

        return deduce_why_result_build_payload(
            rel,
            tuple,
            interp.symbols.intern("error"),
            make_nil(interp),
            0,
            0,
            false,
            interp,
            interp.symbols.intern("deduce/why-result-derived-subject-not-yet-supported")
        );
    }

    Value* path = deduce_why_result_build_seed_path(rel, tuple, interp);
    deduce_why_result_maybe_attach_path_goal_directed_read_context(
        path,
        rel,
        &key_buf[0],
        encoded_key_len,
        interp
    );
    Value* paths = make_cons(interp, path, make_nil(interp));
    Value* out = deduce_why_result_build_payload(
        rel,
        tuple,
        interp.symbols.intern("ok"),
        paths,
        1,
        1,
        false,
        interp
    );
    return out;
}

