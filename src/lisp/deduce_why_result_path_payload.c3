module lisp;

fn Value* deduce_why_result_build_row_subject(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* subject = make_hashmap_checked(interp, 8, "deduce/why-result: out of memory");
    if (subject == null || subject.tag != HASHMAP || subject.hashmap_val == null) return subject;
    if (Value* err = explain_dict_set(subject, interp, "relation", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(subject, interp, "tuple", deduce_build_tuple_list(tuple, interp))) return err;
    return subject;
}

fn Value* deduce_why_result_build_fact_frame(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* frame = make_hashmap_checked(interp, 8, "deduce/why-result: out of memory");
    if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) return frame;
    if (Value* err = explain_dict_set_symbol(frame, interp, "kind", interp.symbols.intern("fact"))) return err;
    if (Value* err = explain_dict_set(frame, interp, "frame-index", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set(frame, interp, "predicate", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(frame, interp, "tuple", deduce_build_tuple_list(tuple, interp))) return err;
    if (Value* err = deduce_why_result_maybe_attach_subject_goal_directed_read_context(frame, rel, tuple, interp)) return err;
    return frame;
}

fn Value* deduce_why_result_build_rule_step_frame(
    Relation* rel,
    Value*[] tuple,
    long rule_index,
    long step_index,
    Interp* interp
) {
    Value* frame = make_hashmap_checked(interp, 12, "deduce/why-result: out of memory");
    if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) return frame;
    if (Value* err = explain_dict_set_symbol(frame, interp, "kind", interp.symbols.intern("rule-step"))) return err;
    if (Value* err = explain_dict_set(frame, interp, "frame-index", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set(frame, interp, "predicate", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(frame, interp, "tuple", deduce_build_tuple_list(tuple, interp))) return err;
    if (Value* err = explain_dict_set(frame, interp, "rule-index", make_int(interp, rule_index))) return err;
    if (Value* err = explain_dict_set(frame, interp, "step-index", make_int(interp, step_index))) return err;
    if (Value* err = deduce_why_result_maybe_attach_subject_goal_directed_read_context(frame, rel, tuple, interp)) return err;
    return frame;
}

fn Value* deduce_why_result_build_seed_path(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    Value* path = make_hashmap_checked(interp, 12, "deduce/why-result: out of memory");
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return path;
    Value* frame = deduce_why_result_build_fact_frame(rel, tuple, interp);
    if (frame == null || frame.tag == ERROR) return frame;
    if (frame.tag != HASHMAP || frame.hashmap_val == null) return frame;
    Value* support = make_cons(interp, frame, make_nil(interp));
    if (Value* err = explain_dict_set(path, interp, "path-id", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set_symbol(path, interp, "path-kind", interp.symbols.intern("seed"))) return err;
    if (Value* err = explain_dict_set(path, interp, "head-predicate", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(path, interp, "head-tuple", deduce_build_tuple_list(tuple, interp))) return err;
    if (Value* err = explain_dict_set(path, interp, "rule-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(path, interp, "support", support)) return err;
    if (Value* err = explain_dict_set(path, interp, "truncated", make_nil(interp))) return err;
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
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return null;
    Value* key_val = make_symbol(interp, key_sym);
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

fn Value* deduce_why_result_maybe_attach_common_support_goal_directed_read_context(
    Value* path,
    Value* support,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null || interp == null) return null;
    Value* existing = deduce_why_result_dict_get(path, "goal-directed-read-context", interp);
    if (existing != null && !is_nil(existing)) return null;

    Value* context = deduce_why_result_find_common_support_goal_directed_read_context(support, interp);
    if (context != null && !is_nil(context)) {
        return explain_dict_set(path, interp, "goal-directed-read-context", context);
    }
    return null;
}

fn Value* deduce_why_result_maybe_attach_support_goal_directed_read_contexts(
    Value* path,
    Value* support,
    Interp* interp
) {
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null || interp == null) return null;
    Value* existing = deduce_why_result_dict_get(path, "goal-directed-read-context", interp);
    if (existing != null && !is_nil(existing)) return null;

    long unique_count = 0;
    Value* contexts = deduce_why_result_build_support_goal_directed_read_contexts(
        support,
        &unique_count,
        interp
    );
    if (contexts != null && !is_nil(contexts) && unique_count > 1) {
        return explain_dict_set(path, interp, "goal-directed-read-contexts", contexts);
    }
    return null;
}

fn Value* deduce_why_result_build_derived_path(
    Relation* rel,
    Value*[] tuple,
    long rule_index,
    Value* support,
    Interp* interp
) {
    Value* path = make_hashmap_checked(interp, 12, "deduce/why-result: out of memory");
    if (path == null || path.tag != HASHMAP || path.hashmap_val == null) return path;
    if (Value* err = explain_dict_set(path, interp, "path-id", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set_symbol(path, interp, "path-kind", interp.symbols.intern("derived"))) return err;
    if (Value* err = explain_dict_set(path, interp, "head-predicate", make_symbol(interp, rel.name))) return err;
    if (Value* err = explain_dict_set(path, interp, "head-tuple", deduce_build_tuple_list(tuple, interp))) return err;
    if (Value* err = explain_dict_set(path, interp, "rule-index", make_int(interp, rule_index))) return err;
    if (Value* err = explain_dict_set(path, interp, "support", support)) return err;
    if (Value* err = explain_dict_set(path, interp, "truncated", make_nil(interp))) return err;
    return path;
}
