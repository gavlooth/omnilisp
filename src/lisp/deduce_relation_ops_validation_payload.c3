module lisp;

fn Value* deduce_build_tuple_list(Value*[] tuple, Interp* interp) {
    Value* rev = make_nil(interp);
    foreach (value : tuple) {
        rev = make_cons(interp, value == null ? make_nil(interp) : value, rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_build_integrity_column_list(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_cons(interp, make_symbol(interp, column), make_nil(interp));
}

fn Value* deduce_build_integrity_columns_list(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_build_tuple_projection_list(Value*[] tuple, usz[] positions, Interp* interp) {
    if (positions.len == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < positions.len; i++) {
        usz pos = positions[i];
        Value* value = pos < tuple.len ? tuple[pos] : make_nil(interp);
        rev = make_cons(interp, value == null ? make_nil(interp) : value, rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_integrity_make_payload_dict(Interp* interp, uint capacity) @inline {
    Value* data = make_hashmap_checked(interp, capacity, "deduce/integrity: out of memory");
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;
    return data;
}

fn bool deduce_integrity_payload_set(Value* err) @inline {
    return err == null;
}

fn Value* deduce_build_single_column_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    SymbolId column,
    usz column_pos,
    char[] violation_kind_name,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern(violation_kind_name)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(column, interp)))) return null;
    usz[1] positions = { column_pos };
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "key-value",
            column_pos < attempted_tuple.len ? attempted_tuple[column_pos] : make_nil(interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "existing-tuple", deduce_build_tuple_list(existing_tuple, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    return data;
}

fn Value* deduce_build_key_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz key_pos,
    Interp* interp
) {
    Value* data = deduce_build_single_column_integrity_payload(
        rel,
        attempted_tuple,
        existing_tuple,
        rel.key_col,
        key_pos,
        "key-conflict",
        interp
    );
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return data;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "key-column", rel.key_col))) return null;
    return data;
}

fn Value* deduce_build_unique_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz unique_pos,
    Interp* interp
) {
    Value* data = deduce_build_single_column_integrity_payload(
        rel,
        attempted_tuple,
        existing_tuple,
        rel.unique_col,
        unique_pos,
        "unique-conflict",
        interp
    );
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return data;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "unique-column", rel.unique_col))) return null;
    return data;
}

fn Value* deduce_build_composite_unique_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz[] positions,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;
    Value* constraint_values = deduce_build_tuple_projection_list(attempted_tuple, positions, interp);

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("unique-conflict")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "columns", deduce_build_integrity_columns_list(rel.unique_cols, rel.unique_col_count, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "constraint-values", constraint_values))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "key-value", constraint_values))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "existing-tuple", deduce_build_tuple_list(existing_tuple, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "unique-columns", deduce_build_integrity_columns_list(rel.unique_cols, rel.unique_col_count, interp)))) return null;
    return data;
}

fn Value* deduce_build_reference_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    DeduceReferenceConstraint* ref_constraint,
    char[] violation_kind_name,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern(violation_kind_name)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(local_column, interp)))) return null;
    usz[1] positions = { local_column_pos };
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "key-value",
            local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "referenced-relation", ref_constraint.target_relation))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "referenced-column", ref_constraint.target_column))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    return data;
}

fn Value* deduce_build_reference_target_in_use_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId target_column,
    usz target_column_pos,
    SymbolId blocking_relation,
    SymbolId blocking_column,
    Value*[] blocking_tuple,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("reference-target-in-use")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(target_column, interp)))) return null;
    usz[1] positions = { target_column_pos };
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "key-value",
            target_column_pos < attempted_tuple.len ? attempted_tuple[target_column_pos] : make_nil(interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "referenced-column", target_column))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "blocking-relation", blocking_relation))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "blocking-column", blocking_column))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "blocking-tuple", deduce_build_tuple_list(blocking_tuple, interp)))) return null;
    return data;
}

fn Value* deduce_build_check_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    SymbolId predicate,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("check-failed")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(local_column, interp)))) return null;
    usz[1] positions = { local_column_pos };
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "key-value",
            local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "predicate", predicate))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    return data;
}

fn Value* deduce_build_check_constraint_context_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    SymbolId predicate,
    Value* cause,
    Interp* interp
) {
    Value* data = deduce_integrity_make_payload_dict(interp, 16);
    if (data == null) return null;

    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-check-constraint")))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "relation", rel.name))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "column", local_column))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set_symbol(data, interp, "predicate", predicate))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "value",
            local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp)))) return null;
    if (!deduce_integrity_payload_set(explain_dict_set(data, interp, "cause", cause != null ? cause : make_nil(interp)))) return null;
    return data;
}
