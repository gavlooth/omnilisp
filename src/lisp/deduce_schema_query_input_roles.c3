module lisp;

fn Value* deduce_relation_collect_columns_from_args(Relation* rel, Value*[] args, usz start, Interp* interp) {
    SymbolId sym_role_key = interp.symbols.intern("__relation-role-key");
    SymbolId sym_role_unique = interp.symbols.intern("__relation-role-unique");
    SymbolId sym_role_index = interp.symbols.intern("__relation-role-index");
    SymbolId sym_role_ref = interp.symbols.intern("__relation-role-ref");
    SymbolId sym_role_check = interp.symbols.intern("__relation-role-check");
    SymbolId sym_role_materialized = interp.symbols.intern("__relation-role-materialized");
    SymbolId sym_role_materialized_manual = interp.symbols.intern("__relation-role-materialized-manual");
    SymbolId sym_role_materialized_on_read = interp.symbols.intern("__relation-role-materialized-on-read");

    usz i = start;
    while (i < args.len) {
        Value* marker = args[i];
        if (marker == null || marker.tag != SYMBOL) break;
        if (marker.sym_val == sym_role_materialized) {
            rel.materialized_view = true;
            i++;
            if (i < args.len && args[i] != null && args[i].tag == SYMBOL &&
                args[i].sym_val == sym_role_materialized_manual) {
                rel.materialized_refresh_policy = interp.symbols.intern("manual");
                i++;
            } else if (i < args.len && args[i] != null && args[i].tag == SYMBOL &&
                args[i].sym_val == sym_role_materialized_on_read) {
                rel.materialized_refresh_policy = interp.symbols.intern("on-read");
                i++;
            } else {
                rel.materialized_refresh_policy = interp.symbols.intern("manual");
            }
            continue;
        }
        break;
    }

    for (; i < args.len;) {
        bool has_key_role = false;
        bool has_unique_role = false;
        bool has_index_role = false;
        bool has_ref_role = false;
        bool has_check_role = false;
        SymbolId ref_target_relation = 0;
        SymbolId ref_target_column = 0;
        SymbolId check_predicate = 0;
        while (i < args.len) {
            Value* marker = args[i];
            if (marker == null || marker.tag != SYMBOL) break;
            if (marker.sym_val == sym_role_key) {
                has_key_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_unique) {
                has_unique_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_index) {
                has_index_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_ref) {
                if (i + 2 >= args.len ||
                    args[i + 1] == null || args[i + 1].tag != SYMBOL ||
                    args[i + 2] == null || args[i + 2].tag != SYMBOL) {
                    return deduce_raise(interp, "deduce/define-relation-reference-invalid", "__define-relation: ref role expects target relation and target column symbols");
                }
                has_ref_role = true;
                ref_target_relation = args[i + 1].sym_val;
                ref_target_column = args[i + 2].sym_val;
                i += 3;
                continue;
            }
            if (marker.sym_val == sym_role_check) {
                if (i + 1 >= args.len || args[i + 1] == null || args[i + 1].tag != SYMBOL) {
                    return deduce_raise(interp, "deduce/define-relation-check-invalid", "__define-relation: check role expects predicate symbol");
                }
                has_check_role = true;
                check_predicate = args[i + 1].sym_val;
                i += 2;
                continue;
            }
            break;
        }

        if (i >= args.len) {
            return deduce_raise(interp, "deduce/define-relation-column-not-symbol", "__define-relation: column arguments must be symbols");
        }

        Value* col = args[i];
        if (col == null || col.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/define-relation-column-not-symbol", "__define-relation: column arguments must be symbols");
        }
        if (!deduce_relation_ensure_column_capacity(rel, rel.col_count + 1)) {
            return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
        }
        rel.columns[rel.col_count++] = col.sym_val;
        if (has_key_role) rel.key_col = col.sym_val;
        if (has_unique_role) {
            Value* unique_err = deduce_relation_note_unique_column(rel, col.sym_val, interp);
            if (unique_err != null) return unique_err;
        }
        if (has_ref_role) {
            Value* ref_err = deduce_relation_note_reference_constraint(rel, col.sym_val, ref_target_relation, ref_target_column, interp);
            if (ref_err != null) return ref_err;
        }
        if (has_check_role) {
            Value* check_err = deduce_relation_note_check_constraint(rel, col.sym_val, check_predicate, interp);
            if (check_err != null) return check_err;
        }
        if (has_index_role) rel.index_col = col.sym_val;
        i++;
    }
    return null;
}
