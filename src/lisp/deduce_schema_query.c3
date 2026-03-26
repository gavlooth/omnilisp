module lisp;

import std::core::mem;
extern fn double deduce_c_floor(double x) @extern("floor");
extern fn double deduce_c_ceil(double x) @extern("ceil");
extern fn double deduce_c_round(double x) @extern("round");
extern fn double deduce_c_fabs(double x) @extern("fabs");
extern fn double deduce_c_sin(double x) @extern("sin");
extern fn double deduce_c_cos(double x) @extern("cos");
extern fn double deduce_c_tan(double x) @extern("tan");
extern fn double deduce_c_asin(double x) @extern("asin");
extern fn double deduce_c_acos(double x) @extern("acos");
extern fn double deduce_c_atan(double x) @extern("atan");
extern fn double deduce_c_atan2(double y, double x) @extern("atan2");
extern fn double deduce_c_exp(double x) @extern("exp");
extern fn double deduce_c_log(double x) @extern("log");
extern fn double deduce_c_log10(double x) @extern("log10");
extern fn double deduce_c_pow(double x, double y) @extern("pow");
extern fn double deduce_c_sqrt(double x) @extern("sqrt");

struct DeduceQueryHeadDemandCases {
    DeduceRuleHeadDemand* entries;
    usz                   count;
    usz                   capacity;
}

fn void deduce_query_head_demand_cases_release(DeduceQueryHeadDemandCases* cases) {
    if (cases == null) return;
    if (cases.entries != null) {
        for (usz i = 0; i < cases.count; i++) {
            deduce_rule_head_demand_release(&cases.entries[i]);
        }
        mem::free(cases.entries);
    }
    *cases = {};
}

fn Value* deduce_query_head_demand_cases_append_take(
    DeduceQueryHeadDemandCases* cases,
    DeduceRuleHeadDemand* demand,
    Interp* interp
) {
    if (cases == null || demand == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand case state");
    }

    for (usz i = 0; i < cases.count; i++) {
        DeduceRuleHeadDemand* existing = &cases.entries[i];
        if (existing.predicate != demand.predicate || existing.term_count != demand.term_count ||
            existing.term_bound == null || existing.term_values == null ||
            demand.term_bound == null || demand.term_values == null) {
            continue;
        }

        bool same = true;
        for (usz j = 0; j < demand.term_count; j++) {
            if (existing.term_bound[j] != demand.term_bound[j]) {
                same = false;
                break;
            }
            if (demand.term_bound[j] && !values_equal(existing.term_values[j], demand.term_values[j])) {
                same = false;
                break;
            }
        }
        if (same) {
            deduce_rule_head_demand_release(demand);
            return null;
        }
    }

    if (cases.count == cases.capacity) {
        usz new_cap = cases.capacity > 0 ? cases.capacity * 2 : 4;
        DeduceRuleHeadDemand* next =
            (DeduceRuleHeadDemand*)mem::realloc(cases.entries, DeduceRuleHeadDemand.sizeof * new_cap);
        if (next == null) {
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
        }
        cases.entries = next;
        cases.capacity = new_cap;
    }

    cases.entries[cases.count++] = *demand;
    *demand = {};
    return null;
}

fn bool deduce_query_expr_contains_row_param(Expr* expr, SymbolId row_param) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_VAR:
            return expr.var_expr.name == row_param;
        case E_BLOCK:
            if (expr.block == null || expr.block.exprs == null) return false;
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (deduce_query_expr_contains_row_param(expr.block.exprs[i], row_param)) return true;
            }
            return false;
        case E_LET:
            return (expr.let_expr.init != null && deduce_query_expr_contains_row_param(expr.let_expr.init, row_param)) ||
                (expr.let_expr.body != null && deduce_query_expr_contains_row_param(expr.let_expr.body, row_param));
        case E_IF:
            return (expr.if_expr.test != null && deduce_query_expr_contains_row_param(expr.if_expr.test, row_param)) ||
                (expr.if_expr.then_branch != null && deduce_query_expr_contains_row_param(expr.if_expr.then_branch, row_param)) ||
                (expr.if_expr.else_branch != null && deduce_query_expr_contains_row_param(expr.if_expr.else_branch, row_param));
        case E_AND:
            return (expr.and_expr.left != null && deduce_query_expr_contains_row_param(expr.and_expr.left, row_param)) ||
                (expr.and_expr.right != null && deduce_query_expr_contains_row_param(expr.and_expr.right, row_param));
        case E_OR:
            return (expr.or_expr.left != null && deduce_query_expr_contains_row_param(expr.or_expr.left, row_param)) ||
                (expr.or_expr.right != null && deduce_query_expr_contains_row_param(expr.or_expr.right, row_param));
        case E_CALL:
            if (expr.call == null) return false;
            if (deduce_query_expr_contains_row_param(expr.call.func, row_param)) return true;
            if (expr.call.args != null) {
                for (usz i = 0; i < expr.call.arg_count; i++) {
                    if (deduce_query_expr_contains_row_param(expr.call.args[i], row_param)) return true;
                }
            }
            return false;
        case E_APP:
            return (expr.app.func != null && deduce_query_expr_contains_row_param(expr.app.func, row_param)) ||
                (expr.app.arg != null && deduce_query_expr_contains_row_param(expr.app.arg, row_param));
        case E_PATH:
            // Path segments are symbols, not expressions, so they cannot contain row-param references.
            return false;
        case E_INDEX:
            return (expr.index.collection != null && deduce_query_expr_contains_row_param(expr.index.collection, row_param)) ||
                (expr.index.index != null && deduce_query_expr_contains_row_param(expr.index.index, row_param));
        case E_SET:
            return expr.set_expr.value != null && deduce_query_expr_contains_row_param(expr.set_expr.value, row_param);
        case E_DEFINE:
            return expr.define.value != null && deduce_query_expr_contains_row_param(expr.define.value, row_param);
        case E_RESET:
            return expr.reset.body != null && deduce_query_expr_contains_row_param(expr.reset.body, row_param);
        case E_SHIFT:
            return expr.shift.body != null && deduce_query_expr_contains_row_param(expr.shift.body, row_param);
        case E_PERFORM:
            return expr.perform.arg != null && deduce_query_expr_contains_row_param(expr.perform.arg, row_param);
        case E_RESOLVE:
            return expr.resolve.value != null && deduce_query_expr_contains_row_param(expr.resolve.value, row_param);
        case E_HANDLE:
            if (expr.handle.body != null && deduce_query_expr_contains_row_param(expr.handle.body, row_param)) return true;
            if (expr.handle.clauses != null) {
                for (usz i = 0; i < expr.handle.clause_count; i++) {
                    if (expr.handle.clauses[i].handler_body != null &&
                        deduce_query_expr_contains_row_param(expr.handle.clauses[i].handler_body, row_param)) return true;
                }
            }
            return false;
        case E_QUASIQUOTE:
            return expr.quasiquote.body != null && deduce_query_expr_contains_row_param(expr.quasiquote.body, row_param);
        case E_UNQUOTE:
            return expr.unquote.body != null && deduce_query_expr_contains_row_param(expr.unquote.body, row_param);
        case E_UNQUOTE_SPLICING:
            return expr.unquote_splicing.body != null && deduce_query_expr_contains_row_param(expr.unquote_splicing.body, row_param);
        case E_LAMBDA:
            if (expr.lambda.body == null) return false;
            // Conservative: if the wrapper body contains the row param at all, treat it as row-dependent.
            return deduce_query_expr_contains_row_param(expr.lambda.body, row_param);
        default:
            return false;
    }
}

// ============================================================
// Relation definition + query
// ============================================================
fn Value* deduce_relation_extract_spec(
    Value* pair,
    Interp* interp,
    DeduceDb** db_out,
    SymbolId* rel_name_out,
    Value** cols_out
) {
    // arg 0: (cons db (name col1 col2 ...))
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(pair)) return deduce_raise(interp, "deduce/define-relation-expected-cons-pair", "__define-relation: expected cons pair");

    Value* db_val = car(pair);
    if (db_val == null || db_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/define-relation-db-not-handle", "__define-relation: car must be a deduce-open handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/define-relation-db-handle-closed", "__define-relation: database handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/define-relation-db-not-deduce-db", "__define-relation: car must be a deduce database handle");
    }
    *db_out = (DeduceDb*)db_val.ffi_val.lib_handle;

    Value* spec = cdr(pair);
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(spec)) return deduce_raise(interp, "deduce/define-relation-expected-name-columns-list", "__define-relation: expected (name col...) in cdr");
    // fault: lisp::EXPECTED_SYMBOL
    if (car(spec).tag != SYMBOL) return deduce_raise(interp, "deduce/define-relation-name-not-symbol", "__define-relation: first element must be relation name");

    *rel_name_out = car(spec).sym_val;
    *cols_out = cdr(spec);
    return null;
}

fn Relation* deduce_relation_alloc(DeduceDb* db, SymbolId rel_name) {
    Relation* rel = (Relation*)mem::malloc(Relation.sizeof);
    if (rel == null) return null;
    rel.name = rel_name;
    rel.columns = null;
    rel.column_key_values = null;
    rel.db = db;
    rel.col_count = 0;
    rel.col_capacity = 0;
    rel.dbi = 0;
    rel.key_col = 0;
    rel.unique_col = 0;
    rel.unique_cols = null;
    rel.unique_col_count = 0;
    rel.unique_col_capacity = 0;
    rel.reference_constraints = null;
    rel.reference_constraint_count = 0;
    rel.reference_constraint_capacity = 0;
    rel.check_constraints = null;
    rel.check_constraint_count = 0;
    rel.check_constraint_capacity = 0;
    rel.index_col = 0;
    rel.materialized_view = false;
    rel.materialized_refresh_policy = (SymbolId)0;
    rel.materialized_refresh_count = 0;
    rel.materialized_last_refresh_mutation_epoch = 0;
    rel.materialized_last_stale_mutation_epoch = 0;
    rel.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    rel.dropped = false;
    return rel;
}

fn bool deduce_relation_ensure_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.col_capacity) return true;

    usz new_cap = rel.col_capacity > 0 ? rel.col_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.columns, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.columns = new_columns;
    rel.col_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_unique_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.unique_col_capacity) return true;

    usz new_cap = rel.unique_col_capacity > 0 ? rel.unique_col_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.unique_cols, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.unique_cols = new_columns;
    rel.unique_col_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_reference_constraint_capacity(Relation* rel, usz needed) {
    if (needed <= rel.reference_constraint_capacity) return true;

    usz new_cap = rel.reference_constraint_capacity > 0 ? rel.reference_constraint_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceReferenceConstraint* next =
        (DeduceReferenceConstraint*)mem::realloc(rel.reference_constraints, DeduceReferenceConstraint.sizeof * new_cap);
    if (next == null) return false;
    rel.reference_constraints = next;
    rel.reference_constraint_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_check_constraint_capacity(Relation* rel, usz needed) {
    if (needed <= rel.check_constraint_capacity) return true;

    usz new_cap = rel.check_constraint_capacity > 0 ? rel.check_constraint_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceCheckConstraint* next =
        (DeduceCheckConstraint*)mem::realloc(rel.check_constraints, DeduceCheckConstraint.sizeof * new_cap);
    if (next == null) return false;
    rel.check_constraints = next;
    rel.check_constraint_capacity = new_cap;
    return true;
}

fn Value* deduce_relation_note_unique_column(Relation* rel, SymbolId col, Interp* interp) {
    if (rel == null || col == (SymbolId)0) return null;
    for (usz i = 0; i < rel.unique_col_count; i++) {
        if (rel.unique_cols != null && rel.unique_cols[i] == col) {
            rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
            return null;
        }
    }
    if (!deduce_relation_ensure_unique_column_capacity(rel, rel.unique_col_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.unique_cols[rel.unique_col_count++] = col;
    rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
    return null;
}

fn Value* deduce_relation_note_reference_constraint(
    Relation* rel,
    SymbolId local_col,
    SymbolId target_relation,
    SymbolId target_column,
    Interp* interp
) {
    if (rel == null || local_col == (SymbolId)0 || target_relation == (SymbolId)0 || target_column == (SymbolId)0) {
        return null;
    }
    for (usz i = 0; i < rel.reference_constraint_count; i++) {
        if (rel.reference_constraints == null || rel.reference_constraints[i].local_column != local_col) continue;
        rel.reference_constraints[i].target_relation = target_relation;
        rel.reference_constraints[i].target_column = target_column;
        return null;
    }
    if (!deduce_relation_ensure_reference_constraint_capacity(rel, rel.reference_constraint_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.reference_constraints[rel.reference_constraint_count++] = {
        .local_column = local_col,
        .target_relation = target_relation,
        .target_column = target_column
    };
    return null;
}

fn Value* deduce_relation_note_check_constraint(
    Relation* rel,
    SymbolId local_col,
    SymbolId predicate,
    Interp* interp
) {
    if (rel == null || local_col == (SymbolId)0 || predicate == (SymbolId)0) return null;
    for (usz i = 0; i < rel.check_constraint_count; i++) {
        if (rel.check_constraints == null || rel.check_constraints[i].local_column != local_col) continue;
        rel.check_constraints[i].predicate = predicate;
        return null;
    }
    if (!deduce_relation_ensure_check_constraint_capacity(rel, rel.check_constraint_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.check_constraints[rel.check_constraint_count++] = {
        .local_column = local_col,
        .predicate = predicate
    };
    return null;
}

fn Value* deduce_relation_collect_columns(Relation* rel, Value* cols, Interp* interp) {
    Value* curr = cols;
    while (is_cons(curr)) {
        Value* col = car(curr);
        if (col.tag == SYMBOL) {
            if (!deduce_relation_ensure_column_capacity(rel, rel.col_count + 1)) {
                return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
            }
            rel.columns[rel.col_count++] = col.sym_val;
        }
        curr = cdr(curr);
    }
    return null;
}

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

enum DeduceRelationOpenStatus {
    REL_OPEN_OK,
    REL_OPEN_OUT_OF_MEMORY,
    REL_OPEN_TXN_BEGIN_FAILED,
    REL_OPEN_DBI_FAILED,
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi(Relation* rel, Interp* interp) {
    // Open LMDB named database for this relation.
    char[] name_str = interp.symbols.get_name(rel.name);
    return deduce_relation_open_dbi_with_name(rel, name_str);
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi_with_name(Relation* rel, char[] name_str) {
    char* dbi_name = deduce_copy_c_string(name_str);
    if (dbi_name == null) return REL_OPEN_OUT_OF_MEMORY;
    defer mem::free(dbi_name);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_BEGIN_FAILED;

    rc = mdb_dbi_open(txn, dbi_name, MDB_CREATE, &rel.dbi);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return REL_OPEN_DBI_FAILED;
    }

    mdb_txn_commit(txn);
    return REL_OPEN_OK;
}

fn Value* prim_define_relation(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/define-relation-expected-spec", "__define-relation: expected (cons db '(name col...))");

    DeduceDb* db = null;
    SymbolId rel_name = 0;
    Value* cols = null;
    bool direct_form = args.len >= 2 &&
                       args[0] != null && args[0].tag == FFI_HANDLE &&
                       args[1] != null && args[1].tag == SYMBOL;
    if (direct_form) {
        if (args[0].ffi_val == null || args[0].ffi_val.lib_handle == null) {
            return deduce_raise(interp, "deduce/define-relation-db-handle-closed", "__define-relation: database handle is closed");
        }
        if (!deduce_ffi_handle_name_equals(args[0], "deduce-db")) {
            return deduce_raise(interp, "deduce/define-relation-db-not-deduce-db", "__define-relation: database handle must be a deduce database handle");
        }
        db = (DeduceDb*)args[0].ffi_val.lib_handle;
        rel_name = args[1].sym_val;
    } else {
        Value* parse_err = deduce_relation_extract_spec(args[0], interp, &db, &rel_name, &cols);
        if (parse_err != null) return parse_err;
    }

    Relation* rel = deduce_relation_alloc(db, rel_name);
    if (rel == null) return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    if (direct_form) {
    Value* col_err = deduce_relation_collect_columns_from_args(rel, args, 2, interp);
    if (col_err != null) {
        deduce_relation_free(rel);
        return col_err;
    }
    (void)deduce_relation_meta_apply_persisted_state(
        db,
        rel_name,
        &rel.materialized_view,
        &rel.materialized_refresh_policy,
        &rel.materialized_refresh_count,
        &rel.materialized_last_refresh_mutation_epoch,
        &rel.materialized_last_stale_mutation_epoch,
        &rel.materialized_last_stale_reason,
        interp
    );
    } else {
        Value* col_err = deduce_relation_collect_columns(rel, cols, interp);
        if (col_err != null) {
            deduce_relation_free(rel);
            return col_err;
        }
    }
    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi(rel, interp);
    if (open_status != REL_OPEN_OK) {
        deduce_relation_free(rel);
        if (open_status == REL_OPEN_OUT_OF_MEMORY) {
            return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
        }
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            // fault: lisp::WRITE_FAILED
            return deduce_raise(interp, "deduce/define-relation-txn-open-failed", "__define-relation: txn open failed");
        }
        // fault: lisp::WRITE_FAILED
        return deduce_raise(interp, "deduce/define-relation-dbi-open-failed", "__define-relation: dbi open failed");
    }

    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(
            db,
            rel_name,
            rel.columns,
            rel.col_count,
            rel.key_col,
            rel.unique_cols,
            rel.unique_col_count,
            rel.reference_constraints,
            rel.reference_constraint_count,
            rel.check_constraints,
            rel.check_constraint_count,
            rel.index_col,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason
        );
    if (schema_status != DEDUCE_SCHEMA_REGISTER_OK) {
        deduce_relation_free(rel);
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "__define-relation: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    if (rel.materialized_view) {
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            rel_name,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) {
            deduce_relation_free(rel);
            return meta_err;
        }
    }

    Value* v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (v == null) {
        deduce_relation_free(rel);
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    return v;
}

// (deduce 'open-named db relation-name db-name col...) -> relation handle
fn Value* prim_deduce_open_named(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 4) {
        return deduce_raise(interp, "deduce/open-named-expected-db-relation-dbname-columns", "deduce-open-named: expected (deduce 'open-named db relation-name db-name col...)");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/open-named-db-not-handle", "deduce-open-named: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/open-named-db-handle-closed", "deduce-open-named: database handle is closed");
    }
    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/open-named-db-not-open", "deduce-open-named: database is not open");
    }

    Value* rel_name_v = args[1];
    if (rel_name_v == null || rel_name_v.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/open-named-relation-name-not-symbol", "deduce-open-named: relation-name must be a symbol");
    }

    char[] db_name = {};
    Value* db_name_v = args[2];
    if (db_name_v == null) {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name_v.tag == STRING) {
        db_name = db_name_v.str_chars[:db_name_v.str_len];
    } else if (db_name_v.tag == SYMBOL) {
        db_name = interp.symbols.get_name(db_name_v.sym_val);
    } else {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name.len == 0) {
        return deduce_raise(interp, "deduce/open-named-db-name-empty", "deduce-open-named: db-name cannot be empty");
    }

    Relation* rel = deduce_relation_alloc(db, rel_name_v.sym_val);
    if (rel == null) {
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }

    Value* col_err = deduce_relation_collect_columns_from_args(rel, args, 3, interp);
    if (col_err != null) {
        deduce_relation_free(rel);
        return col_err;
    }
    (void)deduce_relation_meta_apply_persisted_state(
        db,
        rel_name_v.sym_val,
        &rel.materialized_view,
        &rel.materialized_refresh_policy,
        &rel.materialized_refresh_count,
        &rel.materialized_last_refresh_mutation_epoch,
        &rel.materialized_last_stale_mutation_epoch,
        &rel.materialized_last_stale_reason,
        interp
    );

    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi_with_name(rel, db_name);
    if (open_status != REL_OPEN_OK) {
        deduce_relation_free(rel);
        if (open_status == REL_OPEN_OUT_OF_MEMORY) {
            return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
        }
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            return deduce_raise(interp, "deduce/open-named-txn-open-failed", "deduce-open-named: txn open failed");
        }
        return deduce_raise(interp, "deduce/open-named-dbi-open-failed", "deduce-open-named: dbi open failed");
    }

    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(
            db,
            rel_name_v.sym_val,
            rel.columns,
            rel.col_count,
            rel.key_col,
            rel.unique_cols,
            rel.unique_col_count,
            rel.reference_constraints,
            rel.reference_constraint_count,
            rel.check_constraints,
            rel.check_constraint_count,
            rel.index_col,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason
        );
    if (schema_status != DEDUCE_SCHEMA_REGISTER_OK) {
        deduce_relation_free(rel);
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "deduce-open-named: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }
    if (rel.materialized_view) {
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            rel_name_v.sym_val,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) {
            deduce_relation_free(rel);
            return meta_err;
        }
    }

    Value* rel_v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (rel_v == null) {
        deduce_relation_free(rel);
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }
    return rel_v;
}

// ============================================================
// (deduce-query relation filter-fn [rule-index] [engine]) → filtered list of dicts
// filter-fn takes a dict, returns truthy/nil
// ============================================================

fn bool deduce_query_expr_try_extract_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (out_value == null || expr == null) return false;
    *out_value = null;

    switch (expr.tag) {
        case E_LIT:
            *out_value = expr.lit.value;
            return *out_value != null;
        case E_QUOTE:
            *out_value = expr.quote.datum;
            return *out_value != null;
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_literal_value(
                tail,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_LET: {
            if (interp == null || expr.let_expr.init == null || expr.let_expr.body == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;
            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_expr_try_extract_literal_value(
                expr.let_expr.body,
                row_param,
                scoped_env,
                interp,
                out_value
            );
        }
        case E_IF: {
            if (interp == null || expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.if_expr.test, row_param, capture_env, interp, &test_value)) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_expr_try_extract_literal_value(
                selected_branch,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_AND: {
            if (interp == null || expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.and_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.and_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_OR: {
            if (interp == null || expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.or_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (!is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.or_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_LAMBDA: {
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null || interp == null) return false;
            Value* closure = runtime_eval_expr(expr, lookup_env, interp);
            if (closure != null && !is_error(closure)) {
                *out_value = closure;
                return true;
            }
            return false;
        }
        case E_VAR: {
            if (expr.var_expr.name == row_param) return false;
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null) return false;
            *out_value = lookup_env.lookup(expr.var_expr.name);
            return *out_value != null;
        }
        case E_CALL: {
            if (interp == null || expr.call == null || expr.call.func == null || expr.call.args == null) return false;
            if (expr.call.func.tag != E_VAR) return false;
            char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);

            if (deduce_name_equals(func_name, "=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;
                Value* lhs = null;
                Value* rhs = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &lhs)) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &rhs)) return false;
                *out_value = values_equal(lhs, rhs) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "<") || deduce_name_equals(func_name, "<=")
                || deduce_name_equals(func_name, ">") || deduce_name_equals(func_name, ">=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;
                Value* lhs = null;
                Value* rhs = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &lhs)) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &rhs)) return false;
                if (lhs == null || rhs == null || !is_number(lhs) || !is_number(rhs)) return false;
                bool result;
                if (is_double(lhs) || is_double(rhs)) {
                    double left = to_double(lhs);
                    double right = to_double(rhs);
                    result = deduce_name_equals(func_name, "<") ? left < right
                        : deduce_name_equals(func_name, "<=") ? left <= right
                        : deduce_name_equals(func_name, ">") ? left > right
                        : left >= right;
                } else {
                    long left = lhs.int_val;
                    long right = rhs.int_val;
                    result = deduce_name_equals(func_name, "<") ? left < right
                        : deduce_name_equals(func_name, "<=") ? left <= right
                        : deduce_name_equals(func_name, ">") ? left > right
                        : left >= right;
                }
                *out_value = result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "not")) {
                if (expr.call.arg_count != 1 || expr.call.args[0] == null) return false;
                Value* arg = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &arg)) return false;
                *out_value = is_falsy(arg, interp) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }

            Value* arg0 = null;
            Value* arg1 = null;
            if (expr.call.arg_count >= 1) {
                if (expr.call.args[0] == null) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &arg0)) return false;
                if (arg0 == null || !is_number(arg0)) return false;
            }
            if (expr.call.arg_count >= 2) {
                if (expr.call.args[1] == null) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &arg1)) return false;
                if (arg1 == null || !is_number(arg1)) return false;
            }

            if (deduce_name_equals(func_name, "+")) {
                if (expr.call.arg_count == 1) {
                    *out_value = arg0;
                    return true;
                }
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) + to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val + arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "-")) {
                if (expr.call.arg_count == 1) {
                    if (is_double(arg0)) {
                        *out_value = make_double(interp, -arg0.double_val);
                        return *out_value != null;
                    }
                    *out_value = make_int(interp, -arg0.int_val);
                    return *out_value != null;
                }
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) - to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val - arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "*")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) * to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val * arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "/")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    double rhs = to_double(arg1);
                    if (rhs == 0.0) return false;
                    *out_value = make_double(interp, to_double(arg0) / rhs);
                    return *out_value != null;
                }
                if (arg1.int_val == 0) return false;
                *out_value = make_int(interp, arg0.int_val / arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "abs")) {
                if (expr.call.arg_count != 1) return false;
                if (is_double(arg0)) {
                    *out_value = make_double(interp, deduce_c_fabs(arg0.double_val));
                    return *out_value != null;
                }
                long n = arg0.int_val;
                *out_value = make_int(interp, n < 0 ? -n : n);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "floor")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                long out = 0;
                double floor_v = deduce_c_floor(value);
                if (floor_v != floor_v || floor_v > (double)long.max || floor_v < (double)long.min) {
                    return false;
                }
                out = (long)floor_v;
                *out_value = make_int(interp, out);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "ceiling")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                double ceil_v = deduce_c_ceil(value);
                if (ceil_v != ceil_v || ceil_v > (double)long.max || ceil_v < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)ceil_v);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "round")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                double round_v = deduce_c_round(value);
                if (round_v != round_v || round_v > (double)long.max || round_v < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)round_v);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "truncate")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                if (value != value || value > (double)long.max || value < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)value);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "sqrt")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_sqrt(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "exp")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_exp(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "log")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_log(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "log10")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_log10(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "min")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) < to_double(arg1) ? to_double(arg0) : to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val < arg1.int_val ? arg0.int_val : arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "max")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) > to_double(arg1) ? to_double(arg0) : to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val > arg1.int_val ? arg0.int_val : arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "pow")) {
                if (expr.call.arg_count != 2) return false;
                *out_value = make_double(interp, deduce_c_pow(to_double(arg0), to_double(arg1)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "sin")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_sin(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "cos")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_cos(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "tan")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_tan(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "asin")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_asin(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "acos")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_acos(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "atan")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_atan(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "atan2")) {
                if (expr.call.arg_count != 2) return false;
                *out_value = make_double(interp, deduce_c_atan2(to_double(arg0), to_double(arg1)));
                return *out_value != null;
            }
            if (expr.call.func.tag == E_VAR) {
                Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
                if (lookup_env != null) {
                    Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
                    if (callee != null && callee.tag == CLOSURE && callee.closure_val != null) {
                        Closure* closure = callee.closure_val;
                        if (!closure.has_rest &&
                            closure.param_count == 1 &&
                            closure.params != null &&
                            expr.call.arg_count == 1 &&
                            expr.call.args != null &&
                            expr.call.args[0] != null) {
                            Value* call_arg = null;
                            if (!deduce_query_expr_try_extract_literal_value(
                                    expr.call.args[0],
                                    row_param,
                                    capture_env,
                                    interp,
                                    &call_arg
                                )) return false;
                            if (call_arg == null) return false;

                            Env* parent_env = closure.env != null ? closure.env : interp.global_env;
                            if (parent_env == null) return false;
                            Env* call_env = make_env(interp, parent_env);
                            if (call_env == null) return false;
                            call_env.define(closure.params[0], call_arg);

                            if (deduce_query_expr_try_extract_literal_value(
                                closure.body,
                                row_param,
                                call_env,
                                interp,
                                out_value
                            )) {
                            return true;
                            }

                            // Last-resort fallback for closed one-arg wrapper closures.
                            // Evaluate the wrapper body directly in the wrapper call env
                            // so small pure wrappers such as `id` still collapse even if
                            // the recursive extractor cannot simplify the body shape.
                            Value* call_value = runtime_eval_expr(closure.body, call_env, interp);
                            if (call_value == null) {
                                call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
                            }
                            if (call_value != null) {
                            *out_value = call_value;
                            return true;
                            }
                        }
                    }
                }
            }
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env != null) {
                Value* call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
                if (call_value != null) {
                    *out_value = call_value;
                    return true;
                }
            }
            return false;
        }
        default:
            return false;
    }
}

fn bool deduce_query_expr_try_extract_row_column_ref(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    Interp* interp,
    usz* out_column_index
) {
    if (out_column_index == null || expr == null || rel == null || interp == null) return false;
    *out_column_index = 0;

    Expr* unwrapped_expr = null;
    SymbolId unwrapped_row_param = 0;
    Env* unwrapped_env = null;
    if (deduce_query_expr_try_unwrap_preserved_row_call_chain(
            expr,
            row_param,
            capture_env,
            interp,
            &unwrapped_expr,
            &unwrapped_row_param,
            &unwrapped_env
        )) {
        return deduce_query_expr_try_extract_row_column_ref(
            unwrapped_expr,
            unwrapped_row_param,
            unwrapped_env,
            rel,
            interp,
            out_column_index
        );
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_row_column_ref(
                tail,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_LET: {
            if (expr.let_expr.init == null || expr.let_expr.body == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_expr_try_extract_row_column_ref(
                expr.let_expr.body,
                row_param,
                scoped_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_expr_try_extract_row_column_ref(
                selected_branch,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_AND: {
            if (expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.and_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (!is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.or_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_CALL:
            break;
        default:
            return false;
    }
    if (expr.call == null || expr.call.arg_count != 2) return false;
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return false;
    char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
    if (!deduce_name_equals(func_name, "ref")) return false;
    if (expr.call.args == null || expr.call.args[0] == null || expr.call.args[1] == null) return false;
    if (expr.call.args[0].tag != E_VAR || expr.call.args[0].var_expr.name != row_param) return false;

    Value* column_value = null;
    if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &column_value)) return false;
    if (column_value == null || column_value.tag != SYMBOL) return false;
    return deduce_find_column_position(rel.columns, rel.col_count, column_value.sym_val, out_column_index);
}

fn bool deduce_query_expr_try_unwrap_preserved_row_call_chain(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Expr** out_expr,
    SymbolId* out_row_param,
    Env** out_capture_env
) {
    if (expr == null || interp == null || out_expr == null || out_row_param == null || out_capture_env == null) {
        return false;
    }

    Expr* current_expr = expr;
    SymbolId current_row_param = row_param;
    Env* current_env = capture_env != null ? capture_env : interp.global_env;
    bool unwrapped_any = false;

    for (usz depth = 0; depth < 4; depth++) {
        if (current_expr == null || current_expr.tag != E_CALL || current_expr.call == null) break;
        if (current_expr.call.func == null || current_expr.call.func.tag != E_VAR) break;
        if (current_expr.call.args == null || current_expr.call.arg_count == 0) break;
        if (current_env == null) return false;

        Value* callee = current_env.lookup(current_expr.call.func.var_expr.name);
        if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) break;

        Closure* closure = callee.closure_val;
        if (closure.has_rest ||
            closure.param_count != current_expr.call.arg_count ||
            closure.params == null ||
            closure.body == null) break;

        usz row_arg_index = usz.max;
        Env* parent_env = closure.env != null ? closure.env : interp.global_env;
        if (parent_env == null) return false;
        Env* call_env = make_env(interp, parent_env);
        if (call_env == null) return false;

        for (usz i = 0; i < current_expr.call.arg_count; i++) {
            Expr* arg_expr = current_expr.call.args[i];
            if (arg_expr == null) return false;

            if (arg_expr.tag == E_VAR && arg_expr.var_expr.name == current_row_param) {
                if (row_arg_index != usz.max) return false;
                row_arg_index = i;
                continue;
            }

            if (deduce_query_expr_contains_row_param(arg_expr, current_row_param)) return false;

            Value* literal_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    arg_expr,
                    current_row_param,
                    current_env,
                    interp,
                    &literal_value
                )) return false;
            if (literal_value == null) return false;
            call_env.define(closure.params[i], literal_value);
        }

        if (row_arg_index == usz.max) break;

        current_expr = closure.body;
        current_row_param = closure.params[row_arg_index];
        current_env = call_env;
        unwrapped_any = true;
    }

    if (!unwrapped_any) return false;
    *out_expr = current_expr;
    *out_row_param = current_row_param;
    *out_capture_env = current_env;
    return true;
}

fn bool deduce_query_filter_expr_try_collect_scalar_param_equalities(
    Expr* expr,
    SymbolId scalar_param,
    usz scalar_column_index,
    Env* capture_env,
    Relation* rel,
    bool[] term_bound,
    Value*[] term_values,
    Interp* interp,
    bool* has_any_bound_out
) {
    if (expr == null || rel == null || interp == null || has_any_bound_out == null) return false;

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                tail,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    scalar_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                selected_branch,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    scalar_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.let_expr.body,
                scalar_param,
                scalar_column_index,
                scoped_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    scalar_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                    expr.or_expr.right,
                    scalar_param,
                    scalar_column_index,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    scalar_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                    expr.or_expr.left,
                    scalar_param,
                    scalar_column_index,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            return false;
        }
        case E_AND: {
            bool left_supported = deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.and_expr.left,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            bool right_supported = deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.and_expr.right,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            return left_supported || right_supported;
        }
        case E_CALL: {
            if (expr.call == null || expr.call.func == null || expr.call.args == null) return false;
            if (expr.call.func.tag != E_VAR) return false;
            char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
            if (deduce_name_equals(func_name, "=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;

                bool left_is_scalar = expr.call.args[0].tag == E_VAR && expr.call.args[0].var_expr.name == scalar_param;
                bool right_is_scalar = expr.call.args[1].tag == E_VAR && expr.call.args[1].var_expr.name == scalar_param;
                if (left_is_scalar == right_is_scalar) return false;

                Expr* literal_expr = left_is_scalar ? expr.call.args[1] : expr.call.args[0];
                Value* literal_value = null;
                if (!deduce_query_expr_try_extract_literal_value(
                        literal_expr,
                        scalar_param,
                        capture_env,
                        interp,
                        &literal_value
                    )) return false;
                if (literal_value == null) return false;

                if (term_bound[scalar_column_index]) {
                    return values_equal(term_values[scalar_column_index], literal_value);
                }
                term_bound[scalar_column_index] = true;
                term_values[scalar_column_index] = literal_value;
                *has_any_bound_out = true;
                return true;
            }

            Env* lookup_env = capture_env != null ? capture_env : interp.global_env;
            if (lookup_env == null) return false;
            Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
            if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) return false;

            Closure* closure = callee.closure_val;
            if (closure.has_rest ||
                closure.param_count != expr.call.arg_count ||
                closure.params == null ||
                closure.body == null) return false;

            Env* parent_env = closure.env != null ? closure.env : interp.global_env;
            if (parent_env == null) return false;
            Env* call_env = make_env(interp, parent_env);
            if (call_env == null) return false;

            usz scalar_arg_index = usz.max;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                Expr* arg_expr = expr.call.args[i];
                if (arg_expr == null) return false;

                if (arg_expr.tag == E_VAR && arg_expr.var_expr.name == scalar_param) {
                    if (scalar_arg_index != usz.max) return false;
                    scalar_arg_index = i;
                    continue;
                }

                if (deduce_query_expr_contains_row_param(arg_expr, scalar_param)) return false;

                Value* literal_value = null;
                if (!deduce_query_expr_try_extract_literal_value(
                        arg_expr,
                        scalar_param,
                        capture_env,
                        interp,
                        &literal_value
                    )) return false;
                if (literal_value == null) return false;
                call_env.define(closure.params[i], literal_value);
            }

            if (scalar_arg_index == usz.max) return false;
            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                closure.body,
                closure.params[scalar_arg_index],
                scalar_column_index,
                call_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        default:
            return false;
    }
}

fn bool deduce_query_filter_expr_try_collect_equalities_from_captured_scalar_call(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    bool[] term_bound,
    Value*[] term_values,
    Interp* interp,
    bool* has_any_bound_out
) {
    if (expr == null || rel == null || interp == null || has_any_bound_out == null) return false;
    if (expr.tag != E_CALL || expr.call == null || expr.call.func == null || expr.call.args == null) return false;
    if (expr.call.func.tag != E_VAR) return false;

    Env* lookup_env = capture_env != null ? capture_env : interp.global_env;
    if (lookup_env == null) return false;
    Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
    if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) return false;

    Closure* closure = callee.closure_val;
    if (closure.has_rest ||
        closure.param_count != expr.call.arg_count ||
        closure.params == null ||
        closure.body == null) return false;

    Env* parent_env = closure.env != null ? closure.env : interp.global_env;
    if (parent_env == null) return false;
    Env* call_env = make_env(interp, parent_env);
    if (call_env == null) return false;

    usz scalar_arg_index = usz.max;
    usz scalar_column_index = 0;
    for (usz i = 0; i < expr.call.arg_count; i++) {
        Expr* arg_expr = expr.call.args[i];
        if (arg_expr == null) return false;

        usz column_index = 0;
        if (deduce_query_expr_try_extract_row_column_ref(
                arg_expr,
                row_param,
                capture_env,
                rel,
                interp,
                &column_index
            )) {
            if (scalar_arg_index != usz.max) return false;
            scalar_arg_index = i;
            scalar_column_index = column_index;
            continue;
        }

        if (deduce_query_expr_contains_row_param(arg_expr, row_param)) return false;

        Value* literal_value = null;
        if (!deduce_query_expr_try_extract_literal_value(
                arg_expr,
                row_param,
                capture_env,
                interp,
                &literal_value
            )) return false;
        if (literal_value == null) return false;
        call_env.define(closure.params[i], literal_value);
    }

    if (scalar_arg_index == usz.max) return false;
    return deduce_query_filter_expr_try_collect_scalar_param_equalities(
        closure.body,
        closure.params[scalar_arg_index],
        scalar_column_index,
        call_env,
        rel,
        term_bound,
        term_values,
        interp,
        has_any_bound_out
    );
}

fn bool deduce_query_filter_expr_try_collect_equalities(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    bool[] term_bound,
    Value*[] term_values,
    Interp* interp,
    bool* has_any_bound_out
) {
    if (expr == null || rel == null || interp == null || has_any_bound_out == null) return false;

    Expr* unwrapped_expr = null;
    SymbolId unwrapped_row_param = 0;
    Env* unwrapped_env = null;
    if (deduce_query_expr_try_unwrap_preserved_row_call_chain(
            expr,
            row_param,
            capture_env,
            interp,
            &unwrapped_expr,
            &unwrapped_row_param,
            &unwrapped_env
        )) {
        return deduce_query_filter_expr_try_collect_equalities(
            unwrapped_expr,
            unwrapped_row_param,
            unwrapped_env,
            rel,
            term_bound,
            term_values,
            interp,
            has_any_bound_out
        );
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_filter_expr_try_collect_equalities(
                tail,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) return false;
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_filter_expr_try_collect_equalities(
                selected_branch,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_filter_expr_try_collect_equalities(
                expr.let_expr.body,
                row_param,
                scoped_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_equalities(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_equalities(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            return false;
        }
        case E_AND: {
            bool left_supported = deduce_query_filter_expr_try_collect_equalities(
                expr.and_expr.left,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            bool right_supported = deduce_query_filter_expr_try_collect_equalities(
                expr.and_expr.right,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            // Residual conjuncts are still applied by the original filter
            // after the read, so we can mine any supported equality subset
            // without requiring the whole conjunction to be demand-safe.
            return left_supported || right_supported;
        }
        case E_CALL: {
            if (expr.call == null || expr.call.func == null || expr.call.args == null || expr.call.arg_count != 2) {
                return deduce_query_filter_expr_try_collect_equalities_from_captured_scalar_call(
                    expr,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }
            if (expr.call.func.tag != E_VAR) return false;
            char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
            if (!deduce_name_equals(func_name, "=")) {
                return deduce_query_filter_expr_try_collect_equalities_from_captured_scalar_call(
                    expr,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            usz left_column_index = 0;
            usz right_column_index = 0;
            Value* literal_value = null;
            bool left_is_ref = deduce_query_expr_try_extract_row_column_ref(
                expr.call.args[0],
                row_param,
                capture_env,
                rel,
                interp,
                &left_column_index
            );
            bool right_is_ref = deduce_query_expr_try_extract_row_column_ref(
                expr.call.args[1],
                row_param,
                capture_env,
                rel,
                interp,
                &right_column_index
            );
            if (left_is_ref == right_is_ref) return false;

            usz column_index = left_is_ref ? left_column_index : right_column_index;
            Expr* literal_expr = left_is_ref ? expr.call.args[1] : expr.call.args[0];
            if (!deduce_query_expr_try_extract_literal_value(
                    literal_expr,
                    row_param,
                    capture_env,
                    interp,
                    &literal_value
                )) return false;
            if (literal_value == null) return false;
            if (term_bound[column_index]) {
                return values_equal(term_values[column_index], literal_value);
            }
            term_bound[column_index] = true;
            term_values[column_index] = literal_value;
            *has_any_bound_out = true;
            return true;
        }
        default:
            return false;
    }
}

fn Value* deduce_query_build_head_demand_from_filter_expr(
    Relation* rel,
    Expr* body,
    SymbolId row_param,
    Env* capture_env,
    DeduceRuleHeadDemand* out_head_demand,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || body == null || interp == null || row_param == (SymbolId)0) return null;
    if (rel.col_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }

    bool has_any_bound = false;
    if (!deduce_query_filter_expr_try_collect_equalities(
            body,
            row_param,
            capture_env,
            rel,
            term_bound,
            term_values,
            interp,
            &has_any_bound
        ) || !has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *has_any_bound_out = has_any_bound;
    *supported_out = true;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}

fn Value* deduce_query_collect_disjunctive_head_demands_from_expr(
    Relation* rel,
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (supported_out != null) *supported_out = false;
    if (rel == null || expr == null || out_demands == null || supported_out == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return null;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return null;
            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                tail,
                row_param,
                capture_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return null;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return null;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                selected_branch,
                row_param,
                capture_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return null;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return null;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return null;
            if (init_value == null) return null;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return null;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) {
                return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
            }
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.let_expr.body,
                row_param,
                scoped_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return null;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
                );
            }

            DeduceQueryHeadDemandCases left_demands = {};
            defer deduce_query_head_demand_cases_release(&left_demands);
            DeduceQueryHeadDemandCases right_demands = {};
            defer deduce_query_head_demand_cases_release(&right_demands);
            bool left_supported = false;
            bool right_supported = false;
            Value* left_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.left,
                row_param,
                capture_env,
                &left_demands,
                &left_supported,
                interp
            );
            if (left_err != null) return left_err;
            Value* right_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.right,
                row_param,
                capture_env,
                &right_demands,
                &right_supported,
                interp
            );
            if (right_err != null) return right_err;
            if (!left_supported || !right_supported || left_demands.count == 0 || right_demands.count == 0) {
                return null;
            }

            for (usz i = 0; i < left_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &left_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }
            for (usz i = 0; i < right_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &right_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }

            *supported_out = true;
            return null;
        }
        default: {
            DeduceRuleHeadDemand demand = {};
            bool has_any_bound = false;
            bool supported = false;
            Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
                rel,
                expr,
                row_param,
                capture_env,
                &demand,
                &has_any_bound,
                &supported,
                interp
            );
            if (demand_err != null) return demand_err;
            if (!supported || !has_any_bound) return null;
            Value* append_err = deduce_query_head_demand_cases_append_take(out_demands, &demand, interp);
            if (append_err != null) {
                deduce_rule_head_demand_release(&demand);
                return append_err;
            }
            *supported_out = true;
            return null;
        }
    }
}

fn Value* deduce_query_try_build_head_demand_disjunction_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (out_demands == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    *out_demands = {};
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_collect_disjunctive_head_demands_from_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_demands,
        supported_out,
        interp
    );
    if (demand_err != null) {
        deduce_query_head_demand_cases_release(out_demands);
        return demand_err;
    }
    if (!*supported_out || out_demands.count < 2) {
        deduce_query_head_demand_cases_release(out_demands);
        *supported_out = false;
    }
    return null;
}

fn Value* deduce_query_build_union_head_demand_positions(
    Relation* rel,
    DeduceRuleHeadDemand* demands,
    usz demand_count,
    DeduceRuleHeadDemand* out_union,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_union == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid union head demand state");
    }

    *out_union = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0 || demands == null || demand_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }
    for (usz i = 0; i < demand_count; i++) {
        DeduceRuleHeadDemand* demand = &demands[i];
        if (demand.predicate != rel.name || demand.term_bound == null) continue;
        usz limit = demand.term_count < rel.col_count ? demand.term_count : rel.col_count;
        for (usz j = 0; j < limit; j++) {
            if (!demand.term_bound[j]) continue;
            term_bound[j] = true;
            has_any_bound = true;
        }
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_union = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    *has_any_bound_out = true;
    return null;
}

fn bool deduce_query_head_demand_try_get_single_bound_position(
    DeduceRuleHeadDemand* demand,
    usz* out_position,
    Value** out_value
) {
    if (demand == null || out_position == null || out_value == null) return false;
    if (demand.term_bound == null || demand.term_values == null || demand.term_count == 0) return false;

    bool found = false;
    usz position = 0;
    Value* value = null;
    for (usz i = 0; i < demand.term_count; i++) {
        if (!demand.term_bound[i]) continue;
        if (found) return false;
        found = true;
        position = i;
        value = demand.term_values[i];
    }
    if (!found || value == null) return false;
    *out_position = position;
    *out_value = value;
    return true;
}

fn usz deduce_query_head_demand_bound_position_count(DeduceRuleHeadDemand* demand) {
    if (demand == null || demand.term_bound == null || demand.term_count == 0) return 0;

    usz count = 0;
    for (usz i = 0; i < demand.term_count; i++) {
        if (demand.term_bound[i]) count++;
    }
    return count;
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_relaxation(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return true;

    return !deduce_component_can_use_relation_head_demand_at_same_indices(
        db,
        plan,
        component_id,
        projected_head_demand
    );
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_fallback(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return false;

    if (deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        return false;
    }

    if (deduce_component_head_rule_has_multiple_recursive_body_atoms(
            db,
            plan,
            component_id,
            projected_head_demand.predicate
        )) {
        return true;
    }

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (projected_head_demand.term_bound == null || !projected_head_demand.term_bound[i]) continue;
        if (deduce_component_position_has_same_index_recursive_carrier(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            return false;
        }
    }
    return true;
}

fn bool deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (projected_head_demand == null || projected_head_demand.term_bound == null || projected_head_demand.term_count == 0) {
        return false;
    }

    bool same_index_only = false;
    if (deduce_component_can_use_relation_head_demand_for_query(
            db,
            plan,
            component_id,
            projected_head_demand
        ) && !deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        same_index_only = true;
    }

    usz kept_position = projected_head_demand.term_count;
    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (!projected_head_demand.term_bound[i]) continue;
        if (same_index_only) {
            if (!deduce_component_position_has_same_index_recursive_carrier(
                    db,
                    plan,
                    component_id,
                    projected_head_demand.predicate,
                    i
                )) {
                continue;
            }
        } else if (!deduce_component_position_can_use_relation_head_demand_for_query(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            continue;
        }
        kept_position = i;
        break;
    }
    if (kept_position >= projected_head_demand.term_count) return false;

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (i == kept_position || !projected_head_demand.term_bound[i]) continue;
        projected_head_demand.term_bound[i] = false;
        if (projected_head_demand.term_values != null) projected_head_demand.term_values[i] = null;
    }
    return true;
}

fn void deduce_query_append_list_to_reversed(Value* list, Value** reversed_out, Interp* interp) {
    if (reversed_out == null || interp == null) return;
    Value* current = list;
    while (current != null && current.tag == CONS) {
        *reversed_out = make_cons(interp, current.cons_val.car, *reversed_out);
        current = current.cons_val.cdr;
    }
}

fn Value* deduce_query_try_build_head_demand_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null ||
        has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_head_demand,
        has_any_bound_out,
        supported_out,
        interp
    );
    if (demand_err != null || !*supported_out || out_head_demand.term_bound == null || out_head_demand.term_values == null) {
        return demand_err;
    }
    *out_term_bound = out_head_demand.term_bound[..out_head_demand.term_count];
    *out_term_values = out_head_demand.term_values[..out_head_demand.term_count];
    return null;
}

fn Value* deduce_query_scan_relation_in_txn(
    Relation* rel,
    Value* filter_fn,
    MdbTxn* txn,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || filter_fn == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid in-transaction query state");
    }
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open_in_txn(&it, rel, interp, txn, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            if (subject_keys_out != null) {
                char[4096] key_buf;
                usz? key_len = encode_tuple(it.vals, key_buf[..], key_buf.len);
                if (try encoded_row_key_len = key_len) {
                    deduce_goal_directed_read_subject_key_capture_record(
                        subject_keys_out,
                        &key_buf[0],
                        encoded_row_key_len
                    );
                } else {
                    subject_keys_out.complete = false;
                }
            }
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    return deduce_reverse_list(filtered_rev, interp);
}

fn Value* deduce_query_execute_goal_directed_selector_ephemeral(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid goal-directed query state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        rel.db,
        rel.name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            rel.db,
            &scc_plan,
            selected_components,
            256,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            {},
            interp
        );
        if (exec_err != null) return exec_err;
        deduce_fixpoint_clear_selected_dirty_predicates(rel.db, &scc_plan, selected_components);
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("selected-component-closure"),
            interp.symbols.intern("query"),
            selector_rule_index,
            null,
            null,
            null,
            interp.symbols.intern("unsupported-captured-call-shape")
        );
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        rel.db,
        selector_rule_index,
        &observed_rules,
        interp
    );
    if (observed_err != null) return observed_err;
    defer deduce_free_selected_rule_observed_counters(observed_rules);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        rel.db,
        &scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        &projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        observed_rules,
        interp
    );
    if (exec_err != null) return exec_err;
    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        txn,
        interp,
        &subject_keys
    );
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        selector_rule_index,
        head_demand,
        &projected_head_demand,
        &observed_rules[(usz)selector_rule_index]
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}

fn SymbolId deduce_materialized_refresh_policy_value(
    DeduceRelationSchema* schema,
    Interp* interp
) @inline {
    if (schema != null && schema.materialized_refresh_policy != (SymbolId)0) {
        return schema.materialized_refresh_policy;
    }
    return interp.symbols.intern("manual");
}

fn Value* deduce_relation_maybe_refresh_on_read(
    Value* rel_handle,
    Relation* rel,
    Env* env,
    SymbolId read_surface,
    Interp* interp,
    bool* handled_out
) {
    (void)read_surface;
    if (handled_out != null) *handled_out = false;
    if (rel == null || rel.db == null || interp == null) return null;

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    SymbolId on_read_policy = interp.symbols.intern("on-read");
    if (schema == null || !schema.materialized_view || schema.materialized_refresh_policy != on_read_policy) {
        return null;
    }

    if (handled_out != null) *handled_out = true;
    if (!deduce_relation_schema_is_materialized_stale(rel.db, schema)) return null;

    if (!deduce_relation_schema_is_materialized_ready(rel.db, schema)) {
        return deduce_raise(
            interp,
            "deduce/on-read-refresh-unavailable",
            "deduce: on-read materialized relation cannot refresh because derived rules are unavailable in the current state"
        );
    }

    if (!rel.db.incremental_full_recompute_required) {
        long refreshed_stale_count = 0;
        Value* refreshed_relations = make_nil(interp);
        Value* cleared_dirty_predicates = make_nil(interp);
        return deduce_refresh_materialized_relation_targeted(
            rel.db,
            rel,
            schema,
            env,
            interp,
            &refreshed_stale_count,
            &refreshed_relations,
            &cleared_dirty_predicates,
            null
        );
    }

    if (rel_handle == null) {
        return deduce_raise(
            interp,
            "deduce/on-read-refresh-invalid-state",
            "deduce: on-read materialized relation refresh requires a live relation handle"
        );
    }

    Value*[1] refresh_args = { rel_handle };
    Value* refresh_result = prim_deduce_refresh(refresh_args[..], env, interp);
    if (refresh_result == null || refresh_result.tag == ERROR) return refresh_result;
    return null;
}

fn Value* deduce_query_execute_goal_directed_plain_ephemeral(
    Relation* rel,
    Value* filter_fn,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || filter_fn == null || head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid plain goal-directed query state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);
    if (!has_projected_head_demand) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        rel.db,
        &scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        &projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        {},
        interp
    );
    if (exec_err != null) return exec_err;

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        txn,
        interp,
        &subject_keys
    );
    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        -1,
        head_demand,
        &projected_head_demand
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}

fn Value* deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceQueryHeadDemandCases* head_demands,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demands == null || head_demands.count == 0 ||
        surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive goal-directed query state");
    }

    DeduceRuleHeadDemand requested_union = {};
    bool has_requested_union = false;
    Value* requested_err = deduce_query_build_union_head_demand_positions(
        rel,
        head_demands.entries,
        head_demands.count,
        &requested_union,
        &has_requested_union,
        interp
    );
    if (requested_err != null) return requested_err;
    defer deduce_rule_head_demand_release(&requested_union);

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        rel.db,
        rel.name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    DeduceQueryHeadDemandCases projected_demands = {};
    defer deduce_query_head_demand_cases_release(&projected_demands);
    for (usz i = 0; i < head_demands.count; i++) {
        DeduceRuleHeadDemand projected_head_demand = {};
        bool[] projected_term_bound = {};
        Value*[] projected_term_values = {};
        bool has_projected_head_demand = false;
        Value* project_err = deduce_component_project_relation_head_demand_for_query(
            rel.db,
            &scc_plan,
            selected_component_id,
            &head_demands.entries[i],
            &projected_head_demand,
            &projected_term_bound,
            &projected_term_values,
            &has_projected_head_demand,
            interp
        );
        if (project_err != null) return project_err;
        if (!has_projected_head_demand) {
            long iterations = 0;
            long derived = 0;
            long max_component_iterations = 0;
            Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
                rel.db,
                &scc_plan,
                selected_components,
                256,
                use_seminaive_recursive,
                &iterations,
                &derived,
                &max_component_iterations,
                {},
                interp
            );
            if (exec_err != null) return exec_err;
            deduce_fixpoint_clear_selected_dirty_predicates(rel.db, &scc_plan, selected_components);
            deduce_db_note_goal_directed_read(
                rel.db,
                rel.name,
                interp.symbols.intern("selected-component-closure"),
                interp.symbols.intern("query"),
                selector_rule_index,
                has_requested_union ? &requested_union : null,
                null,
                null,
                interp.symbols.intern("unsupported-disjunction-shape")
            );
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }

        Value* append_err = deduce_query_head_demand_cases_append_take(
            &projected_demands,
            &projected_head_demand,
            interp
        );
        if (append_err != null) {
            deduce_rule_head_demand_release(&projected_head_demand);
            return append_err;
        }
    }

    DeduceRuleHeadDemand applied_union = {};
    bool has_applied_union = false;
    Value* applied_err = deduce_query_build_union_head_demand_positions(
        rel,
        projected_demands.entries,
        projected_demands.count,
        &applied_union,
        &has_applied_union,
        interp
    );
    if (applied_err != null) return applied_err;
    defer deduce_rule_head_demand_release(&applied_union);

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        rel.db,
        selector_rule_index,
        &observed_rules,
        interp
    );
    if (observed_err != null) return observed_err;
    defer deduce_free_selected_rule_observed_counters(observed_rules);

    Value* filtered_rev = make_nil(interp);
    for (usz i = 0; i < projected_demands.count; i++) {
        if (i > 0) deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);

        MdbTxn* txn = null;
        int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
        }

        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
            rel.db,
            &scc_plan,
            selected_components,
            txn,
            256,
            use_seminaive_recursive,
            &projected_demands.entries[i],
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        );
        if (exec_err != null) {
            if (txn != null) mdb_txn_abort(txn);
            return exec_err;
        }

        Value* branch_rows = deduce_query_scan_relation_in_txn(rel, filter_fn, txn, interp);
        if (txn != null) {
            mdb_txn_abort(txn);
            txn = null;
        }
        if (branch_rows != null && branch_rows.tag == ERROR) return branch_rows;
        deduce_query_append_list_to_reversed(branch_rows, &filtered_rev, interp);
    }

    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        selector_rule_index,
        has_requested_union ? &requested_union : null,
        has_applied_union ? &applied_union : null,
        &observed_rules[(usz)selector_rule_index]
    );
    return deduce_reverse_list(filtered_rev, interp);
}

fn Value* deduce_query_execute_goal_directed_plain_ephemeral_disjunction(
    Relation* rel,
    Value* filter_fn,
    bool use_seminaive_recursive,
    DeduceQueryHeadDemandCases* head_demands,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || filter_fn == null || head_demands == null || head_demands.count == 0 || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid plain disjunctive goal-directed query state");
    }

    DeduceRuleHeadDemand requested_union = {};
    bool has_requested_union = false;
    Value* requested_err = deduce_query_build_union_head_demand_positions(
        rel,
        head_demands.entries,
        head_demands.count,
        &requested_union,
        &has_requested_union,
        interp
    );
    if (requested_err != null) return requested_err;
    defer deduce_rule_head_demand_release(&requested_union);

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    DeduceQueryHeadDemandCases projected_demands = {};
    defer deduce_query_head_demand_cases_release(&projected_demands);
    for (usz i = 0; i < head_demands.count; i++) {
        DeduceRuleHeadDemand projected_head_demand = {};
        bool[] projected_term_bound = {};
        Value*[] projected_term_values = {};
        bool has_projected_head_demand = false;
        Value* project_err = deduce_component_project_relation_head_demand_for_query(
            rel.db,
            &scc_plan,
            selected_component_id,
            &head_demands.entries[i],
            &projected_head_demand,
            &projected_term_bound,
            &projected_term_values,
            &has_projected_head_demand,
            interp
        );
        if (project_err != null) return project_err;
        if (!has_projected_head_demand) return null;
        if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }

        Value* append_err = deduce_query_head_demand_cases_append_take(
            &projected_demands,
            &projected_head_demand,
            interp
        );
        if (append_err != null) {
            deduce_rule_head_demand_release(&projected_head_demand);
            return append_err;
        }
    }

    DeduceRuleHeadDemand applied_union = {};
    bool has_applied_union = false;
    Value* applied_err = deduce_query_build_union_head_demand_positions(
        rel,
        projected_demands.entries,
        projected_demands.count,
        &applied_union,
        &has_applied_union,
        interp
    );
    if (applied_err != null) return applied_err;
    defer deduce_rule_head_demand_release(&applied_union);

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    Value* filtered_rev = make_nil(interp);
    for (usz i = 0; i < projected_demands.count; i++) {
        if (i > 0) deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);

        MdbTxn* txn = null;
        int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
        }

        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
            rel.db,
            &scc_plan,
            selected_components,
            txn,
            256,
            use_seminaive_recursive,
            &projected_demands.entries[i],
            &iterations,
            &derived,
            &max_component_iterations,
            {},
            interp
        );
        if (exec_err != null) {
            if (txn != null) mdb_txn_abort(txn);
            return exec_err;
        }

        Value* branch_rows = deduce_query_scan_relation_in_txn(rel, filter_fn, txn, interp);
        if (txn != null) {
            mdb_txn_abort(txn);
            txn = null;
        }
        if (branch_rows != null && branch_rows.tag == ERROR) return branch_rows;
        deduce_query_append_list_to_reversed(branch_rows, &filtered_rev, interp);
    }

    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        -1,
        has_requested_union ? &requested_union : null,
        has_applied_union ? &applied_union : null
    );
    return deduce_reverse_list(filtered_rev, interp);
}

fn Value* prim_deduce_query(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2 || args.len > 4) {
        return deduce_raise(interp, "deduce/query-expected-relation-filter",
            "deduce-query: expected (deduce-query relation filter-fn [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/query-relation-not-handle", "deduce-query: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/query-relation-handle-closed", "deduce-query: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/query-relation-not-deduce-relation", "deduce-query: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/query-database-not-open", "deduce-query: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/query-relation-dropped", "deduce-query: relation is dropped");

    Value* filter_fn = args[1];
    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/query-out-of-memory",
        .out_of_memory_message = "deduce/query: out of memory",
        .invalid_selector_code = "deduce/query-invalid-selector",
        .invalid_selector_message = "deduce-query: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/query-invalid-engine",
        .invalid_engine_message = "deduce-query: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/query-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-query: rule-index is out of range",
        .invalid_state_code = "deduce/query-invalid-state",
        .invalid_state_message = "deduce-query: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/query-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/query: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/query-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/query: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/query-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/query: selected rule closure does not produce the requested relation",
    };
    bool use_seminaive_recursive = true;
    long selector_rule_index = -1;
    Value* parse_err = deduce_parse_goal_directed_selector_and_engine(
        args,
        2,
        &surface,
        &selector_rule_index,
        &use_seminaive_recursive,
        interp
    );
    if (parse_err != null) return parse_err;
    Value* exec_err = null;

    if (!deduce_relation_ensure_column_key_values(rel, interp, null)) {
        return make_nil(interp);
    }

    DeduceRuleHeadDemand head_demand = {};
    bool[] term_bound = {};
    Value*[] term_values = {};
    bool has_any_bound = false;
    bool supports_head_demand = false;
    Value* demand_err = deduce_query_try_build_head_demand_from_filter(
        rel,
        filter_fn,
        &head_demand,
        &term_bound,
        &term_values,
        &has_any_bound,
        &supports_head_demand,
        interp
    );
    if (demand_err != null) return demand_err;
    defer if (term_bound.ptr != null) mem::free(term_bound.ptr);
    defer if (term_values.ptr != null) mem::free(term_values.ptr);

    if (supports_head_demand && has_any_bound) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &head_demand,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &head_demand,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    DeduceQueryHeadDemandCases disjunctive_head_demands = {};
    defer deduce_query_head_demand_cases_release(&disjunctive_head_demands);
    bool supports_disjunctive_head_demand = false;
    if (!supports_head_demand || !has_any_bound) {
        Value* disjunctive_err = deduce_query_try_build_head_demand_disjunction_from_filter(
            rel,
            filter_fn,
            &disjunctive_head_demands,
            &supports_disjunctive_head_demand,
            interp
        );
        if (disjunctive_err != null) return disjunctive_err;
    }

    if (supports_disjunctive_head_demand && disjunctive_head_demands.count > 0) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral_disjunction(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    if (selector_rule_index >= 0) {
        exec_err = deduce_execute_goal_directed_relation_selector(
            rel.db,
            rel.name,
            selector_rule_index,
            use_seminaive_recursive,
            interp.symbols.intern("query"),
            &surface,
            interp
        );
        if (exec_err != null) return exec_err;
        bool handled = false;
        exec_err = deduce_relation_maybe_refresh_on_read(
            rel_val,
            rel,
            env,
            interp.symbols.intern("query"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    } else {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("query"),
            env,
            interp
        );
        if (exec_err != null) return exec_err;
    }

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            deduce_goal_directed_read_subject_key_capture_record(
                &subject_keys,
                it.key.mv_data,
                it.key.mv_size
            );
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    if (selector_rule_index >= 0) {
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("no-op"),
            interp.symbols.intern("query"),
            selector_rule_index
        );
    }
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return deduce_reverse_list(filtered_rev, interp);
}

fn Value* deduce_relation_schema_columns_list(DeduceRelationSchema* schema, Interp* interp) {
    if (schema == null || schema.columns == null || schema.arity == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.arity; i++) {
        rev = make_cons(interp, make_symbol(interp, schema.columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_index_columns_list(
    DeduceRelationSchema* schema,
    DeduceIndexDescriptor* index,
    Interp* interp
) {
    if (schema == null || index == null || index.column_positions == null || index.column_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < index.column_count; i++) {
        usz column_pos = index.column_positions[i];
        if (column_pos >= schema.arity || schema.columns == null) {
            rev = make_cons(interp, make_nil(interp), rev);
            continue;
        }
        rev = make_cons(interp, make_symbol(interp, schema.columns[column_pos]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_schema_role_column(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_symbol(interp, column);
}

fn Value* deduce_relation_schema_role_columns(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool deduce_relation_schema_unique_equivalent_to_key(DeduceRelationSchema* schema) @inline {
    return schema != null &&
        schema.key_column != (SymbolId)0 &&
        schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == schema.key_column;
}

fn bool deduce_relation_schema_column_is_candidate_key(DeduceRelationSchema* schema, SymbolId column) @inline {
    if (schema == null || column == (SymbolId)0) return false;
    if (schema.key_column == column) return true;
    return schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == column;
}

fn long deduce_relation_schema_constraint_count(DeduceRelationSchema* schema) @inline {
    if (schema == null) return 0;
    long count = 0;
    if (schema.key_column != (SymbolId)0) count++;
    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) count++;
    count += (long)schema.reference_constraint_count;
    count += (long)schema.check_constraint_count;
    return count;
}

fn void deduce_reference_constraint_resolution(
    DeduceDb* db,
    DeduceReferenceConstraint* ref_constraint,
    bool* target_declared_out,
    bool* target_column_declared_out,
    bool* target_live_out,
    bool* target_unique_out
) {
    bool target_declared = false;
    bool target_column_declared = false;
    bool target_live = false;
    bool target_unique = false;
    if (db != null && ref_constraint != null) {
        DeduceRelationSchema* target_schema = deduce_db_lookup_relation_schema(db, ref_constraint.target_relation);
        if (target_schema != null) {
            target_declared = true;
            usz ignored = 0;
            target_column_declared =
                deduce_find_column_position(target_schema.columns, target_schema.arity, ref_constraint.target_column, &ignored);
            target_live = !target_schema.dropped;
            if (target_column_declared && target_live) {
                target_unique = deduce_relation_schema_column_is_candidate_key(target_schema, ref_constraint.target_column);
            }
        }
    }
    if (target_declared_out != null) *target_declared_out = target_declared;
    if (target_column_declared_out != null) *target_column_declared_out = target_column_declared;
    if (target_live_out != null) *target_live_out = target_live;
    if (target_unique_out != null) *target_unique_out = target_unique;
}

fn long deduce_relation_schema_resolved_reference_constraint_count(
    DeduceDb* db,
    DeduceRelationSchema* schema
) {
    if (db == null || schema == null || schema.reference_constraints == null || schema.reference_constraint_count == 0) {
        return 0;
    }
    long count = 0;
    for (usz i = 0; i < schema.reference_constraint_count; i++) {
        bool target_declared = false;
        bool target_column_declared = false;
        bool target_live = false;
        bool target_unique = false;
        deduce_reference_constraint_resolution(db, &schema.reference_constraints[i], &target_declared, &target_column_declared, &target_live, &target_unique);
        if (target_declared && target_column_declared && target_live && target_unique) count++;
    }
    return count;
}

fn Value* deduce_relation_schema_constraints_payload(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    if (schema == null) return make_nil(interp);

    Value* rev = make_nil(interp);

    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp);
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("unique"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    if (schema.reference_constraint_count > 0 && schema.reference_constraints != null) {
        for (usz i = 0; i < schema.reference_constraint_count; i++) {
            DeduceReferenceConstraint* ref = &schema.reference_constraints[i];
            Value* entry = make_hashmap(interp, 16);
            if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
                return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
            }
            bool target_declared = false;
            bool target_column_declared = false;
            bool target_live = false;
            bool target_unique = false;
            deduce_reference_constraint_resolution(db, ref, &target_declared, &target_column_declared, &target_live, &target_unique);
            Value* columns = make_cons(interp, make_symbol(interp, ref.local_column), make_nil(interp));
            explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("reference"));
            explain_dict_set(entry, interp, "columns", columns);
            explain_dict_set_symbol(entry, interp, "referenced-relation", ref.target_relation);
            explain_dict_set_symbol(entry, interp, "referenced-column", ref.target_column);
            explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
            explain_dict_set(entry, interp, "target-declared", explain_bool_value(interp, target_declared));
            explain_dict_set(entry, interp, "target-column-declared", explain_bool_value(interp, target_column_declared));
            explain_dict_set(entry, interp, "target-live", explain_bool_value(interp, target_live));
            explain_dict_set(entry, interp, "target-unique", explain_bool_value(interp, target_unique));
            explain_dict_set(entry, interp, "resolved", explain_bool_value(interp, target_declared && target_column_declared && target_live && target_unique));
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.check_constraint_count > 0 && schema.check_constraints != null) {
        for (usz i = 0; i < schema.check_constraint_count; i++) {
            DeduceCheckConstraint* check = &schema.check_constraints[i];
            Value* entry = make_hashmap(interp, 10);
            if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
                return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
            }
            Value* columns = make_cons(interp, make_symbol(interp, check.local_column), make_nil(interp));
            explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("check"));
            explain_dict_set(entry, interp, "columns", columns);
            explain_dict_set_symbol(entry, interp, "predicate", check.predicate);
            explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, false));
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.key_column != (SymbolId)0) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = make_cons(interp, make_symbol(interp, schema.key_column), make_nil(interp));
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("key"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    return rev == null ? make_nil(interp) : explain_reverse_list(rev, interp);
}

fn Value* deduce_integrity_violation_class_value(
    DeduceIntegrityViolationClass violation_class,
    Interp* interp
) {
    if (interp == null) return null;
    switch (violation_class) {
        case DEDUCE_INTEGRITY_VIOLATION_KEY:
            return make_symbol(interp, interp.symbols.intern("key"));
        case DEDUCE_INTEGRITY_VIOLATION_UNIQUE:
            return make_symbol(interp, interp.symbols.intern("unique"));
        case DEDUCE_INTEGRITY_VIOLATION_REFERENCE:
            return make_symbol(interp, interp.symbols.intern("reference"));
        case DEDUCE_INTEGRITY_VIOLATION_CHECK:
            return make_symbol(interp, interp.symbols.intern("check"));
    }
}

fn Value* deduce_integrity_violation_history_payload(
    DeduceIntegrityViolationEntry[] history,
    usz count,
    Interp* interp
) {
    if (interp == null || history.ptr == null || count == 0) return make_nil(interp);

    Value* list = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        DeduceIntegrityViolationEntry entry_data = history[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/integrity-history-out-of-memory",
                "deduce: integrity history payload out of memory");
        }
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
        explain_dict_set(entry, interp, "sequence", make_int(interp, (long)entry_data.sequence));
        explain_dict_set(entry, interp, "relation",
            entry_data.relation != (SymbolId)0
                ? make_symbol(interp, entry_data.relation)
                : make_nil(interp));
        explain_dict_set(entry, interp, "code",
            entry_data.code != (SymbolId)0
                ? make_symbol(interp, entry_data.code)
                : make_nil(interp));
        explain_dict_set(entry, interp, "violation-class",
            deduce_integrity_violation_class_value(entry_data.violation_class, interp));
        list = make_cons(interp, entry, list);
    }
    return list;
}

fn Value* deduce_bound_positions_payload(
    usz* positions,
    ulong count,
    Interp* interp
) {
    if (interp == null || positions == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < (usz)count; i++) {
        rev = make_cons(interp, make_int(interp, (long)positions[i]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_materialized_stale_reason_value(
    DeduceMaterializedStaleReason reason,
    Interp* interp
) {
    if (interp == null) return null;
    switch (reason) {
        case DEDUCE_MATERIALIZED_STALE_NONE:
            return make_nil(interp);
        case DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED:
            return make_symbol(interp, interp.symbols.intern("never-refreshed"));
        case DEDUCE_MATERIALIZED_STALE_RULE_SET_CHANGE:
            return make_symbol(interp, interp.symbols.intern("rule-set-change"));
        case DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY:
            return make_symbol(interp, interp.symbols.intern("dependency-dirty"));
        case DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED:
            return make_symbol(interp, interp.symbols.intern("full-recompute-required"));
    }
}

fn Value* deduce_relation_schema_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    Value* out = make_hashmap(interp, 32);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
    }

    long resolved_reference_constraint_count =
        deduce_relation_schema_resolved_reference_constraint_count(rel != null ? rel.db : null, schema);
    long unresolved_reference_constraint_count =
        schema != null ? (long)schema.reference_constraint_count - resolved_reference_constraint_count : 0;

    Value* constraints = deduce_relation_schema_constraints_payload(rel != null ? rel.db : null, schema, interp);
    if (constraints != null && constraints.tag == ERROR) return constraints;
    DeduceMaterializedStaleReason effective_stale_reason =
        deduce_relation_schema_effective_materialized_stale_reason(schema);

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-schema"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "columns", deduce_relation_schema_columns_list(schema, interp));
    explain_dict_set(out, interp, "arity", make_int(interp, (long)schema.arity));
    explain_dict_set(out, interp, "key-column", deduce_relation_schema_role_column(schema.key_column, interp));
    explain_dict_set(out, interp, "unique-column", deduce_relation_schema_role_column(schema.unique_column, interp));
    explain_dict_set(out, interp, "unique-columns", deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp));
    explain_dict_set(out, interp, "reference-constraint-count", make_int(interp, (long)schema.reference_constraint_count));
    explain_dict_set(out, interp, "check-constraint-count", make_int(interp, (long)schema.check_constraint_count));
    explain_dict_set(out, interp, "resolved-reference-constraint-count", make_int(interp, resolved_reference_constraint_count));
    explain_dict_set(out, interp, "unresolved-reference-constraint-count", make_int(interp, unresolved_reference_constraint_count));
    explain_dict_set(out, interp, "index-column", deduce_relation_schema_role_column(schema.index_column, interp));
    explain_dict_set(out, interp, "index-count", make_int(interp, (long)schema.index_count));
    explain_dict_set(out, interp, "constraint-count", make_int(interp, deduce_relation_schema_constraint_count(schema)));
    explain_dict_set(out, interp, "constraints", constraints);
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "materialized-view", explain_bool_value(interp, schema.materialized_view));
    explain_dict_set(out, interp, "materialized-derived-ready",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_ready(rel != null ? rel.db : null, schema)));
    explain_dict_set(out, interp, "materialized-refresh-policy",
        schema.materialized_refresh_policy != (SymbolId)0
            ? make_symbol(interp, schema.materialized_refresh_policy)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-refresh-count", make_int(interp, (long)schema.materialized_refresh_count));
    explain_dict_set(out, interp, "materialized-last-refresh-mutation-epoch",
        schema.materialized_refresh_count > 0
            ? make_int(interp, (long)schema.materialized_last_refresh_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-mutation-epoch",
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE &&
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED
            ? make_int(interp, (long)schema.materialized_last_stale_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-reason",
        deduce_materialized_stale_reason_value(effective_stale_reason, interp));
    explain_dict_set(out, interp, "materialized-stale",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_stale(rel != null ? rel.db : null, schema)));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    explain_dict_set(out, interp, "database-open", explain_bool_value(interp, rel.db != null && rel.db.open));
    return out;
}

fn Value* deduce_relation_indexes_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    (void)rel;
    if (schema == null || schema.indexes == null || schema.index_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* index = &schema.indexes[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/indexes-out-of-memory", "deduce/indexes: out of memory");
        }
        explain_dict_set(entry, interp, "index-id", make_int(interp, (long)i));
        explain_dict_set(entry, interp, "columns", deduce_relation_index_columns_list(schema, index, interp));
        explain_dict_set(entry, interp, "column-count", make_int(interp, (long)index.column_count));
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, index.unique));
        rev = make_cons(interp, entry, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* prim_deduce_schema(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/schema-expected-relation", "deduce/schema: expected (deduce/schema relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/schema-relation-not-handle", "deduce/schema: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/schema-relation-handle-closed", "deduce/schema: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/schema-relation-not-deduce-relation", "deduce/schema: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/schema-database-not-open", "deduce/schema: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/schema-relation-not-found", "deduce/schema: relation schema is not registered");
    }

    return deduce_relation_schema_payload(rel, schema, interp);
}

fn Value* prim_deduce_indexes(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/indexes-expected-relation", "deduce/indexes: expected (deduce/indexes relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/indexes-relation-not-handle", "deduce/indexes: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/indexes-relation-handle-closed", "deduce/indexes: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/indexes-relation-not-deduce-relation", "deduce/indexes: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/indexes-database-not-open", "deduce/indexes: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/indexes-relation-not-found", "deduce/indexes: relation schema is not registered");
    }

    return deduce_relation_indexes_payload(rel, schema, interp);
}

fn Value* deduce_parallel_batch_topology_counts(
    DeduceDb* db,
    long* recursive_component_count_out,
    long* batched_component_count_out,
    long* batch_count_out,
    long* max_batch_size_out,
    Interp* interp
) {
    if (recursive_component_count_out != null) *recursive_component_count_out = 0;
    if (batched_component_count_out != null) *batched_component_count_out = 0;
    if (batch_count_out != null) *batch_count_out = 0;
    if (max_batch_size_out != null) *max_batch_size_out = 0;
    if (db == null || interp == null) return null;

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_err != null) return parallel_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

    long recursive_component_count = 0;
    long batched_component_count = 0;
    long batch_count = 0;
    long max_batch_size = 0;
    for (usz component_id = 0; component_id < scc_plan.component_count; component_id++) {
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[component_id];
        if (!recursive) continue;

        recursive_component_count++;

        long batch_size =
            component_id < parallel_batch_sizes.len ? (long)parallel_batch_sizes[component_id] : 0;
        if (batch_size > 1) batched_component_count++;
        if (batch_size > max_batch_size) max_batch_size = batch_size;

        bool batch_seen = false;
        if (component_id < parallel_wave_ids.len && component_id < scc_plan.component_strata.len) {
            usz stratum = scc_plan.component_strata[component_id];
            usz wave = parallel_wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    scc_plan.component_recursive.ptr != null &&
                    prev < scc_plan.component_recursive.len &&
                    scc_plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= parallel_wave_ids.len || prev >= scc_plan.component_strata.len) continue;
                if (scc_plan.component_strata[prev] != stratum) continue;
                if (parallel_wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (!batch_seen) batch_count++;
    }

    if (recursive_component_count_out != null) *recursive_component_count_out = recursive_component_count;
    if (batched_component_count_out != null) *batched_component_count_out = batched_component_count;
    if (batch_count_out != null) *batch_count_out = batch_count;
    if (max_batch_size_out != null) *max_batch_size_out = max_batch_size;
    return null;
}

fn Value* prim_deduce_stats(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/stats-expected-relation", "deduce/stats: expected (deduce/stats relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/stats-relation-not-handle", "deduce/stats: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/stats-relation-handle-closed", "deduce/stats: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/stats-relation-not-deduce-relation", "deduce/stats: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/stats-database-not-open", "deduce/stats: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/stats-relation-not-found", "deduce/stats: relation schema is not registered");
    }

    Value* out = make_hashmap(interp, 32);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/stats-out-of-memory", "deduce/stats: out of memory");
    }

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-stats"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "dirty-predicate-count", make_int(interp, (long)rel.db.incremental_dirty_count));
    Value* dirty_predicates = deduce_dirty_predicates_payload(rel.db, interp);
    if (dirty_predicates != null && dirty_predicates.tag == ERROR) return dirty_predicates;
    explain_dict_set(out, interp, "dirty-predicates", dirty_predicates);
    explain_dict_set(out, interp, "dirty-self",
        explain_bool_value(interp, deduce_db_is_incremental_dirty_predicate(rel.db, rel.name)));
    explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)rel.db.incremental_mutation_epoch));
    explain_dict_set(out, interp, "full-recompute-required", explain_bool_value(interp, rel.db.incremental_full_recompute_required));
    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(rel.db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);
    long parallel_recursive_component_count = 0;
    long parallel_batched_component_count = 0;
    long parallel_batch_count = 0;
    long parallel_max_batch_size = 0;
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_meta_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_meta_err != null) return parallel_meta_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);
    Value* parallel_err = deduce_parallel_batch_topology_counts(
        rel.db,
        &parallel_recursive_component_count,
        &parallel_batched_component_count,
        &parallel_batch_count,
        &parallel_max_batch_size,
        interp
    );
    if (parallel_err != null) return parallel_err;
    explain_dict_set(out, interp, "parallel-recursive-component-count",
        make_int(interp, parallel_recursive_component_count));
    explain_dict_set(out, interp, "parallel-batched-component-count",
        make_int(interp, parallel_batched_component_count));
    explain_dict_set(out, interp, "parallel-batch-count",
        make_int(interp, parallel_batch_count));
    explain_dict_set(out, interp, "parallel-max-batch-size",
        make_int(interp, parallel_max_batch_size));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            &scc_plan,
            parallel_wave_ids[..],
            parallel_batch_sizes[..],
            interp
        ));
    explain_dict_set(out, interp, "integrity-violation-count", make_int(interp, (long)schema.integrity_violation_count));
    explain_dict_set(out, interp, "key-integrity-violation-count", make_int(interp, (long)schema.key_integrity_violation_count));
    explain_dict_set(out, interp, "unique-integrity-violation-count", make_int(interp, (long)schema.unique_integrity_violation_count));
    explain_dict_set(out, interp, "reference-integrity-violation-count", make_int(interp, (long)schema.reference_integrity_violation_count));
    explain_dict_set(out, interp, "check-integrity-violation-count", make_int(interp, (long)schema.check_integrity_violation_count));
    explain_dict_set(out, interp, "last-integrity-violation-code",
        schema.last_integrity_violation_code != (SymbolId)0
            ? make_symbol(interp, schema.last_integrity_violation_code)
            : make_nil(interp));
    Value* recent_integrity_violations = deduce_integrity_violation_history_payload(
        schema.integrity_violation_history[..],
        schema.integrity_violation_history_count,
        interp
    );
    if (recent_integrity_violations != null && recent_integrity_violations.tag == ERROR) {
        return recent_integrity_violations;
    }
    DeduceMaterializedStaleReason effective_stale_reason =
        deduce_relation_schema_effective_materialized_stale_reason(schema);
    explain_dict_set(out, interp, "recent-integrity-violations", recent_integrity_violations);
    explain_dict_set(out, interp, "materialized-view", explain_bool_value(interp, schema.materialized_view));
    explain_dict_set(out, interp, "materialized-derived-ready",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_ready(rel.db, schema)));
    explain_dict_set(out, interp, "materialized-refresh-policy",
        schema.materialized_refresh_policy != (SymbolId)0
            ? make_symbol(interp, schema.materialized_refresh_policy)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-refresh-count", make_int(interp, (long)schema.materialized_refresh_count));
    explain_dict_set(out, interp, "materialized-last-refresh-mutation-epoch",
        schema.materialized_refresh_count > 0
            ? make_int(interp, (long)schema.materialized_last_refresh_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-mutation-epoch",
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE &&
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED
            ? make_int(interp, (long)schema.materialized_last_stale_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-reason",
        deduce_materialized_stale_reason_value(effective_stale_reason, interp));
    explain_dict_set(out, interp, "materialized-stale",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_stale(rel.db, schema)));
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_requested_bound_positions,
                schema.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_applied_bound_positions,
                schema.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-step-counters",
        schema.last_goal_directed_read_execution_path != (SymbolId)0 &&
        schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    return out;
}

fn Value* prim_deduce_materialize(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/materialize-expected-relation",
            "deduce/materialize!: expected (deduce 'materialize! relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/materialize-relation-not-handle",
            "deduce/materialize!: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/materialize-relation-handle-closed",
            "deduce/materialize!: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/materialize-relation-not-deduce-relation",
            "deduce/materialize!: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/materialize-database-not-open",
            "deduce/materialize!: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/materialize-relation-not-found",
            "deduce/materialize!: relation schema is not registered");
    }
    if (rel.dropped || schema.dropped) {
        return deduce_raise(interp, "deduce/materialize-relation-dropped",
            "deduce/materialize!: relation is dropped");
    }
    if (!deduce_relation_schema_has_derived_rules(rel.db, schema)) {
        return deduce_raise(interp, "deduce/materialize-relation-not-derived",
            "deduce/materialize!: relation has no derived rule heads to materialize");
    }

    SymbolId manual_policy = interp.symbols.intern("manual");
    SymbolId materialized_policy =
        schema.materialized_view && schema.materialized_refresh_policy != (SymbolId)0
            ? schema.materialized_refresh_policy
            : manual_policy;
    Value* meta_err = deduce_relation_meta_persist_state(
        rel.db,
        rel.name,
        true,
        materialized_policy,
        schema.materialized_refresh_count,
        schema.materialized_last_refresh_mutation_epoch,
        schema.materialized_last_stale_mutation_epoch,
        schema.materialized_last_stale_reason,
        false,
        interp
    );
    if (meta_err != null) return meta_err;
    schema.materialized_view = true;
    schema.materialized_refresh_policy = materialized_policy;
    rel.materialized_view = true;
    rel.materialized_refresh_policy = materialized_policy;
    rel.materialized_refresh_count = schema.materialized_refresh_count;
    rel.materialized_last_refresh_mutation_epoch = schema.materialized_last_refresh_mutation_epoch;
    rel.materialized_last_stale_mutation_epoch = schema.materialized_last_stale_mutation_epoch;
    rel.materialized_last_stale_reason = schema.materialized_last_stale_reason;
    return make_void(interp);
}

fn Value* prim_deduce_dematerialize(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/dematerialize-expected-relation",
            "deduce/dematerialize!: expected (deduce 'dematerialize! relation)");
    }

    Value* rel_val = args[0];
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-deduce-relation",
            "deduce/dematerialize!: first argument must be a deduce relation handle");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/dematerialize-relation-handle-closed",
            "deduce/dematerialize!: relation handle is closed");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/dematerialize-database-not-open",
            "deduce/dematerialize!: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-found",
            "deduce/dematerialize!: relation schema is not registered");
    }
    if (rel.dropped || schema.dropped) {
        return deduce_raise(interp, "deduce/dematerialize-relation-dropped",
            "deduce/dematerialize!: relation is dropped");
    }
    if (!schema.materialized_view) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-materialized",
            "deduce/dematerialize!: relation is not a materialized view");
    }

    Value* meta_err = deduce_relation_meta_persist_state(
        rel.db,
        rel.name,
        false,
        (SymbolId)0,
        0,
        0,
        0,
        DEDUCE_MATERIALIZED_STALE_NONE,
        true,
        interp
    );
    if (meta_err != null) return meta_err;

    schema.materialized_view = false;
    schema.materialized_refresh_policy = (SymbolId)0;
    schema.materialized_refresh_count = 0;
    schema.materialized_last_refresh_mutation_epoch = 0;
    schema.materialized_last_stale_mutation_epoch = 0;
    schema.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    deduce_relation_schema_clear_pending_incremental_insertions(schema);

    rel.materialized_view = false;
    rel.materialized_refresh_policy = (SymbolId)0;
    rel.materialized_refresh_count = 0;
    rel.materialized_last_refresh_mutation_epoch = 0;
    rel.materialized_last_stale_mutation_epoch = 0;
    rel.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    return make_void(interp);
}

fn bool deduce_refresh_select_dependency_components(
    DeduceSccPlan* plan,
    usz target_component_id,
    bool[] selected_components
) {
    if (plan == null || target_component_id >= plan.component_count) return false;
    if (selected_components.len < plan.component_count) return false;

    for (usz i = 0; i < selected_components.len; i++) selected_components[i] = false;
    selected_components[target_component_id] = true;

    bool changed = true;
    while (changed) {
        changed = false;
        foreach (edge : plan.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= plan.predicate_component.len || (usz)edge.to >= plan.predicate_component.len) continue;

            int from_component = plan.predicate_component[(usz)edge.from];
            int to_component = plan.predicate_component[(usz)edge.to];
            if (from_component < 0 || to_component < 0 || from_component == to_component) continue;
            if ((usz)from_component >= selected_components.len || (usz)to_component >= selected_components.len) continue;
            if (!selected_components[(usz)from_component] || selected_components[(usz)to_component]) continue;

            selected_components[(usz)to_component] = true;
            changed = true;
        }
    }
    return true;
}

fn bool deduce_refresh_predicate_in_selected_components(
    DeduceSccPlan* plan,
    bool[] selected_components,
    SymbolId predicate
) {
    if (plan == null || predicate == (SymbolId)0) return false;
    int predicate_index = deduce_scc_find_predicate_index(plan.predicates, predicate);
    if (predicate_index < 0 || (usz)predicate_index >= plan.predicate_component.len) return false;

    int component_id = plan.predicate_component[(usz)predicate_index];
    if (component_id < 0 || (usz)component_id >= selected_components.len) return false;
    return selected_components[(usz)component_id];
}

fn void deduce_refresh_clear_selected_dirty_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) return;

    usz write_index = 0;
    for (usz read_index = 0; read_index < db.incremental_dirty_count; read_index++) {
        SymbolId predicate = db.incremental_dirty_predicates[read_index];
        if (deduce_refresh_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        db.incremental_dirty_predicates[write_index++] = predicate;
    }
    db.incremental_dirty_count = write_index;
}

fn long deduce_refresh_count_selected_stale_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null) return 0;

    long refreshed_stale_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;

        if (deduce_relation_schema_is_materialized_stale(db, schema)) refreshed_stale_count++;
    }
    return refreshed_stale_count;
}

fn Value* deduce_refresh_selected_stale_materialized_relations(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
        if (!deduce_relation_schema_is_materialized_stale(db, schema)) continue;
        rev = make_cons(interp, make_symbol(interp, schema.name), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_refresh_stale_materialized_relations(
    DeduceDb* db,
    bool ready_only,
    Interp* interp
) {
    if (db == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (ready_only && !deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_relation_schema_is_materialized_stale(db, schema)) continue;
        rev = make_cons(interp, make_symbol(interp, schema.name), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_dirty_predicates_payload(DeduceDb* db, Interp* interp) {
    if (db == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates == null) break;
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_refresh_selected_dirty_predicates_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates == null) break;
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn void deduce_refresh_note_selected_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null) return;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
        deduce_relation_schema_note_materialized_refresh(db, schema);
    }
}

fn Value* deduce_refresh_persist_selected_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null || interp == null) return null;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
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

fn Value* deduce_refresh_try_incremental_extensional_copy_relation(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    bool* used_incremental_out,
    Interp* interp
) {
    if (used_incremental_out != null) *used_incremental_out = false;
    if (db == null || rel == null || target_schema == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid incremental refresh state");
    }
    if (db.incremental_full_recompute_required) return null;
    if (target_schema.materialized_refresh_count == 0) return null;
    if (target_schema.materialized_last_stale_reason != DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY) return null;
    if (target_schema.maintained_pending_insertions.count == 0) return null;

    SymbolId source_predicate = (SymbolId)0;
    if (!deduce_relation_schema_incremental_extensional_copy_source(db, target_schema, &source_predicate) ||
        source_predicate == (SymbolId)0) {
        source_predicate = (SymbolId)0;
        if (!deduce_relation_schema_incremental_derived_copy_source(db, target_schema, &source_predicate) ||
            source_predicate == (SymbolId)0) {
            return null;
        }
        DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
        if (source_schema == null || deduce_relation_schema_is_materialized_stale(db, source_schema)) {
            return null;
        }
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    MdbDbi target_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, rel.name, &target_dbi, &head_schema, interp) || head_schema == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: materialized relation storage is unavailable for incremental refresh");
    }

    for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
        DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
        if (tuple == null || (tuple.key_bytes == null && tuple.key_len > 0)) {
            return deduce_raise(interp, "deduce/refresh-invalid-state",
                "deduce/refresh!: invalid pending incremental tuple state");
        }

        Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
            db,
            head_schema,
            txn,
            target_dbi,
            tuple.key_bytes,
            tuple.key_len,
            interp
        );
        if (integrity_err != null) return integrity_err;

        char empty_key = 0;
        MdbVal key = {
            .mv_size = tuple.key_len,
            .mv_data = tuple.key_len > 0 ? (void*)tuple.key_bytes : &empty_key
        };
        MdbVal existing = {};
        if (mdb_get(txn, target_dbi, &key, &existing) == MDB_SUCCESS) continue;

        MdbVal data = {};
        char empty = 0;
        data.mv_size = 0;
        data.mv_data = &empty;

        rc = mdb_put(txn, target_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/refresh-put-failed",
                "deduce/refresh!: failed to insert incrementally maintained tuple");
        }

        if (head_schema.cardinality_estimate < ulong.max) {
            head_schema.cardinality_estimate++;
            if (head_schema.distinct_estimate < head_schema.cardinality_estimate) {
                head_schema.distinct_estimate = head_schema.cardinality_estimate;
            }
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn commit failed");
    }
    txn = null;

    bool queued_downstream = true;
    if (target_schema.maintained_pending_insertions.count > 0) {
        for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
            DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
            if (tuple == null) continue;
            if (!deduce_db_queue_supported_incremental_insert_from_materialized_source(
                    db,
                    rel.name,
                    tuple.key_bytes,
                    tuple.key_len
                )) {
                queued_downstream = false;
                break;
            }
        }
    }
    deduce_relation_schema_clear_pending_incremental_insertions(target_schema);
    if (!queued_downstream) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
        Value* persist_err = deduce_db_persist_materialized_view_states(db, interp);
        if (persist_err != null) return persist_err;
    }
    if (used_incremental_out != null) *used_incremental_out = true;
    return null;
}

fn Value* deduce_refresh_materialized_relation_targeted(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    Env* env,
    Interp* interp,
    long* refreshed_stale_count_out,
    Value** refreshed_relations_out,
    Value** cleared_dirty_predicates_out,
    bool* used_incremental_out
) {
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = 0;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = make_nil(interp);
    if (used_incremental_out != null) *used_incremental_out = false;
    if (db == null || rel == null || target_schema == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid relation-scoped refresh state");
    }
    if (!deduce_relation_schema_is_materialized_ready(db, target_schema)) {
        return deduce_raise(interp, "deduce/refresh-materialized-relation-not-derived",
            "deduce/refresh!: materialized relation has no derived rule heads to refresh");
    }

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    int predicate_index = deduce_scc_find_predicate_index(scc_plan.predicates, rel.name);
    if (predicate_index < 0 || (usz)predicate_index >= scc_plan.predicate_component.len) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: derived materialized relation is missing from the dependency plan");
    }

    int target_component = scc_plan.predicate_component[(usz)predicate_index];
    if (target_component < 0 || (usz)target_component >= scc_plan.component_count) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid target component for materialized relation");
    }

    bool[] selected_components = mem::new_array(bool, scc_plan.component_count);
    if (selected_components.ptr == null && scc_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/refresh-out-of-memory", "deduce/refresh!: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_refresh_select_dependency_components(&scc_plan, (usz)target_component, selected_components)) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: failed to select refresh dependency closure");
    }

    bool used_incremental = false;
    Value* incremental_err = deduce_refresh_try_incremental_extensional_copy_relation(
        db,
        rel,
        target_schema,
        &used_incremental,
        interp
    );
    if (incremental_err != null) return incremental_err;

    if (!used_incremental) {
        for (usz i = 0; i < scc_plan.component_order.len; i++) {
            usz component_id = scc_plan.component_order[i];
            if (component_id >= selected_components.len || !selected_components[component_id]) continue;
            Value* err = deduce_naive_evaluate_component(
                db,
                &scc_plan,
                component_id,
                256,
                true,
                null,
                null,
                {},
                interp
            );
            if (err != null) return err;
        }
    }

    long refreshed_stale_count =
        deduce_refresh_count_selected_stale_materialized_views(db, &scc_plan, selected_components);
    Value* refreshed_relations =
        deduce_refresh_selected_stale_materialized_relations(db, &scc_plan, selected_components, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;
    Value* cleared_dirty_predicates =
        deduce_refresh_selected_dirty_predicates_payload(db, &scc_plan, selected_components, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
    deduce_refresh_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    deduce_refresh_note_selected_materialized_views(db, &scc_plan, selected_components);
    Value* persist_err = deduce_refresh_persist_selected_materialized_views(
        db,
        &scc_plan,
        selected_components,
        interp
    );
    if (persist_err != null) return persist_err;
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = refreshed_stale_count;
    if (refreshed_relations_out != null) *refreshed_relations_out = refreshed_relations;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = cleared_dirty_predicates;
    if (used_incremental_out != null) *used_incremental_out = used_incremental;
    return null;
}

fn Value* prim_deduce_refresh(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/refresh-expected-db-or-relation",
            "deduce/refresh!: expected (deduce 'refresh! db-or-materialized-relation)");
    }

    DeduceDb* db = null;
    Relation* rel = null;
    DeduceRelationSchema* target_schema = null;
    bool relation_scope = false;
    bool target_was_stale = false;
    Value* handle = args[0];

    if (!deduce_ffi_handle_name_equals(handle, "deduce-db") &&
        !deduce_ffi_handle_name_equals(handle, "deduce-relation")) {
        return deduce_raise(interp, "deduce/refresh-handle-not-deduce-db-or-relation",
            "deduce/refresh!: first argument must be a deduce database or materialized relation handle");
    }
    if (handle.ffi_val == null || handle.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/refresh-handle-closed",
            "deduce/refresh!: handle is closed");
    }

    if (deduce_ffi_handle_name_equals(handle, "deduce-db")) {
        db = (DeduceDb*)handle.ffi_val.lib_handle;
        if (!db.open || db.env == null) {
            return deduce_raise(interp, "deduce/refresh-db-not-open",
                "deduce/refresh!: database is not open");
        }
    } else {
        relation_scope = true;
        rel = (Relation*)handle.ffi_val.lib_handle;
        if (rel.db == null || !rel.db.open || rel.db.env == null) {
            return deduce_raise(interp, "deduce/refresh-db-not-open",
                "deduce/refresh!: database is not open");
        }
        target_schema = deduce_relation_lookup_schema(rel);
        if (target_schema == null) {
            return deduce_raise(interp, "deduce/refresh-relation-not-found",
                "deduce/refresh!: relation schema is not registered");
        }
        if (rel.dropped || target_schema.dropped) {
            return deduce_raise(interp, "deduce/refresh-relation-dropped",
                "deduce/refresh!: relation is dropped");
        }
        if (!target_schema.materialized_view) {
            return deduce_raise(interp, "deduce/refresh-relation-not-materialized",
                "deduce/refresh!: relation is not a materialized view");
        }
        if (!deduce_relation_schema_is_materialized_ready(rel.db, target_schema)) {
            return deduce_raise(interp, "deduce/refresh-materialized-relation-not-derived",
                "deduce/refresh!: materialized relation has no derived rule heads to refresh");
        }
        target_was_stale = deduce_relation_schema_is_materialized_stale(rel.db, target_schema);
        db = rel.db;
    }

    long stale_materialized_view_count = 0;
    long refreshable_stale_materialized_view_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (deduce_relation_schema_is_materialized_stale(db, schema)) {
            stale_materialized_view_count++;
            if (deduce_relation_schema_is_materialized_ready(db, schema)) {
                refreshable_stale_materialized_view_count++;
            }
        }
    }

    if (relation_scope && !db.incremental_full_recompute_required) {
        long refreshed_stale_count = 0;
        Value* refreshed_relations = make_nil(interp);
        Value* cleared_dirty_predicates = make_nil(interp);
        bool used_incremental = false;
        Value* refresh_err = deduce_refresh_materialized_relation_targeted(
            db,
            rel,
            target_schema,
            env,
            interp,
            &refreshed_stale_count,
            &refreshed_relations,
            &cleared_dirty_predicates,
            &used_incremental
        );
        if (refresh_err != null) return refresh_err;
        if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
        Value* remaining_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
        if (remaining_dirty_predicates != null && remaining_dirty_predicates.tag == ERROR) return remaining_dirty_predicates;
        Value* remaining_stale_relations = deduce_refresh_stale_materialized_relations(db, false, interp);
        if (remaining_stale_relations != null && remaining_stale_relations.tag == ERROR) return remaining_stale_relations;

        Value* out = deduce_relation_schema_payload(rel, target_schema, interp);
        if (out == null || out.tag == ERROR || out.tag != HASHMAP || out.hashmap_val == null) return out;
        explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-refresh"));
        explain_dict_set_symbol(
            out,
            interp,
            "refresh-policy",
            deduce_materialized_refresh_policy_value(target_schema, interp)
        );
        explain_dict_set_symbol(out, interp, "refresh-scope", interp.symbols.intern("relation"));
        explain_dict_set_symbol(
            out,
            interp,
            "refresh-execution-path",
            interp.symbols.intern(used_incremental ? "incremental-targeted" : "targeted")
        );
        explain_dict_set(out, interp, "refresh-fallback-reason", make_nil(interp));
        explain_dict_set(out, interp, "requested-refresh-relation", make_symbol(interp, rel.name));
        explain_dict_set(out, interp, "refreshed-relation", make_symbol(interp, rel.name));
        explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, refreshed_stale_count));
        explain_dict_set(out, interp, "refreshed-materialized-relations", refreshed_relations);
        explain_dict_set(out, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
        explain_dict_set(out, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
        explain_dict_set(out, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
        return out;
    }

    Value* cleared_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
    Value* refreshed_relations = deduce_refresh_stale_materialized_relations(db, true, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;

    Value* analyze_handle = args[0];
    if (relation_scope) {
        analyze_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false);
        if (analyze_handle == null) {
            return deduce_raise(interp, "deduce/refresh-out-of-memory",
                "deduce/refresh!: out of memory");
        }
    }

    Value*[1] analyze_args = { analyze_handle };
    Value* result = prim_deduce_analyze(analyze_args[..], env, interp);
    if (result == null || result.tag == ERROR) return result;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i])) continue;
        deduce_relation_schema_note_materialized_refresh(db, &db.relation_schemas[i]);
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            db.relation_schemas[i].name,
            db.relation_schemas[i].materialized_view,
            db.relation_schemas[i].materialized_refresh_policy,
            db.relation_schemas[i].materialized_refresh_count,
            db.relation_schemas[i].materialized_last_refresh_mutation_epoch,
            db.relation_schemas[i].materialized_last_stale_mutation_epoch,
            db.relation_schemas[i].materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) return meta_err;
    }

    long remaining_stale_materialized_view_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!db.relation_schemas[i].materialized_view) continue;
        if (deduce_relation_schema_is_materialized_stale(db, &db.relation_schemas[i])) {
            remaining_stale_materialized_view_count++;
        }
    }
    Value* remaining_stale_relations = deduce_refresh_stale_materialized_relations(db, false, interp);
    if (remaining_stale_relations != null && remaining_stale_relations.tag == ERROR) return remaining_stale_relations;
    Value* remaining_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
    if (remaining_dirty_predicates != null && remaining_dirty_predicates.tag == ERROR) return remaining_dirty_predicates;

    if (!relation_scope) {
        if (result.tag == HASHMAP && result.hashmap_val != null) {
            explain_dict_set_symbol(result, interp, "kind", interp.symbols.intern("deduce-refresh"));
            explain_dict_set_symbol(result, interp, "refresh-policy", interp.symbols.intern("manual"));
            explain_dict_set_symbol(result, interp, "refresh-scope", interp.symbols.intern("database"));
            explain_dict_set_symbol(result, interp, "refresh-execution-path", interp.symbols.intern("db-wide"));
            explain_dict_set(result, interp, "refresh-fallback-reason", make_nil(interp));
            explain_dict_set(result, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count));
            explain_dict_set(result, interp, "refreshed-materialized-view-count", make_int(interp, refreshable_stale_materialized_view_count));
            explain_dict_set(result, interp, "refreshed-materialized-relations", refreshed_relations);
            explain_dict_set(result, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
            explain_dict_set(result, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
            explain_dict_set(result, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
        }
        return result;
    }

    Value* out = deduce_relation_schema_payload(rel, target_schema, interp);
    if (out == null || out.tag == ERROR || out.tag != HASHMAP || out.hashmap_val == null) return out;
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-refresh"));
    explain_dict_set_symbol(
        out,
        interp,
        "refresh-policy",
        deduce_materialized_refresh_policy_value(target_schema, interp)
    );
    explain_dict_set_symbol(out, interp, "refresh-scope", interp.symbols.intern("relation"));
    explain_dict_set_symbol(out, interp, "refresh-execution-path", interp.symbols.intern("db-wide"));
    explain_dict_set_symbol(out, interp, "refresh-fallback-reason", interp.symbols.intern("full-recompute-required"));
    explain_dict_set(out, interp, "requested-refresh-relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "refreshed-relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, refreshable_stale_materialized_view_count));
    explain_dict_set(out, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count));
    explain_dict_set(out, interp, "refreshed-materialized-relations", refreshed_relations);
    explain_dict_set(out, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
    explain_dict_set(out, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
    explain_dict_set(out, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
    return out;
}
