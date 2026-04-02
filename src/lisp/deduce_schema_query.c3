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
    REL_OPEN_TXN_COMMIT_FAILED,
}

bool g_deduce_force_relation_open_commit_fail = false;

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

    if (g_deduce_force_relation_open_commit_fail) {
        mdb_txn_abort(txn);
        return REL_OPEN_TXN_COMMIT_FAILED;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_COMMIT_FAILED;
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
        if (open_status == REL_OPEN_TXN_COMMIT_FAILED) {
            // fault: lisp::WRITE_FAILED
            return deduce_raise(interp, "deduce/define-relation-txn-commit-failed", "__define-relation: txn commit failed");
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
        if (open_status == REL_OPEN_TXN_COMMIT_FAILED) {
            return deduce_raise(interp, "deduce/open-named-txn-commit-failed", "deduce-open-named: txn commit failed");
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
