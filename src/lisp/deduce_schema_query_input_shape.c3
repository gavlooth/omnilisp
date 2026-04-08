module lisp;
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

fn Value* deduce_relation_extract_spec(
    Value* pair,
    Interp* interp,
    DeduceDb** db_out,
    SymbolId* rel_name_out,
    Value** cols_out
) {
    if (!is_cons(pair)) return deduce_raise(interp, "deduce/define-relation-expected-cons-pair", "__define-relation: expected cons pair");

    Value* db_val = car(pair);
    if (db_val == null || db_val.tag != FFI_HANDLE) {
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
    if (!is_cons(spec)) return deduce_raise(interp, "deduce/define-relation-expected-name-columns-list", "__define-relation: expected (name col...) in cdr");
    if (car(spec).tag != SYMBOL) return deduce_raise(interp, "deduce/define-relation-name-not-symbol", "__define-relation: first element must be relation name");

    *rel_name_out = car(spec).sym_val;
    *cols_out = cdr(spec);
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
