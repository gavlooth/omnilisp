module lisp;

fn Value* prim_define_relation(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/define-relation-expected-spec", "__define-relation: expected (cons db '(name col...))");

    DeduceDb* db = null;
    SymbolId rel_name = (SymbolId)0;
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

    Value* schema_result =
        deduce_define_relation_finalize_open_relation(
            db,
            rel_name,
            rel,
            interp
        );
    if (schema_result != null && schema_result.tag == ERROR) {
        deduce_relation_free(rel);
        return schema_result;
    }
    return schema_result;
}
