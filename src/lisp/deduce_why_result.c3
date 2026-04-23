module lisp;

<*
@require interp != null
*>
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

    return deduce_why_result_build_lookup_payload(
        rel,
        tuple,
        txn,
        &key_buf[0],
        encoded_key_len,
        interp
    );
}
