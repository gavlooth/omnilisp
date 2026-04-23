module lisp;

<*
@require interp != null
*>
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

    Value* out = explain_make_dict(interp, 32);
    return deduce_stats_build_payload(out, rel, schema, interp);
}
