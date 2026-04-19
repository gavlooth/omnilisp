module lisp;

import std::core::mem;

fn void deduce_db_rollback_rule_signature_restore(DeduceDb* db, usz rule_start, usz schema_start) {
    if (db == null) return;
    while (db.rule_signature_count > rule_start) {
        db.rule_signature_count--;
        deduce_rule_signature_release_allocations(&db.rule_signatures[db.rule_signature_count]);
        db.rule_signatures[db.rule_signature_count] = {};
    }
    while (db.relation_schema_count > schema_start) {
        db.relation_schema_count--;
        deduce_relation_schema_release_storage(&db.relation_schemas[db.relation_schema_count]);
        db.relation_schemas[db.relation_schema_count] = {};
    }
}

fn Value* deduce_db_restore_persisted_rule_signatures(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) return null;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-signature-restore-txn-open-failed",
            "deduce/open: persisted rule signature txn open failed");
    }
    defer mdb_txn_abort(txn);

    MdbDbi rule_signature_dbi = 0;
    int dbi_rc = deduce_open_rule_signature_dbi_rc(db, txn, false, &rule_signature_dbi);
    if (dbi_rc == MDB_NOTFOUND) return null;
    if (dbi_rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-signature-restore-dbi-open-failed",
            "deduce/open: persisted rule signature dbi open failed");
    }

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rule_signature_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/rule-signature-restore-cursor-open-failed",
            "deduce/open: persisted rule signature cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    usz restore_rule_start = db.rule_signature_count;
    usz restore_schema_start = db.relation_schema_count;
    bool committed = false;
    defer if (!committed) deduce_db_rollback_rule_signature_restore(db, restore_rule_start, restore_schema_start);

    MdbVal key = {};
    MdbVal data = {};
    usz record_index = 0;
    for (int op = MDB_FIRST; ; op = MDB_NEXT) {
        rc = mdb_cursor_get(cursor, &key, &data, op);
        if (rc == MDB_NOTFOUND) break;
        if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRuleSignatureRecordV1.sizeof) {
            return deduce_raise(interp, "deduce/rule-signature-restore-read-failed",
                "deduce/open: persisted rule signature read failed");
        }

        char[] record = ((char*)data.mv_data)[:data.mv_size];
        DeducePersistedRuleSignatureRecordV1 header = {};
        mem::copy(&header, record.ptr, DeducePersistedRuleSignatureRecordV1.sizeof);
        if (header.version != DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-version",
                "deduce/open: persisted rule signature has unsupported version");
        }

        usz cursor_offset = DeducePersistedRuleSignatureRecordV1.sizeof;
        char[] body_predicates_bytes = {};
        char[] body_negated_bytes = {};
        char[] body_bound_masks_bytes = {};
        char[] body_selected_indexes_bytes = {};
        char[] body_filter_pushdown_bytes = {};
        char[] body_projection_masks_bytes = {};
        char[] head_term_kinds_bytes = {};
        char[] head_term_aggregate_ops_bytes = {};
        char[] head_term_is_variable_bytes = {};
        char[] head_term_var_ids_bytes = {};
        char[] head_literal_bytes = {};
        char[] body_term_offsets_bytes = {};
        char[] body_term_counts_bytes = {};
        char[] body_term_is_variable_bytes = {};
        char[] body_term_var_ids_bytes = {};
        char[] body_literal_bytes = {};
        if (!deduce_record_array_slice(record, &cursor_offset, SymbolId.sizeof, header.body_count, &body_predicates_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, bool.sizeof, header.body_count, &body_negated_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, ulong.sizeof, header.body_count, &body_bound_masks_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, int.sizeof, header.body_count, &body_selected_indexes_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, bool.sizeof, header.body_count, &body_filter_pushdown_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, ulong.sizeof, header.body_count, &body_projection_masks_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, DeduceRuleTermKind.sizeof, header.head_term_count, &head_term_kinds_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, DeduceAggregateOpKind.sizeof, header.head_term_count, &head_term_aggregate_ops_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, bool.sizeof, header.head_term_count, &head_term_is_variable_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, uint.sizeof, header.head_term_count, &head_term_var_ids_bytes) ||
            !deduce_record_slice(record, &cursor_offset, header.head_literal_blob_len, &head_literal_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, usz.sizeof, header.body_count, &body_term_offsets_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, usz.sizeof, header.body_count, &body_term_counts_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, bool.sizeof, header.body_term_total_count, &body_term_is_variable_bytes) ||
            !deduce_record_array_slice(record, &cursor_offset, uint.sizeof, header.body_term_total_count, &body_term_var_ids_bytes) ||
            !deduce_record_slice(record, &cursor_offset, header.body_literal_blob_len, &body_literal_bytes) ||
            cursor_offset != record.len) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature record is malformed");
        }

        if (record_index >= db.persisted_rule_catalog_count) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature record has no matching rule catalog entry");
        }
        DeducePersistedRuleCatalogEntry* catalog_entry = &db.persisted_rule_catalog[record_index];
        if (catalog_entry.body_count != header.body_count) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature body count does not match rule catalog");
        }

        SymbolId[] body_predicates = mem::new_array(SymbolId, header.body_count);
        bool[] body_negated = mem::new_array(bool, header.body_count);
        ulong[] body_bound_masks = mem::new_array(ulong, header.body_count);
        int[] body_selected_indexes = mem::new_array(int, header.body_count);
        bool[] body_filter_pushdown = mem::new_array(bool, header.body_count);
        ulong[] body_projection_masks = mem::new_array(ulong, header.body_count);
        DeduceRuleTermKind[] head_term_kinds = mem::new_array(DeduceRuleTermKind, header.head_term_count);
        DeduceAggregateOpKind[] head_term_aggregate_ops = mem::new_array(DeduceAggregateOpKind, header.head_term_count);
        bool[] head_term_is_variable = mem::new_array(bool, header.head_term_count);
        uint[] head_term_var_ids = mem::new_array(uint, header.head_term_count);
        Value*[] head_term_literals = mem::new_array(Value*, header.head_term_count);
        usz[] body_term_offsets = mem::new_array(usz, header.body_count);
        usz[] body_term_counts = mem::new_array(usz, header.body_count);
        bool[] body_term_is_variable = mem::new_array(bool, header.body_term_total_count);
        uint[] body_term_var_ids = mem::new_array(uint, header.body_term_total_count);
        Value*[] body_term_literals = mem::new_array(Value*, header.body_term_total_count);
        if ((header.body_count > 0 &&
                (body_predicates.ptr == null || body_negated.ptr == null || body_bound_masks.ptr == null ||
                 body_selected_indexes.ptr == null || body_filter_pushdown.ptr == null ||
                 body_projection_masks.ptr == null || body_term_offsets.ptr == null ||
                 body_term_counts.ptr == null)) ||
            (header.head_term_count > 0 &&
                (head_term_kinds.ptr == null || head_term_aggregate_ops.ptr == null ||
                 head_term_is_variable.ptr == null || head_term_var_ids.ptr == null ||
                 head_term_literals.ptr == null)) ||
            (header.body_term_total_count > 0 &&
                (body_term_is_variable.ptr == null || body_term_var_ids.ptr == null ||
                 body_term_literals.ptr == null))) {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
            if (body_bound_masks.ptr != null) mem::free(body_bound_masks.ptr);
            if (body_selected_indexes.ptr != null) mem::free(body_selected_indexes.ptr);
            if (body_filter_pushdown.ptr != null) mem::free(body_filter_pushdown.ptr);
            if (body_projection_masks.ptr != null) mem::free(body_projection_masks.ptr);
            if (head_term_kinds.ptr != null) mem::free(head_term_kinds.ptr);
            if (head_term_aggregate_ops.ptr != null) mem::free(head_term_aggregate_ops.ptr);
            if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);
            if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);
            if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);
            if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);
            if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);
            if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);
            if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);
            if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);
            return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
                "deduce/open: persisted rule signature restore out of memory");
        }
        defer {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
            if (body_bound_masks.ptr != null) mem::free(body_bound_masks.ptr);
            if (body_selected_indexes.ptr != null) mem::free(body_selected_indexes.ptr);
            if (body_filter_pushdown.ptr != null) mem::free(body_filter_pushdown.ptr);
            if (body_projection_masks.ptr != null) mem::free(body_projection_masks.ptr);
            if (head_term_kinds.ptr != null) mem::free(head_term_kinds.ptr);
            if (head_term_aggregate_ops.ptr != null) mem::free(head_term_aggregate_ops.ptr);
            if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);
            if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);
            if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);
            if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);
            if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);
            if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);
            if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);
            if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);
        }

        if (header.body_count > 0) {
            for (usz i = 0; i < header.body_count; i++) {
                body_predicates[i] = catalog_entry.body_predicates[i];
                bool catalog_negated = catalog_entry.body_negated != null && catalog_entry.body_negated[i];
                bool encoded_negated = ((bool*)body_negated_bytes.ptr)[i];
                if (catalog_negated != encoded_negated) {
                    return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                        "deduce/open: persisted rule signature negation flags do not match rule catalog");
                }
                body_negated[i] = catalog_negated;
            }
            mem::copy(body_bound_masks.ptr, body_bound_masks_bytes.ptr, body_bound_masks_bytes.len);
            mem::copy(body_selected_indexes.ptr, body_selected_indexes_bytes.ptr, body_selected_indexes_bytes.len);
            mem::copy(body_filter_pushdown.ptr, body_filter_pushdown_bytes.ptr, body_filter_pushdown_bytes.len);
            mem::copy(body_projection_masks.ptr, body_projection_masks_bytes.ptr, body_projection_masks_bytes.len);
            mem::copy(body_term_offsets.ptr, body_term_offsets_bytes.ptr, body_term_offsets_bytes.len);
            mem::copy(body_term_counts.ptr, body_term_counts_bytes.ptr, body_term_counts_bytes.len);
        }
        if (header.head_term_count > 0) {
            mem::copy(head_term_kinds.ptr, head_term_kinds_bytes.ptr, head_term_kinds_bytes.len);
            mem::copy(head_term_aggregate_ops.ptr, head_term_aggregate_ops_bytes.ptr, head_term_aggregate_ops_bytes.len);
            mem::copy(head_term_is_variable.ptr, head_term_is_variable_bytes.ptr, head_term_is_variable_bytes.len);
            mem::copy(head_term_var_ids.ptr, head_term_var_ids_bytes.ptr, head_term_var_ids_bytes.len);
            usz? decoded_head_literals = decode_tuple(head_literal_bytes.ptr, head_literal_bytes.len, head_term_literals, interp);
            if (catch err = decoded_head_literals) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature head literal decode failed");
            }
            if (decoded_head_literals != header.head_term_count) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature head literal decode failed");
            }
        }
        if (header.body_term_total_count > 0) {
            mem::copy(body_term_is_variable.ptr, body_term_is_variable_bytes.ptr, body_term_is_variable_bytes.len);
            mem::copy(body_term_var_ids.ptr, body_term_var_ids_bytes.ptr, body_term_var_ids_bytes.len);
            usz? decoded_body_literals = decode_tuple(body_literal_bytes.ptr, body_literal_bytes.len, body_term_literals, interp);
            if (catch err = decoded_body_literals) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature body literal decode failed");
            }
            if (decoded_body_literals != header.body_term_total_count) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature body literal decode failed");
            }
        }

        DeduceRuleRegisterStatus register_status =
            deduce_db_add_rule_signature(
                db,
                catalog_entry.head_predicate,
                body_predicates,
                body_negated,
                body_bound_masks,
                body_selected_indexes,
                body_filter_pushdown,
                body_projection_masks,
                head_term_kinds,
                head_term_aggregate_ops,
                head_term_is_variable,
                head_term_var_ids,
                head_term_literals,
                body_term_offsets,
                body_term_counts,
                body_term_is_variable,
                body_term_var_ids,
                body_term_literals,
                header.max_var_id
            );
        if (register_status != DEDUCE_RULE_REGISTER_OK) {
            return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
                "deduce/open: persisted rule signature restore out of memory");
        }
        Value* schema_err = deduce_db_ensure_restored_rule_signature_schemas(
            db,
            &db.rule_signatures[db.rule_signature_count - 1],
            interp
        );
        if (schema_err != null) return schema_err;
        record_index++;
    }

    committed = true;
    return null;
}
