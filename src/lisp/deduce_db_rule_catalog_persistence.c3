module lisp;

fn Value* deduce_db_restore_persisted_rule_catalog(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) return null;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-catalog-restore-txn-open-failed",
            "deduce/open: persisted rule catalog txn open failed");
    }
    defer mdb_txn_abort(txn);

    MdbDbi rule_catalog_dbi = 0;
    int dbi_rc = deduce_open_rule_catalog_dbi_rc(db, txn, false, &rule_catalog_dbi);
    if (dbi_rc == MDB_NOTFOUND) return null;
    if (dbi_rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-catalog-restore-dbi-open-failed",
            "deduce/open: persisted rule catalog dbi open failed");
    }

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rule_catalog_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/rule-catalog-restore-cursor-open-failed",
            "deduce/open: persisted rule catalog cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    deduce_db_clear_persisted_rule_catalog(db);

    MdbVal key = {};
    MdbVal data = {};
    usz record_index = 0;
    for (int op = MDB_FIRST; ; op = MDB_NEXT) {
        rc = mdb_cursor_get(cursor, &key, &data, op);
        if (rc == MDB_NOTFOUND) break;
        if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRuleCatalogRecordV1.sizeof) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-read-failed",
                "deduce/open: persisted rule catalog read failed");
        }

        char[] record = ((char*)data.mv_data)[:data.mv_size];
        DeducePersistedRuleCatalogRecordV1 header = {};
        mem::copy(&header, record.ptr, DeducePersistedRuleCatalogRecordV1.sizeof);
        if (header.version != DEDUCE_PERSISTED_RULE_CATALOG_VERSION) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-version",
                "deduce/open: persisted rule catalog has unsupported version");
        }

        usz cursor_offset = DeducePersistedRuleCatalogRecordV1.sizeof;
        char[] head_name = {};
        if (!deduce_record_slice(record, &cursor_offset, header.head_name_len, &head_name)) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                "deduce/open: persisted rule catalog record is truncated");
        }

        SymbolId head_predicate = interp.symbols.intern(head_name);
        if (head_predicate == INVALID_SYMBOL_ID) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                "deduce/open: persisted rule catalog restore out of memory");
        }

        usz body_atom_min_bytes = 0;
        if (!deduce_checked_mul_usz(DeducePersistedRuleCatalogBodyAtomV1.sizeof, header.body_count, &body_atom_min_bytes) ||
            !deduce_cursor_has(cursor_offset, body_atom_min_bytes, record.len)) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                "deduce/open: persisted rule catalog record is truncated");
        }

        SymbolId[] body_predicates = {};
        bool[] body_negated = {};
        if (header.body_count > 0) {
            body_predicates = mem::new_array(SymbolId, header.body_count);
            body_negated = mem::new_array(bool, header.body_count);
            if (body_predicates.ptr == null || body_negated.ptr == null) {
                if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
                if (body_negated.ptr != null) mem::free(body_negated.ptr);
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                    "deduce/open: persisted rule catalog restore out of memory");
            }
        }
        defer {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
        }

        for (usz i = 0; i < header.body_count; i++) {
            char[] atom_bytes = {};
            if (!deduce_record_slice(record, &cursor_offset, DeducePersistedRuleCatalogBodyAtomV1.sizeof, &atom_bytes)) {
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                    "deduce/open: persisted rule catalog record is truncated");
            }
            DeducePersistedRuleCatalogBodyAtomV1 atom = {};
            mem::copy(&atom, atom_bytes.ptr, DeducePersistedRuleCatalogBodyAtomV1.sizeof);
            char[] body_name = {};
            if (!deduce_record_slice(record, &cursor_offset, atom.name_len, &body_name)) {
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                    "deduce/open: persisted rule catalog record is truncated");
            }
            body_predicates[i] = interp.symbols.intern(body_name);
            if (body_predicates[i] == INVALID_SYMBOL_ID) {
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                    "deduce/open: persisted rule catalog restore out of memory");
            }
            body_negated[i] = atom.negated != 0;
        }
        if (cursor_offset != record.len) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                "deduce/open: persisted rule catalog record has trailing data");
        }

        if (!deduce_db_append_persisted_rule_catalog_entry(db, head_predicate, body_predicates, body_negated)) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                "deduce/open: persisted rule catalog restore out of memory");
        }
        record_index++;
    }

    return null;
}

fn Value* deduce_db_persist_rule_catalog(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) {
        return deduce_raise(interp, "deduce/rule-catalog-invalid-state",
            "deduce: invalid persisted rule catalog state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-catalog-txn-open-failed",
            "deduce: persisted rule catalog txn open failed");
    }

    MdbDbi rule_catalog_dbi = 0;
    if (!deduce_open_rule_catalog_dbi(db, txn, true, &rule_catalog_dbi)) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-catalog-open-failed",
            "deduce: persisted rule catalog dbi open failed");
    }

    rc = mdb_drop(txn, rule_catalog_dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-catalog-clear-failed",
            "deduce: persisted rule catalog clear failed");
    }

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        usz record_size = 0;
        if (!deduce_rule_catalog_record_size(sig, interp, &record_size) || record_size == 0) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-encode-failed",
                "deduce: persisted rule catalog encode failed");
        }
        char* record_buf = (char*)mem::malloc(record_size);
        if (record_buf == null) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-out-of-memory",
                "deduce: persisted rule catalog out of memory");
        }
        defer mem::free(record_buf);

        if (!deduce_encode_rule_catalog_record(sig, interp, record_buf[:record_size])) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-encode-failed",
                "deduce: persisted rule catalog encode failed");
        }

        char[ulong.sizeof] key_buf = {};
        deduce_encode_rule_catalog_key(i, &key_buf);
        MdbVal key = {
            .mv_size = key_buf.len,
            .mv_data = &key_buf[0]
        };
        MdbVal data = {
            .mv_size = record_size,
            .mv_data = record_buf
        };
        rc = mdb_put(txn, rule_catalog_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-write-failed",
                "deduce: persisted rule catalog write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-catalog-commit-failed",
            "deduce: persisted rule catalog commit failed");
    }

    return deduce_db_restore_persisted_rule_catalog(db, interp);
}
